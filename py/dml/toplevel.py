# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Launch the parser, and expand imports

import re
import os
import sys

import bz2
import hashlib
import pickle

from ply import lex, yacc

from . import objects, logging, codegen, ctree, ast
from . import symtab
from .messages import *
from .logging import *
import dml.globals
import dml.dmllex
import dml.dmlparse

__all__ = ('produce_dmlast', 'get_parser', 'parse_main_file')

# Add the current directory to the search path, to find the parsetab module
sys.path.append('.')

version_warning = True
supported_versions = [(1, 2), (1, 3), (1, 4)]

space_and_comments = r'\s*(?:(?:(?://.*$)|(?:/\*(?:[^*]|\*+[^/*])*\*+/))\s*)*'

has_version = re.compile('(%sdml%s)' % (space_and_comments, space_and_comments),
                         re.M | re.X)

check_version = re.compile(r'(?P<major>\d+)\.(?P<minor>\d+)%s;'
                           % (space_and_comments,), re.M | re.X)

def fmt_version(data):
    (major, minor) = data
    return "%d.%d" % (major, minor)

parsers = {}

def get_parser(version, tabmodule=None, debugfile=None):
    if version in parsers:
        return parsers[version]

    lexer = lex.lex(module = dml.dmlparse.lexers[version],
                    optimize = 0)#not debug_mode)
    parser = yacc.yacc(
        module = dml.dmlparse.grammars[version],
        method='LALR',
        tabmodule=(tabmodule
                   or 'dml.dml%s_parsetab' % (''.join(map(str, version)),)),
        debug = debugfile is not None,
        debugfile = debugfile,
        optimize = 0)#not debug_mode)

    parsers[version] = (lexer, parser)
    return (lexer, parser)

def determine_version(filestr, filename):
    # Look for version tag
    m = has_version.match(filestr)
    if m:
        ver_start = m.end(0)
        lineno = filestr[:ver_start].count('\n') + 1
        column = ver_start - filestr.rfind('\n', 0, ver_start)
        m = check_version.match(filestr, pos=ver_start)
        if not m:
            raise ESYNTAX(SimpleSite("%s:%d:%d" % (filename, lineno, column)),
                          None, "malformed DML version tag")
        version = (int(m.group('major')), int(m.group('minor')))
        # Remove the language version tag, but preserve newlines.
        # This helps us avoid having to parse the version tag in the
        # grammar.
        ver_end = m.end(0)
        filestr = ' ' * ver_end + filestr[ver_end:]
    else:
        # TODO: make this an error
        report(WNOVER(SimpleSite("%s:1" % (filename,))))
        version = (1, 2)
        lineno = 1
        column = 1

    if version not in supported_versions:
        raise ESYNTAX(SimpleSite("%s:%d:%d" % (filename, lineno, column)), None,
                      "DML version %s not supported; allowed: %s"
                      % (fmt_version(version),
                         ", ".join(map(fmt_version, supported_versions))))
    if version == (1, 3):
        report(WDEPRECATED(
            SimpleSite("%s:%d:%d" % (filename, lineno, column)),
            "'dml 1.3' is a deprecated alias of dml 1.4"))
        version = (1, 4)

    if logging.show_porting and version == (1, 2):
        report(PVERSION(SimpleSite("%s:%d:%d" % (filename, lineno, column))))

    return version, filestr

def parse(s, file_info, filename, version):
    #sys.stderr.write("parsing %s\n" % filename)
    lexer, parser = get_parser(version)
    parser.filename = filename
    parser.file_info = file_info
    lexer.filename = filename
    lexer.file_info = file_info
    lexer.lineno = 1
    try:
        ast = parser.parse(s, lexer = lexer, tracking = True)
    except dml.dmlparse.UnexpectedEOF:
        raise ESYNTAX(DumpableSite(file_info, file_info.size()),
                       None, "unexpected end-of-file")
    return ast

def scan_statements(filename, site, stmts):
    """Categorize and preprocess the list of top-level AST statements, and
    return a tuple: (imports, headers, footers, global-defs, spec)
    where global-defs is ASTs of top-level definitions (typedefs, constants,
    templates, extern), and spec is the device-level ObjectSpec."""
    headers = []
    footers = []
    imports = []
    global_defs = []
    spec_asts = [ast.is_(site, [(site, 'device')])]
    for s in stmts:
        if s.kind == 'import':
            [i_filename] = s.args
            imports.append((i_filename, s.site))
            spec_asts.append(ast.is_(s.site, [(s.site, '@' + i_filename)]))
        elif s.kind == 'header':
            [text, impl] = s.args
            headers.append(ctree.mkCText(s.site, text, impl))
        elif s.kind == 'footer':
            [text] = s.args
            footers.append(ctree.mkCText(s.site, text))
        elif s.kind == 'toplevel_if':
            [cond, tbranch, fbranch] = s.args
            scope = symtab.Symtab()
            bsite = SimpleSite('<builtin>', dml_version=dml.globals.dml_version)
            scope.add(ctree.ExpressionSymbol(
                'dml_1_2',
                ctree.BoolConstant(bsite, dml.globals.dml_version == (1, 2)),
                bsite))
            try:
                expr = ctree.as_bool(codegen.codegen_expression(
                    cond, None, scope))
                if not expr.constant:
                    raise ENCONST(expr.site, expr)
            except DMLError as e:
                report(e)
            else:
                (sub_imports, sub_headers, sub_footers, sub_global_defs,
                 sub_spec_asts) = scan_statements(
                     filename, s.site, tbranch if expr.value else fbranch)
                imports.extend(sub_imports)
                headers.extend(sub_headers)
                footers.extend(sub_footers)
                global_defs.extend(sub_global_defs)
                spec_asts.extend(sub_spec_asts)
        elif s.kind in {'constant', 'dml_typedef', 'extern', 'extern_typedef',
                        'loggroup', 'struct', 'template', 'template_dml12'}:
            global_defs.append(s)
        else:
            spec_asts.append(s)
    return (imports, headers, footers, global_defs, spec_asts)

bidi_re = re.compile(
    '[\u2066-\u2069\u202a-\u202e]')

def parse_file(dml_filename):
    try:
        with open(dml_filename, 'r') as f:
            filestr = f.read()
    except IOError as msg:
        raise EIMPORT(SimpleSite("%s:0" % (dml_filename,)),
                      "%s: %s" % (dml_filename, msg))
    except UnicodeDecodeError:
        with open(dml_filename, 'rb') as f:
            for (lineno, line) in enumerate(f):
                try:
                    line.decode('utf-8')
                except UnicodeDecodeError as e:
                    raise ESYNTAX(
                        SimpleSite("%s:%d:%d"
                                   % (dml_filename, lineno + 1, e.start + 1)),
                        repr(line[e.start:e.end]),
                        'utf-8 decoding error: ' + e.reason)
        # should not happen
        raise
    # Plug "Trojan Source" attack by completely disallowing BiDi characters
    for m in bidi_re.finditer(filestr):
        lineno = filestr[:m.start()].count('\n') + 1
        col = m.start() - filestr.rfind('\n', 0, m.start())
        report(ESYNTAX(SimpleSite(f"{dml_filename}:{lineno}:{col}"),
                       repr(m.group())[1:-1],
                       "Unicode BiDi character not allowed"))
    version, contents = determine_version(filestr, dml_filename)
    file_info = logging.FileInfo(dml_filename, version, None)
    if version == (1, 2) and logging.show_porting:
        with open(dml_filename, 'rb') as f:
            sha1 = hashlib.sha1(f.read()).hexdigest()  # nosec
        report(PSHA1(SimpleSite('%s:1:0' % (dml_filename,)), sha1))
    ast = parse(contents, file_info, dml_filename, version)
    return ast

def load_dmlast(ast_filename):
    '''Return a previously compiled AST, or None'''
    try:
        return pickle.loads(bz2.BZ2File(ast_filename).read())  # nosec
    except Exception as e:
        raise ICE(SimpleSite(ast_filename),
                   "Failed to load AST from %r: %s"
                   % (ast_filename, e))

def save_dmlast(parsedata, dmlast_filename):
    pickle.dump(parsedata, bz2.BZ2File(dmlast_filename, 'wb'), -1)

def produce_dmlast(dml_file):
    full_contents = dml_file.read_text()
    version, body = determine_version(full_contents, dml_file)
    file_info = logging.FileInfo(dml_file.resolve(), version)
    try:
        parsedata = parse(body, file_info, dml_file, version)
    except logging.DMLError as e:
        report(e)
        sys.exit(2)
    if logging.failure:
        sys.exit(2)
    file_info.set_name(None)
    save_dmlast((file_info, parsedata), dml_file.with_suffix('.dmlast'))

def parse_dmlast_or_dml(dml_filename):
    ast_filename = dml_filename + 'ast'
    if os.path.exists(ast_filename):
        # 10 seconds of fuzz, in case someone copies the installation
        # on a slow filesystem. Happened at least twice, see bug 17707
        if os.stat(ast_filename).st_mtime + 10 < os.stat(dml_filename).st_mtime:
            # This detects a common error: after getting a compile
            # error in dml-builtins.dml, one accidentally edits the
            # copy in [host]/bin/dml/, instead of the one in the repo.
            report(WOLDAST(dml_filename))
        else:
            file_info, parsedata = load_dmlast(ast_filename)
            if file_info.name is None:
                file_info.set_name(dml_filename)
            return parsedata
    return parse_file(dml_filename)

def find_file_in_dirs(file, dirs):
    for dir in dirs:
        path = os.path.join(dir, file)
        if os.path.exists(path):
            return path
    return None

def import_file(importsite, path):
    tag, site, name, stmts = parse_dmlast_or_dml(path)

    version = site.dml_version()
    if (version != dml.globals.dml_version
        and (version, dml.globals.dml_version) != ((1, 4), (1, 2))):
        raise EVERS(SimpleSite("%s:0" % (path,)),
                     importsite,
                     fmt_version(version),
                     fmt_version(importsite.dml_version()))

    # this is the only version this compiler supports now
    assert version in ((1, 2), (1, 4))

    if name is not None:
        raise EDEVIMP(importsite)
    return (site, stmts)

def exists(filename):
    "Wrapper around os.path.exists"
    # Work around a bug in python (http://bugs.python.org/issue1311)
    if filename == os.devnull:
        return True
    return os.path.exists(filename)

def parse_main_file(inputfilename, explicit_import_path, strict):
    if not exists(inputfilename):
        raise ENOFILE(SimpleSite("%s:0" % (inputfilename,)))
    (kind, site, name, stmts) = parse_dmlast_or_dml(inputfilename)
    # guaranteed by grammar
    assert kind == 'dml'

    version = site.dml_version()
    dml.globals.dml_version = version
    version_str = fmt_version(version)
    dml.globals.compat_dml12 = version == (1, 2) and not strict

    implicit_imports = [
        ast.import_(site, "dml-builtins.dml"),
        ast.import_(site, "simics/device-api.dml")]

    (unimported, headers, footers, global_defs, spec_asts) = scan_statements(
        inputfilename, site, implicit_imports + stmts)
    # Transform object specifications into an (automatically
    # instantiated) template, to handle multiple overrides smoothly
    global_defs.append(ast.template_dml12(site, '@' + inputfilename, spec_asts))

    if name is None:
        raise EDEVICE(site)

    # Also look in a subdir named like the DML version
    import_path = [
        path
        for orig_path in explicit_import_path + [os.path.dirname(inputfilename)]
        for path in [os.path.join(orig_path, version_str),
                     orig_path]]

    if version == (1, 2) and dml.globals.api_version not in (
            # we may want to add "7" if we want to postpone the
            # deprecation of DML 1.2
            "4.8", "5", "6", "internal"):
        raise ESIMAPI(site, fmt_version(version),
                       dml.globals.api_version)

    # Map normalized, absolute path of an imported file, to list of
    # seen spellings. One spelling is a string in an import statement which
    # resolves to the file.
    imported = {}
    # List of dependencies
    deps = []

    while unimported:
        (importfile, importsite) = unimported.pop()

        if (importfile.startswith('./')
            or (importfile.startswith('../')
                and importsite.dml_version() != (1, 2))):
            path = importsite.filename()
            if not os.path.isfile(path):
                # I don't think this can happen
                raise ICE(importsite, 'relative import from non-file?')
            path = os.path.join(os.path.dirname(path), importfile)
        else:
            path = find_file_in_dirs(importfile, import_path)
        try:
            if path is None:
                raise EIMPORT(importsite, importfile)

            path = os.path.abspath(os.path.realpath(path))
            normalized = os.path.normcase(path)
            if normalized in imported:
                # Already imported
                if importfile not in imported[normalized]:
                    global_defs.append(ast.template_dml12(
                        importsite, '@' + importfile,
                        [ast.is_(
                            importsite,
                            [(importsite, '@' + imported[normalized][0])])]))
                    imported[normalized].append(importfile)
                continue

            imported[normalized] = [importfile]
            deps.append(path)

            (i_site, i_stmts) = import_file(importsite, path)
        except DMLError as e:
            report(e)
            global_defs.append(ast.template_dml12(
                importsite, '@' + importfile, []))
            continue
        (i_imports, i_headers, i_footers, i_global_defs,
         spec_asts) = scan_statements(importfile, i_site, i_stmts)
        # Transform object specifications into an (automatically
        # instantiated) template, to handle multiple overrides smoothly
        global_defs.append(ast.template_dml12(i_site, '@' + importfile,
                                              spec_asts))
        unimported.extend(i_imports)
        headers[0:0] = i_headers
        footers[0:0] = i_footers
        global_defs.extend(i_global_defs)

    return (version, name, headers, footers, global_defs, '@' + inputfilename,
            deps)
