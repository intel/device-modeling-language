# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys, os, traceback, re
import argparse
import tempfile
import shutil
import time
from pathlib import Path

from . import structure, logging, messages, ctree, ast, expr_util, toplevel
from . import serialize
from . import dmlparse
from . import output
from . import compat

import dml.c_backend
import dml.info_backend
import dml.g_backend
import dml.globals
import dml.dmlparse
from .logging import *
from .messages import *
from .env import api_versions, default_api_version
import tarfile

def prerr(msg):
    sys.stderr.write(msg + "\n")

if sys.platform == "cygwin":
    prerr("dmlc is not a cygwin application")
    sys.exit(1)

outputbase = None
output_c = True


# Ignore some warnings by default
ignore_warning('WASSERT')
ignore_warning('WNDOC')
ignore_warning('WSHALL')
ignore_warning('WUNUSED')
ignore_warning('WNSHORTDESC')

if os.getenv('DMLC_DEBUG'):
    debug_mode = True
else:
    debug_mode = False

# Resource module is optional
try:
    import resource
except ImportError:
    pass

time_dmlc = False
lasttime = 0.0
def logtime(lbl):
    # set time_dmlc to true to enable timing log
    if not time_dmlc:
        return
    global lasttime

    if lbl == 'total':
        lasttime = 0.0

    rusage = resource.getrusage(resource.RUSAGE_SELF)
    now = rusage[0] + rusage[1]
    sys.stderr.write("   %-10s %2.3f\n" % (lbl, now - lasttime))
    lasttime = now

def process(devname, global_defs, top_tpl, extra_params):
    if extra_params:
        # Parameters from the command-line are induced as a template
        # inheriting the main file, thereby overriding any default
        # declarations from there
        param_tpl = '@<command-line>'
        param_site = SimpleSite("<command-line>:0",
                                dml_version=dml.globals.dml_version)
        top_site = SimpleSite(top_tpl[1:] + ':1',
                              dml_version=dml.globals.dml_version)
        global_defs.append(ast.template_dml12(
            top_site, param_tpl, [
                ast.param(param_site, name, None, True, None)
                for name in extra_params] + [
                ast.param(param_site, name, None, False, extra_params[name])
                for name in extra_params]
            + [ast.is_(top_site, [(top_site, top_tpl)])]))
        top_tpl = param_tpl

    # Evaluate globals
    structure.mkglobals(global_defs)

    top_tpl = dml.globals.templates[top_tpl]
    # Create device tree
    return structure.mkdev(devname, [top_tpl.spec])

# Enable this with a call to sys.settrace(mytrace) to get some tracing
import inspect
trace_lvl = 0
def mytrace(frame, event, arg):
    sys.settrace(None)
    if event in {'call', 'return'}:
        filename, lineno, fn, ctx, lidx = inspect.getframeinfo(frame)
        if re.match(r'.*/(codegen|c_backend|ctree)', filename):
            global trace_lvl
            if event == 'call':
                dbg('%s%s %r {' % (" " * trace_lvl, fn, arg))
                trace_lvl += 1
            if event == 'return':
                trace_lvl -= 1
                dbg('%s} => %r' % (" " * trace_lvl, arg))
    sys.settrace(mytrace)
    return mytrace

def parse_define(arg):
    "Parse a parameter assignment given as a -D option"
    define_site = SimpleSite('<command-line>:0',
                             dml_version=dml.globals.dml_version)
    (lexer, _) = toplevel.get_parser((1, 4))
    lexer.filename = filename = "<command-line>"
    lexer.file_info = logging.FileInfo(filename, (1, 4), content_lines=[arg])
    lexer.input(arg)
    tokens = list(lexer)
    if len(tokens) < 3 or tokens[1].type != 'EQUALS':
        raise ESYNTAX(define_site, '-D' + arg, "expected name=value")
    (ident, _, literal) = tokens[:3]
    if ident.type != 'ID':
        raise ESYNTAX(define_site, '-D' + arg,
                      "invalid identifier: %s" % (ident.value,))

    if literal.type == 'FCONST':
        value = ast.float(define_site, literal.value)
    elif literal.type in {'HCONST', 'BCONST', 'ICONST'}:
        value = ast.int(define_site, literal.value)
    elif literal.type == 'SCONST':
        value = ast.string(define_site, literal.value)
    elif literal.type == 'ID' and literal.value in {'true', 'false'}:
        value = ast.variable(define_site, literal.value)
    else:
        raise ESYNTAX(define_site, '-D' + arg,
                      "Invalid literal %s" % (literal.value,))
    if len(tokens) > 3:
        raise ESYNTAX(define_site, '-D' + arg,
                      "garbage after literal: " + str(tokens[3].value))

    return (ident.value, value)

def print_cdep(outputbase, headers, footers):
    '''Generate -cdep.c, which is a minimal C file, which contains the
    same imports as -dml.c and thus can be used for dependency
    generation'''
    f = output.FileOutput(outputbase + '-cdep.c')
    with f:
        with output.allow_linemarks():
            for block in headers + footers:
                block.toc()
    f.close()
    f.commit()

def dotdot_depth(path):
    '''return how many levels of parent directories are required to resolve a
    relative path'''
    return os.path.normpath(path).count('..' + os.path.sep)

assert dotdot_depth('./../../foo/../../../bar.dml') == 4

def dump_input_files(outputbase, imported):
    # DML is often invoked within a complex build system, with a large
    # number of -I flags and many auto-generated files. It is often
    # hard to reproduce a DML issue outside of this build system. This
    # function tries to save all relevant DML files in a tree that is
    # buildable or near-buildable, by placing all DML files in the
    # same directory. For each files that is imported with a relative
    # path, a back-pointing symlink is created on that relative path.
    max_dotdot = max(dotdot_depth(p)
                     for (_, paths) in imported.items()
                     for p in paths)
    # HACK: if any file is imported via ../, then store all DML files
    # sufficiently deep in a directory hierarchy _/_/.../, so that the
    # .. path can be resolved within the archive.
    prefix = '/'.join(['_'] * max_dotdot)
    # map basename to one of its absolute paths
    basenames: dict[str, str] = {}
    with tarfile.open(outputbase + ".tar.bz2", 'w:bz2') as tf:
        # HACK: if two different files foo.dml are imported, where one is
        # imported as "foo.dml" or "./foo.dml" and the other is only
        # imported using a qualified path such as "bar/foo.dml", then
        # the rename hack below has a better chance to work if we process
        # the unqualified paths first.
        for f in sorted(imported, key=lambda f: min(map(len, imported[f]))):
            paths = imported[f]
            base = os.path.basename(f)
            while base in basenames:
                if Path(f).read_bytes() == Path(basenames[base]).read_bytes():
                    # One file imported using two different paths.
                    # Keep one copy only.
                    break
                # HACK: all DML files end up in the same directory; if
                # two different files have the same name, then dodge
                # that by renaming. In this case we may need to change
                # some import directive manually in order for all
                # files to be imported.
                base = '_' + base
            else:
                basenames[base] = f
                path_in_tar = f'{prefix}/{base}'
                tf.add(f, path_in_tar)
                (_, hfile) = ctree.dmldir_macro(f)
                if os.path.exists(hfile):
                    (_, h_path_in_tar) = ctree.dmldir_macro(path_in_tar)
                    tf.add(hfile, h_path_in_tar)
            for path in paths:
                symlink = os.path.normpath(os.path.join(prefix, path))
                if (os.path.normpath(os.path.dirname(symlink))
                    == os.path.normpath(prefix)):
                    # import path resolves to file already present in tarfile
                    continue
                # file imported through relative path, create symlink where
                # the relative path is resolved
                ti = tarfile.TarInfo(symlink)
                ti.type = tarfile.SYMTYPE
                ti.linkname = os.path.normpath(os.path.join(
                    '../' * (len(symlink.split(os.sep)) - 1) + prefix, base))
                tf.addfile(ti)

def unexpected_error(exc_type, exc_value, exc_traceback):
    if debug_mode:
        traceback.print_exception(exc_type, exc_value, exc_traceback)

    with open("dmlc-error.log", "w") as dumpfile:
        traceback.print_exception(
            exc_type, exc_value, exc_traceback, file=dumpfile)
    prerr("*** An unexpected dmlc error occurred!")
    prerr("    Please report this incident, together with the details")
    prerr("    found in the dmlc-error.log file.")

def flush_porting_log(tmpfile, porting_filename):
    '''Flush logs saved in '''
    # Use directory creation as a platform-independent mutex
    # to avoid simultaneous flushing during a parallel build
    lockfile = porting_filename + '.lock'
    try:
        for tries in range(100):
            try:
                # trylock
                os.mkdir(lockfile)
            except OSError:
                time.sleep(0.1)
            else:
                break
        else:
            raise Exception('Cannot lock porting log, possibly'
                            + ' stale lockfile (%s). Removing.' % (lockfile,))
        # lock acquired!
        tmpfile.seek(0)
        with open(porting_filename, 'a') as f:
            shutil.copyfileobj(tmpfile, f)
    finally:
        # this will also remove any stale lockfile
        # (so retrying compile will succeed)
        os.rmdir(lockfile)

class HelpAction(argparse.Action):
    def __init__(self, option_strings, dest, **kwargs):
        super().__init__(option_strings, dest, nargs=0, **kwargs)

    def __call__(self, parser, namespace, values, option_string=None):
        self.print_help()
        parser.exit()

class WarnHelpAction(HelpAction):
    def print_help(self):
        print('''Tags accepted by --warn and --nowarn:''')
        by_ignored = {True: [], False: []}
        for tag in sorted(messages.warnings):
            by_ignored[warning_is_ignored(tag)].append(tag)
        print('  Enabled by default:')
        for tag in by_ignored[False]:
            print(f'    {tag}')
        print('  Disabled by default:')
        for tag in by_ignored[True]:
            print(f'    {tag}')

class CompatHelpAction(HelpAction):
    def print_help(self):
        print('''\
Tags accepted by --no-compat. Each of these represents a deprecated
compatibility feature that will be unavailable in all API versions
newer than a particular version.  The --no-compat=TAG flag disables a
feature also when an older API version is used. This allows migration
to a new API version in smaller steps, and can also allow disabling
features that are scheduled for removal in a future API version.''')
        by_version = {}
        for feature in compat.features.values():
            if (feature.last_api_version.str in api_versions()
                or (feature.last_api_version
                    > compat.apis[default_api_version()])):
                by_version.setdefault(feature.last_api_version, []).append(
                    feature)
        longest_name = max(len(name) for name in compat.features)
        for (api, features) in sorted(by_version.items()):
            print(f'  Features available with --simics-api={api.str}'
                  ' or older:')
            for feature in sorted(features, key=lambda f: f.tag()):
                print(f'    {feature.tag().ljust(longest_name)} '
                      + feature.short)

def main(argv):
    # DML files must be utf8, but are generally opened without specifying
    # the 'encoding' arg. This works only if utf8_mode is enabled.
    assert sys.flags.utf8_mode
    parser = argparse.ArgumentParser()

    # <add id="dmlc options">
    # <name>Command Line Options</name>
    # The following are the available command line options to
    # <file>dmlc</file>:
    # <dl>

    # <dt>-h, --help</dt>
    # <dd>Print usage help.</dd>

    # <dt>-I <i>path</i></dt>
    # <dd>Add <arg>path</arg> to the search path for imported
    # modules.</dd>
    parser.add_argument(
        '-I', dest='import_path', action='append',
        metavar='PATH',
        default=[],
        help='add PATH to import search path')

    # <dt>-D <i>name</i>=<i>definition</i></dt>
    # <dd>Define a compile-time parameter.  The definition
    # must be a literal expression, and can be a quoted
    # string, a boolean, an integer, or a floating point
    # constant. The parameter will appear in the top-level scope.</dd>
    parser.add_argument(
        '-D', dest='defines', action='append',
        metavar='NAME=VALUE',
        default=[],
        help='set compile time constant NAME to VALUE')

    # <dt>--dep</dt>
    # <dd>Output makefile rules describing dependencies.</dd>
    parser.add_argument(
        '--dep', action='store',
        help='generate makefile dependencies')

    # <dt>--no-dep-phony</dt>
    # <dd>With --dep, avoid addition of a phony target for each dependency
    # other than the main file.</dd>
    parser.add_argument(
        '--no-dep-phony', action='store_true',
        help='With --dep, avoid addition of a phony target for each'
        + ' dependency other than the main file.')

    # <dt>--dep-target</dt>
    # <dd>With --dep, change the target of the rule emitted by dependency
    # generation. Specify multiple times to have multiple targets.</dd>
    parser.add_argument(
        '--dep-target', action='append', metavar='TARGET', default=[],
        help='With --dep, change the target of the rule emitted by'
        + ' dependency generation. Specify multiple times to have multiple'
        + ' targets.')

    # <dt>-T</dt>
    # <dd>Show tags on warning messages. The tags can be used with
    # the <tt>--nowarn</tt> and <tt>--warn</tt> options.</dd>
    parser.add_argument(
        '-T', dest='include_tag', action='store_true',
        help='show tags on warning messages')

    # Deprecated with SIMICS_API > 4.8
    parser.add_argument(
        '-m', dest='full_module', action='store_true',
        help=argparse.SUPPRESS)

    # <dt>-g</dt>
    # <dd>Generate artifacts that allow for easier source-level debugging.
    # This generates a DML debug file leveraged by debug-simics, and
    # causes generated C code to follow the DML code more closely.</dd>
    parser.add_argument(
        '-g', dest='debuggable', action='store_true',
        help='generate artifacts and C code that allow for easier debugging')

    # <dt>--warn=<i>tag</i></dt>
    # <dd>Enable selected warnings. The tags can be found using
    # the <tt>-T</tt> option.</dd>
    parser.add_argument(
        '--warn', dest='enabled_warnings', action='append',
        metavar='TAG',
        default=[],
        help='enable warning TAG')

    # <dt>--nowarn=<i>tag</i></dt>
    # <dd>Suppress selected warnings. The tags can be found using
    # the <tt>-T</tt> option.</dd>
    parser.add_argument(
        '--nowarn', dest='disabled_warnings', action='append',
        metavar='TAG',
        default=[],
        help='disable warning TAG')

    parser.add_argument('--help-warn', action=WarnHelpAction,
                        help='List warning tags available for --warn/--nowarn')
    # <dt>--werror</dt>
    # <dd>Turn all warnings into errors.</dd>
    parser.add_argument('--werror', action='store_true',
                      help='Turn all warnings into errors')

    parser.add_argument('--strict-dml12', action='store_true',
                        help='Alias for --no-compat=dml12_inline'
                        ',dml12_not,dml12_goto,dml12_misc,dml12_int')
    parser.add_argument('--strict-int', action='store_true',
                        help='Alias for --no-compat=dml12_int')

    # <dt>--coverity</dt>
    # <dd> Adds Synopsys® Coverity® analysis annotations to suppress common
    # false positives in generated C code created from DML 1.4 device models.
    # </dd>
    parser.add_argument(
        '--coverity', action='store_true',
        help=('Adds Synopsys® Coverity® analysis annotations to suppress '
                + 'common false positives in generated C code created from '
                + 'DML 1.4 device models.'))
    # <dt>--noline</dt>
    # <dd>Suppress line directives for the C preprocessor so
    # that the C code can be debugged.</dd>
    parser.add_argument(
        '--noline', action='store_true',
        help='suppress line directives in generated C code')

    # <dt>--info</dt>
    # <dd>Enable the output of an XML file describing register layout.</dd>
    parser.add_argument(
        '--info', action='store_true',
        help='generate XML file describing register layout')

    # <dt>--simics-api=<i>version</i></dt>
    # <dd>Use Simics API version <i>version</i>.</dd>
    parser.add_argument(
        '--simics-api', action='store',
        metavar='VERSION',
        default=str(default_api_version()),
        help=('specify Simics API version (default %s)'
              % default_api_version()))

    # <dt>--max-errors=<i>N</i></dt>
    # <dd>Limit the number of error messages to <i>N</i>.</dd>
    parser.add_argument(
        '--max-errors', action='store',
        metavar='N',
        default="0",
        help=('Limit the number of error messages to N'))

    # </dl>
    # </add>

    # <dt>--no-compat=<i>TAG</i></dt>
    # <dd>Disable a compatibility feature</dd>
    parser.add_argument(
        '--no-compat', action='append', default=[],
        help='Disable a compatibility feature')

    parser.add_argument(
        '--help-no-compat', action=CompatHelpAction,
        help='List the available tags for --no-compat')

    # </dl>
    # </add>

    # Legacy: write deps to stdout, and assume it's redirected to <file>.dmldep
    # Should be removed in 6
    parser.add_argument(
        '-M', dest="makedep_old", action='store_true',
        help=argparse.SUPPRESS)

    parser.add_argument(
        '-P', dest='porting_filename', action='store',
        help="""Append messages to file with tags for automatic porting
        to DML 1.4""")

    parser.add_argument(
        '--state-change-dml12', action='store_true',
        help=argparse.SUPPRESS)

    # Generate multiple C files, splitting at the specified file size
    parser.add_argument(
        '--split-c-file', action='store', default='0',
        help=argparse.SUPPRESS)

    # Enable features for internal testing
    parser.add_argument(
        '--enable-features-for-internal-testing-dont-use-this',
        dest='enable_testing_features', action='store_true',
        help=argparse.SUPPRESS)

    parser.add_argument(
        'input_filename',
        help="Main DML file to compile. Must have a `device` statement"
        " on top level.")
    parser.add_argument(
        'output_base', nargs="?",
        help="Prefix for names of generated files. '.c' is appended to the"
        " name of the main C file. Defaults to input_filename with the"
        " '.dml' suffix stripped.")
    options = parser.parse_args(argv[1:])

    global outputbase, output_c

    if options.include_tag:
        set_include_tag(True)

    if options.debuggable:
        dml.globals.debuggable = True

    defs = {}
    for d in options.defines:
        try:
            (name, value) = parse_define(d)
        except ESYNTAX as e:
            report(e)
        else:
            defs[name] = value

    api = dml.globals.api_version = compat.apis.get(options.simics_api)
    if api is None:
        parser.error(f"dmlc: the version '{options.simics_api}'"
                     " is not a valid API version")

    if options.full_module and api != compat.api_4_8:
        parser.error("dmlc: the -m option is only valid together with --api=4.8"
                     " or older")

    if options.werror:
        DMLWarning.enable_werror()

    try:
        logging.max_errors = int(options.max_errors)
    except ValueError:
        prerr("dmlc: Expected integer for --max-errors, got %r"
              % (options.max_errors))
        sys.exit(1)

    try:
        size = int(options.split_c_file)
        if size < 0:
            raise ValueError()
        dml.c_backend.c_split_threshold = size
    except ValueError:
        prerr("dmlc: Expected positive integer for --split-c-file, got %r"
              % (options.split_c_file,))
        sys.exit(1)

    dml.globals.coverity = options.coverity

    dml.globals.linemarks_enabled = not options.noline

    dml.globals.state_change_dml12 = options.state_change_dml12

    dml.globals.enable_testing_features = options.enable_testing_features

    if dml.globals.enable_testing_features:
        prerr("dmlc: ***Features for internal testing are enabled!!! "
              + "These are near-guaranteed to NOT SURVIVE in their current "
              + "form. They are COMPLETELY UNSUPPORTED. "
              + "The DMLC developers WILL NOT respect their use. "
              + "NEVER enable this flag for any kind of production code!!!***")

    features = {tag: feature
                for (tag, feature) in compat.features.items()
                if feature.last_api_version >= dml.globals.api_version}

    for flag in options.no_compat:
        for tag in flag.split(','):
            if tag in compat.features:
                if tag in features:
                    del features[tag]
            else:
                parser.error(f'invalid tag {tag} for --no-compat.'
                             ' Try --help-no-compat.')

    if options.strict_int:
        tag = compat.dml12_int.tag()
        if tag in features:
            del features[tag]

    if options.strict_dml12:
        for feature in [compat.dml12_inline, compat.dml12_not,
                        compat.dml12_goto, compat.dml12_misc,
                        compat.dml12_int]:
            tag = feature.tag()
            if tag in features:
                del features[tag]
    dml.globals.enabled_compat = set(features.values())

    if compat.suppress_WLOGMIXUP in dml.globals.enabled_compat:
        ignore_warning('WLOGMIXUP')

    for w in options.disabled_warnings:
        if not is_warning_tag(w):
            prerr("dmlc: the tag '%s' is not a valid warning tag" % w)
            sys.exit(1)
        ignore_warning(w)

    for w in options.enabled_warnings:
        if not is_warning_tag(w):
            prerr("dmlc: the tag '%s' is not a valid warning tag" % w)
            sys.exit(1)
        enable_warning(w)

    inputfilename = options.input_filename

    if options.makedep_old:
        options.dep = os.path.basename(inputfilename) + 'dep'
    if options.dep and options.porting_filename:
        prerr("dmlc: the -P flag cannot be used together with --dep")
        sys.exit(1)
    if options.debuggable and options.porting_filename:
        prerr("dmlc: the -P flag cannot be used together with -g")
        sys.exit(1)

    if options.dep:
        # stops parser warnings from appearing twice during a build
        dml.globals.ignore_all_warnings = True

    if options.porting_filename:
        porting_dirname = os.path.dirname(options.porting_filename)
        if porting_dirname and not os.path.isdir(porting_dirname):
            prerr('dmlc: -P: no such directory: ' + porting_dirname)
            sys.exit(1)
        logging.show_porting = True
        logging.PortingMessage.outfile = tempfile.TemporaryFile(mode='w+')
        # track additional position information
        dmlparse.track_lexspan()

    outputbase = options.output_base
    if outputbase is None:
        outputbase = os.path.basename(inputfilename)
        if outputbase.endswith('.dml'):
            outputbase = outputbase[:-4]

    # Profiling setup
    if os.getenv('DMLC_PROFILE') and not options.dep:
        import cProfile, pstats
        dmlc_profiler = cProfile.Profile()
        dmlc_profiler.enable()
    else:
        dmlc_profiler = False

    logtime("startup")

    try:
        dml.globals.serialized_traits = serialize.SerializedTraits()
        (dml_version, devname, headers, footers, global_defs,
         top_tpl, imported) = toplevel.parse_main_file(
             inputfilename, options.import_path)
        logtime("parsing")

        if dml_version != (1, 2):
            logging.show_porting = False

        if 'DMLC_DUMP_INPUT_FILES' in os.environ:
            dump_input_files(outputbase, dict(
                imported, **{inputfilename: [os.path.basename(inputfilename)]}))
        if options.dep:
            print_cdep(outputbase, headers, footers)
            if logging.failure:
                sys.exit(2)
            deplist = [inputfilename] + list(imported.keys())
            now = time.time()
            future_timestamps = [path for path in deplist
                                 if os.stat(path).st_mtime > now]
            if future_timestamps:
                # If any dependency has a future timestamp, and we
                # create a depfile, then make will re-generate the dep
                # file repeatedly until time has caught up. If we just don't
                # generate it, make will just rebuild unconditionally instead.
                sys.stderr.write(
                    '*** WARNING files with future timestamp detected:\n')
                for f in future_timestamps:
                    sys.stderr.write(f'  {f}\n')
                # this avoids an infinite make loop, at the cost of ruining
                # dependencies
                deplist = [d for d in deplist if d not in future_timestamps]
            deps = ' '.join(path.replace(" ", "\\ ") for path in deplist)
            if options.dep_target:
                targetlist = options.dep_target
            else:
                targetlist = [options.dep, f"{outputbase}.c"]
            targets = ' '.join(path.replace(" ", "\\ ") for path in targetlist)
            if options.makedep_old:
                f = sys.stdout
            else:
                f = open(options.dep, 'w')
            with f:
                f.write('%s : %s\n' % (targets, deps))
                # By default generate phony targets similar to -MP GCC
                # argument. Phony targets provide better experience with GNU
                # Make when dependencies get removed.
                if not options.no_dep_phony:
                    f.write('%s :\n' % (deps,))
            sys.exit(0)

        dev = process(devname, global_defs, top_tpl, defs)
        logtime("process")

        if options.info:
            dml.info_backend.generate(dev, outputbase + '.xml')
            logtime("info")

        if output_c:
            dml.c_backend.generate(dev, headers, footers, outputbase,
                                   [inputfilename] + list(imported.keys()),
                                   options.full_module)
            logtime("c")
            structure.check_unused_and_warn(dev)
            if dml.globals.debuggable:
                dml.g_backend.generate(expr_util.param_str(dev, 'classname'),
                                       dev, dml_version, outputbase + '.g')
                logtime("g")

        if not logging.failure:
            # report WREF for broken unused parameters. But just ignore it
            # if there's already a hard error somewhere, because if the
            # parameter was actually used, then the WREF is a duplicate
            # of an already reported error.
            for wref in messages.WREF.instances:
                report(wref)

        logtime("total")

        return 2 if logging.failure else 0

    except SystemExit:
        # Someone called sys.exit
        raise

    except logging.DMLError as msg:
        # Ordinary fatal compile error
        report(msg)
        return 2

    except KeyboardInterrupt:
        prerr('*** Keyboard interrupt')
        return 3

    except logging.ICE as msg:
        report(msg)
        unexpected_error(sys.exc_info()[0], sys.exc_info()[1], sys.exc_info()[2])
        return 3

    except (RuntimeError, Exception) as e:
        ctx = ErrorContext.last_entered
        if ctx:
            report(ICE(ctx.node,
                       "Unexpected exception '%s' in %s"
                       % (e, ctx.node.identity())))
        unexpected_error(sys.exc_info()[0], sys.exc_info()[1], sys.exc_info()[2])
        return 3

    finally:
        if dmlc_profiler:
            dmlc_profiler.disable()
            stats = pstats.Stats(dmlc_profiler)
            stats.strip_dirs()
            stats.sort_stats('time', 'calls')
            stats.print_stats(20)
            stats.dump_stats(f'{outputbase}.prof')
        if options.porting_filename:
            flush_porting_log(logging.PortingMessage.outfile,
                              options.porting_filename)
