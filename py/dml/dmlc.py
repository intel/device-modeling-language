# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys, os, traceback, re
import optparse
import tempfile
import shutil
import time
from pathlib import Path

from . import structure, logging, messages, ctree, ast, expr_util, toplevel
from . import dmlparse
from . import output

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
                ast.parameter(param_site, name,
                              (extra_params[name], None, None))
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
    if event in ('call', 'return'):
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
    elif literal.type == 'ID' and literal.value in ['true', 'false']:
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
    basenames = set()
    with tarfile.open(outputbase + ".tar.xz", 'w:xz') as tf:
        # HACK: if two different files foo.dml are imported, where one is
        # imported as "foo.dml" or "./foo.dml" and the other is only
        # imported using a qualified path such as "bar/foo.dml", then
        # the rename hack below has a better chance to work if we process
        # the unqualified paths first.
        for f in sorted(imported, key=lambda f: min(map(len, imported[f]))):
            paths = imported[f]
            base = os.path.basename(f)
            while base in basenames:
                # HACK: all DML files end up in the same directory; if two
                # have the same name, then dodge that by renaming. In
                # this case we may need to change some import directive
                # manually in order for all files to be imported.
                base = '_' + base
            basenames.add(base)
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

def is_warning_tag(w):
    return w and w[0] == 'W' and hasattr(messages, w)

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

def main(argv):
    # DML files must be utf8, but are generally opened without specifying
    # the 'encoding' arg. This works only if utf8_mode is enabled.
    assert sys.flags.utf8_mode
    optpar = optparse.OptionParser(
        """
  dmlc [flags] <file> [output-base]""")

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
    optpar.add_option(
        '-I', dest = 'import_path', action = 'append',
        metavar = 'PATH',
        default = [],
        help = 'add PATH to import search path')

    # <dt>-D <i>name</i>=<i>definition</i></dt>
    # <dd>Define a compile-time parameter.  The definition
    # must be a literal expression, and can be a quoted
    # string, a boolean, an integer, or a floating point
    # constant. The parameter will appear in the top-level scope.</dd>
    optpar.add_option(
        '-D', dest = 'defines', action = 'append',
        metavar = 'NAME=VALUE',
        default = [],
        help = 'set compile time constant NAME to VALUE')

    # <dt>--dep</dt>
    # <dd>Output makefile rules describing dependencies.</dd>
    optpar.add_option(
        '--dep', dest = "makedep", action = 'store',
        help = 'generate makefile dependencies')

    # <dt>--no-dep-phony</dt>
    # <dd>With --dep, avoid addition of a phony target for each dependency
    # other than the main file.</dd>
    optpar.add_option(
        '--no-dep-phony', dest = "no_dep_phony", action = 'store_true',
        help = 'With --dep, avoid addition of a phony target for each'
        + ' dependency other than the main file.')

    # <dt>--dep-target</dt>
    # <dd>With --dep, change the target of the rule emitted by dependency
    # generation. Specify multiple times to have multiple targets.</dd>
    optpar.add_option(
        '--dep-target', dest = "dep_target", action = 'append',
        metavar = 'TARGET',
        default = [],
        help = 'With --dep, change the target of the rule emitted by'
        + ' dependency generation. Specify multiple times to have multiple'
        + ' targets.')

    # <dt>-T</dt>
    # <dd>Show tags on warning messages. The tags can be used with
    # the <tt>--nowarn</tt> and <tt>--warn</tt> options.</dd>
    optpar.add_option(
        '-T', dest = 'include_tag', action = 'callback',
        callback = lambda *args: set_include_tag(True),
        help = 'show tags on warning messages')

    # Deprecated with SIMICS_API > 4.8
    optpar.add_option(
        '-m', dest = 'full_module', action = 'store_true',
        help = optparse.SUPPRESS_HELP)

    # <dt>-g</dt>
    # <dd>Generate artifacts that allow for easier source-level debugging.
    # This generates a DML debug file leveraged by debug-simics, and
    # causes generated C code to follow the DML code more closely.</dd>
    def set_debuggable(option, opt, value, parser):
        dml.globals.debuggable = True
    optpar.add_option(
        '-g', dest = 'debuggable', action = 'callback',
        callback = set_debuggable,
        help = 'generate artifacts and C code that allow for easier debugging')

    # <dt>--warn=<i>tag</i></dt>
    # <dd>Enable selected warnings. The tags can be found using
    # the <tt>-T</tt> option.</dd>
    optpar.add_option(
        '--warn', dest = 'enabled_warnings', action = 'append',
        metavar = 'TAG',
        default = [],
        help = 'enable warning TAG')

    # <dt>--nowarn=<i>tag</i></dt>
    # <dd>Suppress selected warnings. The tags can be found using
    # the <tt>-T</tt> option.</dd>
    optpar.add_option(
        '--nowarn', dest = 'disabled_warnings', action = 'append',
        metavar = 'TAG',
        default = [],
        help = 'disable warning TAG')

    # <dt>--werror</dt>
    # <dd>Turn all warnings into errors.</dd>
    optpar.add_option('--werror', dest='werror', action='store_true',
                      help='Turn all warnings into errors')

    # <dt>--strict</dt>
    # <dd>Report errors for some constructs that will be forbidden in
    # future versions of the DML language</dd>
    optpar.add_option('--strict-dml12', dest='strict', action='store_true',
                      help='Report errors for some constructs that will be'
                      + ' forbidden in future versions of the DML language')
    optpar.add_option('--strict-int', dest='strict_int', action='store_true',
                      help='Use DML 1.4 style integer arithmetic semantics'
                      + ' when compiling DML 1.2 files. Implied by'
                      + ' --strict-dml12.')

    # <dt>--noline</dt>
    # <dd>Suppress line directives for the C preprocessor so
    # that the C code can be debugged.</dd>
    optpar.add_option(
        '--noline', dest = 'noline', action = 'store_true',
        help = 'suppress line directives in generated C code')

    # <dt>--info</dt>
    # <dd>Enable the output of an XML file describing register layout.</dd>
    optpar.add_option(
        '--info', dest = 'output_info', action = 'store_true',
        help = 'generate XML file describing register layout')

    # <dt>--version</dt>
    # <dd>Print version information.</dd>
    optpar.add_option(
        '--version', dest = 'handled_in_c', action = 'store_true',
        help = 'print version information')

    # <dt>--simics-api=<i>version</i></dt>
    # <dd>Use Simics API version <i>version</i>.</dd>
    optpar.add_option(
        '--simics-api', dest = 'api_version', action = 'store',
        metavar='VERSION',
        default=default_api_version(),
        help=('specify Simics API version (default %s)'
              % default_api_version()))

    # <dt>--max-errors=<i>N</i></dt>
    # <dd>Limit the number of error messages to <i>N</i>.</dd>
    optpar.add_option(
        '--max-errors', dest = 'max_errors', action = 'store',
        metavar = 'N',
        default = "0",
        help = ('Limit the number of error messages to N'))

    # </dl>
    # </add>

    # Legacy: write deps to stdout, and assume it's redirected to <file>.dmldep
    # Should be removed in 6
    optpar.add_option(
        '-M', dest = "makedep_old", action = 'store_true',
        help = optparse.SUPPRESS_HELP)

    optpar.add_option(
        '-P', dest='porting_filename', action='store',
        help="""Append messages to file with tags for automatic porting
        to DML 1.4""")

    def show_illegal_attributes(option, opt, value, parser):
        for n in dml.globals.illegal_attributes:
            print(n)
        sys.exit(0)

    # Not documented
    optpar.add_option(
        '--illegal-attributes', dest = 'illegal_attributes',
        action = 'callback',
        callback = show_illegal_attributes,
        help = optparse.SUPPRESS_HELP)
    optpar.add_option(
        '--state-change-dml12', action='store_true',
        help=optparse.SUPPRESS_HELP)

    # Generate multiple C files, splitting at the specified file size
    optpar.add_option(
        '--split-c-file', dest = 'split_c_file',
        action = 'store',
        default = '0',
        help = optparse.SUPPRESS_HELP)

    (options, args) = optpar.parse_args(argv[1:])

    global outputbase, output_c

    defs = {}
    for d in options.defines:
        try:
            (name, value) = parse_define(d)
        except ESYNTAX as e:
            report(e)
        else:
            defs[name] = value

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

    dml.globals.linemarks = not options.noline

    dml.globals.state_change_dml12 = options.state_change_dml12

    if options.api_version not in api_versions():
        prerr("dmlc: the version '%s' is not a valid API version" % (
                options.api_version))
        sys.exit(1)

    if options.full_module and options.api_version not in ['4.8']:
        prerr("dmlc: the -m option is only valid together with --api=4.8"
              " or older")
        sys.exit(1)

    dml.globals.api_version = options.api_version

    if len(args) not in [1, 2]:
        optpar.error('wrong number of arguments')

    inputfilename = args[0]

    if options.makedep_old:
        options.makedep = os.path.basename(inputfilename) + 'dep'
    if options.makedep and options.porting_filename:
        prerr("dmlc: the -P flag cannot be used together with --dep")
        sys.exit(1)
    if options.debuggable and options.porting_filename:
        prerr("dmlc: the -P flag cannot be used together with -g")
        sys.exit(1)

    if options.porting_filename:
        porting_dirname = os.path.dirname(options.porting_filename)
        if porting_dirname and not os.path.isdir(porting_dirname):
            prerr('dmlc: -P: no such directory: ' + porting_dirname)
            sys.exit(1)
        logging.show_porting = True
        logging.PortingMessage.outfile = tempfile.TemporaryFile(mode='w+')
        # track additional position information
        dmlparse.track_lexspan()

    if len(args) == 2:
        outputbase = args[1]
    else:
        outputbase = os.path.basename(inputfilename)
        if outputbase.endswith('.dml'):
            outputbase = outputbase[:-4]

    # Profiling setup
    if os.getenv('DMLC_PROFILE') and not options.makedep:
        import cProfile, pstats
        dmlc_profiler = cProfile.Profile()
        dmlc_profiler.enable()
    else:
        dmlc_profiler = False

    logtime("startup")

    try:
        (dml_version, devname, headers, footers, global_defs,
         top_tpl, imported) = toplevel.parse_main_file(
             inputfilename, options.import_path, options.strict)
        logtime("parsing")

        if dml_version != (1, 2):
            logging.show_porting = False

        dml.globals.strict_int_flag = options.strict or options.strict_int

        if 'DMLC_DUMP_INPUT_FILES' in os.environ:
            dump_input_files(outputbase, dict(
                imported, **{inputfilename: [os.path.basename(inputfilename)]}))
        if options.makedep:
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
                targetlist = [options.makedep, f"{outputbase}.c"]
            targets = ' '.join(path.replace(" ", "\\ ") for path in targetlist)
            if options.makedep_old:
                f = sys.stdout
            else:
                f = open(options.makedep, 'w')
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

        if options.output_info:
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
