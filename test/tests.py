# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys, time
import os, os.path
import re
import functools
import itertools
import queue
import threading
import random
import subprocess
import difflib
from pathlib import Path
from os.path import join, isdir, isfile, exists
import glob
from fnmatch import fnmatch
from simicsutils.host import host_type, is_windows, batch_suffix
from simicsutils.internal import package_path
import testparams
from testparams import simics_root_path
import traceback

sys.path.append(join(simics_root_path(), "scripts", "build"))
import module_id

def host_path():
    return join(testparams.simics_root_path(),
                        testparams.host_platform())

def project_host_path():
    return join(testparams.project_path(),
                        testparams.host_platform())

# Matches "/// (ANNOTATION) (details)", used for annotating error
# expectations etc in test .dml files
annotation_re = re.compile(b' */// +([A-Z-]+)(?: +(.*))?')
# matches "(basename.dml):(line): (message)" in stderr
message_re = re.compile('([^:\\\\/]*):([0-9]+):(?:[0-9]+:)? (.*)')

testdir = os.getcwd()

def active_test(fullname):
    return testparams.matches_test_pattern(fullname)

import socket
hostname = socket.gethostname()

if is_windows():
    so_sfx = '.dll'
else:
    so_sfx = '.so'
bat_sfx = batch_suffix()
exe_sfx = '.exe' if is_windows() else ""

dmlc = [join(simics_root_path(), host_type(), "bin", "py3",
             "mini-python" + exe_sfx),
        join(project_host_path(), "bin", "dml", "python")]

if not is_windows():
    dmlc_lib_path = ":".join(
        [join(simics_root_path(), host_type(), p) for p in ('bin', 'sys/lib')]
        + [os.environ.get('LD_LIBRARY_PATH', '')])

# map from test fullname (subtest name) to timeout multiplier
dmlc_timeout_multipliers = {"1.2/registers/largearray": 3,
                            }
dmlc_default_timeout = 80
def dmlc_reaper_args(exitcode_file, timeout_multiplier = 1):
    return [join(host_path(), "bin", "py3", "reaper"),
            "-t", str(dmlc_default_timeout * timeout_multiplier),
            "-r", "-e", exitcode_file]

common_cflags = ["-O2", "-std=gnu99", '-Wall', '-Werror', '-Wpointer-arith',
                 '-Wwrite-strings', '-Wformat-nonliteral',]
if is_windows():
    cc = [testparams.mingw_cc()]
    cflags = common_cflags + [
        "-DUSE_MODULE_HOST_CONFIG", "-D__USE_MINGW_ANSI_STDIO=1"]
    ldflags = [f"-L{testparams.mingw_root()}/lib/gcc/x86_64-w64-mingw32/lib"]
else:
    cc = [join(package_path(), "gcc_6.4.0", "bin", "gcc")]
    cflags = ['-g', '-fPIC', '-Wundef'] + common_cflags
    ldflags = []
cflags_shared = ["-shared"]

os.environ['DMLC_DEBUG'] = 't'

class TestFail(Exception):
    def __init__(self, reason):
        Exception.__init__(self)
        self.reason = reason

latest_api_version = "6"

special_versions = {}
for f in ["T_WREF.dml"]:
    special_versions[f] = "5"

def simics_api_version(filename):
    base = os.path.basename(filename)
    if base in special_versions:
        return special_versions[base]
    else:
        return latest_api_version

class DeferredOutput(object):
    def __init__(self):
        self.log = []
    def call(self, fun, *args):
        self.log.append((fun, args))

class BaseTestCase(object):
    __slots__ = ('output', 'fullname', 'finished')
    def __init__(self, fullname):
        self.fullname = fullname
        # Set to true when the subtest is finished
        self.finished = threading.Event()
        self.output = None
    def pr(self, msg):
        self.output.call(testparams.pr, msg)

    def run_test(self, output_handler):
        self.output = output_handler
        call = self.output.call
        try:
            self.test()
        except KeyboardInterrupt:
            self.pr("ABORT")
            call(testparams.fail, "aborted")
        except TestFail as e:
            call(testparams.fail, e.reason)
        except Exception as e:
            self.pr(traceback.format_exc())
            call(testparams.fail,
                 'unexpected exception: %s' % (str(e),))
    # list of tests required for this test to run
    prereqs = []

class DMLFileTestCase(BaseTestCase):
    '''Test case based on compiling a DML file'''
    __slots__ = (
        'filename',                     # Full path to the .dml file
        'api_version',                  # API version (a string)
        'path',                      # File path as a list of segments
        'includepath',                  # Include path to use for dmlc
        'dmlc_extraargs',               # Extra cmdline args for dmlc
        'cc_extraargs',                 # Extra cmdline args for gcc
        'dmlc_stdout',                  # Output from dmlc
        'dmlc_stderr',
        'cc_stdout',                    # Output from cc
        'cc_stderr',
        'ld_stdout',                    # Output from linker
        'ld_stderr',
        'simics_stdout',                # Output from simics
        'simics_stderr',

        'status'                        # Expected status
        )
    def __init__(self, fullname, filename, **info):
        BaseTestCase.__init__(self, fullname)
        # Defaults
        self.filename = filename
        self.api_version = None
        self.includepath = None
        self.dmlc_extraargs = []
        self.cc_extraargs = []
        self.status = 0
        # Override defaults
        for k,v in info.items():
            setattr(self, k, v)
        if self.includepath == None:
            assert self.api_version
            if self.api_version == "4.8":
                libdir = "dml-old-4.8"
            else: 
                libdir = "dml"
            self.includepath = (join(project_host_path(), "bin", libdir),
                                join(project_host_path(), "bin", "dml", "api",
                                     self.api_version),
                                testdir)

    @property
    def scriptname(self):
        return join(self.scratchdir, "test.py")
    @property
    def cfilename(self):
        return join(self.scratchdir, "T_" + self.shortname)
    @property
    def pyfilename(self):
        return join(os.path.splitext(self.filename)[0] + ".py")

    shortname = property(lambda self: self.fullname.split('/')[-1])
    scratchdir = property(lambda self:
                          join(testparams.sandbox_path(), "scratch",
                               self.fullname))
    logdir = property(lambda self: join(testparams.sandbox_path(),
                                        "logs", self.fullname))

    def runlog(self, what, f, *args):
        self.pr("Running %s" % what)
        time1 = os.times()
        wtime1 = time.time()
        status = f(*args)
        time2 = os.times()
        wtime2 = time.time()
        self.pr(" -> exit status %d; %.2fs CPU; %.2fs real" %  (
                status,
                (time2[0] + time2[2]) - (time1[0] + time1[2]),
                wtime2 - wtime1))
        return status

    def run_dmlc(self, filename, dmlc_extraargs):
        name = self.shortname
        self.dmlc_stdout = join(self.scratchdir, name+'.dmlc_stdout')
        self.dmlc_stderr = join(self.scratchdir, name+'.dmlc_stderr')
        exitcode_file = join(self.scratchdir, name + '.dmlc_exitcode')
        args =[]
        args += dmlc_reaper_args(exitcode_file,
                                 dmlc_timeout_multipliers.get(self.fullname, 1))
        args += dmlc + ["-T", "--noline"]
        args += dmlc_extraargs
        if self.api_version:
            args += ["--simics-api=" + self.api_version]
        if self.api_version in ["4.8"]:
            args += ["-m"]

        for d in self.includepath:
            args.extend(["-I", d])

        args.extend([filename, os.path.basename(self.cfilename)])

        self.pr(" ".join(args))

        status = subprocess.call(args,
                                 stdout = open(self.dmlc_stdout, "w"),
                                 stderr = open(self.dmlc_stderr, "w"),
                                 cwd=self.scratchdir)
        if status == 0:
            return 0
        elif status == 1:
            raise TestFail("dmlc timeout")
        elif status == 2:
            with open(exitcode_file) as f:
                return int(f.read())
        else:
            raise TestFail("reaper failed with return code %d" % (status,))

    def check_for_error(self, status, exp_errors, act_errors):
        """
        Check that each expected error happens
        """
        if status == 0:
            self.pr("Exit status was 0")
            raise TestFail("no error triggered")

        for (efile, eline, etag) in exp_errors:
            self.pr("Looking for %s in stderr" % (etag,))
            for (atag, alocs) in act_errors:
                if etag == atag:
                    if any(afile == efile and eline in [None, aline]
                           for (afile, aline) in alocs):
                        # Found!
                        break
            else:
                raise TestFail("expected error %s in %s" % (
                    etag,
                    efile if eline is None else "%s:%d" % (efile, eline)))

    @staticmethod
    def parse_messages(stderr_lines):
        """Extract warning and error messages found in stderr_file.  Produces
        a generator yielding triplets (kind, tag, lines). One triplet
        is yielded for each line containing 'error ETAG:' or 'warning
        WTAG:'; adjacent lines with error context messages are
        included in the 'lines' list. The location of the 'error' or
        'warning' line appears first in the location list.

        One error message consists of zero or more lines with context
        on the form "In ..." (e.g., "In template XYZ"),
        followed by one line "error: ..." or "warning: ...", followed
        by zero or more context lines not on the form "In ..." (e.g.,
        "previously declared here").

        The unittest_parse_messages() function contains a concrete example.

        """

        # The text after "error: " or "warning: " of the current
        # message, or None if there is no current message, or if all
        # we've seen in the current message is context lines so far
        current_msg = None
        # Locations of the current message, as pairs (file, line)
        locs = []
        def is_pre_line(s):
            '''Return True for lines expected to occur before the real error,
            such as 'In template ...', 'In method ...', or
            'dmlc: warnings being treated as errors'
            '''
            return s.startswith('In ') or s.startswith('dmlc: ')
        for line in stderr_lines:
            m = message_re.search(line)
            if m:
                (filename, linestr, next_msg) = m.groups()
                # Parse 'error EXYZ: blah'
                for kind in ['warning', 'error', 'porting']:
                    if next_msg.startswith(kind + ' '):
                        if current_msg is not None:
                            # Current message terminated by next
                            # message
                            yield current_msg + (locs,)
                            (current_msg, locs) = (None, [])
                        tag = next_msg[len(kind) + 1 : next_msg.find(':')]
                        current_msg = (kind, tag)
                        locs.insert(0, (filename, int(linestr), line))
                        break
                else: # Context line
                    if next_msg.startswith('internal compiler error'):
                        raise TestFail('ICE')
                    if current_msg is not None and is_pre_line(next_msg):
                        # Current message terminated by context of next message
                        yield current_msg + (locs,)
                        (current_msg, locs) = (None, [])
                    elif current_msg is None and not is_pre_line(next_msg):
                        # If this happens on a valid message, then we
                        # may have to redefine the heuristics for
                        # detecting message context lines
                        raise TestFail(
                            'Message parse heuristics failed: all context'
                            ' lines before the main message are expected to'
                            ' start with "In ": %r' % (line,))
                    locs.append((filename, int(linestr), line))
            else: # Current line does not belong to a message
                if current_msg is not None:
                    # Current message terminated by non-location line
                    yield current_msg + (locs,)
                    (current_msg, locs) = (None, [])
                elif locs:
                    raise TestFail('Found a location message not adjacent to an'
                                   ' error or warning: %r'
                                   % (line,))
        if locs:
            if not current_msg:
                raise TestFail('Unterminated error message context: %r'
                               % (locs,))
            yield current_msg + (locs,)

    def expect_messages(self, kind, actual, expected):
        # kind is 'warning' or 'error'
        # actual has the format [(tag, [(file, lineno, full_line), ...])]
        # expected has the format [(file, line, tag), ...]

        # 'actual' contains the list of warning or error messages
        # encountered; each message may be reported in multiple source
        # locations. The expectation is that each message in 'actual'
        # is covered by at least one entry from 'expected' (i.e., no
        # message is unexpected), and that each message in expected
        # matches one source location with matching tag in 'actual'
        # (i.e., all expected errors are found).
        if expected:
            self.pr("Expecting %s%s:" % (kind, "s" * (len(expected) != 1)))
            self.pr(" " + " ".join("%s:%s:%s" % (t, os.path.basename(f), l)
                                   for (f, l, t) in sorted(expected, key=str)))

        def matches_expectation(left, right):
            (act_file, act_line, act_tag) = left
            (exp_file, exp_line, exp_tag) = right
            return ((act_file, act_tag) == (os.path.basename(exp_file), exp_tag)
                    and exp_line in [None, act_line])

        unexpected = [location
                      for (tag, locations) in actual
                      if not any(matches_expectation((fil, line, tag),
                                                     expectation)
                                 for (fil, line, _) in locations
                                 for expectation in expected)
                      for location in locations]

        missing = [expectation
                   for expectation in expected
                   if not any(matches_expectation((fil, line, tag),
                                                  expectation)
                              for (tag, locations) in actual
                              for (fil, line, _) in locations)]

        if unexpected:
            self.pr("--- found unexpected %s message%s in stderr:"
                    % (kind, "s" * (len(unexpected) != 1)))
            for (_, _, line) in unexpected:
                self.pr(line.rstrip())
        if missing:
            self.pr("--- did not find expected %s message%s in stderr:"
                    % (kind, "s" * (len(missing) != 1)))
            for (f, l, t) in sorted(missing):
                if not os.path.isabs(f):
                    f = join(os.path.dirname(self.filename), f)
                self.pr("%s:%s: %s %s:" % (f, l or '0', kind, t))
        if unexpected or missing:
            if missing and unexpected:
                msg = 'wrong'
            elif unexpected:
                msg = 'unexpected'
            else:
                assert missing
                msg = 'missing'
            raise TestFail('%s %s' % (msg, kind))

    def copy_log(self, logfile):
        for l in open(logfile, "rb"):
            self.pr(l.decode('utf-8', 'replace')[:-1])

    def print_logs(self, prog, stdout, stderr):
        for (name, filename) in [('stdout', stdout), ('stderr', stderr)]:
            if list(open(filename)):
                self.pr("--- %s %s:" % (prog, name))
                self.pr(filename + ":0:")
                self.copy_log(filename)

    class TestFlags(object):
        def __init__(self,
                     exp_warnings=(),
                     exp_errors=(),
                     # list of Python regexps
                     exp_stdout=(),
                     cc_flags=(),
                     dmlc_flags=(),
                     instantiate_manually=False,
                     compile_only=False):
            self.exp_warnings = list(exp_warnings)
            self.exp_errors = list(exp_errors)
            self.exp_stdout = list(exp_stdout)
            self.cc_flags = list(cc_flags)
            self.dmlc_flags = list(dmlc_flags)
            self.instantiate_manually = instantiate_manually
            self.compile_only = compile_only
    def test_flags(self, filename=None, append_to=None):
        if filename is None:
            filename = self.filename
        if append_to is None:
            flags = self.TestFlags()
        else:
            flags = append_to
        for (linenum, line) in enumerate(open(filename, "rb"), 2):
            m = annotation_re.match(line)
            if not m:
                continue
            (key, data) = m.groups()
            key = key.decode('utf-8')
            if data:
                data = data.decode('utf-8')
            self.pr("%s : %s" % (key, data))
            if key in ['WARNING', 'ERROR']:
                words = data.split()
                # An error or warning message may span multiple lines,
                # possibly with multiple source locations. However,
                # only one of the lines has the form 'error: EXYZ' or
                # 'warning: WXYZ'. An "/// ERROR EXYZ" or "/// WARNING
                # WXYZ" annotation in a test .dml file expects the
                # given message to be reported. It also expects that
                # the source line following the annotation matches one
                # of the locations of the message.
                if len(words) == 1:
                    expectation = (filename, linenum, data)
                else:
                    # Special notation: "/// ERROR EXYZ file.dml"
                    # means that the error is expected anywhere in
                    # file.dml.  This is useful for imported files, or
                    # if expecting multiple messages on the same
                    # source line.
                    assert len(words) == 2
                    [tag, fname] = words
                    expectation = (fname, None, tag)
                if key == 'ERROR':
                    flags.exp_errors.append(expectation)
                    self.status = 2
                elif key == 'WARNING':
                    flags.exp_warnings.append(expectation)
            elif key == 'SCAN-FOR-TAGS':
                path = os.path.join(os.path.dirname(filename),
                                    data.strip())
                flags = self.test_flags(filename=path, append_to=flags)
            elif key == 'DMLC-FLAG':
                flags.dmlc_flags.append(data)
            elif key == 'CC-FLAG':
                flags.cc_flags.append(data)
            elif key == 'GREP':
                flags.exp_stdout.append(data)
            elif key == 'INSTANTIATE-MANUALLY':
                flags.instantiate_manually = True
            elif key == 'COMPILE-ONLY':
                flags.compile_only = True
            else:
                raise TestFail('Unexpected key %s' % (key,))
        return flags

    def verify_dmlc_messages(self, stderr, expected_msgs):
        actual_msgs = {}
        with open(stderr, 'r') as f:
            lines = list(f)
        for (kind, tag, locs) in self.parse_messages(lines):
            actual_msgs.setdefault(kind, []).append((tag, locs))
        for kind in set().union(actual_msgs, expected_msgs):
            self.expect_messages(
                kind, actual_msgs.get(kind, []), expected_msgs.get(kind, []))

class CTestCase(DMLFileTestCase):
    '''Compile a test DML file with the C backend and verify correct
    behaviour'''
    __slots__ = ()
    def __init__(self, path, filename, **info):
        DMLFileTestCase.__init__(
            self, "/".join(path), filename, **info)

    def run_cc(self, cc_extraargs):
        name = self.shortname
        self.cc_stdout = join(self.scratchdir, name+'.cc_stdout')
        self.cc_stderr = join(self.scratchdir, name+'.cc_stderr')

        args = (
            cc + cflags + [
                "-c", f"-I{self.scratchdir}",
                f"-I{Path(simics_root_path()) / 'src' / 'include'}",
                f"-I{Path(project_host_path()) / 'bin' / 'dml' / 'include'}"]
            + list(self.cc_extraargs) + cc_extraargs + [
                "-o", self.cfilename + ".o", self.cfilename + ".c"])

        result = subprocess.call(args,
                                 stdout = open(self.cc_stdout, "w"),
                                 stderr = open(self.cc_stderr, "w"))
        if result != 0:
            self.pr("CC: " + " ".join(args))
        return result

    def run_linker(self):
        """Build a loadable Simics module from the test case"""
        # Assume that run_cc is already called
        assert os.path.exists(self.cfilename + ".o")

        self.pr("Creating module_id.c")

        module_id_base = join(self.scratchdir, "module_id")
        class options(object):
            output = module_id_base + ".c"
            modname = 'dml-test-' + self.shortname
            classes = "test"
            components = ""
            user_version = None
            user_build_id = ("__dmlc_tests__", 0)
            cpumod = None
            date = None
            product = None
            thread_safe = "no"
            host_type = host_type()
            py_version = None
            py_iface_lists = []
            iface_py_modules = []
            init_c_wrappers = False
            dml_devs = ([] if self.api_version in ["4.8"]
                        else ["T_" + self.shortname])
            user_init_local = self.api_version in ["4.8"]

        module_id.CModuleId(options, False).create_module_id()

        name = self.shortname
        self.ld_stdout = join(self.scratchdir, name+'.ld_stdout')
        self.ld_stderr = join(self.scratchdir, name+'.ld_stderr')

        self.pr("Compiling module_id.c")
        args = cc + cflags + \
               ["-c",
                "-I" + self.scratchdir,
                "-I" + join(simics_root_path(), "src", "include"),
                "-o", module_id_base + ".o",
                module_id_base + ".c"]

        status = subprocess.call(args,
                                 stdout = open(self.ld_stdout, "w"),
                                 stderr = open(self.ld_stderr, "w"))
        if status:
            self.pr("CC: %r" % args)
            return status

        self.pr("Linking module")
        modfile = join(self.scratchdir,
                       'dml-test-' + self.shortname + so_sfx)
        args = cc + cflags_shared + \
               ["-o", modfile,
                self.cfilename + ".o",
                module_id_base + ".o",
                "-L"+join(host_path(), "bin", "py3")] + ldflags + \
                ["-lsimics-common", "-lvtutils"]
        args.append("-Wl,--no-undefined")

        status = subprocess.call(args,
                                 stdout = open(self.ld_stdout, "a"),
                                 stderr = open(self.ld_stderr, "a"))
        if status != 0:
            self.pr("LD: %r" % args)
            return status

        self.pr("Signing module")
        args = [join(host_path(), "bin", "py3", "simics-common"), "-core",
                "-sign-module", os.path.abspath(modfile)]
        return subprocess.call(args, 
                               stdout = open(self.ld_stdout, "a"),
                               stderr = open(self.ld_stderr, "a"))

    def run_simics(self, pyfile=None, auto_instantiate=True):
        name = self.shortname
        self.simics_stdout = join(self.scratchdir, name+'.simics_stdout')
        self.simics_stderr = join(self.scratchdir, name+'.simics_stderr')

        self.pr("Creating Simics script")
        sc = open(self.scriptname, "w")
        #sc.write("print conf.sim.module_searchpath\n")
        #sc.write("run_command('list-modules')\n")
        sc.write("testname = %r\n" % self.shortname)
        sc.write("scratchdir = %r\n" % self.scratchdir)
        sc.write("basedir = %r\n" % join(os.path.dirname(self.filename)))
        if auto_instantiate:
            sc.write("try:\n")
            sc.write(f"    SIM_load_module('dml-test-{self.shortname}')\n")
            sc.write("except:\n")
            sc.write("    run_command('list-failed-modules -v')\n")
            sc.write("    raise\n")
            sc.write("obj = SIM_create_object('test', 'obj', [])\n")
        else:
            assert pyfile

        if pyfile:
            sc.write("print('running', %r)\n" % pyfile)
            sc.write("import sys\n")
            sc.write(
                "sys.path.append(%r)\n" % join(os.getcwd(), 'common'))
            sc.write("sys.path.append(%r)\n" % os.path.dirname(pyfile))
            sc.write("SIM_source_python(%r)\n" % pyfile)
        else:
            sc.write("if not obj.runtest:\n")
            sc.write("    print('test attribute returned false')\n")
            sc.write("    SIM_quit(1)\n")
        sc.write("SIM_quit(0)\n")
        sc.close()

        self.pr("Running Simics")
        args = [join(simics_root_path(), "bin", "simics" + bat_sfx),
                "-batch-mode", "-quiet", "-no-copyright", "-no-settings",
                "-core", "-werror",
                '-py3k-warnings',
                "-L", self.scratchdir,
                "-project", testparams.project_path(),
                "-p", self.scriptname]
        env = os.environ.copy()
        env['SIMICS_HOST'] = os.path.basename(host_path())
        env['SIMICS_ROOT'] = simics_root_path()
        # self.pr("ARGS: %r" % args)
        return subprocess.call(args,
                               stdout = open(self.simics_stdout, "w"),
                               stderr = open(self.simics_stderr, "w"),
                               env = env,
                               cwd=self.scratchdir)

    def test(self):
        "This actually runs the test, after filtering"
        if not isdir(self.scratchdir):
            os.makedirs(self.scratchdir)
        flags = self.test_flags()
        # Run dmlc
        status = self.runlog("dmlc", self.run_dmlc, self.filename,
                             self.dmlc_extraargs + flags.dmlc_flags)

        if status != self.status:
            self.print_logs('dmlc', self.dmlc_stdout, self.dmlc_stderr)
            raise TestFail("dmlc status=%d (expected %d)"
                           % (status, self.status))

        try:
            self.verify_dmlc_messages(self.dmlc_stderr,
                {'error': flags.exp_errors, 'warning': flags.exp_warnings})
        except TestFail:
            self.print_logs('dmlc', self.dmlc_stdout, self.dmlc_stderr)
            raise
        if (not flags.exp_errors and not flags.exp_warnings):
            # this will normally give no output, but it allows
            # convenient debug printing
            self.print_logs('dmlc', self.dmlc_stdout, self.dmlc_stderr)

        if status != 0: # No use compiling if dmlc failed
            return

        # Run the C compiler
        status = self.runlog("CC", self.run_cc, flags.cc_flags)

        self.pr("Finished cc with exit status "+str(status))
        if status != 0:
            # This is a lie, which causes gcc error messages to be
            # clickable in Emacs
            self.pr("Entering directory `%s'" % self.scratchdir)
            self.print_logs('cc', self.cc_stdout, self.cc_stderr)
            raise TestFail("cc error")

        # Link the module
        status = self.runlog("linker", self.run_linker)

        self.pr("Finished linker with exit status "+str(status))
        if status != 0:
            # This is a lie, which causes gcc error messages to be
            # clickable in Emacs
            self.pr("Entering directory `%s'" % self.scratchdir)
            self.print_logs('linker', self.ld_stdout, self.ld_stderr)
            raise TestFail("linker status=%d" % status)

        pyfile = self.pyfilename if exists(self.pyfilename) else None

        if flags.compile_only:
            assert not pyfile
            return

        # Run simics
        status = self.runlog("Simics", lambda: self.run_simics(
            pyfile, not flags.instantiate_manually))
        if status != 0:
            self.print_logs('simics', self.simics_stdout, self.simics_stderr)
            raise TestFail("simics status=%d" % status)

        if flags.exp_stdout:
            rxs = [(r, re.compile(r)) for r in flags.exp_stdout]
            found = set()
            self.pr("Grepping simics output")
            for l in open(self.simics_stdout, "r"):
                for s,r in rxs:
                    if r.match(l):
                        self.pr("Found %r" % s)
                        found.add(r)
            for s,r in rxs:
                if not r in found:
                    self.pr(self.simics_stdout + ":0: Didn't find %r" % s)
                    raise TestFail("grep miss")

        self.pr("Everything seems OK")

class XmlTestCase(CTestCase):
    __slots__ = ()
    def run_simics(self, pyfile=None, auto_instantiate=True):
        os.rename('%s.xml' % self.cfilename, join(self.scratchdir, 'test.xml'))
        return CTestCase.run_simics(self, pyfile, auto_instantiate)


def _unittest_parse_messages():
    stderr = '''
/tmp/f.dml:3:20: In template abc
/tmp/g.dml:4:9: error EDPARAM: duplicate assignment to parameter 'f'
/tmp/f.dml:8:12: conflicting definition
'''.splitlines()
    assert (list(DMLFileTestCase.parse_messages(stderr))
            == [('error', 'EDPARAM',
                 [('g.dml', 4), ('f.dml', 3), ('f.dml', 8)])])

all_tests = []

# First, some special cases
class ErrorTest(CTestCase):
    __slots__ = ('errors', 'warnings')
    def __init__(self, path, filename, **info):
        self.errors = []
        self.warnings = []
        CTestCase.__init__(self, path, filename, status=2, **info)
    def test_flags(self):
        return self.TestFlags(exp_errors=self.errors,
                              exp_warnings=self.warnings)

all_tests.append(ErrorTest(["missing"], "xyz",
                           errors=[("xyz", 0, "ENOFILE")],
                           includepath=()))
all_tests.append(ErrorTest(
    ["empty"], os.devnull,
    errors=[(os.path.basename(os.devnull), 1, "EDEVICE")],
    warnings=[(os.path.basename(os.devnull), 1, "WNOVER")],
    includepath=()))
all_tests.append(CTestCase(["minimal"], join(testdir, "minimal.dml"),
                           api_version=latest_api_version))

# Test that it fails with a good error message if it can't find
# dml-builtins.dml etc.
all_tests.append(ErrorTest(["noinclude"], join(testdir, "minimal.dml"),
                           errors=[("minimal.dml", 6, "EIMPORT")],
                           includepath=(), api_version=latest_api_version,
                           dmlc_extraargs=['--max-errors=1']))

# Test that it fails with a good error message if it can't create the
# output files.
#run_test(join(testdir, "minimal.dml"), name = "not-writable",
#         scratchdir = "./dontexist",
#         ERROR = "??")

# Test the -g flag
all_tests.append(CTestCase(
         ["debuggable-compile"],
         join(testdir, "1.2", "methods", "T_inline.dml"),
         api_version=latest_api_version,
         dmlc_extraargs = ["-g"]))
all_tests.append(CTestCase(
         ["debuggable-compile-connect"],
         join(testdir, "1.2", "structure", "T_connect_obj.dml"),
         api_version=latest_api_version,
         dmlc_extraargs = ["-g"]))

class SplitTestCase(CTestCase):
    __slots__ = ()
    def run_cc(self, cc_extraargs):
        assert not cc_extraargs
        files = glob.glob(self.cfilename + "-[0-9]*.c")
        assert len(files) > 10, files
        for fn in files:
            self.cc_stdout = join(self.scratchdir, fn + '.cc_stdout')
            self.cc_stderr = join(self.scratchdir, fn + '.cc_stderr')
            args = (cc + cflags + [
                "-c",  f"-I{self.scratchdir}",
                f"-I{Path(simics_root_path()) / 'src' / 'include'}",
                f"-I{Path(project_host_path()) / 'bin' / 'dml' / 'include'}"]
                    + self.cc_extraargs + ["-o", fn[:-2] + ".o", fn])

            result = subprocess.call(args,
                                     stdout = open(self.cc_stdout, "w"),
                                     stderr = open(self.cc_stderr, "w"))
            if result != 0:
                self.pr("CC: " + " ".join(args))
                return result
        self.cc_stdout = join(self.scratchdir, self.shortname + '.cc_stdout')
        self.cc_stderr = join(self.scratchdir, self.shortname + '.cc_stderr')
        ld = cc + ['-Wl,-r', '-nostdlib', '-o', self.cfilename + '.o'] + [
            fn[:-2] + '.o' for fn in files]

        result = subprocess.call(ld,
                                 stdout = open(self.cc_stdout, "w"),
                                 stderr = open(self.cc_stderr, "w"))
        if result != 0:
            self.pr("CCLD: " + " ".join(ld))
        return result
all_tests.append(SplitTestCase(
         ["split"],
         join(testdir, "1.2", "misc", "T_split_output.dml"),
         api_version=latest_api_version,
         dmlc_extraargs=["--split-c-file=1", "--state-change-dml12"]))

all_tests.append(CTestCase(
         ["1.2", "misc", "test_dmlc_g"],
         join(testdir, "1.2", "misc", "test_dmlc_g.dml"),
         api_version=latest_api_version,
         dmlc_extraargs = ["-g"]))

# Test the --werror flag
all_tests.append(CTestCase(
         ["werror"],
         join(testdir, "1.2", "werror", "T_WUNUSEDDEFAULT.dml"),
         api_version=latest_api_version,
         status = 2,
         dmlc_extraargs = ["--werror"]))

class DebuggableCheck(BaseTestCase):
    __slots__ = ()
    def test(self):
        lines = open(join(testparams.sandbox_path(), "scratch",
                          "debuggable-compile", "T_debuggable-compile.c"),
                     "r").readlines()
        if not ("static bool _DML_M_mmm(test_t *_dev, int x, int *y)\n"
                in lines):
            raise TestFail("didn't find expected code")
    prereqs = ['debuggable-compile']

all_tests.append(DebuggableCheck('debuggable-check'))

class DmlDep(CTestCase):
    '''Stability test, '''
    __slots__ = ()
    def run_cc(self, cc_extraargs):
        assert not cc_extraargs
        name = self.shortname
        self.cc_stdout = join(self.scratchdir, name+'.cc_stdout')
        self.cc_stderr = join(self.scratchdir, name+'.cc_stderr')

        args = (
            cc + cflags + [
                self.cfilename + '-cdep.c',
                f"-I{self.scratchdir}",
                f"-I{Path(simics_root_path()) / 'src' / 'include'}",
                f"-I{Path(project_host_path()) / 'bin' / 'dml' / 'include'}",
                '-M', '-MP', '-MF', self.cfilename + '.d',
                '-MT', self.cfilename + '.c'])

        result = subprocess.call(args,
                                 stdout = open(self.cc_stdout, "w"),
                                 stderr = open(self.cc_stderr, "w"))
        if result != 0:
            self.pr("CC: " + " ".join(args))
        return result
    def run_linker(self):
        # suppress link step
        return 0
    def expect_equal_sets(self, a, b):
        assert isinstance(a, set) and isinstance(b, set)
        if a != b:
            import traceback
            for line in traceback.format_stack():
                self.pr(line.rstrip())
            self.pr('missing elements: %r' % ((b - a),))
            self.pr('unexpected elements: %r' % ((a - b),))
            raise TestFail('difference found')

    def parse_depfile(self, f):
        lines = []
        for line in f:
            line = line.rstrip()
            if lines and lines[-1] and lines[-1][-1] == '\\':
                lines[-1] = lines[-1][:-1] + ' ' + line
            else:
                lines.append(line)
        target_prereqs = {}
        for line in filter(None, lines):
            # finding : is awkward because of Windows c:\
            # MingW gcc 10.3 dep output path has inconsistent case
            (targets, prereqs) = (line + ' ').replace('d:', 'D:').split(': ')
            for target in targets.split():
                target_prereqs.setdefault(target, set()).update(prereqs.split())
        return target_prereqs

    def test(self):
        super(DmlDep, self).test()
        # unit test depfile parsing
        assert self.parse_depfile([
            s + '\n' for s in [
                'a \\ ', ' b: c\\', 'd', ' ', 'a : e', 'f:']]) == {
                    'a': {'c', 'd', 'e'},
                    'b': {'c', 'd'},
                    'f': set()}

        # test .dmldep
        with open(self.cfilename + '.dmldep') as f:
            target_prereqs = self.parse_depfile(f)
        c_target = 'T_%s.c' % (self.shortname,)
        dmldep_target = 'T_%s.dmldep' % (self.shortname,)
        non_lib = {os.path.normpath(p) for p in target_prereqs[c_target]
                   if not p.startswith(project_host_path())}
        # Main .c target depends on main DML file and all DML imports.
        # We test that all non-library DML files are included, as well as
        # one library file
        self.expect_equal_sets(non_lib, {
            os.path.normpath(join(testdir, '1.2', 'misc', f + '.dml'))
            for f in ['T_import_rel_1', 'rel/a/x', 'rel/a/y',
                      'rel/z', 'rel/misc/rel/z']})
        self.expect_equal_sets(target_prereqs[dmldep_target],
                               target_prereqs[c_target])
        base_types_dml = os.path.join(
            project_host_path(), 'bin', 'dml', 'api', latest_api_version,
            '1.2', 'simics', 'base-types.dml')
        if all(os.path.normpath(p) != os.path.normpath(base_types_dml)
               for p in target_prereqs[c_target]):
            raise TestFail('missing %s' % (base_types_dml,))
        # each DML import has a dummy target, depending on nothing
        empty_targets = set(target_prereqs) - {c_target, dmldep_target}
        self.expect_equal_sets(empty_targets, target_prereqs[c_target])
        for target in empty_targets:
            self.expect_equal_sets(target_prereqs[target], set())

        # test .d
        # Similar to .dmldep check. Covers that dependency generation
        # extracts #include directives from headers/footers, handling
        # DMLDIR_x_H correctly.
        with open(self.cfilename + '.d') as f:
            target_prereqs = self.parse_depfile(f)
        prereqs = target_prereqs[self.cfilename + '.c']
        expected_subset = [
            self.cfilename + '-cdep.c',
            os.path.join(simics_root_path(), 'src', 'include',
                         'simics', 'base-types.h'),
            join(testdir, '1.2', 'misc', 'rel', 'a', 'x.h'),
            join(testdir, '1.2', 'misc', 'rel', 'a', 'y.h')]
        for exp in expected_subset:
            if all(os.path.normpath(p) != os.path.normpath(exp)
                   for p in prereqs):
                raise TestFail('missing prerequisite: ' + exp)

        empty_targets = set(target_prereqs) - {self.cfilename + '.c'}
        self.expect_equal_sets(empty_targets,
                               prereqs - {self.cfilename + '-cdep.c'})
        for target in empty_targets:
            self.expect_equal_sets(target_prereqs[target], set())

all_tests.append(DmlDep(['dmldep'],
                        join(testdir, '1.2', 'misc', 'T_import_rel_1.dml'),
                        dmlc_extraargs = ['--dep', 'T_dmldep.dmldep'],
                        api_version=latest_api_version))

class PortingConvert(CTestCase):
    __slots__ = ('PORTING',)
    port_dml_status = 0

    def __init__(self, *args, **kwargs):
        super(PortingConvert, self).__init__(*args, **kwargs)
        self.dmlc_extraargs = ["-P", self.tagfile()]

    def run_port_script(self, args, stdout, stderr):
        env = os.environ.copy()
        env['DMLC_DIR'] = join(project_host_path(), 'bin')
        script = join(testparams.project_path(), 'bin',
                      'port-dml' + batch_suffix())
        args = [script] + args
        status = self.runlog(
            " ".join(args),
            lambda: subprocess.call(
                args, env=env,
                stdout=open(stdout, 'w'), stderr=open(stderr, 'w')))
        self.print_logs('port-dml', stdout, stderr)
        if status != self.port_dml_status:
            raise testparams.TestFailure(
                'wrong port-dml exit status: expected %d, got %d'
                % (self.port_dml_status, status))

    def port_test(self, tagfile, srcfile, destfile, expfile, stdout, stderr,
                  extra_flags=[]):
        self.run_port_script(
            ['--src', srcfile, '--dest', destfile, '--tags', tagfile]
            + extra_flags,
            stdout, stderr)
        with open(destfile, 'r') as got, open(expfile, 'r') as expected:
            (got, expected) = (list(got), list(expected))
            for (i, (g, e)) in enumerate(zip(got, expected)):
                # differences in tag comments are permitted
                if (g.lstrip().startswith('//') and
                    e.lstrip().startswith('//') and
                    (g.lstrip().startswith('///')
                     or e.lstrip().startswith('///'))):
                    got[i] = expected[i]
            diff = list(difflib.unified_diff(got, expected,
                                             destfile, expfile))
        if diff:
            diff_file = destfile + '.diff'
            with open(diff_file, 'w') as f:
                f.write(''.join(diff))
            self.pr('%s:1: Difference found' % (diff_file,))
            self.pr(''.join(diff))
            raise testparams.TestFailure('difference found')
        warnings = []
        errors = []
        for path in [srcfile, destfile]:
            with open(path, 'r') as f:
                for (lineno, line) in enumerate(f):
                    if line.startswith('// PORT-DML-WARNING'):
                        warnings.append((path, lineno + 2, line.split()[2]))
                    if line.startswith('// PORT-DML-ERROR'):
                        errors.append((path, lineno + 2, line.split()[2]))
        super(PortingConvert, self).verify_dmlc_messages(
            stderr, {'warning': warnings, 'error': errors})

    def verify_dmlc_messages(self, stderr, expected_msgs):
        super(PortingConvert, self).verify_dmlc_messages(stderr, expected_msgs)
        super(PortingConvert, self).verify_dmlc_messages(
            self.tagfile(), {'porting': self.PORTING})

    def tagfile(self):
        return join(self.scratchdir, 'tags')
    def test(self):
        super(PortingConvert, self).test()
        stdout = join(self.scratchdir, 'stdout.port-dml')
        stderr = join(self.scratchdir, 'stderr.port-dml')
        self.run_port_script(['--unittest'], stdout, stderr)
        for name in ['porting.dml',
                     'porting-common.dml',
                     'porting-import.dml',
                     'porting-imported.dml']:
            destfile = join(self.scratchdir, name)
            srcfile = join(testdir, '1.2', 'misc', name)
            expfile = join(testdir, '1.4', 'misc', name)
            self.port_test(self.tagfile(), srcfile, destfile, expfile,
                           stdout, stderr)
        self.port_test(self.tagfile(),
                       join(testdir, '1.2', 'misc', 'porting-common.dml'),
                       join(self.scratchdir, 'porting-common-compat.dml'),
                       join(testdir, '1.4', 'misc',
                            'porting-common-compat.dml'),
                       stdout, stderr,
                       extra_flags=['--compat'])
        lines = list(open(self.tagfile()))
        # shuffle tags and retry, to smoke out non-commutative rules
        for i in range(5):
            destfile = join(self.scratchdir, 'porting-%d.dml' % (i,))
            expfile = join(testdir, '1.4', 'misc', 'porting.dml')
            shuffled_file = join(self.scratchdir, 'shuffled-tags-%d' % (i,))
            seed = hash(time.time())
            self.pr('Shuffling with seed %d' % seed)
            random.seed(seed)
            shuffled = list(lines)
            random.shuffle(shuffled)
            with open(shuffled_file, 'w') as f:
                for line in shuffled:
                    f.write(line)
            self.port_test(shuffled_file,
                           join(testdir, '1.2', 'misc', 'porting.dml'),
                           destfile, expfile, stdout, stderr, [])

class PortingConvertFail(PortingConvert):
    __slots__ = ()
    port_dml_status = 1
    def test(self):
        super(PortingConvert, self).test()
        stdout = join(self.scratchdir, 'stdout.port-dml')
        stderr = join(self.scratchdir, 'stderr.port-dml')
        destfile = join(self.scratchdir, os.path.basename(self.filename))
        srcfile = self.filename
        expfile = join(testdir, '1.4', 'misc', os.path.basename(self.filename))
        self.port_test(self.tagfile(), srcfile, destfile, expfile,
                       stdout, stderr)

# Test that porting works
all_tests.append(PortingConvert(
    ["porting"],
    join(testdir, "1.2", "misc", "T_porting.dml"),
    api_version=latest_api_version,
    PORTING=[("porting.dml", None, tag) for tag in [
        'PSHA1',
        'PVERSION',
        'PNOTHROW',
        'PTHROWS',
        'PINPARAMLIST',
        'PSTRUCTDECL',
        'PFIELDRANGE',
        'PINLINEDECL',
        'PTYPEDOUTPARAM',
        'PINARGTYPE',
        'PINVOKE',
        'PLOGKIND',
        'PAFTER',
        'PAUTO',
        'PRETVAL',
        'PRETURNARGS',
        'PSESSION',
        'PHARD_RESET_VALUE',
        'PSOFT_RESET_VALUE',
        'PMISS_PATTERN',
        'PATTRIBUTE',
        'PEVENT',
        'POVERRIDE',
        'PBEFAFT',
        'PABSTRACT_TEMPLATE',
        'PCHANGE_INARGS',
        'PBITNEQ',
        'PVAL',
        'PNODOLLAR',
        'PDOLLAR_QUALIFY',
        'PCONSTANT',
        'PPARAMETER',
        'PARRAY',
        'PARRAY_I',
        'PHASH',
        'PHASHELSE',
        'PANDOR',
        'PIFAND',
        'PSTRINGIFY',
        'PWUNUSED',
        'PNO_WUNUSED',
        'PRENAME_TEMPLATE',
    ]]
    + [('dml-builtins.dml', None, 'PRETURNARGS'),
       ('porting-import.dml', None, 'PSHA1',),
       ('porting-import.dml', None, 'PVERSION'),
       ('porting-import.dml', None, 'PABSTRACT_TEMPLATE'),
       ('porting-import.dml', None, 'POVERRIDE_IMPORT'),
       ('porting-imported.dml', None, 'PSHA1',),
       ('porting-imported.dml', None, 'PVERSION'),
       ('porting-common.dml', None, 'PSHA1',),
       ('porting-common.dml', None, 'PVERSION'),
       ('porting-common.dml', None, 'PNO_WUNUSED',),
       ('porting-common.dml', None, 'PBEFAFT'),
       ('porting-common.dml', None, 'PTRAMPOLINE'),
       ('porting-common.dml', None, 'PIMPORT_DML12COMPAT',),
       ('porting-common.dml', None, 'PINLINEDECL',),
       ('porting-common.dml', None, 'PRETVAL',),
       ('porting-common.dml', None, 'PCHANGE_INARGS',),
       ('porting-common.dml', None, 'PABSTRACT_TEMPLATE',),
       ('T_porting.dml', None, 'PSHA1',),
       ('T_porting.dml', None, 'PVERSION'),
       ('dml-builtins.dml', None, 'PWUNUSED'),
       ('dml-builtins.dml', None, 'PNO_WUNUSED'),
       ('utility.dml', None, 'PWUNUSED'),
       ('utility.dml', None, 'PNO_WUNUSED'),
       ('utility.dml', None, 'POVERRIDE'),
       ('utility.dml', None, 'PRENAME_TEMPLATE'),
       ('utility.dml', None, 'PABSTRACT_TEMPLATE'),
       ('utility.dml', None, 'PHASH'),
       ('utility.dml', None, 'PIFAND'),
       ('utility.dml', None, 'PANDOR'),
       ('utility.dml', None, 'PCHANGE_INARGS'),
    ]))

all_tests.append(PortingConvertFail(
    ["porting_fail"],
    join(testdir, "1.2", "misc", "T_porting_fail.dml"),
    api_version=latest_api_version,
    PORTING=[("T_porting_fail.dml", None, tag) for tag in [
        'PSHA1',
        'PVERSION',
        'POVERRIDE',
        'PPARAMETER',
        'PNO_WUNUSED',
        ]]
    + [('dml-builtins.dml', None, 'PRETURNARGS'),
       ('dml-builtins.dml', None, 'PWUNUSED'),
       ('dml-builtins.dml', None, 'PNO_WUNUSED')]))

class CompareIllegalAttrs(BaseTestCase):
    __slots__ = ()
    prereqs = ['minimal']
    def test(self):
       # Extract list of automatic attributes from Simics
        sl = {x[5:].strip()
              for x in subprocess.run(
                      [join(simics_root_path(), "bin", "simics" + bat_sfx),
                       "-batch-mode", "-quiet", "-no-copyright", "-no-settings",
                       "-core", "-werror",
                       "-L", join(testparams.sandbox_path(), "scratch",
                                  "minimal"),
                       "-e", "@SIM_create_object('test', 'o', [])",
                       "-e", "@for n in conf.o.attributes: print('ATTR', end=' '), print(n[0])"],
                      capture_output=True,
                      encoding='utf-8').stdout.splitlines()
              if x.startswith('ATTR ')}
        #self.pr("Automatic attributes: %r" % sl)

        # Extract list of illegal attributes from dmlc
        dl = {x.strip()
              for x in subprocess.run(
                      [join(simics_root_path(), "bin", "dmlc" + bat_sfx),
                       "--illegal-attributes"],
                      capture_output=True,
                      encoding='utf-8').stdout.splitlines()}
        #self.pr("Illegal attributes: %r" % dl)

        if dl != sl:
            for n in dl - sl:
                self.pr("- " + n)
            for n in sl - dl:
                self.pr("+ " + n)
            raise TestFail('difference')

all_tests.append(CompareIllegalAttrs('compare-illegal-attrs'))

all_tests.append(CTestCase(["T_EIDXVAR_info"],
                           join(testdir, "1.2", "errors", "T_EIDXVAR.dml"),
                           api_version=latest_api_version,
                           status=2, dmlc_extraargs = ["--info"]))

class DevInfoCompare(BaseTestCase):
    __slots__ = ('expected', 'prereqs', 'xml_path')
    def __init__(self, name, prereq, xml_path, version):
        BaseTestCase.__init__(self, name)
        self.prereqs = [prereq]
        self.xml_path = xml_path
        if version == "1.2":
            self.expected = '''\
<?xml version="1.0" encoding="UTF-8"?>
<device name="test" bitorder="le">
  <bank name="b" vsize="4 2" desc="bank desc" documentation="bank doc" byte_order="little-endian" function="3 4 5 6 7 8 9 10">
    <register name="r1" vsize="2" documentation="reg1 doc" offset="18 22 18 22 18 22 18 22 18 22 18 22 18 22 18 22" size="4">
      <field name="f0" desc="f0 desc" msb="7" lsb="3" />
      <field name="f1" msb="2" lsb="1" />
    </register>
    <register name="rma" vsize="3 3" offset="38 42 46 50 -1 58 62 66 70 38 42 46 50 -1 58 62 66 70 38 42 46 50 -1 58 62 66 70 38 42 46 50 -1 58 62 66 70 38 42 46 50 -1 58 62 66 70 38 42 46 50 -1 58 62 66 70 38 42 46 50 -1 58 62 66 70 38 42 46 50 -1 58 62 66 70" size="4">
    </register>
    <register name="ru" vsize="3" offset="26 -1 34 26 -1 34 26 -1 34 26 -1 34 26 -1 34 26 -1 34 26 -1 34 26 -1 34" size="4">
    </register>
  </bank>
</device>
'''
        else:
            self.expected = '''\
<?xml version="1.0" encoding="UTF-8"?>
<device name="test" bitorder="le">
  <bank name="b" vsize="4 2" byte_order="little-endian" function="3 4 5 6 7 8 9 10">
  </bank>
</device>
'''

    def test(self):
        with open(self.xml_path) as f:
            actual_lines = [line.rstrip('\n') for line in f]
        # TODO: we should do a structural comparison of XMLg
        # content, instead of checking that characters match
        fail = False
        for (line, (actual, expected)) in enumerate(itertools.zip_longest(
                actual_lines,
                self.expected.strip().splitlines(),
                fillvalue='')):
            if expected.strip() != actual.strip():
                self.pr('%s:%d: difference found' % (self.xml_path, line + 1))
                self.pr('Expected: %r' % (expected.strip(),))
                self.pr('Actual:   %r' % (actual.strip(),))
                fail = True
        if fail:
            raise TestFail('difference')

this_year = time.gmtime().tm_year

class CopyrightTestCase(BaseTestCase):
    __slots__ = ()
    def validate_file(self, path, regexp):
        match = regexp.match(path.read_bytes())
        if not match:
            return 'did not find copyright banner'
        start = int(match.group(1))
        end = start if match.group(2) is None else int(match.group(2))
        if end != this_year:
            return 'copyright year too old'
        if not 2000 < start <= end:
            return f'strange copyright year: {start}'
        return None
    def test(self):
        mpl_lines = f'''\
© (2[0-9]*)(?:-(2[0-9]*))? Intel Corporation
SPDX-License-Identifier: MPL-2.0
'''.encode('utf-8').splitlines()
        bsd0_copyright_re = re.compile(f'''/[*]
  © (2[0-9]*)(?:-(2[0-9]*))? Intel Corporation
  SPDX-License-Identifier: 0BSD
[*]/'''.encode('utf-8'))
        dml_copyright_re = re.compile(
            b'/[*]\n' + b'\n'.join(line and b'  ' + line
                                 for line in mpl_lines) + b'\n[*]/')
        py_copyright_re = re.compile(b'\n'.join(b'#' + (line and b' ' + line)
                                               for line in mpl_lines))
        xml_copyright_re = re.compile(
            b'<!--\n' + b'\n'.join(line and b'  ' + line
                                   for line in mpl_lines) + b'\n-->')

        root = Path(__file__).parent.parent
        assert (root / 'Makefile').is_file()
        files = sorted(subprocess.run(
            ['git', 'ls-tree', '-r', '--name-only', '@', '.'],
            cwd=root, capture_output=True, check=True,
            encoding='utf-8').stdout.splitlines())
        assert len(files) > 500
        errors = []
        ignorelist = {
            # states its own terms
            'LICENSE',
            'include/simics/LICENSE',
            'lib/1.2/LICENSE',
            'lib/1.4/LICENSE',
            'lib-old-4.8/1.2/LICENSE',
            # file listing
            'MODULEINFO',
            # essentially data files
            'doc/1.2/toc.json',
            'doc/1.4/toc.json',
            # data file
            'py/dml/snyk-requirements.txt',
            # Would either need blurb with \r\n, or inconsistent line endings.
            # Don't bother.
            'test/1.2/misc/T_dos_newline.dml',
            # empty
            'test/SUITEINFO',
            # data file
            'test/XFAIL',
            # config files
            '.github/workflows/dependent-issues.yml',
            '.gitignore',
        }
        for f in ignorelist:
            assert (root / f).is_file(), f
        nonexisting = ignorelist.difference(files)
        assert not nonexisting, nonexisting
        for f in files:
            if f in ignorelist:
                continue
            if f.startswith(('lib', 'include')):
                assert f.endswith(('.dml', '.h'))
                error = self.validate_file(root / f, bsd0_copyright_re)
                if error:
                    errors.append((f, error))
            elif f.endswith(('.dml', '.h')):
                error = self.validate_file(root / f, dml_copyright_re)
                if error:
                    errors.append((f, error))
            elif f.endswith('.py') or f == 'Makefile':
                error = self.validate_file(root / f, py_copyright_re)
                if error:
                    errors.append((f, error))
            elif f.endswith(('.md', '.docu')):
                error = self.validate_file(root / f, xml_copyright_re)
                if error:
                    errors.append((f, error))
            else:
                errors.append((f, 'unknown file type'))
        for (f, error) in errors:
            self.pr(f'{root / f}:1: error: {error}')
        if errors:
            raise TestFail('missing copyrights')

all_tests.append(CopyrightTestCase('copyright'))

# Device info XML generation works
for version in ['1.2', '1.4']:
    testname = '%s-devinfo-generate' % (version,)
    generate = CTestCase(
        [testname],
        join(testdir, version, 'misc', 'devinfo.dml'),
        api_version=latest_api_version,
        dmlc_extraargs=['--info'])
    all_tests.append(generate)
    all_tests.append(DevInfoCompare(
        '%s-devinfo-compare' % (version,),
        testname, generate.cfilename + '.xml', version))

for version in ['1.2', '1.4']:
    all_tests.append(
        XmlTestCase(
            ['%s-register-view' % (version,)],
            join(testdir, version, 'misc', 'register_view.dml'),
            api_version=latest_api_version,
            dmlc_extraargs=['--info']))
    all_tests.append(
        XmlTestCase(
            ['%s-register-view-descriptions' % (version,)],
            join(testdir, version, 'misc', 'register_view_descriptions.dml'),
            api_version=latest_api_version,
            dmlc_extraargs=['--info']))
    all_tests.append(
        XmlTestCase(
            ['%s-register-view-bitorder-le' % (version,)],
            join(testdir, version, 'misc', 'register_view_bitorder_le.dml'),
            api_version=latest_api_version,
            dmlc_extraargs=['--info']))
    all_tests.append(
        XmlTestCase(
            ['%s-register-view-bitorder-be' % (version,)],
            join(testdir, version, 'misc', 'register_view_bitorder_be.dml'),
            api_version=latest_api_version,
            dmlc_extraargs=['--info']))
    all_tests.append(
        XmlTestCase(
            ['%s-register-view-inquiry' % (version,)],
            join(testdir, version, 'misc', 'register_view_inquiry.dml'),
            api_version=latest_api_version,
            dmlc_extraargs=['--info']))
    all_tests.append(
        XmlTestCase(
            ['%s-register-view-fields' % (version,)],
            join(testdir, version, 'misc', 'register_view_fields.dml'),
            api_version=latest_api_version,
            dmlc_extraargs=['--info']))

def walk(rootdir):
    # This is similar to os.walk()
    queue = [[rootdir]]
    while queue:
        qh = queue.pop(0)
        qfn = join(*qh)
        if isdir(qfn):
            queue[0:0] = [qh + [fn] for fn in sorted(os.listdir(qfn))]
        elif re.match("^T_(.*).dml$", qh[-1]):
            qh[-1] = qh[-1][2:-4]
            yield (qfn, qh)

for (testfile, testpath) in walk(testdir):
    all_tests.append(
        CTestCase(testpath[1:], testfile,
                 api_version=simics_api_version(testfile)))

class ImportTest(CTestCase):
    __slots__ = ('files', 'extra_code', 'dml_version')
    def __init__(self, testname, dml_version, api_version, files,
                 extra_code = "", cc_extraargs=()):
        filename = join(testparams.sandbox_path(), "scratch",
                        testname + ".dml")
        CTestCase.__init__(self,  [testname], filename,
                           api_version=api_version,
                           cc_extraargs=cc_extraargs)
        self.files = files
        self.extra_code = extra_code
        self.dml_version = dml_version
    def test(self):
        filelist = ' '.join(self.files)
        if len(filelist) > 500:
            filelist = filelist[:500] + "..."
        self.pr('Importing files: ' + filelist)
        with open(self.filename, "w") as f:
            print("dml %s;" % (self.dml_version,), file=f)
            print("device test;", file=f)
            print("/// COMPILE-ONLY", file=f)
            for apifile in self.files:
                print('import "%s";' % apifile.replace('\\', '/'), file=f)
            print(self.extra_code, file=f)
        super(ImportTest, self).test()

pci_hotplug = "parameter pci_hotplug = true;"
rapidio_bank = "bank regs;"
for dmlver in ['1.2', '1.4']:
    basedir = join(project_host_path(), "bin", "dml", dmlver, "*.dml")
    lib_files = list(map(os.path.basename, glob.glob(basedir)))
    all_tests.append(ImportTest(
        'lib-dml-%s-api-%s' % (dmlver, latest_api_version),
        dmlver, latest_api_version, lib_files,
        rapidio_bank if dmlver == '1.2' else ''))

# header files that should not be tested with all API versions
limited_api_testing = {
    join("simics", "util", "data-structs.dml") : ["4.8"]}

def api_files(dml_version, api_version):
    files = []
    basedir = join(project_host_path(), "bin", "dml", "api", api_version,
                   dml_version)

    for (root, dname, fnames) in os.walk(basedir):
        for fn in fnames:
            if fn.endswith('.dml'):
                p = join(root[len(basedir) + 1:], fn)
                if p in limited_api_testing:
                    if api_version not in limited_api_testing[p]:
                        continue
                files.append(p)

    return files

api_versions = sorted(os.listdir(join(project_host_path(), "bin", "dml",
                                      "api")))
assert api_versions
for dmlver in ["1.2", "1.4"]:
    for apiver in api_versions:
        testname = "api-dml-%s-api-%s" % (dmlver, apiver)
        files = api_files(dmlver, apiver)
        assert files, (dmlver, apiver)
        if dmlver == "1.4" and is_windows():
            # Prevents some name clashes with windows.h,
            # implicitly imported from a few headers
            cc_extraargs = ["-DWIN32_LEAN_AND_MEAN"]
        else:
            cc_extraargs = []
        all_tests.append(ImportTest(
            testname, dmlver, apiver, files, cc_extraargs=cc_extraargs))

def filter_tests(tests):
    # Map test name to test object
    test_dict = {t.fullname: t for t in tests}
    # No duplicate names allowed
    assert len(test_dict) == len(tests)
    # Map test name to list of names of tests it is a prerequisite of
    prereqs = {}
    for t in tests:
        for p in t.prereqs:
            # input list must be topologically ordered
            assert tests.index(test_dict[p]) < tests.index(t)
            prereqs.setdefault(p, []).append(t.fullname)

    for test in tests:
        if any(active_test(name)
               for name in [test.fullname] + prereqs.get(test.fullname, [])):
            yield test

def worker(job_queue, test_dict):
    while True:
        try:
            test = job_queue.get_nowait()
        except queue.Empty:
            break
        try:
            # Handle prerequisites by blocking until they all are
            # finished. This is somewhat wasteful, but simple, and
            # prerequisites are rare.
            for prereq in test.prereqs:
                test_dict[prereq].finished.wait()
            test.run_test(DeferredOutput())
        finally:
            test.finished.set()
        job_queue.task_done()

def run_tests_in_parallel(tests, num_threads):
    # Map test name to test object
    test_dict = {t.fullname: t for t in tests}
    q = queue.Queue()
    for t in tests:
        q.put(t)
    for _ in range(num_threads):
        t = threading.Thread(target=worker, args=(q, test_dict))
        t.daemon = True
        t.start()

try:
    num_threads = int(os.environ['T126_JOBS'])
except KeyError:
    num_threads = 8

def await_test_finish(t):
    def wait():
        t.finished.wait()
        for (fun, args) in t.output.log:
            fun(*args)
    return wait

def tests(suite):
    tests = list(filter_tests(all_tests))
    # our filtering is better than testparams, because we run a
    # filtered-out test if it's needed as a dependency of another test.
    testparams.test_patterns = None
    # Use our own mechanism for parallelism, because it respects test
    # dependencies properly. TODO: we should fix testparams to support
    # dependency-based declaration of test parallelism, and switch to
    # use that
    run_tests_in_parallel(tests, num_threads)
    for t in tests:
        suite.add_test(t.fullname, await_test_finish(t))
