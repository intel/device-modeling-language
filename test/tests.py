# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys, time
import os, os.path
import re
import itertools
import queue
import threading
import random
import subprocess
import difflib
from pathlib import Path
from os.path import join, isdir, exists
import glob
import json
from simicsutils.host import host_type, is_windows, batch_suffix
from simicsutils.internal import package_path, get_simics_major
import testparams
from testparams import simics_base_path
import traceback
from depfile import parse_depfile
import pstats
import tarfile

def project_host_path():
    return join(testparams.project_path(),
                        testparams.host_platform())

sys.path.append(join(project_host_path(), 'bin', 'dml', 'python'))
import dml.globals
import dead_dml_methods
from dead_dml_methods import line_directive_re

class TestFail(Exception):
    def __init__(self, reason):
        Exception.__init__(self)
        self.reason = reason

sys.path.append(join(simics_base_path(), "scripts", "build"))
import module_id

def host_path():
    return join(testparams.simics_base_path(),
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

mini_python = [join(testparams.project_path(), "bin",
                    f"mini-python{batch_suffix()}")]
dmlc_py = [join(project_host_path(), "bin", "dml", "python")]
main_dmlc = mini_python + dmlc_py
# Alternative python interpreter for DMLC including args,
# e.g. '["pypy3", "-X", "utf8"]'
pypy = os.environ.get('DMLC_PYTHON')
pypy_dmlc = None if pypy is None else json.loads(pypy) + dmlc_py

line_directives = {None: True, 'yes': True, 'no': False}[
    os.environ.get('DMLC_LINE_DIRECTIVES')]

if not is_windows():
    dmlc_lib_path = ":".join(
        [join(simics_base_path(), host_type(), p) for p in ('bin', 'sys/lib')]
        + [os.environ.get('LD_LIBRARY_PATH', '')])

# map from test fullname (subtest name) to timeout multiplier
dmlc_timeout_multipliers = {"1.2/registers/largearray": 3,
                            }
dmlc_default_timeout = 80
def dmlc_reaper_args(exitcode_file, timeout_multiplier = 1):
    return [join(host_path(), "bin", "reaper"),
            "-t", str(dmlc_default_timeout * timeout_multiplier),
            "-r", "-e", exitcode_file]

common_cflags = ["-O2", "-std=gnu99", '-Wall', '-Werror', '-Wpointer-arith',
                 '-Wwrite-strings', '-Wformat-nonliteral',]
cc = os.environ.get('DMLC_CC')
if is_windows():
    if not cc:
        try:
            cc = testparams.mingw_cc()
            cc_found = os.path.exists(cc)
        except Exception:
            cc_found = False
        if not cc_found:
            raise TestFail('gcc not found(specify gcc by env var DMLC_CC)')
        ldflags = [f"-L{testparams.mingw_root()}/lib/gcc/x86_64-w64-mingw32/lib"]
    else:
        ldflags = []
    cflags = common_cflags + [
        "-DUSE_MODULE_HOST_CONFIG", "-D__USE_MINGW_ANSI_STDIO=1"]
else:
    if not cc:
        cc = join(package_path(), "gcc_6.4.0", "bin", "gcc")
        if not os.path.exists(cc):
            raise TestFail('gcc not found(specify gcc by env var DMLC_CC)')
    cflags = ['-g', '-fPIC', '-Wundef'] + common_cflags
    ldflags = []
cc = [cc]
cflags_shared = ["-shared"]

os.environ['DMLC_DEBUG'] = 't'

latest_api_version = "6"

special_versions = {}

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

    def expect_equal_sets(self, a, b):
        assert isinstance(a, set) and isinstance(b, set)
        if a != b:
            import traceback
            for line in traceback.format_stack():
                self.pr(line.rstrip())
            self.pr('missing elements: %r' % ((b - a),))
            self.pr('unexpected elements: %r' % ((a - b),))
            raise TestFail('difference found')

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
        'extraenv',                     # Extra environment variables

        'status',                       # Expected status

        'thread_safe',                  # If the device module is considered thread-safe
        )
    def __init__(self, fullname, filename, **info):
        BaseTestCase.__init__(self, fullname)
        # Defaults
        self.filename = filename
        self.api_version = latest_api_version
        self.includepath = None
        self.dmlc_extraargs = []
        self.cc_extraargs = []
        self.status = 0
        self.extraenv = {}
        self.thread_safe = False
        # Override defaults
        for k,v in info.items():
            setattr(self, k, v)
        if self.includepath is None:
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

    def compare_pypy_dmlc(self, reaper, args, env):
        files = ([self.dmlc_stdout, self.dmlc_stderr]
                 + [f'{self.cfilename}{suff}'
                    for suff in ['.c', '-protos.c', '.h', '-struct.h']])
        for f in files:
            os.rename(f, f + '.mini-python')
        argv = reaper + pypy_dmlc + args
        self.pr(' '.join(argv))
        with open(self.dmlc_stdout, "w") as stdout, open(
                self.dmlc_stderr, "w") as stderr:
            status = subprocess.call(
                argv, stdout=stdout, stderr=stderr,
                cwd=self.scratchdir, env=env)
        if status != 0:
            self.print_logs('pypy-dmlc', self.dmlc_stdout, self.dmlc_stderr)
            raise TestFail(f'exit code {status} from {pypy_dmlc}')
        for f in files:
            orig = Path(f + '.mini-python').read_text().splitlines(
                keepends=True)
            new = Path(f).read_text().splitlines(keepends=True)
            if orig != new:
                diff = ''.join(difflib.unified_diff(
                    orig, new, f + '.mini-python', f))
                diff_file = Path(f + '.diff')
                diff_file.write_text(diff)
                self.pr(f'{diff_file}:1: Difference found')
                self.pr(diff)
                self.print_logs('pypy-dmlc', self.dmlc_stdout, self.dmlc_stderr)
                raise TestFail('difference found')
        return 0

    def run_dmlc(self, filename, dmlc_extraargs):
        name = self.shortname
        self.dmlc_stdout = join(self.scratchdir, name+'.dmlc_stdout')
        self.dmlc_stderr = join(self.scratchdir, name+'.dmlc_stderr')
        exitcode_file = join(self.scratchdir, name + '.dmlc_exitcode')
        reaper = dmlc_reaper_args(
            exitcode_file,
            dmlc_timeout_multipliers.get(self.fullname, 1))
        args = ["-T"]
        if not line_directives:
            args += ["--noline"]
        args += dmlc_extraargs
        if self.api_version:
            args += ["--simics-api=" + self.api_version]
        if self.api_version in ["4.8"]:
            args += ["-m"]

        for d in self.includepath:
            args.extend(["-I", d])

        args.extend([filename, os.path.basename(self.cfilename)])

        argv = reaper + main_dmlc + args

        self.pr(" ".join(argv))

        env = os.environ.copy()
        env.update(self.extraenv)
        with open(self.dmlc_stdout, "w") as stdout, open(
                self.dmlc_stderr, "w") as stderr:
            status = subprocess.call(argv,
                                     stdout=stdout,
                                     stderr=stderr,
                                     cwd=self.scratchdir, env = env)
        if status == 0:
            if pypy_dmlc:
                return self.compare_pypy_dmlc(reaper, args, env)
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
                     compile_only=False,
                     no_cc=False):
            self.exp_warnings = list(exp_warnings)
            self.exp_errors = list(exp_errors)
            self.exp_stdout = list(exp_stdout)
            self.cc_flags = list(cc_flags)
            self.dmlc_flags = list(dmlc_flags)
            self.instantiate_manually = instantiate_manually
            self.compile_only = compile_only
            self.no_cc = no_cc
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
            elif key == 'NO-CC':
                flags.no_cc = True
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

def unittest_parse_messages():
    stderr = '''
/tmp/f.dml:3:20: In template abc
/tmp/g.dml:4:9: error EDPARAM: blah blah
/tmp/f.dml:8:12: conflicting definition
'''.splitlines()
    assert (list(DMLFileTestCase.parse_messages(stderr))
            == [('error', 'EDPARAM',
                 [('g.dml', 4, "/tmp/g.dml:4:9: error EDPARAM: blah blah"),
                  ('f.dml', 3, "/tmp/f.dml:3:20: In template abc"),
                  ('f.dml', 8, "/tmp/f.dml:8:12: conflicting definition")])])
unittest_parse_messages()

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
                f"-I{Path(simics_base_path()) / 'src' / 'include'}",
                f"-I{Path(project_host_path()) / 'bin' / 'dml' / 'include'}"]
            + list(self.cc_extraargs) + cc_extraargs + [
                "-o", self.cfilename + ".o", self.cfilename + ".c"])

        env = os.environ.copy()
        env.update(self.extraenv)
        result = subprocess.call(args,
                                 stdout = open(self.cc_stdout, "w"),
                                 stderr = open(self.cc_stderr, "w"),
                                 env = env)
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
            thread_safe = "yes" if self.thread_safe else "no"
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

        env = os.environ.copy()
        env.update(self.extraenv)

        self.pr("Compiling module_id.c")
        args = cc + cflags + \
               ["-c",
                "-I" + self.scratchdir,
                "-I" + join(simics_base_path(), "src", "include"),
                "-o", module_id_base + ".o",
                module_id_base + ".c"]

        status = subprocess.call(args,
                                 stdout = open(self.ld_stdout, "w"),
                                 stderr = open(self.ld_stderr, "w"),
                                 env = env)
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
                "-L"+join(host_path(), "bin")] + ldflags + \
                ["-lsimics-common", "-lvtutils"]
        args.append("-Wl,--no-undefined")

        status = subprocess.call(args,
                                 stdout = open(self.ld_stdout, "a"),
                                 stderr = open(self.ld_stderr, "a"),
                                 env = env)
        if status != 0:
            self.pr("LD: %r" % args)
        return status

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
        sc.write("SIM_add_module_dir(scratchdir)\n")
        sc.write("SIM_module_list_refresh()\n")
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
        elif self.fullname.startswith(('1.2/', 'bugs/')):
            sc.write("if not obj.runtest:\n")
            sc.write("    print('test attribute returned false')\n")
            sc.write("    SIM_quit(1)\n")
        sc.write("SIM_quit(0)\n")
        sc.close()

        self.pr("Running Simics")
        args = [join(simics_base_path(), "bin", "simics" + bat_sfx),
                "--batch-mode", "--quiet", "--no-copyright", "--no-settings",
                "--dump-core", "--werror",
                "--project", testparams.project_path(),
                self.scriptname]
        env = os.environ.copy()
        env.update(self.extraenv)
        env['SIMICS_HOST'] = os.path.basename(host_path())
        env['SIMICS_ROOT'] = simics_base_path()
        # self.pr("ARGS: %r" % args)
        ret = subprocess.call(args,
                              stdout = open(self.simics_stdout, "w"),
                              stderr = open(self.simics_stderr, "w"),
                              env = env,
                              cwd=self.scratchdir)
        if ret:
            self.pr(f"Simics command-line: {' '.join(args)}")
        return ret

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

        if flags.no_cc:
            assert flags.compile_only
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
                if r not in found:
                    self.pr(self.simics_stdout + ":0: Didn't find %r" % s)
                    raise TestFail("grep miss")

        self.pr("Everything seems OK")

class XmlTestCase(CTestCase):
    __slots__ = ()
    def run_simics(self, pyfile=None, auto_instantiate=True):
        os.rename('%s.xml' % self.cfilename, join(self.scratchdir, 'test.xml'))
        return CTestCase.run_simics(self, pyfile, auto_instantiate)

class DMLCProfileTestCase(CTestCase):
    __slots__ = ()

    def compare_pypy_dmlc(*args):
        # Skip check, indeterministic stdout is expected from dmlc
        return 0

    def test(self):
        super().test()
        # Check the existence of profiling data
        stats_file_name = os.path.splitext(self.cfilename)[0] + ".prof"
        if exists(stats_file_name):
            try:
                # Check the validity of profiling data
                pstats.Stats(stats_file_name)
            except:
                raise TestFail('cannot load profile data')
        else:
            raise TestFail('stats file not generated')


class SizeStatsTestCase(CTestCase):
    '''Test that the DMLC_GATHER_SIZE_STATS variable works.  It creates a
    json file with a list of [size, num, location] tuples.'''
    __slots__ = ()
    def test(self):
        super().test()
        stats = json.loads(
            (Path(self.scratchdir)
             / f'T_{self.shortname}-size-stats.json').read_text())
        funcs = [tuple(x) for x in stats if 'size_stats.dml:' in x[2]]
        self.pr(f'{funcs}')
        # five methods: m1, m2, m3, m4, init
        assert len(funcs) == 5
        # sorted by size, descending
        sz = [s for (s, *_) in funcs]
        assert sz[0] >= sz[1] >= sz[2] >= sz[3] >= sz[4]
        # the biggest two are large (caused by code explosion)
        assert sz[1] > 5000
        # the third and fourth are optimized and much smaller
        assert sz[2] < 2000
        assert sz[3] > 500
        # the last one (init) is small
        assert sz[4] < 500
        # the second field is the number of instances of each method. One
        # of the two largest is multiplied, remaining are singleton
        assert {num for (_, num, _) in funcs[:2]} == {1, 12}
        assert [num for (_, num, _) in funcs[2:4]] == [1, 1]
        by_lineno = sorted(funcs, key=lambda t: int(t[2].rsplit(':')[-2]))
        # the largest two appear second and fourth in the file
        assert {by_lineno[1], by_lineno[3]} == {funcs[0], funcs[1]}


class DumpInputFilesTestCase(CTestCase):
    '''Test that the DMLC_DUMP_INPUT_FILES variable works.  It creates a
    tarball of input DML files that can be compiled standalone.'''
    __slots__ = ('prefix',)
    def __init__(self, *args, prefix='', **kwargs):
        self.prefix = prefix
        super().__init__(*args, **kwargs)
    def test(self):
        super().test()
        dir = Path(self.scratchdir) / 'dumped'
        dir.mkdir()
        with tarfile.open(Path(self.scratchdir) / f'T_{self.shortname}.tar.xz',
                          'r:xz') as tf:
            tf.extractall(dir)
        assert (dir.joinpath(*self.prefix)
                / os.path.basename(self.filename)).is_file()
        if not is_windows():
            # This does not work on Windows, for unknown reasons.
            # Seems related to symlink semantics somehow, but no
            # need to explore deeper until we have a use case for it.
            cmd = main_dmlc + [
                join(*(self.prefix + [os.path.basename(self.filename)])),
                self.shortname]
            self.pr(f"Running: {' '.join(cmd)}")
            output = Path(self.scratchdir) / 'recompile.out'
            try:
                with open(output, 'w') as f:
                    subprocess.check_call(cmd, cwd=dir,
                                          stdout=f,
                                          stderr=subprocess.STDOUT)
            finally:
                self.pr(output.read_text())
            assert (dir / (self.shortname + '.c')).is_file()

all_tests = []
def subtest(*args, **kwargs):
    def register(cls):
        all_tests.append(cls(*args, **kwargs))
        return cls
    return register


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
all_tests.append(CTestCase(["minimal"], join(testdir, "minimal.dml")))

# Test that it fails with a good error message if it can't find
# dml-builtins.dml etc.
all_tests.append(ErrorTest(["noinclude"], join(testdir, "minimal.dml"),
                           errors=[("minimal.dml", 6, "EIMPORT")],
                           includepath=(), dmlc_extraargs=['--max-errors=1']))

# Test DMLC_PROFILE
all_tests.append(DMLCProfileTestCase(["dmlc_profile"],
                                     join(testdir, "minimal.dml"),
                                     extraenv={'DMLC_PROFILE': '1'}))

all_tests.append(DumpInputFilesTestCase(
    ["dump-input-files-minimal"],
    join(testdir, 'minimal.dml'),
    prefix=[],
    extraenv={'DMLC_DUMP_INPUT_FILES': '1'}))

all_tests.append(DumpInputFilesTestCase(
    ["dump-input-files"],
    join(testdir, '1.4', 'misc', 'T_import_rel.dml'),
    prefix=['_'],
    extraenv={'DMLC_DUMP_INPUT_FILES': '1'}))

all_tests.append(SizeStatsTestCase(
    ["size-stats"],
    join(testdir, '1.4', 'misc', 'size_stats.dml'),
    extraenv={'DMLC_GATHER_SIZE_STATISTICS': '1'}))

# Test that it fails with a good error message if it can't create the
# output files.
#run_test(join(testdir, "minimal.dml"), name = "not-writable",
#         scratchdir = "./dontexist",
#         ERROR = "??")

# Test the -g flag
all_tests.append(CTestCase(
         ["debuggable-compile"],
         join(testdir, "1.2", "methods", "T_inline.dml"),
         dmlc_extraargs = ["-g"]))
all_tests.append(CTestCase(
         ["debuggable-compile-connect"],
         join(testdir, "1.2", "structure", "T_connect_obj.dml"),
         dmlc_extraargs = ["-g"]))
all_tests.append(CTestCase(
         ["debuggable-compile-inlined_param"],
         join(testdir, "1.4", "expressions", "T_inlined_param.dml"),
         dmlc_extraargs = ["-g"]))

class SplitTestCase(CTestCase):
    __slots__ = ()

    def compare_pypy_dmlc(*args):
        # Skip check, output files don't follow standard naming scheme
        return 0

    def run_cc(self, cc_extraargs):
        assert not cc_extraargs
        files = glob.glob(self.cfilename + "-[0-9]*.c")
        assert len(files) > 10, files
        env = os.environ.copy()
        env.update(self.extraenv)
        for fn in files:
            self.cc_stdout = join(self.scratchdir, fn + '.cc_stdout')
            self.cc_stderr = join(self.scratchdir, fn + '.cc_stderr')
            args = (cc + cflags + [
                "-c",  f"-I{self.scratchdir}",
                f"-I{Path(simics_base_path()) / 'src' / 'include'}",
                f"-I{Path(project_host_path()) / 'bin' / 'dml' / 'include'}"]
                    + self.cc_extraargs + ["-o", fn[:-2] + ".o", fn])

            result = subprocess.call(args,
                                     stdout = open(self.cc_stdout, "w"),
                                     stderr = open(self.cc_stderr, "w"),
                                     env = env)
            if result != 0:
                self.pr("CC: " + " ".join(args))
                return result
        self.cc_stdout = join(self.scratchdir, self.shortname + '.cc_stdout')
        self.cc_stderr = join(self.scratchdir, self.shortname + '.cc_stderr')
        ld = cc + ['-r', '-nostdlib', '-o', self.cfilename + '.o'] + [
            fn[:-2] + '.o' for fn in files]

        result = subprocess.call(ld,
                                 stdout = open(self.cc_stdout, "w"),
                                 stderr = open(self.cc_stderr, "w"),
                                 env = env)
        if result != 0:
            self.pr("CCLD: " + " ".join(ld))
        return result
all_tests.append(SplitTestCase(
         ["split"],
         join(testdir, "1.2", "misc", "T_split_output.dml"),
         dmlc_extraargs=["--split-c-file=1", "--state-change-dml12"]))

all_tests.append(CTestCase(
         ["1.2", "misc", "test_dmlc_g"],
         join(testdir, "1.2", "misc", "test_dmlc_g.dml"),
         dmlc_extraargs = ["-g"]))

all_tests.append(CTestCase(
         ["1.4", "misc", "test_dmlc_g"],
         join(testdir, "1.4", "misc", "test_dmlc_g.dml"),
         dmlc_extraargs = ["-g"]))

# Test the --werror flag
all_tests.append(CTestCase(
         ["werror"],
         join(testdir, "1.2", "werror", "T_WUNUSEDDEFAULT.dml"),
         status = 2,
         dmlc_extraargs = ["--werror"]))

all_tests.append(CTestCase(
         ["1.4", "misc", "thread_aware"],
         join(testdir, "1.4", "misc", "thread_aware.dml"),
         thread_safe=True))

if get_simics_major() == "6":
    all_tests.append(CTestCase(
        ["1.2", "errors", "WREF"],
        join(testdir, "1.2", "errors", "WREF.dml"),
        api_version="5"))

if get_simics_major() == "7":
    all_tests.append(CTestCase(
        ["1.4", "errors", "ETYPE_integer_t"],
        join(testdir, "1.4", "errors", "ETYPE_integer_t.dml"),
        api_version="7"))

class DebuggableCheck(BaseTestCase):
    __slots__ = ()
    def test(self):
        lines = open(join(testparams.sandbox_path(), "scratch",
                          "debuggable-compile", "T_debuggable-compile.c"),
                     "r").readlines()
        if ("static bool _DML_M_mmm(test_t *_dev, int x, int *y)\n"
            not in lines):
            raise TestFail("didn't find expected code")
    prereqs = ['debuggable-compile']

all_tests.append(DebuggableCheck('debuggable-check'))

@subtest('--help-no-compat')
@subtest('--help-warn')
@subtest('--help')
class HelpTest(BaseTestCase):
    '''Check that some DMLC flag works'''
    __slots__ = ()
    def test(self):
        cmd = main_dmlc + [self.fullname]
        self.pr(f"Running: {' '.join(cmd)}")
        ret = subprocess.run(cmd, capture_output=True, text=True)
        self.pr(ret.stderr)
        self.pr(ret.stdout)
        if ret.returncode:
            raise TestFail('dmlc returncode {ret.returncode}')

class DmlDepBase(CTestCase):
    '''Base class for DML dependency test cases.'''
    __slots__ = ()
    def compare_pypy_dmlc(*args):
        # Skip check, not all files created
        return 0

    def run_cc(self, cc_extraargs):
        assert not cc_extraargs
        name = self.shortname
        self.cc_stdout = join(self.scratchdir, name+'.cc_stdout')
        self.cc_stderr = join(self.scratchdir, name+'.cc_stderr')

        args = (
            cc + cflags + [
                self.cfilename + '-cdep.c',
                f"-I{self.scratchdir}",
                f"-I{Path(simics_base_path()) / 'src' / 'include'}",
                f"-I{Path(project_host_path()) / 'bin' / 'dml' / 'include'}",
                '-M', '-MP', '-MF', self.cfilename + '.d',
                '-MT', self.cfilename + '.c'])

        env = os.environ.copy()
        env.update(self.extraenv)
        result = subprocess.call(args,
                                 stdout = open(self.cc_stdout, "w"),
                                 stderr = open(self.cc_stderr, "w"),
                                 env = env)
        if result != 0:
            self.pr("CC: " + " ".join(args))
        return result

    def run_linker(self):
        # suppress link step
        return 0

    def load_dependencies(self, path):
        with open(path) as f:
            return parse_depfile(f)

class DmlDep(DmlDepBase):
    '''Stability test, '''
    __slots__ = ()
    def test(self):
        super(DmlDep, self).test()

        # test .dmldep
        target_prereqs = self.load_dependencies(self.cfilename + '.dmldep')
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
        target_prereqs = self.load_dependencies(self.cfilename + '.d')
        prereqs = target_prereqs[self.cfilename + '.c']
        expected_subset = [
            self.cfilename + '-cdep.c',
            os.path.join(simics_base_path(), 'src', 'include',
                         'simics', 'base-types.h'),
            join(testdir, '1.2', 'misc', 'rel', 'a', 'x.h'),
            join(testdir, '1.2', 'misc', 'rel', 'a', 'y.h')]
        for exp in expected_subset:
            if all(Path(p).resolve() != Path(exp).resolve()
                   for p in prereqs):
                raise TestFail('missing prerequisite: ' + exp)

        empty_targets = set(target_prereqs) - {self.cfilename + '.c'}
        self.expect_equal_sets(empty_targets,
                               prereqs - {self.cfilename + '-cdep.c'})
        for target in empty_targets:
            self.expect_equal_sets(target_prereqs[target], set())

all_tests.append(DmlDep(['dmldep'],
                        join(testdir, '1.2', 'misc', 'T_import_rel_1.dml'),
                        dmlc_extraargs = ['--dep', 'T_dmldep.dmldep']))

class DmlDepArgs(DmlDepBase):
    '''Test optional arguments for dependency generation.'''
    __slots__ = ()
    def test(self):
        super().test()
        dependencies = self.load_dependencies(self.cfilename + '.dmldep')
        assert list(dependencies.keys()) == ['x.dml', 'y.dml'], "Expected custom targets only"

all_tests.append(
    DmlDepArgs(['dmldepargs'],
               join(testdir, '1.2', 'misc', 'T_import_rel_1.dml'),
               dmlc_extraargs=[
                   '--dep=T_dmldepargs.dmldep', '--no-dep-phony',
                   '--dep-target=x.dml', '--dep-target=y.dml'
               ]))

class PortingConvert(CTestCase):
    __slots__ = ('PORTING',)
    port_dml_status = 0

    def __init__(self, *args, **kwargs):
        super(PortingConvert, self).__init__(*args, **kwargs)
        self.dmlc_extraargs = ["-P", self.tagfile()]

    def run_port_script(self, args, stdout, stderr):
        env = os.environ.copy()
        env.update(self.extraenv)
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

    def run_dmlc(self, filename, dmlc_extraargs):
        # A reasonable setting of DMLC_PATHSUBST would disrupt this
        # test by adjusting porting tag generation on utility.dml.
        # We work around this by temporarily unsetting the variable.
        var = 'DMLC_PATHSUBST'
        prev = os.environ.get(var)
        if prev:
            os.environ[var] = ''
        try:
            return super(PortingConvert, self).run_dmlc(
                filename, dmlc_extraargs)
        finally:
            if prev:
                os.environ[var] = prev

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
        'PRETVAL_END',
        'PRETURNARGS',
        'POUTARGRETURN',
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
        'PUNDEFOFFS',
        'PINT1',
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
       ('porting-common.dml', None, 'PRETVAL_END',),
       ('porting-common.dml', None, 'POUTARGRETURN',),
       ('porting-common.dml', None, 'PCHANGE_INARGS',),
       ('porting-common.dml', None, 'PABSTRACT_TEMPLATE',),
       ('T_porting.dml', None, 'PSHA1',),
       ('T_porting.dml', None, 'PVERSION'),
       ('dml-builtins.dml', None, 'PWUNUSED'),
       ('dml-builtins.dml', None, 'PNO_WUNUSED'),
       ('utility.dml', None, 'PWUNUSED'),
       ('utility.dml', None, 'PVAL'),
       ('utility.dml', None, 'PNO_WUNUSED'),
       ('utility.dml', None, 'POVERRIDE'),
       ('utility.dml', None, 'PRENAME_TEMPLATE'),
       ('utility.dml', None, 'PABSTRACT_TEMPLATE'),
       ('utility.dml', None, 'PHASH'),
       ('utility.dml', None, 'PIFAND'),
       ('utility.dml', None, 'PANDOR'),
       ('utility.dml', None, 'PCHANGE_INARGS'),
       ('utility.dml', None, 'PUNDEFOFFS'),
    ]))

all_tests.append(PortingConvertFail(
    ["porting_fail"],
    join(testdir, "1.2", "misc", "T_porting_fail.dml"),
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


class DeadMethods(CTestCase):
    __slots__ = ()
    def test(self):
        super().test()
        c_file = Path(f'{self.cfilename}.c').resolve()
        dml_sources = dead_dml_methods.dml_sources(c_file)
        base_file = Path(self.filename).resolve()
        imported_file = (
            Path(self.filename).parent / 'dead_methods_imported.dml').resolve()
        unimported_file = base_file.parent / 'dead_methods_unimported.dml'
        dml12_file = base_file.parent / 'dead_methods_unimported_dml12.dml'
        assert {base_file, imported_file}.issubset(dml_sources), str(
            dml_sources)
        (dead, skipped) = dead_dml_methods.find_dead_methods(
            {c_file}, {base_file, imported_file, unimported_file, dml12_file})
        assert [p.name for p in skipped] == ['dml-builtins.dml'], f'{skipped}'
        assert set(dead) == {base_file, imported_file, unimported_file,
                             dml12_file}, dead
        for (path, lines) in dead.items():
            blob = path.read_text()
            expected = [blob[:match.start()].count('\n') + 2
                             for match in re.finditer('// DEAD', blob)]
            assert expected == [line for (line, name) in lines], str(
                (expected, lines))

all_tests.append(DeadMethods(["dead_methods"],
                             join(testdir, "1.4", "misc", "dead_methods.dml")))


func_start_re = re.compile(r'^static +\S+ +(\S+)\(.*\)$')
begin_linemarks_func_re = re.compile(r'^// BEGIN-LINEMARKS-FUNC((?:\s+\S+)+)$')

class Linemarks(CTestCase):
    __slots__ = ()

    def dml_lines_iter(self, c_file, c_lines_iter):
        reset = True
        dml_lineno = None
        curr_func = None
        for (c_lineno, line) in enumerate(c_lines_iter, 1):
            if dml_lineno is not None:
                dml_lineno += 1
            line = line.rstrip()
            m = line_directive_re.match(line)
            if m is not None:
                (lineno, filename) = m.groups()
                lineno = int(lineno)
                redir_file = Path(self.scratchdir) / filename
                if redir_file.samefile(c_file):
                    assert not reset, \
                           f'Redundant line directive reset at {c_lineno}'
                    assert lineno == c_lineno + 1, \
                           f'Incorrect line directive reset at {c_lineno}'
                    reset = True
                    dml_lineno = None
                elif redir_file.samefile(self.filename):
                    assert dml_lineno != lineno, \
                           (f'Redundant linemark for {dml_lineno} at '
                            + f'{c_lineno}')
                    reset = False
                    dml_lineno = lineno - 1
                else:
                    reset = False
                    dml_lineno = None
                continue
            m = func_start_re.match(line)
            if m is not None:
                assert curr_func is None
                curr_func = m.group(1)
                assert reset, \
                       f'Line directive not reset by start of {curr_func}'
                continue
            if dml_lineno is not None:
                yield (dml_lineno, c_lineno, curr_func)
            if curr_func is not None and line == '}':
                curr_func = None

        assert reset, "Line directive not reset by end of C file"
        assert curr_func is None, "Unterminated C function"

    def parse_annotations(self):
        base_file = Path(self.filename).resolve()

        func_spans : dict[str, set[(int, int)]] = {}
        cblock_spans : dict[int, int] = {}
        not_linemarked : set(int) = set()
        with open(base_file) as base_file_handle:
            curr_span = None
            for (lineno, line) in enumerate(base_file_handle, 1):
                line = line.strip()
                m = begin_linemarks_func_re.match(line)
                if m:
                    assert curr_span is None, \
                           f'Unterminated span: {curr_span[1:]}'
                    curr_span = (func_spans, m.group(1).split(), lineno + 1)
                elif line == '// BEGIN-LINEMARKS-CBLOCK':
                    curr_span = (cblock_spans, lineno + 1)
                elif line == '// END-LINEMARKS':
                    assert curr_span is not None, f'No span to end at {lineno}'
                    if curr_span[0] is func_spans:
                        for func in curr_span[1]:
                            func_spans.setdefault(func, set()).add(
                                (curr_span[2], lineno - 1))
                    else:
                        cblock_spans[curr_span[1]] = lineno - 1
                    curr_span = None
                elif line == '// NOT-LINEMARKED':
                    assert (curr_span is not None
                            and curr_span[0] is func_spans), \
                           (f"{lineno}: // NOT-LINEMARKED used outside of "
                            + "LINEMARKS-FUNC block")
                    not_linemarked.add(lineno + 1)
                if (curr_span is not None and curr_span[0] is func_spans
                    and (not line or line.startswith('//'))):
                    not_linemarked.add(lineno)
            assert curr_span is None, f'Unterminated span: {curr_span[1:]}'

        return (func_spans, cblock_spans, not_linemarked)

    def analyze_c_files(self, files, func_spans, cblock_spans, not_linemarked):
        expected_func_lines = {
            name: { line: False
                   for (start, end) in ranges
                   for line in range(start, end + 1)
                   if line not in not_linemarked }
            for (name, ranges) in func_spans.items()}
        found_cblocks = set()
        for curr_file in files:
            with open(curr_file) as file_handle:
                curr_cblock_expected = []
                for (dml_lineno, c_lineno, curr_func) in self.dml_lines_iter(
                        curr_file, file_handle.readlines()):
                    if curr_cblock_expected:
                        (expected_dml_lineno,
                         expected_c_lineno) = curr_cblock_expected.pop()
                        assert ((dml_lineno, c_lineno)
                                == (expected_dml_lineno, expected_c_lineno)), \
                               (f'In {curr_file}: Expected DML line '
                                + f'{expected_dml_lineno} at '
                                + f'{expected_c_lineno}, not DML line '
                                + f'{dml_lineno} at {c_lineno}')
                        continue
                    if dml_lineno in cblock_spans:
                        found_cblocks.add(dml_lineno)
                        curr_cblock_expected[:] = (
                            (line, c_lineno + (line - dml_lineno))
                            for line in range(dml_lineno + 1,
                                              cblock_spans[dml_lineno] + 1)
                            if not line in not_linemarked)
                        # Need to reverse as .pop() pops from the end
                        curr_cblock_expected.reverse()
                        continue
                    assert (curr_func in expected_func_lines
                            and dml_lineno in expected_func_lines[curr_func]),\
                           (f'In {curr_file}: Unexpected DML line '
                            + f'{dml_lineno} at {c_lineno} '
                            + (f'(inside {curr_func})' if curr_func else
                               '(top-level)'))
                    expected_func_lines[curr_func][dml_lineno] = True

                assert not curr_cblock_expected, \
                       ('Unterminated C block: expected: '
                        + f'({curr_cblock_expected})')

        remaining_cblocks = [c_block for c_block in cblock_spans
                             if not c_block in found_cblocks]

        assert not remaining_cblocks, \
            f'Linemarked C blocks missing: {remaining_cblocks}'
        for (func_name, lines) in expected_func_lines.items():
            for (line, was_linemarked) in lines.items():
                assert was_linemarked, \
                    f'DML line {line} not found inside {func_name}'

    def test(self):
        super().test()
        c_file = Path(f'{self.cfilename}.c').resolve()
        h_file = Path(f'{self.cfilename}.h').resolve()
        self.analyze_c_files((c_file, h_file),
                             *self.parse_annotations())


all_tests.append(Linemarks(["linemarks"],
                           join(testdir, "1.4", "misc", "linemarks.dml")))
# DMLC_GATHER_SIZE_STATISTICS=1 will cause StrOutput to be used when generating
# functions.
# This tests that linemark generation still functions properly in that
# circumstance.
all_tests.append(Linemarks(["linemarks-size-stats"],
                           join(testdir, "1.4", "misc", "linemarks.dml"),
                           extraenv={'DMLC_GATHER_SIZE_STATISTICS': '1'}))

class CompareIllegalAttrs(BaseTestCase):
    __slots__ = ()
    prereqs = ['minimal']
    def test(self):
        # Extract list of automatic attributes from Simics
        sl = {x[5:].strip()
              for x in subprocess.run(
                      [join(simics_base_path(), "bin", "simics" + bat_sfx),
                       "--batch-mode", "--quiet", "--no-copyright",
                       "--no-settings", "--dump-core", "--werror",
                       "--module-path", join(testparams.sandbox_path(),
                                             "scratch", "minimal"),
                       "-e", "@SIM_create_object('test', 'o', [])",
                       "-e", "@for n in conf.o.attributes: print('ATTR', end=' '), print(n[0])"],
                      capture_output=True,
                      encoding='utf-8').stdout.splitlines()
              if x.startswith('ATTR ')}

        # Extract list of illegal attributes from dmlc
        dl = dml.globals.illegal_attributes

        if dl != sl:
            for n in dl - sl:
                self.pr("- " + n)
            for n in sl - dl:
                self.pr("+ " + n)
            raise TestFail('difference')

all_tests.append(CompareIllegalAttrs('compare-illegal-attrs'))

all_tests.append(CTestCase(["T_EIDXVAR_info"],
                           join(testdir, "1.2", "errors", "T_EIDXVAR.dml"),
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
      <field name="f0" desc="f0 desc" msb="9" lsb="5" />
      <field name="f1" msb="4" lsb="1" />
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
  <bank name="b" vsize="4 2" function="3 4 5 6 7 8 9 10">
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
    class Error(Exception): pass
    def validate_file(self, path, regexp):
        match = regexp.match(path.read_bytes())
        if not match:
            raise self.Error('did not find copyright banner')
        year = int(match.group(1))
        if not 2000 < year <= this_year:
            raise self.Error(f'strange copyright year: {year}')
    def test(self):
        mpl_lines = '''\
© (2[0-9]+) Intel Corporation
SPDX-License-Identifier: MPL-2.0
'''.encode('utf-8').splitlines()
        bsd0_copyright_re = re.compile('''/[*]
  © (2[0-9]+) Intel Corporation
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
            # data files
            'test/XFAIL',
            'spelling-passlist',
            # config files
            '.gitignore',
            '_config.yml',
        }
        for f in ignorelist:
            assert (root / f).is_file(), f
        nonexisting = ignorelist.difference(files)
        assert not nonexisting, nonexisting
        for f in files:
            try:
                if f in ignorelist:
                    continue
                if f.startswith(('lib', 'include')):
                    assert f.endswith(('.dml', '.h'))
                    self.validate_file(root / f, bsd0_copyright_re)
                elif f.endswith(('.dml', '.h')):
                    self.validate_file(root / f, dml_copyright_re)
                elif f.endswith('.py') or f == 'Makefile':
                    self.validate_file(root / f, py_copyright_re)
                elif f.endswith(('.md', '.docu')):
                    self.validate_file(root / f, xml_copyright_re)
                else:
                    raise self.Error('unknown file type')
            except self.Error as exc:
                errors.append((f, str(exc)))
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
        dmlc_extraargs=['--info'])
    all_tests.append(generate)
    all_tests.append(DevInfoCompare(
        '%s-devinfo-compare' % (version,),
        testname, generate.cfilename + '.xml', version))

all_tests.append(
    XmlTestCase(
        ['1.2-register-view'],
        join(testdir, '1.2', 'misc', 'register_view.dml'),
        dmlc_extraargs=['--info']))
all_tests.append(
    XmlTestCase(
        ['1.2-register-view-descriptions'],
        join(testdir, '1.2', 'misc', 'register_view_descriptions.dml'),
        dmlc_extraargs=['--info']))
all_tests.append(
    XmlTestCase(
        ['1.2-register-view-bitorder-le'],
        join(testdir, '1.2', 'misc', 'register_view_bitorder_le.dml'),
        dmlc_extraargs=['--info']))
all_tests.append(
    XmlTestCase(
        ['1.2-register-view-bitorder-be'],
        join(testdir, '1.2', 'misc', 'register_view_bitorder_be.dml'),
        dmlc_extraargs=['--info']))
all_tests.append(
    XmlTestCase(
        ['1.2-register-view-inquiry'],
        join(testdir, '1.2', 'misc', 'register_view_inquiry.dml'),
        dmlc_extraargs=['--info']))
all_tests.append(
    XmlTestCase(
        ['1.2-register-view-fields'],
        join(testdir, '1.2', 'misc', 'register_view_fields.dml'),
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
                 extra_code = ""):
        filename = join(testparams.sandbox_path(), "scratch",
                        testname + ".dml")
        CTestCase.__init__(self,  [testname], filename,
                           api_version=api_version)
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
    lib_files = set(map(os.path.basename, glob.glob(basedir)))
    if int(get_simics_major()) > 6:
        lib_files -= {"mil-std-1553.dml", "rapidio.dml",
                      "rapidio-device.dml"}

    all_tests.append(ImportTest(
        'lib-dml-%s-api-%s' % (dmlver, latest_api_version),
        dmlver, latest_api_version, sorted(lib_files),
        rapidio_bank if (dmlver == '1.2'
                         and int(get_simics_major()) <= 6) else ''))

# header files that should not be tested with all API versions
limited_api_testing = {
    join("simics", "util", "data-structs.dml") : ["4.8"]}

def api_files(dml_version, api_version):
    files = []
    basedir = join(testparams.simics_base_path(), host_type(),
                   "bin", "dml", "api", api_version,
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
        all_tests.append(ImportTest(testname, dmlver, apiver, files))

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
