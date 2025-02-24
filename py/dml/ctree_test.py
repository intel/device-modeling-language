# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import unittest

import os
import sys
import subprocess
import math
import shlex
from pathlib import Path

from dml import ctree, types, logging, messages, output, symtab, traits
from dml.ctree import string_escape, mkCompound, dmldir_macro
from dml.env import is_windows

def apply(f):
    return f()

class Test_string_escape(unittest.TestCase):
    def test(self):
        def eq(a, b): self.assertEqual(a, b)
        eq(string_escape(b""), "")
        eq(string_escape(b'ab\000\001\\\012\015 \"~\177\200\201\376\377'),
                         r'ab\000\001\\\012\015 \"~\177\200\201\376\377')

class Test_dmldir_macro(unittest.TestCase):
    def test(self):
        self.assertEqual(dmldir_macro('/path/to/foo.dml'),
                         ('DMLDIR_FOO_H', '/path/to/foo.h'))
        if is_windows():
            self.assertEqual(dmldir_macro('c:\\path\\to\\foo.dml'),
                         ('DMLDIR_FOO_H', 'c:\\path\\to\\foo.h'))
        # non-DML suffix
        self.assertEqual(dmldir_macro('/path/to/foo.dml.old'),
                         ('DMLDIR_FOO_DML_OLD_H', '/path/to/foo.dml.old.h'))
        # non-alphabetic characters
        self.assertEqual(dmldir_macro('/path/to/1+x-\xe4y.dml'),
                         ('DMLDIR_1_X__Y_H', '/path/to/1+x-\xe4y.h'))

class DummyStatement(ctree.Statement):
    def __repr__(self):
        return 'D'
    def toc_stmt(self): pass

class DummyDecl(ctree.Statement):
    is_declaration = 1
    def __repr__(self):
        return 'decl'
    def toc_stmt(self): pass

class Test_mkcompound(unittest.TestCase):
    def test(self):
        def expect_repr(a, b): self.assertEqual(repr(a), b)
        s = None # dummy site
        n = ctree.mkNull(s)
        d = DummyStatement(s)
        decl = DummyDecl(s)
        expect_repr(mkCompound(s, []), 'Compound([])')
        expect_repr(mkCompound(s, [d]), 'D')
        expect_repr(mkCompound(s, [d, d]), 'Compound([D, D])')
        # Shallowly nested compounds are collapsed
        expect_repr(mkCompound(s, [mkCompound(s, [d, d]), d]),
                    'Compound([D, D, D])')
        # Deeply nested compounds are collapsed
        expect_repr(
            mkCompound(s, [mkCompound(s, [d, mkCompound(s, [d, d])]), d]),
            'Compound([D, D, D, D])')
        # Null statements are collapsed
        expect_repr(mkCompound(s, [n, d, n]), 'D')
        # Declaration statements inhibit collapse
        expect_repr(mkCompound(s, [mkCompound(s, [decl, n]), n]),
                    'Compound([decl])')
        expect_repr(mkCompound(s, [mkCompound(s, [decl]),
                                   mkCompound(s, [decl])]),
                    'Compound([Compound([decl]), Compound([decl])])')

class test_control_flow(unittest.TestCase):
    def expect_flow(self, expr, **flow_props):
        flow = expr.control_flow()
        self.assertEqual(repr(flow), repr(ctree.ControlFlow(**flow_props)))

    def test(self):
        @apply
        class s(logging.SimpleSite):
            # Satisfy some code that wants a site with version info
            def __init__(self):
                logging.SimpleSite.__init__(self, 'nowhere')
            def dml_version(self): return (1, 4)
        cond = ctree.mkLit(s, '', types.TBool())
        intval = ctree.mkLit(s, '', types.TInt(8, True))
        true_const = ctree.mkBoolConstant(s, True)
        false_const = ctree.mkBoolConstant(s, False)
        break_ = ctree.mkBreak(s)
        case = ctree.mkCase(s, intval)
        default = ctree.mkDefault(s)
        throw = ctree.mkThrow(s, 'lbl')
        ret = ctree.mkReturn(s, intval)
        null = ctree.mkNull(s)
        self.expect_flow(ctree.mkIf(s, cond, break_, throw),
                         br=True, throw=True)
        self.expect_flow(ctree.mkIf(s, cond, ret, throw),
                         throw=True)
        self.expect_flow(ctree.mkIf(s, cond, break_),
                         fallthrough=True, br=True)
        self.expect_flow(ctree.mkAssert(s, cond), fallthrough=True)
        self.expect_flow(ctree.mkAssert(s, false_const))
        for loop in [ctree.mkWhile, ctree.mkDoWhile,
                     lambda s, expr, stmt: ctree.mkFor(s, [], expr, [], stmt)]:
            self.expect_flow(
                loop(s, cond, ctree.mkIf(s, cond, ret, break_)),
                fallthrough=True)
            self.expect_flow(loop(s, true_const, null))
            self.expect_flow(
                loop(s, true_const, ctree.mkIf(s, cond, throw)), throw=True)
            self.expect_flow(
                loop(s, cond, ctree.mkIf(s, cond, throw, null)),
                fallthrough=True, throw=True)
            self.expect_flow(loop(s, false_const, break_), fallthrough=True)
        self.expect_flow(ctree.mkWhile(s, cond, ret), fallthrough=True)
        self.expect_flow(ctree.mkFor(s, [], cond, [], ret), fallthrough=True)

        self.expect_flow(ctree.mkSwitch(s, intval, ctree.mkCompound(s, [
            case, null, case, ret, default, null])), fallthrough=True)
        self.expect_flow(ctree.mkSwitch(s, intval, ctree.mkCompound(s, [
            case, throw, case, ret, default, null])),
                         throw=True, fallthrough=True)
        self.expect_flow(ctree.mkSwitch(s, intval, ctree.mkCompound(s, [
            case, null, default, throw])),
                         throw=True, fallthrough=False)
        self.expect_flow(ctree.mkSwitch(s, intval, ctree.mkCompound(s, [
            case, ctree.mkIf(s, cond, throw), default, ret])),
                         throw=True, fallthrough=False)
        self.expect_flow(ctree.mkSwitch(s, intval, ctree.mkCompound(s, [
            case, break_, default, ret])), fallthrough=True)
        self.expect_flow(ctree.mkTryCatch(s, 'lbl', break_, ret),
                         br=True, fallthrough=False)
        self.expect_flow(ctree.mkTryCatch(
            s, 'x', ctree.mkIf(s, cond, throw, ret), ret),
                         fallthrough=False)
        self.expect_flow(ctree.mkTryCatch(s, 'x', throw, throw),
                         fallthrough=False, throw=True)
        self.expect_flow(ctree.mkCompound(
            s, [ctree.mkIf(s, cond, break_), ret]),
                         br=True)
        self.expect_flow(ctree.mkCompound(None, [ctree.mkBreak(None),
                                                 ctree.mkThrow(None, 'lbl')]),
                         br=True)

site = logging.SimpleSite("nowhere")

class GccTests(unittest.TestCase):
    '''Unit tests that run GCC on short generated code snippets'''
    @staticmethod
    def subtest_decorator(subtests):
        def subtest(arglists=[()]):
            '''A subtest function is called once for each arglist, and will
            be called as part of the unit test. '''
            def add_test_to_list(f):
                subtests.append((f, arglists))
            return add_test_to_list
        return subtest

    def check_call(self, args):
        try:
            subprocess.check_call(args)
        except:
            print('command failed:', args, file=sys.stderr)
            raise

    def run_gcc_tests(self):
        expectations = []
        for (f, arglists) in self.subtests:
            for arglist in arglists:
                try:
                    c_lines = f(self, *arglist)
                except:
                    print('%s%r failed' % (f.__name__, tuple(arglist)),
                          file=sys.stderr)
                    raise
                if c_lines is not None:
                    expectations.append((c_lines, f, arglist))
        cfile = 'ctree-test.c'
        with open(cfile, 'w') as f:
            base = os.environ['SIMICS_BASE']
            f.write('#include "ctree-test.h"\n')
            f.write('int\nmain(void)\n{\n')
            indent = ' ' * 8
            for (t, fun, args) in expectations:
                f.write(indent + '{\n')
                f.write('%spy_context = "%s:%d: in code generated by %s%r:";\n'
                        % (indent * 2, __file__.replace('\\', '\\\\'),
                           fun.__code__.co_firstlineno,
                           fun.__name__, args))
                for line in t:
                    f.write('%s%s\n' % (indent * 2, line))
                f.write(indent + '}\n')
            f.write(indent + 'return failure ? 1 : 0;\n')
            f.write('}\n')
        exe = os.path.join('.', 'ctree-test')
        if is_windows():
            exe += '.exe'
        here = Path(__file__).parent
        gcc_cmd = shlex.split(os.environ['CC'], posix=not is_windows()) + [
            '-I%s' % (os.path.join(base, 'src', 'include')),
            f'-I{here}',
            f'-I{here.parent.parent / "include"}',
            '-O', '-std=gnu11', '-Wall', '-Werror',
            '-Wno-int-in-bool-context',  '-o', exe, cfile]
        if is_windows():
            gcc_cmd += [
                "-DUSE_MODULE_HOST_CONFIG", "-D__USE_MINGW_ANSI_STDIO=1"]
        self.check_call(gcc_cmd)
        self.check_call([exe])

def int_const(val, signed=True):
    if not signed and val < 0:
        val += 1 << 64
    elif signed and val >= 1 << 63:
        val -= 1 << 64
    return ctree.mkIntegerConstant(site, val, signed)

def float_const(val):
    return ctree.mkFloatConstant(site, float(val))

def variable(name, t):
    return ctree.mkDereference(site, ctree.mkLit(site, '&' + name,
                                                 types.TPtr(t)))
def lit(t):
    return ctree.mkLit(site, '', t)

logging.DMLWarning.enable_werror()
logging.max_errors = 1

class ExprTests(GccTests):
    subtests = []
    subtest = GccTests.subtest_decorator(subtests)

    def expect_int_type(self, type, signed, bits=64):
        self.assertEqual((type.bits, type.signed), (bits, signed))

    def expect_double(self, type):
        self.assertIsInstance(type, types.TFloat)
        self.assertEqual(type.name, 'double')

    def expect_assert_error(self, line):
        return ['if (capturing_assert_errors) {',
                # must make ASSERT fatal again before we can use it to crash
                '        capturing_assert_errors = false;',
                '        ASSERT(false);',
                '}',
                'ASSERT(captured_assert_errors == 0);',
                'capturing_assert_errors = true;',
                line,
                'capturing_assert_errors = false;',
                'EXPECT(captured_assert_errors == 1);'
                'captured_assert_errors = 0;']

    @subtest()
    def framework_assertions(self):
        '''validate helper macros used later in test'''
        return (['int32 i32 = 0;',
                 'ASSERT(!IS_UINT32(i32));',
                 'ASSERT(!IS_INT64(i32));',
                 'ASSERT(!IS_UINT64(i32));',
                 'uint32 u32 = 0;',
                 'ASSERT(IS_UINT32(u32));',
                 'ASSERT(!IS_INT64(u32));',
                 'ASSERT(!IS_UINT64(u32));',
                 'int64 i = 0;',
                 'ASSERT(!IS_UINT32(i));',
                 'ASSERT(IS_INT64(i));',
                 'ASSERT(!IS_UINT64(i));',
                 'ASSERT(IS_INT64(0x7fffffffffffffff));',
                 'ASSERT(!IS_INT64(0x8000000000000000));',
                 'uint64 u = 0;',
                 'ASSERT(!IS_UINT32(u));',
                 'ASSERT(!IS_INT64(u));',
                 'ASSERT(IS_UINT64(u));',
                 'ASSERT(!IS_UINT64(0x7fffffffffffffff));',
                 'ASSERT(IS_UINT64(0x8000000000000000));']
                + self.expect_assert_error('ASSERT(false);'))

    @subtest()
    def long_types(self):
        def tcmp(t1, t2):
            left = t1.eq(t2)
            right = t2.eq(t1)
            self.assertEqual(left, right)
            return left
        u = types.TLong(False)
        s = types.TLong(True)
        z = types.TSize(False)
        sz = types.TSize(True)
        u64t = types.TInt64_t(False)
        s64t = types.TInt64_t(True)
        int_types = [u, s, z, sz, u64t, s64t,
                 types.TInt(64, False), types.TInt(32, False)]
        # all alternative spellings of integer types are potentially
        # incompatible in C, and therefore defensively considered by DML as
        # incompatible
        for t1 in int_types:
            for t2 in int_types:
                self.assertEqual(tcmp(t1, t2), t1 is t2, (t1, t2))

        return [u.declaration('u') + ';',
                'unsigned long *up UNUSED = &u;',
                s.declaration('s') + ';',
                'long *sp UNUSED = &s;',
                z.declaration('z') + ';',
                'size_t *zp UNUSED = &z;',
                sz.declaration('sz')  + ';',
                'ssize_t *szp UNUSED = &sz;',
                u64t.declaration('u64t') + ';',
                'uint64_t *u64tp UNUSED = &u64t;',
                s64t.declaration('s64t')  + ';',
                'int64_t *s64tp UNUSED = &s64t;']

    @subtest([(0, False),
              (0, True),
              (-1, True),
              ((1 << 64) - 1, False),
              (0xdeadbeef, False),
              (0xdeadbeef, True),
              (1 << 63, False),
              ((1 << 63) - 1, False),
              ((1 << 63) - 1, True),
              (-(1 << 63), True)])
    def int_constant(self, dml_value, signed):
        expr = ctree.mkIntegerConstant(site, dml_value, signed)
        self.expect_int_type(expr.ctype(), signed)
        return ['EXPECT(IS_%sINT64(%s));'
                % ('' if signed else 'U', expr.read())]

    @subtest()
    def float_corners(self):
        '''check that Python floats and C doubles work the same way'''
        self.assertEqual(sys.float_info.mant_dig, 53)
        self.assertEqual(sys.float_info.max_exp, 1024)
        epsilon = sys.float_info.epsilon
        delta = epsilon * epsilon
        self.assertEqual(2.0 + epsilon, 2.0)
        self.assertNotEqual(2.0 + (epsilon + delta), 2.0)
        return ['EXPECT(DBL_MANT_DIG == 53);',
                'EXPECT(DBL_MAX_EXP == 1024);',
                'EXPECT(2.0 + %s == 2.0);' % (epsilon.hex(),),
                'EXPECT(2.0 + (%s + %s) != 2.0);'
                % (epsilon.hex(), delta.hex())]

    @subtest([(0.0,),
              (math.pi,),
              (float.fromhex('0x1.p-1024'),),
              (float.fromhex('0x1.p1023'),),
              (float.fromhex('inf'), 'INFINITY'),
              (float.fromhex('-inf'), '-INFINITY')])
    def float_constant(self, dml_value, c_value=None):
        if c_value is None:
            c_value = dml_value.hex()
        expr = ctree.mkFloatConstant(site, dml_value)
        self.expect_double(expr.ctype())
        return ['EXPECT(IS_DOUBLE(%s));' % (expr.read(),),
                'EXPECT(%s == %s);' % (expr.read(), c_value)]

    @subtest()
    def float_nan(self):
        expr = ctree.mkFloatConstant(site, float('NaN'))
        self.expect_double(expr.ctype())
        return ['EXPECT(IS_DOUBLE(%s));' % (expr.read(),),
                'EXPECT(isnan(%s));' % (expr.read(),)]

    @subtest([(0xdeadbeef, types.TInt(16, False), 0xbeef, 2),
              (0xaaaaaa,   types.TInt(15, True),  0x2aaa, 2),
              (0xaaaaaa,   types.TInt(14, True), -0x2000 | 0xaaa, 2),
    ])
    def int_cast(self, value, new_type, trunc_val, new_size):
        const_expr = ctree.mkCast(
            site, ctree.mkIntegerConstant(site, value, True),
            new_type)
        var_expr = ctree.mkCast(
            site, variable('x', types.TInt(64, True)), new_type)
        self.assertEqual(const_expr.value, trunc_val)
        self.expect_int_type(const_expr.ctype(), new_type.signed, new_type.bits)
        self.expect_int_type(var_expr.ctype(), new_type.signed, new_type.bits)
        return ['uint64 x = 0x%xLL;' % (value,),
                'EXPECT(sizeof(%s) == %d);' % (const_expr.read(), new_size),
                'EXPECT(sizeof(%s) == %d);' % (var_expr.read(), new_size),
                'EXPECT(%s == %d);' % (var_expr.read(), trunc_val)]

    @subtest()
    def int_cast_sequence(self):
        """Checks that sequences of casts are collapsed correctly"""
        for cast_sequence in [(types.TInt(64, False), types.TInt(64, False)),
                             (types.TInt(32, False), types.TInt(32, False)),
                             (types.TInt(64, False), types.TInt(64, True)),
                             (types.TInt(64, True), types.TInt(64, False)),
                             (types.TInt(64, False), types.TInt(32, False))]:
            base = variable('b', types.TInt(1, True))
            curr_expr = base
            for t in cast_sequence:
                curr_expr = ctree.mkCast(site, curr_expr, t)
            self.assertTrue(cast_sequence[-1].eq(curr_expr.ctype()))
            self.assertEqual(curr_expr.expr, base)

    @subtest()
    def int_cast_sequence_bit_increase(self):
        base = variable('b', types.TInt(1, True))
        cast1 = ctree.mkCast(site, base, types.TInt(32, True))
        cast2 = ctree.mkCast(site, cast1, types.TInt(64, False))
        self.assertEqual(cast2.expr, cast1)

    @subtest([(types.TFloat('double'), '1.2', types.TInt(32, False), '1'),
              (types.TBool(), 'true', types.TInt(32, False), '1'),
              (types.TFunction([], types.TVoid()),
               'printf', types.TInt(64, False),
               '(uintptr_t)printf')])
    def cast_to_int(self, old_type, old_value, new_type, new_value):
        cast = ctree.mkCast(site, ctree.mkLit(site, old_value, old_type),
                            new_type)
        self.assertTrue(cast.ctype().eq(new_type))
        return ['EXPECT(%s == %s);' % (cast.read(), new_value)]

    def unendianed_type(self, endian_type):
        return types.TInt(endian_type.bits, endian_type.signed)

    @subtest([(ctree.mkIntegerConstant(site, 5, True), 5),
              (ctree.mkIntegerConstant(site, -5, True), -5),
              (ctree.mkBitSlice(
                  site, ctree.mkIntegerConstant(site, 0x80BD, False),
                  ctree.mkIntegerConstant(site, 5, False),
                  ctree.mkIntegerConstant(site, 2, False), 'le'), 0xF),
              (ctree.mkCast(site, ctree.mkIntegerConstant(site, 5, True),
                            types.TEndianInt(32, True, 'big-endian')), 5),
              (ctree.mkCast(site, ctree.mkIntegerConstant(site, 5, True),
                            types.TEndianInt(8, False, 'little-endian')), 5),
    ])
    def as_int(self, expr, value):
        """Test that as_int correctly converts various types to integers"""
        etype = expr.ctype()
        expect_int64 = (etype.bits, etype.signed) != (64, False)
        expr = ctree.as_int(expr)
        self.expect_int_type(expr.ctype(), expect_int64)
        return ['EXPECT(IS_%sINT64(%s));' % ("" if expect_int64 else "U",
                                             expr.read()),
                'EXPECT(%s == %d);' % (expr.read(), value)]

    @subtest([(expr, endiantype)
              for expr in (
                      ctree.mkFloatConstant(site, -3.14),
                      ctree.mkIntegerConstant(site, -5, True),
                      ctree.mkBitSlice(
                          site, ctree.mkIntegerConstant(site, 0x80BD, False),
                          ctree.mkIntegerConstant(site, 5, False),
                          ctree.mkIntegerConstant(site, 2, False), 'le'))
              for endiantype in (
                      types.TEndianInt(8, False, 'little-endian'),
                      types.TEndianInt(8, True, 'little-endian'),
                      types.TEndianInt(64, False, 'little-endian'),
                      types.TEndianInt(64, True, 'big-endian'))
    ])
    def endian_int_cast(self, expr, endiantype):
        """Check that casting to endianint results in the right type"""
        self.assertTrue(
            ctree.mkCast(site, expr, endiantype).ctype().eq(endiantype))

    @subtest([(targettype, endiantype)
              for targettype in (
                      types.TInt(8, False),
                      types.TInt(8, False,
                                 {"a": (types.TInt(4, False), 5, 2)},
                                 {"b": (types.TInt(2, False), 7, 5)}),
                      types.TPtr(types.TVoid()),
              )
              for endiantype in (
                      types.TEndianInt(8, False, 'little-endian'),
                      types.TEndianInt(8, True, 'little-endian'),
                      types.TEndianInt(64, False, 'little-endian'),
                      types.TEndianInt(64, True, 'big-endian'))
    ])
    def endian_int_cast_from(self, targettype, endiantype):
        """Check that casting from an endianint results in the right type"""
        expr = ctree.mkCast(site,
                            ctree.mkIntegerConstant(site, -5, True),
                            endiantype)
        cast = ctree.mkCast(site, expr, targettype)
        self.assertTrue(cast.ctype().eq(targettype))
        return ['%s UNUSED = %s;' % (targettype.declaration('x'), cast.read())]

    @subtest([(op, lh, rh)
              for op in (ctree.mkMult,
                         ctree.mkDiv,
                         ctree.mkMod,
                         ctree.mkAdd,
                         ctree.mkSubtract,
              )
              for iv in (ctree.mkCast(
                      site, ctree.mkIntegerConstant(site, 5, True),
                      types.TEndianInt(32, True, 'big-endian')),
                         ctree.mkBitSlice(
                             site, ctree.mkIntegerConstant(site, 0x80FF, False),
                             ctree.mkIntegerConstant(site, 20, False),
                             ctree.mkIntegerConstant(site, 12, False), 'le'))
              for (lh, rh) in ((ctree.mkIntegerConstant(site, 5, True), iv),
                               (iv, ctree.mkIntegerConstant(site, 5, True)))])
    def as_int_binop_coverage(self, op, lh, rh):
        """Test that arithmetic binops convert their arguments"""
        expr = op(site, lh, rh)
        self.expect_int_type(expr.ctype(), True)
        return ["EXPECT(IS_INT64(%s));" % (expr.read())]

    @subtest([(ctree.mkBitSlice(
                         site, ctree.mkIntegerConstant(site, 0xF, False),
                         ctree.mkIntegerConstant(site, 2, False),
                         ctree.mkIntegerConstant(site, 1, False), 'le'),
               ctree.mkCast(
                   site, ctree.mkIntegerConstant(site, 3, True),
                   types.TEndianInt(32, True, 'big-endian')),
               types.TInt(64, True)),
              (ctree.mkIntegerConstant(site, 5, True),
               ctree.mkCast(
                   site, ctree.mkIntegerConstant(site, 3, True),
                   types.TEndianInt(32, True, 'big-endian')),
               types.TInt(64, True)),
              (ctree.mkCast(
                   site, ctree.mkIntegerConstant(site, 3, True),
                   types.TEndianInt(32, True, 'big-endian')),
               ctree.mkIntegerConstant(site, 5, True),
               types.TInt(64, True)),
              (ctree.mkCast(
                   site, ctree.mkIntegerConstant(site, 3, True),
                   types.TEndianInt(32, True, 'big-endian')),
               ctree.mkCast(
                   site, ctree.mkIntegerConstant(site, 5, True),
                   types.TEndianInt(32, True, 'big-endian')),
               types.TInt(64, True))])
    def as_int_ifexpr(self, lh, rh, expected_type):
        expr = ctree.mkIfExpr(site, variable('b', types.TBool()), lh, rh)
        self.assertTrue(expr.type.eq(expected_type))
        return ["bool b = false;",
                "EXPECT(%s == %s);" % (
                    expr.read(), ctree.mkCast(site, rh, expected_type).read())]

    @subtest([(op, lh)
              for op in (ctree.mkBitNot,
                         ctree.mkUnaryMinus,
                         ctree.mkUnaryPlus,
                         lambda site, x :
                         ctree.mkCast(site, x, types.TInt(64, True)))
              for lh in (ctree.mkIntegerConstant(site, 5, True),
                         ctree.mkCast(
                             site, ctree.mkIntegerConstant(site, 5, True),
                             types.TEndianInt(32, True, 'big-endian')),
                         ctree.mkBitSlice(
                             site, ctree.mkIntegerConstant(site, 0x80FF, False),
                             ctree.mkIntegerConstant(site, 20, False),
                             ctree.mkIntegerConstant(site, 12, False), 'le'))
        ])
    def as_int_unop_coverage(self, op, lh):
        """Test that arithmetic unops convert their argument"""
        expr = op(site, lh)
        self.expect_int_type(expr.ctype(), True)
        return ["EXPECT(IS_INT64(%s));" % (expr.read())]

    @subtest([(ctree.mkIntegerConstant(site, 3, True),),
              (ctree.mkCast(
                  site, ctree.mkIntegerConstant(site, 3, True),
                  types.TEndianInt(32, True, 'big-endian')),),
              (ctree.mkBitSlice(
                  site, ctree.mkIntegerConstant(site, 0xF, False),
                  ctree.mkIntegerConstant(site, 2, False),
                  ctree.mkIntegerConstant(site, 1, False), 'le'),)])
    def as_int_new_coverage(self, count):
        """Checks for as_int coverage"""
        expr = ctree.mkNew(site, types.TInt(8, False), count)
        self.assertTrue(expr.type.eq(types.TPtr(types.TInt(8, False))))
        return ["EXPECT(%s == 3);" % expr.count.read()]

    def expect_int_unop(self, const, unop, expected):
        const_op = unop(site, const)
        self.expect_int_type(const_op.ctype(), const.type.signed)
        var_op = unop(site, variable('x', const.type))
        self.expect_int_type(var_op.ctype(), const.type.signed)
        if ((const.type.signed and -128 <= const.value < 128)
            or (not const.type.signed and 0 <= const.value < 256)):
            # value fits in 8 bits, so we can test with 8-bit
            # non-constant operands, to cover sign extension
            var8_op = unop(site, variable(
                'y', ctree.TInt(8, const.type.signed)))
            test_8bit = ['%sint8 y = %s;' % ('' if const.type.signed else 'u',
                                             const.read()),
                         'EXPECT(%s == %s);' % (var8_op.read(), expected,)]
        else:
            test_8bit = []
        return ['EXPECT(%s == %s);' % (const_op.read(), expected,),
                'EXPECT(IS_%sINT64(%s));'
                % ('' if const.type.signed else 'U', const_op.read()),
                '%s = %s;' % (const.type.declaration('x'), const.read()),
                'EXPECT(%s == %s);' % (var_op.read(), expected,)] + test_8bit

    @subtest([(op, bits, signed)
              for op in [ctree.mkUnaryMinus,
                         ctree.mkBitNot,
                         ctree.mkPreInc,
                         ctree.mkPreDec,
                         ctree.mkPostInc,
                         ctree.mkPostDec]
              for (bits, signed) in [(8, True),
                                     (32, False),
                                     (63, False),
                                     (64, True),
                                     (64, False)]])
    def int_unop_types_lvalue(self, op, bits, signed):
        '''test that unary operations, applied on lvalues, produce a DML value
        of the right type, which generates a C expression of the right
        type'''
        expect_int64 = (bits, signed) != (64, False)
        t = types.TInt(bits, signed)
        expr = op(site, variable('i', t))
        self.expect_int_type(expr.ctype(), expect_int64)
        return [t.declaration('i') + ' = 1;',
                'EXPECT(IS_%sINT64(%s));' % ("" if expect_int64 else "U",
                                             expr.read())]

    @subtest([(op, bits, signed)
              for op in [ctree.mkUnaryPlus,
                         ctree.mkUnaryMinus,
                         ctree.mkBitNot]
              for (bits, signed) in [(8, True),
                                     (32, False),
                                     (63, False),
                                     (64, True),
                                     (64, False)]])
    def int_unop_types_const(self, op, bits, signed):
        '''test that unary operations, applied on constants, produce a DML
        value of the right type, which generates a C expression of the
        right type'''
        expect_int64 = (bits, signed) != (64, False)
        const_expr = op(site, ctree.mkIntegerConstant(site, 1, expect_int64))
        self.expect_int_type(const_expr.ctype(), expect_int64)
        assert isinstance(const_expr, ctree.IntegerConstant)
        return ['ASSERT(IS_%sINT64(%s));' % ("" if expect_int64 else "U",
                                             const_expr.read())]

    @subtest([x + (signed,)
              for x in [(0, '0'),
                        (0xdeadbeef, '0xdeadbeef')]
              for signed in [False, True]])
    def unary_plus(self, val, expect, signed):
        return self.expect_int_unop(int_const(val, signed),
                                    ctree.mkUnaryPlus,
                                    expect)

    @subtest()
    def unary_plus_rval(self):
        i = ctree.mkUnaryPlus(site, variable('i', types.TInt(64, False)))
        with self.assertRaises(messages.ERVAL):
            ctree.mkAddressOf(site, i)
        f = ctree.mkUnaryPlus(site, variable('f', types.TFloat('double')))
        with self.assertRaises(messages.ERVAL):
            ctree.mkAddressOf(site, f)
        return None

    @subtest([x + (signed,)
              for x in [
                      (0, '0'),
                      (1, '-1'),
                      (127, '-127'),
                      (-128, '128'),
                      (128, '-128'),
                      ((1 << 63) - 1, '-LLONG_MAX'),
                      (1 << 63, '1ULL << 63'),
                      ((1 << 63) + 1, 'LLONG_MAX'),
                      (-(1 << 63), '1ULL << 63')]
              for signed in [False, True]])
    def unary_minus(self, val, expect, signed):
        return self.expect_int_unop(int_const(val, signed),
                                    ctree.mkUnaryMinus,
                                    expect)

    @subtest()
    def unray_minus_float(self):
        c = ctree.mkUnaryMinus(site, float_const(0.3))
        self.assertTrue(c.constant)
        v = ctree.mkUnaryMinus(site, variable('f', types.TFloat('float')))
        return ['EXPECT(%s == -0.3);' % (c.read(),),
                'float f = 1234.25;',
                'EXPECT(%s == -1234.25);' % (v.read(),)]

    @subtest([(ctree.mkUnaryPlus,), (ctree.mkUnaryMinus,)])
    def unary_float_promotion(self, op):
        f = op(site, variable('f', types.TFloat('float')))
        self.assertEqual(f.ctype().name, 'double')
        return ['float f = 1.0;',
                'EXPECT(IS_DOUBLE(%s));' % (f.read(),)]

    @subtest(x + (signed,)
             for x in [
                     (0, '-1'),
                     (-1, '0'),
                     (1, '-2'),
                     (127, '-128'),
                     (-128, '127'),
                     (128, '-129'),
                     ((1 << 63) - 1, '1ULL << 63'),
                     (1 << 63, 'LLONG_MAX'),
                     (0xdeadbeee, '-0xdeadbeefLL')]
             for signed in [False, True])
    def bit_not(self, val, expect, signed):
        return self.expect_int_unop(int_const(val, signed),
                                    ctree.mkBitNot,
                                    expect)

    @subtest([(8, True, 127, -128),
              (8, False, 255, 0),
              (64, False, 0xdeadbeee, 0xdeadbeef)])
    def pre_post_inc_dec_int(self, bits, signed, less, more):
        '''test that ++x, --x, x++ and x-- produces correct values'''
        # TODO: today we inherit the undefined behaviour of (a++ - ++a) from C
        # Would be better to make this well-defined
        t = types.TInt(bits, signed)
        yield t.declaration('x') + ';'
        for (before, after, pre_op, post_op) in [
                (less, more, ctree.PreInc, ctree.PostInc),
                (more, less, ctree.PreDec, ctree.PostDec)]:
            yield 'x = %d;' % (before,)
            yield 'EXPECT(%s == %d);' % (
                pre_op(site, variable('x', t)).read(), after)
            yield 'EXPECT(x == %d);' % (after,)
            yield 'x = %d;' % (before,)
            yield 'EXPECT(%s == %d);' % (
                post_op(site, variable('x', t)).read(), before)
            yield 'EXPECT(x == %d);' % (after,)

    @subtest(
        [
            # basic functionality
            (size, signed, byte_order, value, op, res_expected, var_expected)
            for byte_order in ['big-endian', 'little-endian']
            for signed in (True, False)
            for size in (1, 5, 8)
            for value, preop, postop, expected_pre, expected_post in [
                    (0, ctree.mkPreInc, ctree.mkPostInc, "0", "1"),
                    (1, ctree.mkPreDec, ctree.mkPostDec, "1", "0")]
            for (op, res_expected, var_expected) in [
                    (preop, expected_post, expected_post),
                    (postop, expected_pre, expected_post)]]
        + [
            # overflow/underflow
            (3, signed, 'big-endian', value, op, expected_post, expected_post)
            for (signed, value, op, expected_post) in [
                    (True, 0, ctree.mkPreDec, "-1"),
                    (True, -1, ctree.mkPreInc, "0"),
                    (True, 0x7fffff, ctree.mkPreInc, "-0x800000"),
                    (True, -0x800000, ctree.mkPreDec, "0x7fffff"),
                    (False, 0xffffff, ctree.mkPreInc, "0"),
                    (False, 0, ctree.mkPreDec, "0xffffff"),
            ]])
    def pre_post_inc_dec_endian_int(self, size, signed, byte_order, value, op,
                                res_expected, var_expected):
        '''test that ++x, --x, x++ and x-- on an endian variable correctly
        modify the variable and return the correct values'''
        endian_type = types.TEndianInt(size*8, signed, byte_order)

        initializer = ctree.ExpressionInitializer(
            ctree.mkCast(site, ctree.mkIntegerConstant(site, value, True),
                         endian_type))
        var_declaration = ctree.mkDeclaration(site, "x",
                                              endian_type, initializer)
        endian_var_decl = output.StrOutput()
        with endian_var_decl:
            var_declaration.toc()
        operation = op(site, ctree.mkLocalVariable(
            site, symtab.LocalSymbol('x', 'x', endian_type, site=site)))
        var_read = ctree.as_int(ctree.mkLocalVariable(
            site, symtab.LocalSymbol('x', 'x', endian_type, site=site)))
        expect_int64 = size != 8 or signed
        self.expect_int_type(operation.ctype(), expect_int64)
        return [
            endian_var_decl.buf,
            'EXPECT(%s == %s);' % (operation.read(), res_expected),
            'EXPECT(%s == %s);' % (var_read.read(), var_expected),
            'EXPECT(IS_%sINT64(%s));' % (
                '' if expect_int64 else 'U', operation.read())
        ]

    @subtest((op, lbits, lsigned, rbits, rsigned)
             for op in [ctree.mkBitAnd,
                        ctree.mkBitOr,
                        ctree.mkBitXOr,
                        ctree.mkShL,
                        ctree.mkShR,
                        ctree.mkMult,
                        ctree.mkAdd,
                        ctree.mkSubtract,
                        ctree.mkDiv,
                        ctree.mkMod]
             for (lbits, lsigned, rbits, rsigned) in [
                     (8, False, 8, False),
                     (32, True, 63, False),
                     (64, False, 8, True),
                     (64, True, 64, False)])
    def int_binop_types(self, op, lbits, lsigned, rbits, rsigned):
        '''test that binary operations, when applied on lvalues or constants,
        produce a DML value of the right type, which generates a
        C expression of the right type'''
        expect_int64 = ((lbits, lsigned) != (64, False)
                        and (rbits, rsigned) != (64, False))
        ltype = types.TInt(lbits, lsigned)
        rtype = types.TInt(rbits, rsigned)
        expr = op(site, variable('l', ltype), variable('r', rtype))
        self.expect_int_type(expr.ctype(), expect_int64)
        const_expr = op(
            site,
            ctree.mkIntegerConstant(site, 1, (lbits, lsigned) != (64, False)),
            ctree.mkIntegerConstant(site, 1, (rbits, rsigned) != (64, False)))
        self.assertIsInstance(const_expr, ctree.IntegerConstant)
        self.expect_int_type(const_expr.ctype(), expect_int64)
        return [ltype.declaration('l') + ' = 1;',
                rtype.declaration('r') + ' = 1;',
                'EXPECT(IS_%sINT64(%s));' % ("" if expect_int64 else "U",
                                             expr.read()),
                'ASSERT(IS_%sINT64(%s));' % ("" if expect_int64 else "U",
                                             const_expr.read())]

    def expect_int_binop(self, lhconst, rhconst, binop, expected):
        const_op = binop(site, lhconst, rhconst)
        expect_signed = lhconst.type.signed and rhconst.type.signed
        self.expect_int_type(const_op.ctype(), expect_signed)
        var_op = binop(site, variable('x', rhconst.type),
                       variable('y', lhconst.type))
        self.expect_int_type(var_op.ctype(), expect_signed)
        [lh8type, rh8type] = [
            ctree.TInt(8, const.value < 0)
            if -128 <= const.value < 256 and const.type.signed
            else const.type
            for const in [lhconst, rhconst]]
        if (lh8type, rh8type) != (lhconst.type, rhconst.type):
            # cover sign extension if any operand fits in 8 bits
            var_8bit_op = binop(site, variable('z', lh8type),
                            variable('w', rh8type))
            expect_signed8 = (
                (lh8type.bits < 64 or lh8type.signed)
                and (rh8type.bits < 64 or rh8type.signed))
            self.expect_int_type(var_8bit_op.ctype(), expect_signed8)
            test_8bit = ['%s = %s;' % (lh8type.declaration('z'),
                                       lhconst.read()),
                         '%s = %s;' % (rh8type.declaration('w'),
                                       rhconst.read()),
                         'EXPECT((%s) == %s);' % (var_8bit_op.read(), expected)]
        else:
            test_8bit = []
        return ['EXPECT(%s == %s);' % (const_op.read(), expected),
                'EXPECT(IS_%sINT64(%s));'
                % ('' if expect_signed else 'U', const_op.read()),
                '%s = %s;' % (lhconst.type.declaration('x'), lhconst.read()),
                '%s = %s;' % (rhconst.type.declaration('y'), rhconst.read()),
                'EXPECT((%s) == %s);' % (var_op.read(), expected),
                'EXPECT(IS_%sINT64(%s));'
                % ('' if expect_signed else 'U', var_op.read())] + test_8bit

    def expect_float_binop(self, lhconst, rhconst, binop, expected):
        const_op = binop(site, lhconst, rhconst)
        self.expect_double(const_op.ctype())
        var_op = binop(site, variable('x', lhconst.type),
                       variable('y', rhconst.type))
        self.expect_double(var_op.ctype())
        return ['EXPECT(%s == %s);' % (const_op.read(), expected),
                'EXPECT(IS_DOUBLE(%s));' % (const_op.read(),),
                '%s = %s;' % (lhconst.type.declaration('x'), lhconst.read()),
                '%s = %s;' % (rhconst.type.declaration('y'), rhconst.read()),
                'EXPECT((%s) == %s);' % (var_op.read(), expected),
                'EXPECT(IS_DOUBLE(%s));' % (var_op.read())]

    @subtest((lh, rh, expect, rhsigned)
             for (l, r, expect) in [
                     (3, 6, '2'),
                     (-128, 513, '512'),
                     (3 << 62, 2 << 62, 'LLONG_MIN')]
             for rhsigned in [False, True]
             for (lh, rh) in [(r, l), (l, r)])
    def bit_and(self, lh, rh, expect, rhsigned):
        return self.expect_int_binop(int_const(lh, True),
                                     int_const(rh, rhsigned),
                                     ctree.mkBitAnd,
                                     expect)

    @subtest((lh, rh, expect, rhsigned)
             for (l, r, expect) in [
                     (3, 6, '7'),
                     (-128, 513, '-127'),
                     (3 << 62, 2 << 62, '3LL << 62')]
             for rhsigned in [False, True]
             for (lh, rh) in [(r, l), (l, r)])
    def bit_or(self, lh, rh, expect, rhsigned):
        return self.expect_int_binop(int_const(lh, True),
                                     int_const(rh, rhsigned),
                                     ctree.mkBitOr,
                                     expect)

    @subtest((lh, rh, expect, rhsigned)
             for (l, r, expect) in [
                     (3, 6, '5'),
                     (-128, 513, '-639'),
                     (3 << 62, 2 << 62, '1LL << 62')]
             for rhsigned in [False, True]
             for (lh, rh) in [(r, l), (l, r)])
    def bit_xor(self, lh, rh, expect, rhsigned):
        return self.expect_int_binop(int_const(lh, True),
                                     int_const(rh, rhsigned),
                                     ctree.mkBitXOr,
                                     expect)

    @subtest([(lh, False, rh, False, expect)
              for (lh, rh, expect) in [
                      (1, 0, '1'),
                      (1, 63, 'LLONG_MIN'),
                      (1, 64, 0),
                      (5, 3, '40'),
                      (1, -1, '0')]]
             # sign makes no difference, as long as rh >= 0
             + [(-5, lhsigned, 3, rhsigned, '-40')
                for lhsigned in [False, True]
                for rhsigned in [False, True]])
    def shl(self, lh, lhsigned, rh, rhsigned, expect):
        return self.expect_int_binop(int_const(lh, lhsigned),
                                     int_const(rh, rhsigned),
                                     ctree.mkShL,
                                     expect)

    @subtest((op, 0, shift)
             for op in [ctree.mkShL, ctree.mkShR]
             for shift in [-1, 1 << 63])
    def sh_neg(self, op, lh, rh):
        lhconst = int_const(lh, True)
        rhconst = int_const(rh, True)
        with self.assertRaises(messages.ESHNEG):
            op(site, lhconst, rhconst)
        var_op = op(site, variable('a', lhconst.type),
                              variable('b', rhconst.type))
        return (['%s = %s;' % (lhconst.type.declaration('a'), lhconst.read()),
                 '%s = %s;' % (rhconst.type.declaration('b'), rhconst.read())]
                + self.expect_assert_error('%s;' % (var_op.read(),)))

    @subtest([(lh, False, rh, False, expect)
              for (lh, rh, expect) in [
                      (1, 0, '1'),
                      (1, 1 << 63, '0'),
                      ((1 << 63), 7, '1LL << 56'),
                      (1, -1, '0')]]
             # sign is extended if both operands are signed
             + [(0xdead << 48, lhsigned, 32, rhsigned,
                 '0xffffffffdead0000' if lhsigned and rhsigned
                 else '0xdead0000')
                for lhsigned in [False, True]
                for rhsigned in [False, True]])
    def shl(self, lh, lhsigned, rh, rhsigned, expect):
        return self.expect_int_binop(int_const(lh, lhsigned),
                                     int_const(rh, rhsigned),
                                     ctree.mkShR,
                                     expect)

    @subtest([(lh, lhsigned, rh, rhsigned, expect)
              for (a, b, expect) in [
                      (100, 33, '3300'),
                      (-100, 55, '-5500'),
                      (0x0a0b0c0d, 0x11 << 40, '0xbbccdd0000000000ull')]
              for lhsigned in [False, True]
              for rhsigned in [False, True]
              for (lh, rh) in [(a, b), (b, a)]])
    def mult(self, lh, lhsigned, rh, rhsigned, expect):
        return self.expect_int_binop(int_const(lh, lhsigned),
                                     int_const(rh, rhsigned),
                                     ctree.mkMult,
                                     expect)

    @subtest([
        (float_const(2.5), float_const(2.5), '6.25'),
        # promote unsigned to float
        (int_const(3, True), float_const(2.5), '7.5'),
        # precision truncated to 54 bits (53 mantissa bits)
        (int_const(0xdeadbeefdeadbeef, False), float_const(1.0),
         '0xdeadbeefdeadbc.p8'),
        # promote signed integer to float
        (int_const(-11, True), float_const(2.5), '-27.5'),
    ])
    def fmult(self, lh, rh, result):
        return self.expect_float_binop(lh, rh, ctree.mkMult, result)

    @subtest([(lh, lhsigned, rh, rhsigned, expect, sub)
              for (a, b, expect) in [
                      (100, 33, '133'),
                      (-1, 1, 0),
                      (1 << 63, 3 << 62, '1LL<<62')]
              for lhsigned in [False, True]
              for rhsigned in [False, True]
              for sub in [False, True]
              for (lh, rh) in [(a, b), (b, a)]])
    def add_sub_int(self, lh, lhsigned, rh, rhsigned, expect, sub):
        if sub:
            return self.expect_int_binop(int_const(lh, lhsigned),
                                         int_const(-rh, rhsigned),
                                         ctree.mkSubtract,
                                         expect)
        else:
            return self.expect_int_binop(int_const(lh, lhsigned),
                                         int_const(rh, rhsigned),
                                         ctree.mkAdd,
                                         expect)

    @subtest([
        (types.TInt(8, True),),
        (types.TPtr(types.TVoid()),),
    ])
    def add_sub_ptr(self, ptype):
        p = variable('p', types.TPtr(ptype, const=True))
        a = variable('a', types.TArray(ptype, int_const(10, True), const=True))
        i = variable('i', types.TInt(64, True))
        stmts = ['%s = {};' % (ptype.declaration('a[8]'),),
                 'int64 i = 5;',
                 '%s = a;' % (ptype.declaration('*p'),),
                 '%s = &a[5];' % (ptype.declaration('*exp'))]
        for ptr in [a, p]:
            add1 = ctree.mkAdd(site, ptr, i)
            add2 = ctree.mkAdd(site, i, ptr)
            sub1 = ctree.mkSubtract(site, ptr, ctree.mkUnaryMinus(site, i))
            sub2 = ctree.mkSubtract(site, ptr, add1)
            for expr in (add1, add2, sub1):
                self.assertTrue(expr.ctype().base.eq(ptype))
            self.expect_int_type(sub2.ctype(), True)
            stmts.extend([
                'EXPECT(%s == exp);' % (add1.read(),),
                'EXPECT(%s == exp);' % (add2.read(),),
                'EXPECT(%s == exp);' % (sub1.read(),),
                'EXPECT(IS_INT64(%s));' % (sub2.read(),),
                'EXPECT(%s == -5);' % (sub2.read(),)])
        return stmts

    @subtest()
    def add_sub_voidptr(self):
        p = variable('p', types.TPtr(types.TVoid()))
        i = variable('i', types.TInt(64, True))
        for (op, lh, rh) in [(ctree.mkAdd, p, i),
                         (ctree.mkAdd, i, p),
                         (ctree.mkSubtract, p, i),
                         (ctree.mkSubtract, p, p)]:
            with self.assertRaises(messages.EBTYPE):
                op(site, lh, rh)
        return None

    @subtest()
    def add_stringlit(self):
        lh = ctree.mkStringConstant(site, "hello")
        rh = ctree.mkStringConstant(site, b"world")
        expr = ctree.mkAdd(site, lh, rh)
        self.assertTrue(expr.constant)
        self.assertEqual(expr.value, b"helloworld")

    @subtest([(lh, rh, result, sub)
              for (l, r, result) in [
                      (float_const(2.5), float_const(2.25), '4.75'),
                      # promote unsigned to float
                      (int_const(3, True), float_const(2.5), '5.5'),
                      # precision truncated to 54 bits (53 mantissa bits)
                      (int_const(0x8eadbeefdeadbeef, False), float_const(0.0),
                       '0x8eadbeefdeadbc.p8'),
                      # promote signed integer to float
                      (int_const(-11, True), float_const(2.5), '-8.5')
                      ]
              for (lh, rh) in [(l, r), (r, l)]
              for sub in [False, True]
              # skip the test where sub would negate an unsigned
              # operand before it's promoted to double, because
              # a-(-b)!=a+b in that case
              if (not sub or not isinstance(rh, ctree.IntegerConstant)
                  or rh.signed)])
    def fadd_fsub(self, lh, rh, result, sub):
        if sub:
            return self.expect_float_binop(lh, ctree.mkUnaryMinus(site, rh),
                                           ctree.mkSubtract, result)
        else:
            return self.expect_float_binop(lh, rh, ctree.mkAdd, result)

    @subtest([(int_const(lh, lhsigned), int_const(rh, rhsigned),
               div if do_div else mod, do_div)
              for (lh, lhsigned, rh, rhsigned, div, mod) in [
                      (1, True, 1, True, '1', '0'),
                      (17, True, 5, True, '3', '2'),
                      (1 << 63, False, (1 << 62) + 1, False,
                       '1', '(1LL << 62) - 1'),
                      # RH promoted to unsigned
                      ((1 << 64) - 1, False, -(1 << 63), True,
                       '1', 'LLONG_MAX'),
                      # LH promoted to unsigned
                      (-1, True, 2, False, 'LLONG_MAX', '1'),
                      # negative div: round toward 0; maintain l/r*r+l%r==l
                      (-17, True, 5, True, '-3', '-2'),
                      (-17, True, -5, True, '3', '-2'),
                      (17, True, -5, True, '-3', '2'),
                      ]
              for do_div in [True, False]])
    def divmod(self, lh, rh, expect, do_div):
        return self.expect_int_binop(
            lh, rh, ctree.mkDiv if do_div else ctree.mkMod, expect)

    @subtest([(float_const(3.75), float_const(2.5), '1.5'),
              # promote unsigned to float
              (int_const(3, True), float_const(1.5), '2.0'),
              # precision truncated to 54 bits (53 mantissa bits)
              (int_const(0x8eadbeefdeadbeef, False), float_const(1.0),
               '0x8eadbeefdeadbc.p8'),
              # promote signed integer to float
              (float_const(5), int_const(-2, True), '-2.5'),
              # divisions by zero
              (float_const(1), int_const(0, True), 'INFINITY'),
              (float_const(-1), int_const(0, True), '-INFINITY')])
    def fdiv(self, lh, rh, result):
        return self.expect_float_binop(lh, rh, ctree.mkDiv, result)

    @subtest([(float_const('inf'), float_const('inf'), ctree.mkSubtract),
              (float_const('inf'), float_const('-inf'), ctree.mkAdd),
              (float_const('-inf'), float_const('inf'), ctree.mkAdd),
              (float_const('-inf'), float_const('-inf'), ctree.mkSubtract),
              (float_const('-inf'), float_const(0), ctree.mkMult),
              (float_const(-0.0), int_const(0, True), ctree.mkDiv),
              (float_const(0.0), float_const(-0.0), ctree.mkDiv),
              (int_const(0, False), float_const(-0.0), ctree.mkDiv)] + [
                  (lh, rh, op)
                  for (l, r) in [(int_const(1, False), float_const('nan'))]
                  for op in [ctree.mkAdd, ctree.mkSubtract,
                             ctree.mkMult, ctree.mkDiv]
                  for (lh, rh) in [(l, r), (r, l)]])
    def nan_binops(self, lh, rh, op):
        const_op = op(site, lh, rh)
        self.expect_double(const_op.ctype())
        var_op = op(site, variable('x', lh.type),
                       variable('y', rh.type))
        self.expect_double(var_op.ctype())
        return ['EXPECT(isnan(%s));' % (const_op.read(),),
                'EXPECT(IS_DOUBLE(%s));' % (const_op.read(),),
                '%s = %s;' % (lh.type.declaration('x'), lh.read()),
                '%s = %s;' % (rh.type.declaration('y'), rh.read()),
                'EXPECT(isnan(%s));' % (var_op.read(),),
                'EXPECT(IS_DOUBLE(%s));' % (var_op.read())]

    @subtest([(int_const(lh, lhsigned), op)
              for op in (ctree.mkDiv, ctree.mkMod)
              for lh in [0, 1 << 63]
              for lhsigned in [True, False]])
    def divmod_by_zero(self, lh, op):
        rh = int_const(0, True)
        with self.assertRaises(messages.EDIVZ):
            op(site, lh, rh)
        var_op = op(site, variable('a', lh.type),
                    variable('b', rh.type))
        return (['%s = %s;' % (lh.type.declaration('a'), lh.read()),
                 '%s = %s;' % (rh.type.declaration('b'), rh.read())]
                + self.expect_assert_error('%s;' % (var_op.read(),)))

    @subtest([
        (lh, rh, op, bool(result))
        for (lh, rh, results) in [
                (int_const(0, True), int_const(0, True), (0, 1, 0, 1, 0, 1)),
                (int_const(-1, True), int_const(0, False), (1, 1, 0, 0, 1, 0)),
                (int_const(0, False), int_const(-1, True), (0, 0, 1, 1, 1, 0)),
                (ctree.TypedIntegerConstant(site, 0, types.TInt(8, False)),
                 int_const(-1, True), (0, 0, 1, 1, 1, 0)),
                (int_const(-1, False), int_const(-1, True), (0, 0, 1, 1, 1, 0)),
                (int_const(-1, True), int_const(-1, False), (1, 1, 0, 0, 1, 0)),
                (int_const(0, True), int_const(1, False), (1, 1, 0, 0, 1, 0)),
                (int_const(1, True), int_const(0, False), (0, 0, 1, 1, 1, 0)),
                (float_const(float('nan')), int_const(0, True),
                 (0, 0, 0, 0, 1, 0))
                ]
        for (result, op) in zip(results,
                                [ctree.mkLessThan,
                                 ctree.mkLessThanOrEquals,
                                 ctree.mkGreaterThan,
                                 ctree.mkGreaterThanOrEquals,
                                 ctree.mkNotEquals,
                                 ctree.mkEquals])])
    def compare_numeric(self, lh, rh, op, result):
        if lh.constant and rh.constant:
            const_cond = op(site, lh, rh)
            self.assertIsInstance(const_cond, ctree.BoolConstant)
            self.assertEqual(const_cond.value, result)
        lh_var = variable('lh', lh.ctype())
        rh_var = variable('rh', rh.ctype())
        var_cond = op(site, lh_var, rh_var)
        # compare constant and variable
        logging.ignore_warning('WNEGCONSTCOMP')
        mix_cond = op(site, lh, rh_var)
        logging.enable_warning('WNEGCONSTCOMP')
        self.assertIsInstance(var_cond.ctype(), types.TBool)
        self.assertIsInstance(mix_cond.ctype(), types.TBool)
        return ['%s = %s;' % (lh.ctype().declaration('lh'), lh.read()),
                '%s = %s;' % (rh.ctype().declaration('rh'), rh.read()),
                'EXPECT(%s(%s));' % ('' if result else '!', var_cond.read()),
                'EXPECT(%s(%s));' % ('' if result else '!', mix_cond.read())]

    @subtest([
        (lh, rh, op, bool(result))
        for (lh, rh, results) in [
                (ctree.mkLit(site, 'arr', types.TArray(types.TInt(32, True),
                                                   int_const(0, True))),
                 ctree.mkLit(site, '&arr[1]', types.TPtr(types.TInt(32, True))),
                 (1, 1, 0, 0, 1, 0)),
                (ctree.mkLit(site, '&arr[1]', types.TPtr(types.TInt(32, True))),
                 ctree.mkLit(site, 'arr', types.TArray(types.TInt(32, True),
                                                       int_const(0, True))),
                 (0, 0, 1, 1, 1, 0)),
                (ctree.mkLit(site, '&arr[1]', types.TPtr(types.TInt(32, True))),
                 ctree.mkLit(site, '&arr[1]', types.TPtr(types.TInt(32, True))),
                 (0, 1, 0, 1, 0, 1))]
        for (result, op) in zip(results,
                                [ctree.mkLessThan,
                                 ctree.mkLessThanOrEquals,
                                 ctree.mkGreaterThan,
                                 ctree.mkGreaterThanOrEquals,
                                 ctree.mkNotEquals,
                                 ctree.mkEquals])])
    def compare_pointers(self, lh, rh, op, result):
        cond = op(site, lh, rh)
        self.assertIsInstance(cond.ctype(), types.TBool)
        return ['int32 arr[4] = {};'
                'EXPECT(%s(%s));' % ('' if result else '!', cond.read())]

    @subtest()
    def bad_cond(self):
        def report(error): raise error
        # hack: ENBOOL is report():ed instead of raised; inject stuff
        # to capture it
        assert ctree.report == logging.report
        ctree.report = report
        try:
            for op in [
                    ctree.mkNot,
                    lambda site, rh: ctree.mkIfExpr(
                        site, rh,
                        int_const(0), int_const(0))]:
                for cond in [int_const(0), float_const(0.3)]:
                    with self.assertRaises(messages.ENBOOL):
                        op(site, cond)
        finally:
            ctree.report = logging.report
        with self.assertRaises(messages.EBINOP):
            ctree.mkIfExpr(
                site,
                ctree.mkBoolConstant(site, True),
                ctree.mkLit(site, '', types.TPtr(types.TInt(8, False))),
                ctree.mkLit(site, '', types.TPtr(types.TInt(8, True))))

    @subtest([(cond, tbits, tsigned, fbits, fsigned, rsigned)
              for (tbits, tsigned, fbits, fsigned, rsigned) in [
                      (32, False, 64, True, True),
                      (64, False, 8, True, False)]
              for cond in [True, False]
              ])
    def int_cond(self, cond, tbits, tsigned, fbits, fsigned, rsigned):
        cond_const = ctree.mkBoolConstant(site, cond)
        cond_var = variable('cond', types.TBool())
        tconst = ctree.mkCast(site, int_const(-128), types.TInt(tbits, tsigned))
        fconst = ctree.mkCast(site, int_const(-127), types.TInt(fbits, fsigned))
        tvar = variable('t', tconst.ctype())
        fvar = variable('f', fconst.ctype())
        const_const = ctree.mkIfExpr(site, cond_const, tconst, fconst)
        result = [fconst, tconst][cond].value
        if not rsigned:
            result = result & ((1 << 64) - 1)
        self.expect_int_type(const_const.type, rsigned)
        self.assertTrue(const_const.constant)
        self.assertEqual(const_const.value, result)
        const_var = ctree.mkIfExpr(site, cond_const, tvar, fvar)
        self.expect_int_type(const_var.ctype(), rsigned)
        var_const = ctree.mkIfExpr(site, cond_var, tconst, fconst)
        self.expect_int_type(var_const.ctype(), rsigned)
        var_var = ctree.mkIfExpr(site, cond_var, tvar, fvar)
        self.expect_int_type(var_var.ctype(), rsigned)
        u = '' if rsigned else 'U'
        return ['bool cond = %s;' % (str(cond).lower(),),
                '%s = %d;' % (tconst.type.declaration('t'), -128),
                '%s = %d;' % (fconst.type.declaration('f'), -127),
                'EXPECT(IS_%sINT64(%s));' % (u, const_var.read()),
                'EXPECT(IS_%sINT64(%s));' % (u, var_const.read()),
                'EXPECT(IS_%sINT64(%s));' % (u, var_var.read()),
                'EXPECT(%s == %dULL);' % (const_var.read(), result),
                'EXPECT(%s == %dULL);' % (var_const.read(), result),
                'EXPECT(%s == %dULL);' % (var_var.read(), result)]

    @subtest()
    def ptr_cond(self):
        cond = lit(types.TBool())
        t = ctree.mkIfExpr(
            site, cond, lit(types.TPtr(types.TInt(8, True))),
            lit(types.TPtr(types.TInt(8, True, const=True)))).ctype()
        self.assertIsInstance(t, types.TPtr)
        self.expect_int_type(t.base, True, 8)
        self.assertTrue(t.base.const)
        t = ctree.mkIfExpr(
            site, cond, lit(types.TPtr(types.TVoid())),
            lit(types.TPtr(types.TInt(8, True, const=True)))).ctype()
        self.assertIsInstance(t, types.TPtr)
        self.assertTrue(t.base.const)
        self.assertTrue(t.base.void)
        t = ctree.mkIfExpr(
            site, ctree.mkBoolConstant(site, True),
            int_const(2), float_const(1.3))
        assert t.constant
        self.assertIsInstance(t, ctree.FloatConstant)
        self.assertEqual(t.value, 2.0)

    @subtest([
        (lh, rh, op, result)
        for (lh, rh, equal) in [
                (ctree.mkStringConstant(site, ""),
                 ctree.mkStringConstant(site, ""), True),
                (ctree.mkStringConstant(site, "aaa"),
                 ctree.mkStringConstant(site, "aab"), False),
                (ctree.mkStringConstant(site, "hello"),
                 ctree.mkStringConstant(site, "hello"),
                 True),
                (ctree.mkBoolConstant(site, True),
                 ctree.mkBoolConstant(site, False), False),
                (ctree.mkBoolConstant(site, False),
                 ctree.mkBoolConstant(site, False), True)]
        for (op, result) in [(ctree.mkEquals, equal),
                             (ctree.mkNotEquals, not equal)]])
    def equal_constants(self, lh, rh, op, result):
        cond = op(site, lh, rh)
        self.assertTrue(cond.constant)
        self.assertEqual(cond.value, result)

    @subtest()
    def equal_pointers(self):
        for op in [ctree.mkEquals, ctree.mkNotEquals]:
            # a void pointer can be compared to any pointer
            op(site, lit(types.TPtr(types.TInt(8, True))),
               lit(types.TPtr(types.TVoid())))
        for op in [ctree.mkLessThan,
                   ctree.mkLessThanOrEquals,
                   ctree.mkGreaterThan,
                   ctree.mkGreaterThanOrEquals]:
            with self.assertRaises(messages.EILLCOMP):
                op(site, lit(types.TPtr(types.TInt(8, True))),
                   lit(types.TPtr(types.TVoid())))

    @subtest()
    def null_pointers(self):
        null = ctree.mkNullConstant(site)
        for expr in (lit(types.TPtr(types.TInt(8, True))),
                     ctree.mkCast(site, null,
                                  types.TPtr(types.TInt(8, True)))):
            for op in [ctree.mkEquals, ctree.mkNotEquals]:
                # NULL can be compared to any pointer...
                res = op(site, null, expr)
                # .. but equality comparisons are never constant unless both
                # sides are constant
                self.assertFalse(res.constant)
        for (invert, op) in ((False, ctree.mkEquals),
                             (True, ctree.mkNotEquals)):
            res = op(site, null, ctree.mkNullConstant(site));
            self.assertTrue(res.constant)
            self.assertEqual(res.value, True != invert)
            res = op(site, null, ctree.mkStringConstant(site, "non-null"))
            self.assertTrue(res.constant)
            self.assertEqual(res.value, False != invert)

        return [f'EXPECT({null.read()} == NULL);']

    @subtest()
    def assign_trunc(self):
        target_type = types.TInt(5, True)
        stmt = ctree.mkCopyData(site, int_const(0x5f),
                                variable('x', target_type))
        code = output.StrOutput()
        with code:
            stmt.toc()
        return ['%s;' % (target_type.declaration('x'),),
                code.buf,
                'EXPECT(x == -1);']

    @subtest()
    def const_types(self):
        type_objs = [
            types.TVoid(),
            types.TDevice('struct_t'),
            types.TNamed('struct_t'),
            types.TBool(),
            types.TInt(24, True),
            types.TEndianInt(24, True, 'big-endian'),
            types.TLong(False),
            types.TSize(True),
            types.TInt64_t(True),
            types.TFloat('double'),
            types.TArray(types.TBool(), int_const(3)),
            types.TPtr(types.TBool()),
            types.TPtr(types.TFunction([], types.TVoid())),
            types.TPtr(types.TArray(types.TBool(), int_const(3))),
            types.TTrait(traits.Trait(
                site, 'struct_t', set(), {}, {}, {}, {}, {}, {}, {})),
            types.TTraitList('struct_t'),
            types.TExternStruct({}, 'struct_t', 'struct_t'),
            types.TStruct({'x': types.TBool()}, 'struct_label'),
            types.TLayout(
                'big-endian', [(site, 'x',
                                types.TEndianInt(24, True, 'big-endian'))],
                'struct_label'),
            types.THook([]),
        ]
        covered_types = {type(o) for o in type_objs}
        all_types = {t for t in types.__dict__.values()
                     if isinstance(t, type) and issubclass(t, types.DMLType)}
        assert covered_types <= all_types, covered_types - all_types
        untested_types = {
            # abstract type
            types.DMLType,
            # abstract type
            types.IntegerType,
            # abstract type
            types.StructType,
            # 1.2, weird
            types.TUnknown,
            # known to be broken, hard to fix
            types.TVector,
            # function types don't allow const qualification
            types.TFunction,
        }
        assert all_types - untested_types <= covered_types, (
            all_types - covered_types)
        ret = ['typedef struct struct_label { bool x; } struct_t;']
        for (i, t) in enumerate(type_objs):
            t.resolve()
            assert not t.const
            ct = t.clone()
            ct.const = True
            # ct is const
            ret.append(
                f'void (*p{i})(typeof({ct.declaration("")}) *) UNUSED'
                f' = (void (*)(const typeof({t.declaration("")}) *))NULL;')
            # t is not const
            ret.append(f'void *v{i} UNUSED'
                       f' = (typeof({t.declaration("")}) *)NULL;')
        return ret

    def test(self):
        self.run_gcc_tests()
