# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import dev_util as du
import stest

expect = '''
Write to unimplemented register regs.r1 (0x10) (value written = 0x00001267).
Write to unimplemented register regs.r3 (0x30) (value written = 0x00001267).
Write to read-only register regs.r4 (value written = 0x00001267).
Write to unimplemented register regs.r5 (0x50) (value written = 0x00001267).
Write to unimplemented register with_fields.r6 (0x10) (value written = 0x00001267).
Write to unimplemented field with_fields.r6.f (0x10) (value written = 0x00000001, contents = 0x00000000), will not warn again.
Write to unimplemented field with_fields.r7.f (0x20) (value written = 0x00000001, contents = 0x00000000), will not warn again.
Read from unimplemented register regs.r1 (0x00000010) (contents = 0x0000126a).
Read from unimplemented register regs.r2 (0x00000020) (contents = 0x0000126a).
Read from unimplemented register regs.r4 (0x00000040) (contents = 0x00000000).
Read from write-only register regs.r5 (returning 0).
Read from unimplemented register with_fields.r6 (0x00000010) (contents = 0x00000000).
Write to unimplemented register regs.r1 (0x10) (value written = 0x00001271).
Write to unimplemented register regs.r3 (0x30) (value written = 0x00001271).
Write to read-only register regs.r4 (value written = 0x00001271).
Write to unimplemented register regs.r5 (0x50) (value written = 0x00001271).
Write to unimplemented register with_fields.r6 (0x10) (value written = 0x00001271).
Write to unimplemented field with_fields.r6.f (0x10) (value written = 0x00000001, contents = 0x00000000), will not warn again.
Write to unimplemented field with_fields.r7.f (0x20) (value written = 0x00000001, contents = 0x00000000), will not warn again.
Write to unimplemented register regs.r1 (0x10) (value written = 0x00001274).
Write to unimplemented register regs.r3 (0x30) (value written = 0x00001274).
Write to read-only register regs.r4 (value written = 0x00001274).
Write to unimplemented register regs.r5 (0x50) (value written = 0x00001274).
Write to unimplemented register with_fields.r6 (0x10) (value written = 0x00001274).
Write to unimplemented field with_fields.r6.f (0x10) (value written = 0x00000000, contents = 0x00000001), will not warn again.
Write to unimplemented field with_fields.r7.f (0x20) (value written = 0x00000000, contents = 0x00000001), will not warn again.
Read from unimplemented register regs.r1 (0x00000010) (contents = 0x00001274).
Read from unimplemented register regs.r2 (0x00000020) (contents = 0x00001274).
Read from unimplemented register regs.r4 (0x00000040) (contents = 0x00000000).
Read from write-only register regs.r5 (returning 0).
Read from unimplemented register with_fields.r6 (0x00000010) (contents = 0x00000000).
Read from unimplemented register regs.r1 (0x00000010) (contents = 0x00001274).
Read from unimplemented register regs.r2 (0x00000020) (contents = 0x00001274).
Read from unimplemented register regs.r4 (0x00000040) (contents = 0x00000000).
Read from write-only register regs.r5 (returning 0).
Read from unimplemented register with_fields.r6 (0x00000010) (contents = 0x00000000).
Write to unimplemented register regs.r1 (0x10) (value written = 0x0000127b).
Write to unimplemented register regs.r3 (0x30) (value written = 0x0000127b).
Write to read-only register regs.r4 (value written = 0x0000127b).
Write to unimplemented register regs.r5 (0x50) (value written = 0x0000127b).
Write to unimplemented register with_fields.r6 (0x10) (value written = 0x0000127b).
Write to unimplemented field with_fields.r6.f (0x10) (value written = 0x00000001, contents = 0x00000000), will not warn again.
Write to unimplemented field with_fields.r7.f (0x20) (value written = 0x00000001, contents = 0x00000000), will not warn again.
Write to unimplemented register regs.r1 (0x10) (value written = 0x0000127e).
Write to unimplemented register regs.r3 (0x30) (value written = 0x0000127e).
Write to read-only register regs.r4 (value written = 0x0000127e).
Write to unimplemented register regs.r5 (0x50) (value written = 0x0000127e).
Write to unimplemented register with_fields.r6 (0x10) (value written = 0x0000127e).
Write to unimplemented field with_fields.r6.f (0x10) (value written = 0x00000000, contents = 0x00000001), will not warn again.
Write to unimplemented field with_fields.r7.f (0x20) (value written = 0x00000000, contents = 0x00000001), will not warn again.
Read from unimplemented register regs.r1 (0x00000010) (contents = 0x0000127e).
Read from unimplemented register regs.r2 (0x00000020) (contents = 0x0000127e).
Read from unimplemented register regs.r4 (0x00000040) (contents = 0x00000000).
Read from write-only register regs.r5 (returning 0).
Read from unimplemented register with_fields.r6 (0x00000010) (contents = 0x00000000).
Read from unimplemented register regs.r1 (0x00000010) (contents = 0x0000127e).
Read from unimplemented register regs.r2 (0x00000020) (contents = 0x0000127e).
Read from unimplemented register regs.r4 (0x00000040) (contents = 0x00000000).
Read from write-only register regs.r5 (returning 0).
Read from unimplemented register with_fields.r6 (0x00000010) (contents = 0x00000000).
Write to unimplemented register regs.r1 (0x10) (value written = 0x00001285).
Write to register regs.r1 (addr 0x10) <- 0x00001285
Write to register regs.r2 (addr 0x20) <- 0x00001285
Write to unimplemented register regs.r3 (0x30) (value written = 0x00001285).
Write to register regs.r3 (addr 0x30) <- 0x00001285
Write to read-only register regs.r4 (value written = 0x00001285).
Write to unimplemented register regs.r5 (0x50) (value written = 0x00001285).
Write to register regs.r5 (addr 0x50) <- 0x00001285
Write to unimplemented register with_fields.r6 (0x10) (value written = 0x00001285).
Write to register with_fields.r6 (addr 0x10) <- 0x00001285
Write to unimplemented field with_fields.r6.f (0x10) (value written = 0x00000001, contents = 0x00000000), will not warn again.
Write to register with_fields.r7 (addr 0x20) <- 0x00001285
Write to unimplemented field with_fields.r7.f (0x20) (value written = 0x00000001, contents = 0x00000000), will not warn again.
Write to unimplemented register regs.r1 (0x10) (value written = 0x00001288).
Write to register regs.r1 (addr 0x10) <- 0x00001288
Write to register regs.r2 (addr 0x20) <- 0x00001288
Write to unimplemented register regs.r3 (0x30) (value written = 0x00001288).
Write to register regs.r3 (addr 0x30) <- 0x00001288
Write to read-only register regs.r4 (value written = 0x00001288).
Write to unimplemented register regs.r5 (0x50) (value written = 0x00001288).
Write to register regs.r5 (addr 0x50) <- 0x00001288
Write to unimplemented register with_fields.r6 (0x10) (value written = 0x00001288).
Write to register with_fields.r6 (addr 0x10) <- 0x00001288
Write to unimplemented field with_fields.r6.f (0x10) (value written = 0x00000000, contents = 0x00000001), will not warn again.
Write to register with_fields.r7 (addr 0x20) <- 0x00001288
Write to unimplemented field with_fields.r7.f (0x20) (value written = 0x00000000, contents = 0x00000001), will not warn again.
Read from unimplemented register regs.r1 (0x00000010) (contents = 0x00001288).
Read from register regs.r1 (addr 0x10) -> 0x00001288
Read from unimplemented register regs.r2 (0x00000020) (contents = 0x00001288).
Read from register regs.r2 (addr 0x20) -> 0x00001288
Read from register regs.r3 (addr 0x30) -> 0x00001288
Read from unimplemented register regs.r4 (0x00000040) (contents = 0x00000000).
Read from register regs.r4 (addr 0x40) -> 0x00000000
Read from write-only register regs.r5 (returning 0).
Read from unimplemented register with_fields.r6 (0x00000010) (contents = 0x00000000).
Read from register with_fields.r6 (addr 0x10) -> 0x00000000
Read from register with_fields.r7 (addr 0x20) -> 0x00000000
Read from unimplemented register regs.r1 (0x00000010) (contents = 0x00001288).
Read from register regs.r1 (addr 0x10) -> 0x00001288
Read from unimplemented register regs.r2 (0x00000020) (contents = 0x00001288).
Read from register regs.r2 (addr 0x20) -> 0x00001288
Read from register regs.r3 (addr 0x30) -> 0x00001288
Read from unimplemented register regs.r4 (0x00000040) (contents = 0x00000000).
Read from register regs.r4 (addr 0x40) -> 0x00000000
Read from write-only register regs.r5 (returning 0).
Read from unimplemented register with_fields.r6 (0x00000010) (contents = 0x00000000).
Read from register with_fields.r6 (addr 0x10) -> 0x00000000
Read from register with_fields.r7 (addr 0x20) -> 0x00000000
'''.strip().split('\n')

# No unexpected logging
stest.untrap_log('spec-viol')

# Collect all log messages
log_messages = []
def collect(arg, obj, logtype, msg):
    global log_messages
    log_messages.append(msg)

SIM_hap_add_callback("Core_Log_Message", collect, 0)

def write(reg, val):
    reg.write(val)

def read(reg):
    reg.read()

def write_fun(val):
    return lambda r: write(r, val)

def test(obj, access):
    for offset in (0x10, 0x20, 0x30, 0x40, 0x50):
        reg = du.Register_LE(obj.bank.regs, offset)
        access(reg)
    for offset in (0x10, 0x20):
        reg = du.Register_LE(obj.bank.with_fields, offset)
        access(reg)

o = SIM_create_object("test", "o", [])
n = 4711
for l in range(1, 5):
    o.bank.regs.log_level = l
    o.bank.with_fields.log_level = l
    test(o, write_fun(n))
    test(o, write_fun(n + 3))
    n += 10
    test(o, read)
    test(o, read)

stest.expect_true(o.runtest)

def fail(exp, got):
    print('*** Failure, got:\n')
    print('\n'.join(got))
    print('\nexpected:\n')
    print('\n'.join(exp))
    stest.fail("differences found")

if expect != log_messages:
    fail(expect, log_messages)
