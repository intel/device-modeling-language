# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Test environment parameters injected by the test runner.
# Test scripts should import this module and call the functions below.
_scratchdir: str = ''


def scratchdir() -> str:
    return _scratchdir


def instantiate():
    import simics
    return simics.SIM_create_object('test', 'obj', [])
