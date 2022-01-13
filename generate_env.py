# © 2021-2022 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
import sys
from pathlib import Path

from simicsutils.host import is_windows
from simicsutils.internal import api_versions, default_api_version

def generate_env(out):
    Path(out).write_text(f'''\
def is_windows():
    return {is_windows()}
def api_versions():
    return {api_versions()}
def default_api_version():
    return {repr(default_api_version())}
''')

if __name__ == '__main__':
    (_, out) = sys.argv
    generate_env(out)
