# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path

from simicsutils.host import is_windows
from simicsutils.internal import api_versions, default_api_version

def generate_env(out):
    output = Path(out)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(f'''\
def is_windows():
    return {is_windows()}
def api_versions():
    return {repr(api_versions())}
def default_api_version():
    return {repr(default_api_version())}
''')

if __name__ == '__main__':
    (_, out) = sys.argv
    generate_env(out)
