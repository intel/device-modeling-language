# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path

from simicsutils.host import is_windows
from simicsutils.internal import api_versions, default_api_version

def generate_env(out):
    api_versions_dict = {v: 4 if v == '4.8' else int(v) for v in api_versions()}
    Path(out).write_text(f'''\
def is_windows():
    return {is_windows()}
def api_versions():
    return {api_versions_dict}
def default_api_version():
    return {api_versions_dict[default_api_version()]}
''')

if __name__ == '__main__':
    (_, out) = sys.argv
    generate_env(out)
