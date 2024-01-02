# © 2021-2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
import sys

def generate_parsetabs(dml_package, version, parsetab, debugfile):
    sys.path.append(dml_package)
    from dml import toplevel

    # ply's logic for precompiled parse tables is rather limited, the
    # only available operation is something like "try to load Python
    # module X, and if that fails, create X.py". We want to
    # unconditionally create the parsetab, so we must remove
    # previously built parsetabs.  Note that the parsetab used by dmlc
    # is stored in a .pyc file in the dml package, while the .py file
    # is in the build directory. This way we deprave ply the
    # possibility to carry out any caching logic.
    for suff in ['.py', '.pyc']:
        if os.path.lexists(parsetab + suff):
            os.remove(parsetab + suff)
    toplevel.get_parser(tuple(map(int, version)), parsetab, debugfile)
    # Some shift/reduce conflicts are expected. There is no clean
    # way to capture s/r conflicts, so we just parse a log manually.
    prefix = 'WARNING: '
    with open(debugfile) as f:
        lines = [line[len(prefix):].rstrip() for line in f
                 if line.startswith(prefix)]
    assert lines[0:3] == ['', 'Conflicts:', ''], lines
    conflicts = lines[3:]
    assert len(conflicts) == 10, conflicts
    assert all(conflict.startswith('shift/reduce conflict')
               for conflict in conflicts)

if __name__ == '__main__':
    (_, dml_package, version, parsetab, debugfile) = sys.argv
    generate_parsetabs(dml_package, version, parsetab, debugfile)
