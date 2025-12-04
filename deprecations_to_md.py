# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path
[path_to_dml, header, outfile] = sys.argv[1:]
sys.path.append(path_to_dml)

from dml import breaking_changes
from dml.env import api_versions, default_api_version

by_version = {}
for bc in breaking_changes.changes.values():
    if (bc.required_after.str in api_versions()
        # don't document breaking changes that are unconditionally disabled in
        # this Simics version
        or bc.required_after > breaking_changes.apis[default_api_version()]):
        by_version.setdefault(bc.required_after, []).append(bc)

with open(outfile, 'w') as f:
    f.write(Path(header).read_text())
    for (ver, bcs) in sorted(by_version.items()):
        f.write(fr"""
### Changes for migrating from -\-simics-api={ver.str}
These changes are enabled automatically when compiling using
Simics API {ver.ordinal + 1} or newer. With older Simics API versions, the
changes can be enabled individually by passing
<code>-\-breaking-change=<em>TAG</em></code>
to the `dmlc` compiler.
""")
        f.write("<dl>\n")
        for bc in sorted(bcs, key=lambda f: f.tag()):
            assert bc.__doc__
            f.write(f"  <dt>{bc.tag()}</dt>\n")
            doc = '\n'.join(line[4:] if line.startswith('    ') else line
                            for line in bc.__doc__.strip().splitlines())
            f.write(f"  <dd>\n\n{doc}\n</dd>\n")
        f.write("</dl>\n")
