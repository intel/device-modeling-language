# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path
[path_to_dml, header, outfile] = sys.argv[1:]
sys.path.append(path_to_dml)

from dml import compat
from dml.env import api_versions, default_api_version

by_version = {}
for feature in compat.features.values():
    if (feature.last_api_version.str in api_versions()
        # don't document features that are unconditionally disabled in
        # this Simics version
        or feature.last_api_version > compat.apis[default_api_version()]):
        by_version.setdefault(feature.last_api_version, []).append(feature)

with open(outfile, 'w') as f:
    f.write(Path(header).read_text())
    for (ver, features) in sorted(by_version.items()):
        f.write(f"""
### Features available up to and including --simics-api={ver.str}
These features correspond to functionality removed when compiling using
Simics API {ver.ordinal + 1} or newer. With older Simics API versions, these
features can be disabled individually by passing <tt>--no-compat=<em>TAG</em></tt>
to the `dmlc` compiler.
""")
        f.write("<dl>\n")
        for feature in sorted(features, key=lambda f: f.tag()):
            assert feature.__doc__
            f.write(f"  <dt>{feature.tag()}</dt>\n")
            doc = '\n'.join(line[4:] if line.startswith('    ') else line
                            for line in feature.__doc__.strip().splitlines())
            f.write(f"  <dd>\n\n{doc}\n</dd>\n")
        f.write("</dl>\n")
