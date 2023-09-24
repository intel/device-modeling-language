# © 2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path
[path_to_dml, header, outfile] = sys.argv[1:]
sys.path.append(path_to_dml)

from dml.compat import features
from dml.env import api_versions

api_map = {v: k for (k, v) in api_versions().items()}
with open(outfile, 'w') as f:
    f.write(Path(header).read_text())
    for (ver, deps) in features.items():
        if deps and ver in api_map:
            f.write(
                f"### Features available up to --simics-api={api_map[ver]}\n")
            f.write("<dl>\n")
            for d in deps.values():
                assert d.__doc__
                f.write(f"  <dt>{d.tag()}</dt>\n")
                doc = '\n'.join(line[4:] if line.startswith('    ') else line
                                for line in d.__doc__.strip().splitlines())
                f.write(f"  <dd>\n\n{doc}\n</dd>\n")
            f.write("</dl>\n")
