# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
[path_to_dml, outfile] = sys.argv[1:]
sys.path.append(path_to_dml)

from dml.porting import all_portings

with open(outfile, 'w') as f:
    f.write("# Language differences handled by the port-dml script\n\n")

    f.write("<dl>\n")
    for (tag, m) in sorted(all_portings.items()):
        assert m.__doc__
        f.write(f"  <dt>{tag}</dt>\n")
        doc = '\n'.join(line[4:] if line.startswith('    ') else line
                        for line in m.__doc__.strip().splitlines())
        f.write(f"  <dd>\n\n{doc}\n</dd>\n")
    f.write("</dl>\n")
