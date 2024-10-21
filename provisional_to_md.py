# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path
[path_to_dml, header, outfile, dml_version] = sys.argv[1:]
sys.path.append(path_to_dml)
dml12_only = {'1.2': True, '1.4': False}[dml_version]

from dml import provisional
from dml.env import api_versions, default_api_version

with open(outfile, 'w') as f:
    f.write(Path(header).read_text())
    features = [feature for feature in provisional.features.values()
                if not dml12_only or feature.dml12]
    stable = [feature for feature in features
              if feature.stable]
    unstable = [feature for feature in features
                if not feature.stable]
    for (heading, features) in [
            ('List of stable provisional features', stable),
            ('List of unstable provisional features', unstable)]:
        if not features:
            continue
        f.write(f"""
## {heading}
""")
        f.write("<dl>\n")
        for feature in sorted(features, key=lambda f: f.tag()):
            assert feature.__doc__
            f.write(f"  <dt><tt>{feature.tag()}</tt></dt>\n")
            doc = '\n'.join(line[4:] if line.startswith('    ') else line
                            for line in feature.__doc__.strip().splitlines())
            f.write(f"  <dd>\n\n{doc}\n</dd>\n")
        f.write("</dl>\n")
