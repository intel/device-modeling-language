# © 2024 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path

# Validate and unpack command-line arguments
if len(sys.argv) != 4:
    print("Usage: script.py <path_to_dml> <header> <outfile>")
    sys.exit(1)

path_to_dml, header, outfile = sys.argv[1:]
sys.path.append(path_to_dml)

# Import necessary modules from the dml package
from dml import compat
from dml.env import api_versions, default_api_version

# Group features by their last API version
by_version = {}
for feature in compat.features.values():
    last_version_str = feature.last_api_version.str
    last_version = feature.last_api_version
    if (last_version_str in api_versions() or 
        last_version > compat.apis[default_api_version()]):
        by_version.setdefault(last_version, []).append(feature)

# Write the output file
try:
    with open(outfile, 'w') as f:
        # Write the header content
        f.write(Path(header).read_text())
        
        # Write the features grouped by API version
        for ver, features in sorted(by_version.items()):
            f.write(f"\n### Features available up to --simics-api={ver.str}\n")
            f.write("<dl>\n")
            for feature in sorted(features, key=lambda f: f.tag()):
                assert feature.__doc__, f"Feature {feature.tag()} is missing documentation"
                
                f.write(f"  <dt>{feature.tag()}</dt>\n")
                
                # Process the feature documentation
                doc = '\n'.join(
                    line[4:] if line.startswith('    ') else line
                    for line in feature.__doc__.strip().splitlines()
                )
                f.write(f"  <dd>\n\n{doc}\n</dd>\n")
            f.write("</dl>\n")
except Exception as e:
    print(f"Error writing to {outfile}: {e}")
    sys.exit(1)
