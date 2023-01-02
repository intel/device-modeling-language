# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os
import sys
from pathlib import Path
(src, dest) = sys.argv[1:]
text = Path(src).read_text()
if os.environ.get('DMLC_PATHSUBST'):
    text = f'#line 1 "{src}"\n{text}'
dest = Path(dest)
dest.parent.mkdir(parents=True, exist_ok=True)
dest.write_text(text)
