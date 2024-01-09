# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import sys
from pathlib import Path

def create_dmlasts(dmlc_path, dmlast_path, dml_path, depfile):
    sys.path.append(str(dmlc_path))
    from dml.toplevel import produce_dmlast
    from dml.logging import ignore_warning

    ignore_warning('WEXPERIMENTAL')
    dml_files_abs = list(dml_path.rglob('*.dml'))
    dml_files = [p.relative_to(dml_path) for p in dml_files_abs]
    assert dml_files
    if dmlast_path != dml_path:
        dirs = {f.parent for f in dml_files}
        for d in dirs:
            (dmlast_path / d).mkdir(parents=True, exist_ok=True)
        for f in dml_files:
            (dmlast_path / f).write_bytes((dml_path / f).read_bytes())
    for f in dml_files:
        produce_dmlast(dmlast_path / f)
    depfile.parent.mkdir(parents=True, exist_ok=True)
    dml_file_list = ' '.join(map(str, dml_files_abs))
    depfile.write_text(f'''
{depfile} : {dml_file_list}
{dml_file_list} :
''')

if __name__ == '__main__':
    [dmlc_path, dmlast_path, dml_path, depfile] = map(Path, sys.argv[1:])
    create_dmlasts(dmlc_path.resolve(), dmlast_path.resolve(),
                   dml_path.resolve(), depfile)

