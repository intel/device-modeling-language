# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os, sys
import unittest

if __name__ == '__main__':
    (hostdir, testscript) = sys.argv[1:]
    path = os.path.join(hostdir, "bin", "dml", "python")
    if not os.path.isdir(path):
        optpar.error('not a directory: %r' % path)
    sys.path.append(path)
    if not os.path.isfile(testscript):
        optpar.error('not a file: %r' % testscript)
    sys.path.append(os.path.dirname(testscript))
    base, ext = os.path.splitext(os.path.basename(testscript))
    if ext != '.py':
        optpar.error('file name does end with .py: %r' % testscript)
    unittest.main(module = base, argv = [""])
