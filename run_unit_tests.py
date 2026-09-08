# © 2021 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

import os, sys
import unittest

if __name__ == '__main__':
    (hostdir, testscript) = sys.argv[1:]
    path = os.path.join(hostdir, "bin", "dml", "python")
    if not os.path.isdir(path):
        sys.exit('error: not a directory: %r' % path)
    sys.path.append(path)
    if not os.path.isfile(testscript):
        sys.exit('error: not a file: %r' % testscript)
    sys.path.append(os.path.dirname(testscript))
    base, ext = os.path.splitext(os.path.basename(testscript))
    if ext != '.py':
        sys.exit('error: file name does end with .py: %r' % testscript)
    unittest.main(module = base, argv = [""])
