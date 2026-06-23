#!/bin/bash
# © 2021-2023 Intel Corporation
# SPDX-License-Identifier: MPL-2.0

# Install Simics Base
PACKAGE_REPO=https://af02p-or.devtools.intel.com/artifactory/simics-repos/pub/simics-6/linux64/
INSTALL_DIR=install
ispm install 1000-6.latest -y --install-dir $INSTALL_DIR --package-repo $PACKAGE_REPO
if [ $? -ne 0 ]; then
  echo "*** Failed to install Simics Base, see build-log for details"
  exit 1
fi

# Create project
DMLC_PROJ=proj
mkdir -p $DMLC_PROJ
./$INSTALL_DIR/*/bin/project-setup $DMLC_PROJ
pushd $DMLC_PROJ

# Setup link to DMLC sources in repo
pushd modules/
ln -s ../.. dmlc
popd

# Local build of dmlc
CPUS=$(cat /proc/cpuinfo | grep ^processor | wc -l)
make dmlc -j $CPUS || exit $?

# Run the tests
T126_JOBS=$CPUS DMLC_CC=/usr/itm/gcc/13.1.0/bin/gcc ./bin/test-runner --moduledirs modules/dmlc --suite modules/dmlc/test || exit $?
