#! /bin/sh

set -e

RELEASE=23.0.0
SRC_PKG=/tmp/libadalang-$RELEASE.tar.gz

# Make sure the appropriate Langkit sources are checked out in
# /tmp/langkit-$RELEASE and Libadalang sources in /tmp/lal-$RELEASE
SRC_DIR=/tmp/lal-$RELEASE
cd $SRC_DIR
export PYTHONPATH=/tmp/langkit-$RELEASE:$PYTHONPATH

BUILD_DIR=/tmp/libadalang-$RELEASE
rm -rf $BUILD_DIR

# Generate the Libadalang and Mains projects
./manage.py generate --build-dir=$BUILD_DIR

# Remove the automatically created object directory
rm -rf $BUILD_DIR/obj

# Reorganize the trees for libadalang.gpr
patch -d $BUILD_DIR -p0 -f -i $SRC_DIR/utils/libadalang.gpr.patch

mv $BUILD_DIR/libadalang.h $BUILD_DIR/src/
cp -a $SRC_DIR/extensions/src/* $BUILD_DIR/src/

# Reorganize the trees for mains.gpr
patch -d $BUILD_DIR -p0 -f -i $SRC_DIR/utils/mains.gpr.patch

cp -ar $SRC_DIR/testsuite/ada/*.ad* $BUILD_DIR/src-mains/
cp -ar $SRC_DIR/testsuite/ada/gnat_compare/*.ad* $BUILD_DIR/src-mains/

# Create the release tarball
tar czf $SRC_PKG -C /tmp libadalang-$RELEASE
sha512sum $SRC_PKG
