#! /bin/sh

set -e

RELEASE=21.0.0
SRC_PKG=/tmp/libadalang-$RELEASE.tar.gz

# Make sure the appropriate Langkit sources are checked out in
# /tmp/langkit-$RELEASE and Libadalang sources in /tmp/lal-$RELEASE
SRC_DIR=/tmp/lal-$RELEASE
cd $SRC_DIR
export PYTHONPATH=/tmp/langkit-$RELEASE:$PYTHONPATH

BUILD_DIR=/tmp/libadalang-$RELEASE
rm -rf $BUILD_DIR

# Generate the Libadalang and Mains projects
ada/manage.py --build-dir=$BUILD_DIR --no-langkit-support generate

# Reorganize the trees for libadalang.gpr
mv $BUILD_DIR/lib/gnat/libadalang.gpr $BUILD_DIR/
patch -d $BUILD_DIR -p0 -f -i $SRC_DIR/utils/libadalang.gpr.patch

mv $BUILD_DIR/include/libadalang.h $BUILD_DIR/src/
mv $BUILD_DIR/include/libadalang/*.ad* $BUILD_DIR/src/
mv $BUILD_DIR/include/libadalang/*.c $BUILD_DIR/src/
cp -a $SRC_DIR/ada/extensions/src/* $BUILD_DIR/src/

# Reorganize the trees for mains.gpr
mv $BUILD_DIR/src/mains.gpr $BUILD_DIR/
patch -d $BUILD_DIR -p0 -f -i $SRC_DIR/utils/mains.gpr.patch

mkdir $BUILD_DIR/src-mains
mv $BUILD_DIR/src/parse.adb $BUILD_DIR/src-mains/
cp -ar $SRC_DIR/ada/testsuite/ada/*.ad* $BUILD_DIR/src-mains/
cp -ar $SRC_DIR/ada/testsuite/ada/gnat_compare/*.ad* $BUILD_DIR/src-mains/

mv $BUILD_DIR/bin/lal_playground $BUILD_DIR/python
rm -r $BUILD_DIR/bin
rm -r $BUILD_DIR/include
rm -r $BUILD_DIR/lib
rm -r $BUILD_DIR/obj

# Create the release tarball
tar czf $SRC_PKG -C /tmp libadalang-$RELEASE
sha512sum $SRC_PKG
