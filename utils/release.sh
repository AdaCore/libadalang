#! /bin/sh

set -e

RELEASE=26.0.0
RELEASE_ARCHIVE=/tmp/libadalang-$RELEASE.tar.gz

# Before running this script, make sure the appropriate sources are checked
# out:
# * Langkit    in /tmp/langkit-$RELEASE
# * AdaSAT     in /tmp/adasat-$RELEASE
# * Libadalang in /tmp/lal-$RELEASE
LANGKIT_SRC=/tmp/langkit-$RELEASE
ADASAT_SRC=/tmp/adasat-$RELEASE
LAL_SRC=/tmp/lal-$RELEASE

cd $LAL_SRC
# Build Langkit and add it to the environment
export PYTHONPATH=$LANGKIT_SRC:$PYTHONPATH
export GPR_PROJECT_PATH=$ADASAT_SRC
(cd $LANGKIT_SRC && ./manage.py make)
eval $(cd $LANGKIT_SRC && ./manage.py printenv)

# Generate the Libadalang and Mains projects
LAL_BUILD_DIR=/tmp/libadalang-$RELEASE
rm -rf $LAL_BUILD_DIR
python -m langkit.scripts.lkm generate \
    --build-dir=$LAL_BUILD_DIR \
    --portable-project

# Remove the automatically created object directory
rm -rf $LAL_BUILD_DIR/obj

# Reorganize the trees for libadalang.gpr
sed -ie \
    's/Primary_Source_Dirs := ("src".*/Primary_Source_Dirs := ("src");/' \
    $LAL_BUILD_DIR/libadalang.gpr
cp -a $LAL_SRC/extensions/src/* $LAL_BUILD_DIR/src/

# Reorganize the trees for mains.gpr
sed -ie 's/".*"src-mains"/"src-mains"/' $LAL_BUILD_DIR/mains.gpr
cp -ar $LAL_SRC/testsuite/ada/*.ad* $LAL_BUILD_DIR/src-mains/
cp -ar $LAL_SRC/testsuite/ada/gnat_compare/*.ad* $LAL_BUILD_DIR/src-mains/

# Create the release tarball
tar czf $RELEASE_ARCHIVE -C /tmp libadalang-$RELEASE
sha512sum $RELEASE_ARCHIVE
