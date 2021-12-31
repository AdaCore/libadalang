#!/bin/bash
set -x -e
DEBUG=$1  # Value is '' or 'debug'
RUNNER_OS=$2  #  ${{ runner.os }} is Linux, Windiws, maxOS
export prefix=/tmp/ADALIB_DIR
if [ $RUNNER_OS = Windows ]; then
   export prefix=/opt/ADALIB_DIR
   mount `cmd /c cd | cut -d\: -f1`:/opt /opt
fi
export PROCESSORS=0
export GPR_PROJECT_PATH=$prefix/share/gpr
export CPATH=/usr/local/include:/mingw64/include
export LIBRARY_PATH=/usr/local/lib:/mingw64/lib
export DYLD_LIBRARY_PATH=/usr/local/lib
export PATH=`ls -d $PWD/cached_gnat/*/bin |tr '\n' ':'`$PATH
echo PATH=$PATH

LIBRARY_TYPES="static"

pip install -r langkit/REQUIREMENTS.dev
pip install jsonschema
pip install langkit/

export MACOSX_DEPLOYMENT_TARGET=10.11

alr --non-interactive get xmlada
cd xmlada*
./configure --prefix=$prefix ${DEBUG:+--enable-build=Debug}
make all install
cd ..

make -C gprbuild_ prefix=$prefix BUILD=${DEBUG:-production} GPRBUILD_OPTIONS="-cargs:C -Dst_mtim=st_mtimespec -gargs" libgpr.build.{shared,static} libgpr.install.{shared,static}
BUILD=`echo $DEBUG| tr [a-z] [A-Z]`  # Convert to uppercase

# On MacOS ld links unneeded process-wrappers.o into ALS executable and
# includes ___darwin_check_fd_set_overflow symbols, that prevents execution
# on older Mac OS X. See #875 in als repo.
rm -rfv gnatcoll-core/src/os/unix/process-wrappers.c

make -C gnatcoll-core prefix=$prefix BUILD=${BUILD:-PROD} LIBRARY_TYPES="${LIBRARY_TYPES/,/ }" build install
python gnatcoll-bindings/iconv/setup.py build ${DEBUG:+--debug} -j0 --prefix=$prefix --library-types=$LIBRARY_TYPES
python gnatcoll-bindings/iconv/setup.py install
python gnatcoll-bindings/gmp/setup.py build ${DEBUG:+--debug} -j0 --prefix=$prefix --library-types=$LIBRARY_TYPES
python gnatcoll-bindings/gmp/setup.py install

# Disable SAL for langkit library
sed -i -e '/for Library_Interface use/,/;/d' langkit/support/langkit_support.gpr
eval `langkit/manage.py setenv`
langkit/manage.py build-langkit-support --library-types $LIBRARY_TYPES
langkit/manage.py install-langkit-support $prefix --library-types $LIBRARY_TYPES

BUILD=${DEBUG:+dev}  # Convert debug to dev

# Build libadalang static library
./manage.py generate
# Disable SAL for libadalang library
sed -i -e '/for Interfaces use/,/;/d' ./build/libadalang.gpr
./manage.py build --library-types=static --build-mode ${BUILD:-prod}
./manage.py install --library-types=static --build-mode ${BUILD:-prod} $prefix
gprinstall --uninstall --prefix=$prefix mains.gpr
tar czf libadalang-$RUNNER_OS-`basename $GITHUB_REF`${DEBUG:+-dbg}-static.tar.gz -C $prefix .
