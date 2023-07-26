#!/bin/bash
set -x -e
DEBUG=$1  # Value is '' or 'debug'
RUNNER_OS=$2  #  ${{ runner.os }} is Linux, Windiws, maxOS
export prefix=/tmp/ADALIB_DIR
export CPATH=/usr/local/include
export LIBRARY_PATH=/usr/local/lib
if [ $RUNNER_OS = Windows ]; then
   export prefix=/opt/ADALIB_DIR
   export CPATH=`cygpath -w /mingw64/include`
   export LIBRARY_PATH=`cygpath -w /mingw64/lib`
   mount `cygpath -w $RUNNER_TEMP|cut -d: -f1`:/opt /opt
fi
export PROCESSORS=0
export GPR_PROJECT_PATH=$prefix/share/gpr
export DYLD_LIBRARY_PATH=/usr/local/lib
export PATH=`ls -d $PWD/cached_gnat/*/bin |tr '\n' ':'`$PATH
echo PATH=$PATH

pip install -r langkit/requirements-github.txt
pip install jsonschema
pip install langkit/
eval `langkit/manage.py setenv`
alr --non-interactive get xmlada

build_archive()
{
  LIBRARY_TYPE=$1
  rm -rf $prefix
  cd xmlada*
  ./configure --prefix=$prefix ${DEBUG:+--enable-build=Debug}
  make $LIBRARY_TYPE install-$LIBRARY_TYPE \
    GPRBUILD_OPTIONS="-cargs -g1 -gnatwn -gargs"
  cd ..

  make -C gprbuild prefix=$prefix BUILD=${DEBUG:-production} \
    GPRBUILD_OPTIONS="-cargs:C -Dst_mtim=st_mtimespec -gargs" \
    libgpr.build.$2 libgpr.install.$2

  BUILD=`echo $DEBUG| tr [a-z] [A-Z]`  # Convert to uppercase

  make -C gnatcoll-core prefix=$prefix BUILD=${BUILD:-PROD} LIBRARY_TYPES=$LIBRARY_TYPE \
    GPRBUILD_OPTIONS="-cargs -g1 -gargs" build install
  ADAFLAGS=-g1 CFLAGS=-g1 python gnatcoll-bindings/iconv/setup.py build ${DEBUG:+--debug} -j0 --prefix=$prefix --library-types=$LIBRARY_TYPE
  python gnatcoll-bindings/iconv/setup.py install
  ADAFLAGS=-g1 CFLAGS=-g1 python gnatcoll-bindings/gmp/setup.py build ${DEBUG:+--debug} -j0 --prefix=$prefix --library-types=$LIBRARY_TYPE
  python gnatcoll-bindings/gmp/setup.py install

  BUILD=${DEBUG:+dev}  # Convert debug to dev

  sed -i -e 's/, "-flto"//' langkit/langkit/adasat/adasat.gpr # LTO fails on GNAT from Alire
  gprbuild -p -P langkit/langkit/adasat/adasat.gpr -XLIBRARY_TYPE=$LIBRARY_TYPE -XBUILD_MODE=${BUILD:-prod}
  gprinstall -p -P langkit/langkit/adasat/adasat.gpr -XLIBRARY_TYPE=$LIBRARY_TYPE -XBUILD_MODE=${BUILD:-prod} --prefix=$prefix

  langkit/manage.py build-langkit-support --library-types=$LIBRARY_TYPE --build-mode ${BUILD:-prod}
  langkit/manage.py install-langkit-support $prefix --library-types=$LIBRARY_TYPE --build-mode ${BUILD:-prod}

  make -C gpr2 setup prefix=$prefix GPR2_BUILD=${DEBUG:-release} \
    GPRBUILD_OPTIONS="-cargs -g1 -gargs" GPR2KBDIR=./gprconfig_kb/db \
    build-lib-$LIBRARY_TYPE install-lib-$LIBRARY_TYPE
  # Build libadalang static library
  ./manage.py generate

  GPR_PROJECT_PATH=$prefix/share/gpr \
  ./manage.py build --disable-all-mains --library-types=$LIBRARY_TYPE --build-mode ${BUILD:-prod} --disable-java

  ./manage.py install --disable-all-mains --library-types=$LIBRARY_TYPE --build-mode ${BUILD:-prod} $prefix
  tar czf libadalang-$RUNNER_OS-`basename $GITHUB_REF`${DEBUG:+-dbg}-$LIBRARY_TYPE.tar.gz -C $prefix .
}

# Disable SAL for static libadalang library
export STANDALONE=no

build_archive "static" "static"
