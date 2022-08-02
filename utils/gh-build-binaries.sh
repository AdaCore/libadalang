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

pip install -r langkit/REQUIREMENTS.dev
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
	  GPRBUILD_OPTIONS="-cargs -gnatwn -gargs"
  cd ..

  make -C gprbuild prefix=$prefix BUILD=${DEBUG:-production} \
    GPRBUILD_OPTIONS="-cargs:C -Dst_mtim=st_mtimespec -gargs" \
    libgpr.build.$2 libgpr.install.$2

  BUILD=`echo $DEBUG| tr [a-z] [A-Z]`  # Convert to uppercase

  make -C gnatcoll-core prefix=$prefix BUILD=${BUILD:-PROD} LIBRARY_TYPES=$LIBRARY_TYPE build install
  python gnatcoll-bindings/iconv/setup.py build ${DEBUG:+--debug} -j0 --prefix=$prefix --library-types=$LIBRARY_TYPE
  python gnatcoll-bindings/iconv/setup.py install
  python gnatcoll-bindings/gmp/setup.py build ${DEBUG:+--debug} -j0 --prefix=$prefix --library-types=$LIBRARY_TYPE
  python gnatcoll-bindings/gmp/setup.py install

  langkit/manage.py build-langkit-support --library-types=$LIBRARY_TYPE
  langkit/manage.py install-langkit-support $prefix --library-types=$LIBRARY_TYPE

  BUILD=${DEBUG:+dev}  # Convert debug to dev

  # Build libadalang static library
  ./manage.py generate

  if [ $LIBRARY_TYPE = "static" ]; then
    # Disable SAL for static libadalang library
    sed -i -e '/for .*Interface.* use/,/;/d' ./build/libadalang.gpr
  fi

  ./manage.py build --library-types=$LIBRARY_TYPE --build-mode ${BUILD:-prod}
  ./manage.py install --library-types=$LIBRARY_TYPE --build-mode ${BUILD:-prod} $prefix
  gprinstall --uninstall --prefix=$prefix mains.gpr
  tar czf libadalang-$RUNNER_OS-`basename $GITHUB_REF`${DEBUG:+-dbg}-$LIBRARY_TYPE.tar.gz -C $prefix .
}

# Disable SAL for static langkit library
sed -i -e '/for .*Interface.* use/,/;/d' langkit/langkit/support/langkit_support.gpr

build_archive "static" "static"

if [ $RUNNER_OS = Linux ]; then
   git -C langkit checkout langkit/support/langkit_support.gpr
   build_archive "relocatable" "shared"
fi
