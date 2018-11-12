#!/bin/bash

# Terminate on any error
set -e

export BUILD_FOLDER=/Projects/$APPVEYOR_PROJECT_NAME
export ADALIB_DIR=$BUILD_FOLDER/adalib
export PATH=$ADALIB_DIR/bin:\
$BUILD_FOLDER/build/lib/libadalang.relocatable:\
$BUILD_FOLDER/build/lib/langkit_support.relocatable:\
/c/GNAT/bin:\
/mingw64/bin:\
$PATH
export ADA_PROJECT_PATH=$ADALIB_DIR/share/gpr
export QUEX_PATH=$BUILD_FOLDER/build_tools/quex-0.65.4
export LIBRARY_TYPE=relocatable
export CPATH=/mingw64/include
export LIBRARY_PATH=/mingw64/lib

function do_install()
{
  cd $BUILD_FOLDER
  pacman -S --noconfirm mingw-w64-x86_64-libiconv mingw-w64-x86_64-gmp
  curl --retry 5 -L -o quex-0.65.4.zip https://downloads.sourceforge.net/project/quex/HISTORY/0.65/quex-0.65.4.zip
  7z x quex-0.65.4.zip -obuild_tools
  git clone -q https://github.com/AdaCore/gnatcoll-core
  make build install prefix=$ADALIB_DIR -C gnatcoll-core
  git clone -q https://github.com/AdaCore/gnatcoll-bindings
  for component in iconv gmp ; do
    python gnatcoll-bindings/$component/setup.py build --reconfigure -j0 --prefix="$ADALIB_DIR"
    python gnatcoll-bindings/$component/setup.py install
  done
  git clone -q -b stable https://github.com/AdaCore/langkit
  python -m pip install -r langkit/REQUIREMENTS.dev
}

function do_build()
{
  cd $BUILD_FOLDER
  export PYTHONPATH=$APPVEYOR_BUILD_FOLDER\\langkit
  python ada/manage.py generate -P
  gprbuild -p -Pbuild/lib/gnat/libadalang.gpr \
    -XBUILD_MODE=dev -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable \
    -XLIBADALANG_WARNINGS=true -c -u libadalang_lexer.c
  python ada/manage.py build -j12
  # PM said we need gnatpython to run tests
  # python ada/manage.py test
  python ada/manage.py install "$ADALIB_DIR"
  cd "$ADALIB_DIR"
  7z a $BUILD_FOLDER/libadalang-${APPVEYOR_REPO_BRANCH}-windows.zip .
}

do_$1
