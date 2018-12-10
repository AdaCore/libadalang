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

export QUEX_URL="https://downloads.sourceforge.net/project/quex/HISTORY/0.65/\
quex-0.65.4.zip"

function do_install()
{
    cd $BUILD_FOLDER

    # Install libiconv and gmp
    pacman -S --noconfirm mingw-w64-x86_64-libiconv mingw-w64-x86_64-gmp

    # Get Quex
    curl --retry 5 -L -o quex-0.65.4.zip "$QUEX_URL"
    7z x quex-0.65.4.zip -obuild_tools

    # Get and build gnatcoll-core and gnatcoll-bindings
    git clone -q https://github.com/AdaCore/gnatcoll-core
    make build install prefix=$ADALIB_DIR -C gnatcoll-core
    git clone -q https://github.com/AdaCore/gnatcoll-bindings
    for component in iconv gmp ; do
      python gnatcoll-bindings/$component/setup.py build --reconfigure -j0 \
          --prefix="$ADALIB_DIR"
      python gnatcoll-bindings/$component/setup.py install
    done

    # Get Langkit
    git clone -q -b stable https://github.com/AdaCore/langkit

    # Finally install all Python dependencies for Langkit
    python -m pip install -r langkit/REQUIREMENTS.dev
}

function do_build()
{
    cd $BUILD_FOLDER
    export PYTHONPATH=$APPVEYOR_BUILD_FOLDER\\langkit

    # Generate Libadalang. Avoid pretty-printing: gnatpp for GNAT CE 2018 is
    # known not to work on Libadalang and pretty-printing is useless to check
    # pull requests anyway.
    python ada/manage.py generate -P

    # Build the Quex-generated lexer alone first, as it takes a huge amount of
    # memory. Only then build the rest in parallel.
    gprbuild -Pbuild/lib/gnat/libadalang.gpr \
        -XBUILD_MODE=dev -XLIBRARY_TYPE=relocatable \
        -XXMLADA_BUILD=relocatable -XLIBADALANG_WARNINGS=true \
        -p -c -u libadalang_lexer.c

    # Restrict parallelism to avoid OOM issues
    python ada/manage.py build -j12

    # TODO: on Windows, it's better to use GNATpython in order to run the
    # testsuite, as only GNATpython handled properly Windows-style line
    # terminators.
    #
    # python ada/manage.py test

    # Install Libadalang so we can create a binary distribution
    python ada/manage.py install "$ADALIB_DIR"
    cd "$ADALIB_DIR"
    7z a $BUILD_FOLDER/libadalang-${APPVEYOR_REPO_BRANCH}-windows.zip .
}

do_$1
