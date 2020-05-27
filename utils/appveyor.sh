#!/bin/bash

# Stop at the first error and show all run commands
set -e
set -x

export BUILD_FOLDER=/Projects/$APPVEYOR_PROJECT_NAME
export ADALIB_DIR=$BUILD_FOLDER/adalib
export PATH=/c/Python37-x64:$ADALIB_DIR/bin:\
$BUILD_FOLDER/build/lib/libadalang.relocatable:\
$BUILD_FOLDER/build/lib/langkit_support.relocatable:\
/c/GNAT/bin:\
/mingw64/bin:\
$PATH
export GPR_PROJECT_PATH=$ADALIB_DIR/share/gpr
export LIBRARY_TYPE=relocatable
export CPATH=/mingw64/include
export LIBRARY_PATH=/mingw64/lib

function do_install()
{
    cd $BUILD_FOLDER

    # Get Langkit, in particular the branch that corresponds to the Libadalang
    # commit being tested.
    if ! [ -d langkit ]
    then
        git clone https://github.com/AdaCore/langkit
    fi
    (
        cd langkit
        TRAVIS_PULL_REQUEST_SLUG=$APPVEYOR_PULL_REQUEST_HEAD_REPO_NAME
        TRAVIS_PULL_REQUEST_BRANCH=$APPVEYOR_PULL_REQUEST_HEAD_REPO_BRANCH
        TRAVIS_REPO_SLUG=$APPVEYOR_REPO_NAME
        TRAVIS_BRANCH=$APPVEYOR_REPO_BRANCH
        export TRAVIS_PULL_REQUEST_SLUG TRAVIS_PULL_REQUEST_BRANCH \
            TRAVIS_REPO_SLUG TRAVIS_BRANCH
        python $APPVEYOR_BUILD_FOLDER\\utils\\travis-langkit-branch.py
    )

    # Install libiconv and gmp
    pacman -S --noconfirm mingw-w64-x86_64-libiconv mingw-w64-x86_64-gmp

    # Get and build libgpr, gnatcoll-core and gnatcoll-bindings. Only build
    # relocatable variants, as we don't need the static and static-pic ones.
    git clone -q https://github.com/AdaCore/gprbuild
    (
        cd gprbuild
        make BUILD=production prefix="$ADALIB_DIR" libgpr.build.shared
        make BUILD=production prefix="$ADALIB_DIR" libgpr.install.shared
    )

    git clone -q https://github.com/AdaCore/gnatcoll-core
    make prefix=$ADALIB_DIR LIBRARY_TYPES=relocatable -C gnatcoll-core \
      build install

    git clone -q https://github.com/AdaCore/gnatcoll-bindings
    for component in iconv gmp ; do
      python gnatcoll-bindings/$component/setup.py build --reconfigure -j0 \
          --prefix="$ADALIB_DIR" --library-types=relocatable
      python gnatcoll-bindings/$component/setup.py install
    done

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
    python ada/manage.py -vdebug generate -P

    # Build the generated lexer alone first, as it takes a huge amount of
    # memory. Only then build the rest in parallel.
    gprbuild -Pbuild/lib/gnat/libadalang.gpr \
        -XBUILD_MODE=dev -XLIBRARY_TYPE=relocatable \
        -XXMLADA_BUILD=relocatable -XLIBADALANG_WARNINGS=true \
        -p -c -u libadalang-lexer_state_machine.adb

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
