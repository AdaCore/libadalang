#! /usr/bin/env sh

# This script is used both as a travis install script, and as a helper to get a
# quick user/developper Libadalang setup.

set -v
set -e

if ! [ -d $TOOLS_DIR ]; then
    mkdir -p $TOOLS_DIR
fi
if [ -f $INSTALL_DIR/bin/*gcc-8.3.1 ]; then
    rm -rf $INSTALL_DIR # Delete GNAT Community 2019
fi
if ! [ -d $INSTALL_DIR ]; then
    mkdir -p $INSTALL_DIR
fi

# Get and install GNAT
if ! [ -d gnat_community_install_script ]; then
    git clone https://github.com/AdaCore/gnat_community_install_script.git
else
    (cd gnat_community_install_script && git pull)
fi
if ! [ -f $INSTALL_DIR/bin/gcc ]; then
    if [ $TRAVIS_OS_NAME = linux ]; then
        GNAT_INSTALLER=$TOOLS_DIR/gnat-2020-20200429-x86_64-linux-bin
        GNAT_INSTALLER_URL="https://community.download.adacore.com/v1/4d99b7b2f212c8efdab2ba8ede474bb9fa15888d?filename=gnat-2020-20200429-x86_64-linux-bin"
    else
        GNAT_INSTALLER=$TOOLS_DIR/gnat-2020-20200429-x86_64-darwin-bin.dmg
        GNAT_INSTALLER_URL="https://community.download.adacore.com/v1/4470dd195aec672d7c2f2a2bac3dcf6e59bbb26c?filename=gnat-2020-20200429-x86_64-darwin-bin.dmg"
    fi

    wget -O $GNAT_INSTALLER $GNAT_INSTALLER_URL
    sh gnat_community_install_script/install_package.sh \
        "$GNAT_INSTALLER" "$INSTALL_DIR"
    $INSTALL_DIR/bin/gprinstall --uninstall gnatcoll
fi

# Get gprbuild (to build libgpr)
if [ -d "$TOOLS_DIR/gprbuild" ]; then
    (cd $TOOLS_DIR/gprbuild && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gprbuild)
fi

# Get gnatcoll-core and gnatcoll-bindings
if [ -d "$TOOLS_DIR/gnatcoll-core" ]; then
    (cd $TOOLS_DIR/gnatcoll-core && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gnatcoll-core)
fi
if [ -d "$TOOLS_DIR/gnatcoll-bindings" ]; then
    (cd $TOOLS_DIR/gnatcoll-bindings && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gnatcoll-bindings)
fi

# Get Langkit, in particular the branch that corresponds to the Libadalang
# commit being tested.
if ! [ -d langkit ]; then
    git clone https://github.com/AdaCore/langkit
fi
(
    cd langkit
    python3 ../utils/travis-langkit-branch.py
)

# Install requirements
pip install -r langkit/REQUIREMENTS.dev
pip install jsonschema
(cd langkit && python setup.py install)

# Log content
pwd
export PATH=$INSTALL_DIR/bin:$PATH
export GPR_PROJECT_PATH=$ADALIB_DIR/share/gpr
which gcc
gcc -v

# Build libgpr
(
    cd $TOOLS_DIR/gprbuild
    make BUILD=production prefix="$ADALIB_DIR" libgpr.build.shared
    make BUILD=production prefix="$ADALIB_DIR" libgpr.install.shared
)

# Build gnatcoll-core
(
    cd $TOOLS_DIR/gnatcoll-core
    make PROCESSORS=0 prefix="$ADALIB_DIR" LIBRARY_TYPES=relocatable \
       build install
)

# Build gnatcoll-bindings
(
    cd $TOOLS_DIR/gnatcoll-bindings
    #  Darwin has gmp installed at /usr/local/
    export CPATH=/usr/local/include
    for component in iconv gmp
    do
        (
            cd $component
            python setup.py build --reconfigure -j0 --prefix="$ADALIB_DIR" \
               --library-types=relocatable
            python setup.py install
        )
    done
)

# Make sure libpythonlang is installed for platforms that will
# run the testsuite, as its a test dependency.
if [ "$TRAVIS_OS_NAME" != osx ]; then
    langkit/scripts/manage.sh make
fi
