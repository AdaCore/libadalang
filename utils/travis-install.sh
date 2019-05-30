#! /usr/bin/env sh

set -v
set -e

if ! [ -d $TOOLS_DIR ]
then
    mkdir -p $TOOLS_DIR
fi
if ! [ -d $INSTALL_DIR ]
then
    mkdir -p $INSTALL_DIR
fi

# Get and install GNAT
if ! [ -d gnat_community_install_script ]
then
    git clone https://github.com/AdaCore/gnat_community_install_script.git
else
    (cd gnat_community_install_script && git pull)
fi
if ! [ -f $INSTALL_DIR/bin/gcc ]
then
    if [ $TRAVIS_OS_NAME = linux ]; then
        GNAT_INSTALLER=$TOOLS_DIR/gnat-community-2019-20190517-x86_64-linux-bin
        GNAT_INSTALLER_URL=http://mirrors.cdn.adacore.com/art/5cdffc5409dcd015aaf82626
    else
        GNAT_INSTALLER=$TOOLS_DIR/gnat-community-2019-20190517-x86_64-darwin-bin.dmg
        GNAT_INSTALLER_URL=http://mirrors.cdn.adacore.com/art/5ce0322c31e87a8f1d4253fa
    fi

    wget -O $GNAT_INSTALLER $GNAT_INSTALLER_URL
    sh gnat_community_install_script/install_package.sh \
        "$GNAT_INSTALLER" "$INSTALL_DIR"
    $INSTALL_DIR/bin/gprinstall --uninstall gnatcoll
fi

# Get gprbuild (to build libgpr)
if [ -d "$TOOLS_DIR/gprbuild" ]
then
    (cd $TOOLS_DIR/gprbuild && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gprbuild)
fi

# Get gnatcoll-core and gnatcoll-bindings
if [ -d "$TOOLS_DIR/gnatcoll-core" ]
then
    (cd $TOOLS_DIR/gnatcoll-core && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gnatcoll-core)
fi
if [ -d "$TOOLS_DIR/gnatcoll-bindings" ]
then
    (cd $TOOLS_DIR/gnatcoll-bindings && git pull)
else
    (cd $TOOLS_DIR && git clone https://github.com/AdaCore/gnatcoll-bindings)
fi

# Get Langkit, in particular the branch that corresponds to the Libadalang
# commit being tested.
if ! [ -d langkit ]
then
    git clone https://github.com/AdaCore/langkit
fi
(
    cd langkit
    python2 ../utils/travis-langkit-branch.py
)

# Install requirements
pip install -r langkit/REQUIREMENTS.dev

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
