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
        GNAT_INSTALLER=$TOOLS_DIR/gnat-community-2018-20180528-x86_64-linux-bin
        GNAT_INSTALLER_URL=http://mirrors.cdn.adacore.com/art/5b0d7bffa3f5d709751e3e04
    else
        GNAT_INSTALLER=$TOOLS_DIR/gnat-community-2018-20180523-x86_64-darwin-bin.dmg
        GNAT_INSTALLER_URL=http://mirrors.cdn.adacore.com/art/5b071da0c7a447e573318b01
    fi

    wget -O $GNAT_INSTALLER $GNAT_INSTALLER_URL
    sh gnat_community_install_script/install_package.sh \
        "$GNAT_INSTALLER" "$INSTALL_DIR"
    $INSTALL_DIR/bin/gprinstall --uninstall gnatcoll
fi

# Get QUEX
if ! [ -d $QUEX_PATH ]
then
    wget -O $QUEX_ZIP \
        "https://downloads.sourceforge.net/project/quex/HISTORY/0.65/quex-0.65.4.zip?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fquex%2Ffiles%2FHISTORY%2F0.65%2F&ts=1484909333&use_mirror=heanet"
    unzip -o $QUEX_ZIP -d $TOOLS_DIR
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
export ADA_PROJECT_PATH=$ADALIB_DIR/share/gpr
which gcc
gcc -v

# Build gnatcoll-core
(
    cd $TOOLS_DIR/gnatcoll-core
    make build PROCESSORS=0
    make install prefix="$ADALIB_DIR"
)

# Build gnatcoll-bindings
(
    cd $TOOLS_DIR/gnatcoll-bindings
    for component in iconv gmp
    do
        (
            cd $component
            python setup.py build --reconfigure -j0 --prefix="$ADALIB_DIR"
            python setup.py install
        )
    done
)
