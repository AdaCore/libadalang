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
if ! [ -f $GNAT_INSTALLER ]
then
    wget -O $GNAT_INSTALLER \
        http://mirrors.cdn.adacore.com/art/5b0d7bffa3f5d709751e3e04
    sh gnat_community_install_script/install_package.sh \
        "$GNAT_INSTALLER" "$INSTALL_DIR"
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

# TODO: the following is a hack to workaround the fact that 1) in Ubuntu,
# libiconv is integrated in the libc, so it's not present as a standalone
# library, 2) the build of gnatcoll-iconv passes -liconv to the linker.
LIBICONV_DIR=$TOOLS_DIR/libiconv-hack
rm -rf $LIBICONV_DIR
mkdir $LIBICONV_DIR
(
    cd $LIBICONV_DIR
    touch libiconv.c
    gcc -shared -o libiconv.so libiconv.c
)
if [ -z "$LIBRARY_PATH" ]
then
    export LIBRARY_PATH=$LIBICONV_DIR
else
    export LIBRARY_PATH="$LIBICONV_DIR:$LIBRARY_PATH"
fi

# Log content
pwd
export PATH=$INSTALL_DIR/bin:$PATH
which gcc
gcc -v

# Build gnatcoll-core
(
    cd $TOOLS_DIR/gnatcoll-core
    make build PROCESSORS=0
    make install prefix="$INSTALL_DIR"
)

# Build gnatcoll-bindings
(
    cd $TOOLS_DIR/gnatcoll-bindings
    for component in iconv gmp
    do
        (
            cd $component
            python setup.py build -j0 --prefix="$INSTALL_DIR"
            python setup.py install --prefix="$INSTALL_DIR"
        )
    done
)
