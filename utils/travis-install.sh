#! /usr/bin/env sh

set -v
set -e

# Get GNAT
if ! test -f $GNAT_TAR_PATH
then
    mkdir -p $TOOLS_DIR
    wget -O $GNAT_TAR_PATH \
        "http://mirrors.cdn.adacore.com/art/5739cefdc7a447658e0b016b"
fi

# Get QUEX
if ! test -f $QUEX_ZIP_PATH
then
    wget -O $QUEX_ZIP_PATH \
        "https://downloads.sourceforge.net/project/quex/HISTORY/0.65/quex-0.65.4.zip?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fquex%2Ffiles%2FHISTORY%2F0.65%2F&ts=1484909333&use_mirror=heanet"
fi

# Get GNATCOLL
if ! test -f $GNATCOLL_TAR_PATH
then
    wget -O $GNATCOLL_TAR_PATH \
        "http://mirrors.cdn.adacore.com/art/5739942ac7a447658d00e1e7"
fi

# Extract QUEX, GNAT and GNATCOLL
#tar -xf $GNAT_TAR_PATH -C $TOOLS_DIR
#unzip -o $QUEX_ZIP_PATH -d $TOOLS_DIR
# tar xf $GNATCOLL_TAR_PATH -C $TOOLS_DIR

# Get Langkit
if ! test -d langkit
then
    git clone https://github.com/AdaCore/langkit
fi
(cd langkit && git pull --rebase origin master)

# Install requirements
pip install -r langkit/REQUIREMENTS.dev

# Log content
ls $HOME/build_tools
ls $GNAT_PATH
ls $GNAT_PATH/bin
ls $QUEX_PATH

# Build GNATCOLL
pwd
export PATH=$GNAT_PATH/bin:$PATH
cd $GNATCOLL_PATH
which gcc
./configure --prefix="$GNAT_PATH" --enable-build=Debug --enable-shared --enable-projects
make PROCESSORS=0
make install
cd -
