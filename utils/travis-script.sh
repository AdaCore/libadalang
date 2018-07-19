#! /usr/bin/env sh

set -v
set -e

export PATH=$INSTALL_DIR/bin:$PATH

# See the corresponding code in travis-install.sh
LIBICONV_DIR=$TOOLS_DIR/libiconv-hack
if [ -z "$LIBRARY_PATH" ]
then
    export LIBRARY_PATH=$LIBICONV_DIR
else
    export LIBRARY_PATH="$LIBICONV_DIR:$LIBRARY_PATH"
fi

# Log the toolchain to use
which gcc
which gprbuild
gcc -v
gprbuild -v

# Log which Langkit commit is used
(cd langkit && git log HEAD^..HEAD | cat)

ada/manage.py make -j16
ada/manage.py test
