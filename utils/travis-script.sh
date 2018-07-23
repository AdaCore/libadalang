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
if [ -z "$LD_LIBRARY_PATH" ]
then
    export LD_LIBRARY_PATH=$LIBICONV_DIR
else
    export LD_LIBRARY_PATH="$LIBICONV_DIR:$LD_LIBRARY_PATH"
fi

# Log the toolchain to use
which gcc
which gprbuild
gcc -v
gprbuild -v

# Log which Langkit commit is used
(cd langkit && git log HEAD^..HEAD | cat)

# Avoid pretty-printing, for now: gnatpp from GNAT Community 2018 is known not
# to work on Libadalang.
ada/manage.py generate -P

# Build the Quex-generated lexer alone first, as it takes a huge amount of
# memory. Only then build the rest in parallel.
gprbuild -p -j8 -Pbuild/lib/gnat/libadalang.gpr \
    -XBUILD_MODE=dev -XLIBRARY_TYPE=relocatable -XXMLADA_BUILD=relocatable \
    -XLIBADALANG_WARNINGS=true -c -u quex_lexer.c
ada/manage.py build

# Finally, run the testsuite
ada/manage.py test
