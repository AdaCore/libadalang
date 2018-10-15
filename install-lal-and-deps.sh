#! /bin/sh

set -x
set -e

# This script is a helper to get a quick user/developper Libadalang setup.
# Please see the following section for specific settings you need to do before
# running it.

##############################################################
# Change the above with your favorite installation directory #
##############################################################

INSTALL_DIR=/path/to/install

# Install the following prerequisites:
#
# * Python's virtual env;
# * The GMP library and its development package
#
# On Debian-like systems, the following command should do it:
#
#   $ sudo apt-get install python-virtualenv libgmp3-dev

# Install a GNAT Pro or GNAT Community release for your platform in
# $INSTALL_DIR.  Here, for the example we automatically install GNAT Community
# 2018 for Linux x86_64 (from https://www.adacore.com/download).

GNAT_TARBALL=gnat-community-2018-20180528-x86_64-linux-bin
if ! [ -f "$GNAT_TARBALL" ]
then
    wget -O "$GNAT_TARBALL" \
        http://mirrors.cdn.adacore.com/art/5b0d7bffa3f5d709751e3e04
fi
if ! [ -d gnat_community_install_script ]
then
    git clone https://github.com/AdaCore/gnat_community_install_script.git
else
    (cd gnat_community_install_script && git pull)
fi
sh gnat_community_install_script/install_package.sh \
    ./"$GNAT_TARBALL" "$INSTALL_DIR"

# Now, get GNATCOLL sources (core and bindings). This should match the version
# of the compiler you installed: here, we assume that the "master" branch on
# GitHub fit with the latest GNAT Community release installed above.
if ! [ -d gnatcoll-core ]
then
    wget http://mirrors.cdn.adacore.com/art/5b0819dfc7a447df26c27a99
    git clone https://github.com/AdaCore/gnatcoll-core.git
else
    (cd gnatcoll-core && git pull)
fi
if ! [ -d gnatcoll-bindings ]
then
    git clone https://github.com/AdaCore/gnatcoll-bindings.git
else
    (cd gnatcoll-bindings && git pull)
fi

############################################
# The following should work out of the box #
############################################

export PATH="$INSTALL_DIR/bin:$PATH"
export LD_LIBRARY_PATH="$INSTALL_DIR/lib:$PATH"

# Build GNATCOLL and install it in $INSTALL_DIR
(
    cd gnatcoll-core
    make setup
    make PROCESSORS=0
    make prefix="$INSTALL_DIR" install
)
(
    cd gnatcoll-bindings/iconv
    python2 setup.py build -j0 --prefix="$INSTALL_DIR"
    python2 setup.py install --prefix="$INSTALL_DIR"
)
(
    cd gnatcoll-bindings/gmp
    python2 setup.py build -j0 --prefix="$INSTALL_DIR"
    python2 setup.py install --prefix="$INSTALL_DIR"
)

# Get Quex
if ! [ -d "quex-0.65.4" ]
then
    wget -O quex-0.65.4.zip "https://downloads.sourceforge.net/project/quex/HISTORY/0.65/quex-0.65.4.zip?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fquex%2Ffiles%2FHISTORY%2F0.65%2F&ts=1484909333&use_mirror=heanet"
    unzip quex-0.65.4.zip
fi
export QUEX_PATH="$PWD/quex-0.65.4"

# Now prepare the construction of Libadalang itself
if ! [ -d libadalang ]
then
    git clone https://github.com/AdaCore/libadalang.git
    cd libadalang
else
    cd libadalang
    git pull
fi
rm -rf build

# Create a Python virtualenv and install the Libadalang code generation
# dependencies.
virtualenv2 lal-venv
. lal-venv/bin/activate
pip install -r REQUIREMENTS.dev

# Finally generate Libadalang, build it and install it!
ada/manage.py make
ada/manage.py install "$INSTALL_DIR"

# Make the Python binding available
if [ -z "$PYTHONPATH" ]
then
    export PYTHONPATH="$INSTALL_DIR/python"
else
    export PYTHONPATH="$INSTALL_DIR/python:$PYTHONPATH"
fi

# You are ready to run Python scripts using Libadalang!
python -c 'import libadalang; print libadalang'
