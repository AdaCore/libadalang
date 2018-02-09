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

# Install a GNAT Pro or GNAT GPL release for your platform in $INSTALL_DIR (see
# https://libre.adacore.com/ for GPL releases). Then, get the corresponding
# version of GNATCOLL. For instance, GNATCOLL GPL 2017:

wget -O gnatcoll-gpl-2017-src.tar.gz \
    http://mirrors.cdn.adacore.com/art/5a15c79cc7a4479a23674c66
tar xf gnatcoll-gpl-2017-src.tar.gz
if [ -d gnatcoll ]
then
    rm -rf gnatcoll
fi
mv gnatcoll-gpl-2017-src gnatcoll

############################################
# The following should work out of the box #
############################################

export PATH="$INSTALL_DIR/bin:$PATH"
export LD_LIBRARY_PATH="$INSTALL_DIR/lib:$PATH"

# Build GNATCOLL and install it in $INSTALL_DIR
cd gnatcoll
./configure --prefix="$INSTALL_DIR" \
    --enable-build=Debug --enable-shared --enable-projects
make PROCESSORS=0
make install
cd ..

# Get Quex
wget -O quex-0.65.4.zip "https://downloads.sourceforge.net/project/quex/HISTORY/0.65/quex-0.65.4.zip?r=https%3A%2F%2Fsourceforge.net%2Fprojects%2Fquex%2Ffiles%2FHISTORY%2F0.65%2F&ts=1484909333&use_mirror=heanet"
unzip quex-0.65.4.zip
export QUEX_PATH="$PWD/quex-0.65.4"

# Now prepare the construction of Libadalang itself
git clone https://github.com/AdaCore/libadalang.git
cd libadalang

# Create a Python virtualenv and install the Libadalang code generation
# dependencies.
virtualenv lal-venv
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
