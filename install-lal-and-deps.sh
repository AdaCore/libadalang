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

# Install GNAT GPL for your platform in $INSTALL_DIR (see
# https://libre.adacore.com/), then...

############################################
# The following should work out of the box #
############################################

export PATH="$INSTALL_DIR/bin:$PATH"
export LD_LIBRARY_PATH="$INSTALL_DIR/lib:$PATH"

# Build LibGPR and install it in $INSTALL_DIR
git clone https://github.com/AdaCore/gprbuild.git -b gpl-2016
cd gprbuild
make BUILD=debug prefix="$INSTALL_DIR" libgpr.build
make BUILD=debug prefix="$INSTALL_DIR" libgpr.install
cd ..

# Build GNATCOLL and install it in $INSTALL_DIR
git clone https://github.com/AdaCore/gnatcoll.git -b gpl-2016
cd gnatcoll
# Workaround a release glitch...
git cherry-pick 8f847c2ff0bc94b5cf257ada563d7ea0ad14c984
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
source lal-venv/bin/activate
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
