#! /bin/sh

set -x
set -e

# This script is a helper to get a quick user/developper Libadalang setup.
# Please see the following section for specific settings you need to do before
# running it.

# Install the following prerequisites:
#
# * Python's virtual env;
# * The GMP library and its development package
#
# On Debian-like systems, the following command should do it:
#
#   $ sudo apt-get install python-virtualenv libgmp3-dev

# You also need to have a GNAT compiler available. Here, for the example we
# automatically install GNAT Community 2018 for Linux x86_64 (from
# https://www.adacore.com/download), but a recent FSF release from your
# favorite distro should do it too.

##################################################
# Change the below with your desired directories #
##################################################

export ADALIB_DIR=$HOME/adalib
export TOOLS_DIR=$HOME/build_tools
export INSTALL_DIR=$HOME/install
export LIBRARY_PATH=/usr/local/lib

# This script defers the real work to scripts that are used for travis
# automated builds. You can see and customize those if you need.

# Change that if you're on a different OS. WARNING: You might have loads of
# changes to make if you're not under linux.
export TRAVIS_OS_NAME=linux
export TRAVIS_REPO_SLUG="AdaCore/libadalang"
export TRAVIS_BRANCH="master"

./utils/travis-install.sh
./utils/travis-script.sh
