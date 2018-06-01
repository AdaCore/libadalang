#! /usr/bin/env sh

set -v
set -e

export PATH=$GNAT_PATH/bin:$PATH
gprbuild --version
ada/manage.py make -j16
ada/manage.py test
