#!/bin/bash
          set -x -e
          DEBUG=$1  # Value is '' or 'debug'
          RUNNER_OS=$2  #  ${{ runner.os }} is Linux, Windiws, maxOS
          export prefix=/tmp/ADALIB_DIR
          if [ $RUNNER_OS = Windows ]; then
             export prefix=/opt/ADALIB_DIR
             mount `cmd /c cd | cut -d\: -f1`:/opt /opt
          fi
          export PROCESSORS=0
          export GPR_PROJECT_PATH=$prefix/share/gpr
          export CPATH=/usr/local/include:/mingw64/include
          export LIBRARY_PATH=/usr/local/lib:/mingw64/lib
          export DYLD_LIBRARY_PATH=/usr/local/lib
          #LIBRARY_TYPES="relocatable,static"
          LIBRARY_TYPES="static"
          pip install -r langkit/REQUIREMENTS.dev
          pip install jsonschema
          pip install langkit/
          gprinstall --uninstall gnatcoll
          gprinstall --uninstall gpr

          make -C gprbuild prefix=$prefix BUILD=${DEBUG:-production} GPRBUILD_OPTIONS="-cargs:C -Dst_mtim=st_mtimespec -gargs" libgpr.build.{shared,static} libgpr.install.{shared,static}
          BUILD=`echo $DEBUG| tr [a-z] [A-Z]`  # Convert to uppercase
          make -C gnatcoll-core prefix=$prefix BUILD=${BUILD:-PROD} LIBRARY_TYPES="${LIBRARY_TYPES/,/ }" build install
          python gnatcoll-bindings/iconv/setup.py build ${DEBUG:+--debug} -j0 --prefix=$prefix --library-types=$LIBRARY_TYPES
          python gnatcoll-bindings/iconv/setup.py install
          python gnatcoll-bindings/gmp/setup.py build ${DEBUG:+--debug} -j0 --prefix=$prefix --library-types=$LIBRARY_TYPES
          python gnatcoll-bindings/gmp/setup.py install

          eval `langkit/manage.py setenv`
          langkit/manage.py build-langkit-support --library-types $LIBRARY_TYPES
          langkit/manage.py install-langkit-support $prefix --library-types $LIBRARY_TYPES

          BUILD=${DEBUG:+dev}  # Convert debug to dev
          # Build libadalang static library
          ./manage.py generate
          ./manage.py build --library-types=static --build-mode ${BUILD:-prod}
          ./manage.py install --library-types=static --build-mode ${BUILD:-prod} $prefix
          gprinstall --uninstall --prefix=$prefix mains.gpr
          tar czf libadalang-$RUNNER_OS-`basename $GITHUB_REF`${DEBUG:+-dbg}-static.tar.gz -C $prefix .
          # Build libadalang relocatable library
          if [ "$LIBRARY_TYPES" != static ]; then
            gprinstall --uninstall --prefix=$prefix libadalang.gpr
            rm -rf build
            # The gprbuild losts -lgmp on MacOS X, provide it in .cgpr file:
            gprconfig --batch -o /tmp/file.cgpr --config=c --config=ada
            if [ $RUNNER_OS = macOS ]; then
              sed -i -e '/for Shared_Library_Minimum_Switches use/s/use/use ("-lgmp") \&/' /tmp/file.cgpr
              cat /tmp/file.cgpr
            fi
            ./manage.py generate
            ./manage.py build --library-types=relocatable --build-mode ${BUILD:-prod} --gargs=--config=/tmp/file.cgpr
            ./manage.py install --library-types=relocatable --build-mode ${BUILD:-prod} $prefix
            tar czf libadalang-$RUNNER_OS-`basename $GITHUB_REF`${DEBUG:+-dbg}.tar.gz -C $prefix .
          fi
