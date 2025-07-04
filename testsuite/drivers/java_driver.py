from __future__ import annotations

import os
import os.path as P
import subprocess

from e3.fs import mkdir, rm
from e3.testsuite import logger
from e3.testsuite.driver.classic import TestAbortWithFailure, TestSkip

from drivers.base_driver import BaseDriver


class JavaDriver(BaseDriver):
    """
    Driver to build and run tests for the Java bindings.
    """

    ni_main: str
    """
    Path to the native-image pre-compiled Java test main (see the
    build_ni_main method).
    """

    # Template for the Java test main source
    ni_main_template = """import java.util.Arrays;
    :IMPORTS:

    public final class NativeImageMain {
        private static String[] toForward(String[] srcArgs) {
            return Arrays.copyOfRange(srcArgs, 1, srcArgs.length);
        }

        public static void main(String[] args) {
            if(args.length < 1)
                throw new RuntimeException("Usage: main [test_class] ...");
            switch(args[0]) {
                :TEST_CASES:
            }
        }
    }
    """

    @property
    def mode(self) -> str:
        """
        Return the Java bindings execution mode for this test ("graal_c_api" or
        "jni").
        """
        result = self.test_env["mode"]
        assert result in ("graal_c_api", "jni")
        return result

    @staticmethod
    def jar_file(java_bindings_dir: str) -> str | None:
        """
        Return the path to Libadalang's JAR file, or None if it cannot be
        found.
        """
        bindings_dir = os.path.abspath(java_bindings_dir)
        jar_file = os.path.join(bindings_dir, 'target', 'libadalang.jar')
        return jar_file if os.path.isfile(jar_file) else None

    @staticmethod
    def java_exec() -> str:
        """
        Return the path to the "java" executable.
        """
        return os.path.realpath(os.path.join(
            os.environ['JAVA_HOME'], 'bin', 'java'
        ))

    @staticmethod
    def javac_exec() -> str:
        """
        Return the path to the "javac" executable.
        """
        return os.path.realpath(os.path.join(
            os.environ['JAVA_HOME'], 'bin', 'javac'
        ))

    @staticmethod
    def ni_compiler_exec() -> str:
        """
        Return the path to the "native-image" compiler executable.
        """
        return os.path.realpath(os.path.join(
            os.environ['GRAAL_HOME'],
            'bin',
            ('native-image.cmd' if os.name == 'nt' else 'native-image')
        ))

    @classmethod
    def build_ni_main(
        cls,
        working_dir: str,
        java_bindings_dir: str,
        drivers: list[JavaDriver],
    ) -> None:
        """
        Build a single main for all the given Graal C API test.

        Also set the "ni_main" attribute for all these drivers.
        """
        # If there are no Graal C API tests to run, do nothing
        if not drivers:
            return

        logger.info("Pre-compile Graal C API tests...")

        # Ensure Libadalang's Java bindings are available
        libadalang_jar = cls.jar_file(java_bindings_dir)
        if libadalang_jar is None:
            raise RuntimeError(
                "Cannot find the Java bindings JAR archive, make sure you"
                " built libadalang with the '--enable-java' flag"
            )

        # Create a build directory for the native-image build
        ni_dir = os.path.join(working_dir, "_native_image_build")
        ni_bin_dir = os.path.join(ni_dir, "bin")
        ni_bin = os.path.join(ni_bin_dir, "main")
        if os.path.isdir(ni_dir):
            rm(ni_dir)
        mkdir(ni_dir)
        mkdir(ni_bin_dir)

        # Locate the javac compiler and build a class path to give access to
        # Libadalangs' Java bindings, as well as test classes compiled in
        # "ni_dir".
        javac_exec = cls.javac_exec()
        class_path = os.pathsep.join([libadalang_jar, ni_dir])

        def javac(java_filename: str) -> None:
            """
            Shortcut to compile a Java class.
            """
            subprocess.check_call(
                [
                    javac_exec,
                    '-cp', class_path,
                    '-encoding', 'utf8',
                    '-d', ni_dir,
                    java_filename,
                ]
            )

        # Populate it with the Java sources to build
        import_stmts = []
        case_stmts = []
        for test in drivers:
            test_env = test.test_env

            # Copy the test Java source file and add a "package" statement
            # at the top of it.
            main_class = test_env.get('main_class', 'Main')
            src_java_file_name = os.path.join(
                test_env['test_dir'],
                test_env.get('java_path', '.'),
                f"{main_class}.java"
            )
            java_file_name = os.path.join(ni_dir, f"{main_class}.java")
            with open(src_java_file_name) as f:
                contents = f.read()
            with open(java_file_name, 'w') as f:
                print("package tests;", file=f)
                f.write(contents)

            # Compile the test Java file
            javac(java_file_name)

            # Add the test class to the native-image main Java content
            import_stmts.append(f"import tests.{main_class};")
            case_stmts.append(f'case "{main_class}": {main_class}.main'
                              '(toForward(args)); break;')

            # Communicate the path to the final native-image executable to
            # test drivers.
            test.ni_main = ni_bin

        # Create the native-image Java main file content, write it and
        # compile it.
        ni_main_content = cls.ni_main_template.replace(
            ':IMPORTS:',
            '\n'.join(import_stmts)
        ).replace(
            ':TEST_CASES:',
            '\n'.join(case_stmts)
        )
        ni_main_file_name = os.path.join(ni_dir, "NativeImageMain.java")
        with open(ni_main_file_name, 'w') as f:
            print(ni_main_content, file=f)
        javac(ni_main_file_name)

        os_specific_options = []
        if os.name != "nt":
            def find_file_in_env(file_name, env_var) -> str | None:
                for dir in os.environ.get(env_var, "").split(os.pathsep):
                    if os.path.isfile(os.path.join(dir, file_name)):
                        return dir
                return None

            # Find the directory that contains the "libadalang.h" header file
            header_dir = find_file_in_env("libadalang.h", "C_INCLUDE_PATH")
            assert header_dir is not None

            # Find the directory that contains the LAL shared library
            lib_dirs = [
                dir for dir in [
                    find_file_in_env("libadalang.so", "LIBRARY_PATH"),
                    find_file_in_env("libz.so", "LIBRARY_PATH"),
                ]
                if dir is not None
            ]

            # We also need to provide rpath-links to the compiler to allow it
            # to find libraries during linking phase.
            ld_library_path = os.environ.get('LD_LIBRARY_PATH')
            rpaths = (
                [
                    f"-Wl,-rpath-link={p}"
                    for p in ld_library_path.split(os.pathsep)
                ]
                if ld_library_path else
                []
            )

            # Create native-image options to provide required information when
            # spawning GCC.
            os_specific_options.extend([
                f"--native-compiler-options=-I{header_dir}",
                *[
                    f"--native-compiler-options=-L{lib_dir}"
                    for lib_dir in lib_dirs
                ],
                *[f"--native-compiler-options={rp}" for rp in rpaths],
            ])
        else:
            # Ensure the compiler isn't emitting warnings about CPU features
            os_specific_options.append("-march=native")

        # Run the native-image compiler
        subprocess.check_call([
            cls.ni_compiler_exec(),
            '-cp', class_path,
            '--no-fallback',
            "-Ob",
            "--silent",
            *os_specific_options,
            'NativeImageMain',
            ni_bin,
        ])

        logger.info("Done")

    def run(self):
        # Verify the Java bindings directory
        bindings_dir = self.env.java_bindings
        if bindings_dir is None:
            raise TestSkip('Test requires Java bindings')

        # Verify that the libadalang bindings are compiled
        libadalang_jar = self.jar_file(bindings_dir)
        if libadalang_jar is None:
            raise TestAbortWithFailure(
                "Cannot find the Java bindings JAR archive, make sure you"
                " built libadalang with the '--enable-java' flag"
            )

        # Get the class name and source file for the Java main
        main_class = self.test_env.get("main_class", "Main")
        main_java = self.test_dir(
            self.test_env.get("java_path", "."), f"{main_class}.java"
        )

        # Get the project path
        project_path = self.test_dir(self.test_env.get("projects_path", "."))

        if self.mode == 'graal_c_api':
            # Run the pre-compiled native-image produced executable with the
            # main Java class as first argument.
            assert self.ni_main, (
                "Test driver cannot access the pre-compiled Graal C API tests"
            )
            args = [self.ni_main, main_class]

        else:
            # Run the Java main class as Java program

            # Give access to the Libadalang Java bindings and to the tests'
            # Java sources.
            class_path = P.pathsep.join(
                [libadalang_jar, self.test_env['working_dir']]
            )

            # Get the java.library.path from LD_LIBRARY_PATH and compiled JNI
            # stubs.
            java_library_path = P.pathsep.join(
                [P.join(bindings_dir, 'jni'), os.environ['LD_LIBRARY_PATH']]
            )

            # Prepare the command to run the Java main
            args = [
                self.java_exec(),
                '-cp', class_path,
                '-Dfile.encoding=UTF-8',
                # Enable the Java application to load and use native libraries
                "--enable-native-access=ALL-UNNAMED",
                f"-Djava.library.path={java_library_path}",
            ]
            args.append(main_java)

        # Run the test main. Mains expect a GPR project path as their only
        # argument.
        self.run_and_check(args + [project_path])
