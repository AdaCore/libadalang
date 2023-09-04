import os
import os.path as P

from e3.testsuite.driver.classic import TestAbortWithFailure, TestSkip

from drivers.base_driver import BaseDriver


class JavaDriver(BaseDriver):
    """
    Driver to build and run tests for the Java bindings.
    """

    ni_test_driver: str
    """
    Path to the native-image pre-compiled Java test driver.
    """

    def run(self):
        # Verify the Java bindings directory
        bindings_dir = self.env.java_bindings
        if not bindings_dir:
            raise TestSkip('Test requires Java bindings')

        # Verify that the libadalang bindings are compiled
        libadalang_jar = P.join(bindings_dir, 'target', 'libadalang.jar')
        if not P.isfile(libadalang_jar):
            raise TestAbortWithFailure(
                "Cannot find the Java bindings JAR archive, make sure you "
                "built libadalang with the '--enable-java' flag"
            )
        # Get the Java main class name
        main_class = self.test_env.get("main_class", "Main")

        # Get the java main file
        main_java = P.realpath(P.join(
            self.test_env['test_dir'],
            self.test_env.get('java_path', '.'),
            f"{main_class}.java"
        ))

        # Get the project path
        project_path = P.realpath(P.join(
            self.test_env['test_dir'],
            self.test_env.get('projects_path', '.')
        ))

        # Create the class path
        class_path = P.pathsep.join([
            libadalang_jar,
            self.test_env['working_dir']
        ])

        def java_run():
            """
            Run the Java main class as Java program.
            """

            # Get the java excutable from the Java home
            java_exec = P.realpath(P.join(
                os.environ['JAVA_HOME'],
                'bin',
                'java'
            ))

            # Get the java.library.path from the LD_LIBRARY_PATH and compiled
            # JNI stubs.
            java_library_path = P.pathsep.join([
                P.join(bindings_dir, 'jni'),
                os.environ['LD_LIBRARY_PATH'],
            ])

            # Prepare the command to run the Java main
            args = [
                java_exec,
                '-cp', class_path,
                '-Dfile.encoding=UTF-8',
                f"-Djava.library.path={java_library_path}",
            ]
            if 'graalvm' in os.environ['JAVA_HOME']:
                args.append((
                    '--add-opens=org.graalvm.truffle/com.oracle.truffle.api.'
                    'strings=ALL-UNNAMED'
                ))
            args += [main_java, project_path]

            # Run the test
            self.run_and_check(args)

        def native_image_run():
            """
            Run the pre-compiled native-image produced executable with the
            main Java class as first argument.
            """
            assert self.ni_test_driver, ("Test driver cannot access the pre-"
                                         "compiled Graal C API tests")

            self.run_and_check([
                self.ni_test_driver,
                main_class,
                project_path
            ])

        # Run the test with the wanted mode
        if self.test_env['mode'] == 'graal_c_api':
            native_image_run()
        else:
            java_run()
