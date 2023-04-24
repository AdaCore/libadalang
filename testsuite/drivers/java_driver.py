import os
import os.path as P

from e3.testsuite.driver.classic import TestSkip, TestAbortWithFailure

from drivers.base_driver import BaseDriver

class JavaDriver(BaseDriver):
    """
    Driver to build and run tests for the Java bindings.
    """

    main_java_class = "Main"
    """
    The name of the Java class to run.
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
                'Cannot find the Java bindings JAR archive'
            )

        # Get the java main file
        main_java = P.realpath(P.join(
            self.test_env['test_dir'],
            self.test_env.get('java_path', '.'),
            f"{self.main_java_class}.java"
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
            Run the Java main class after its compilation with native-image.
            """

            # Get the javac executable from the java home
            javac_exec = P.realpath(P.join(
                os.environ['JAVA_HOME'],
                'bin',
                'javac'
            ))

            # Compile the Java main class
            self.run_and_check([
                javac_exec,
                '-cp', class_path,
                '-encoding', 'utf8',
                '-d', self.test_env['working_dir'],
                main_java,
            ])

            # Get the native-image executable
            ni_exec = P.realpath(P.join(
                os.environ['GRAAL_HOME'],
                'bin',
                'native-image'
            ))

            # Run the native-image compilation
            main_exec = P.join(
                self.test_env['working_dir'],
                'main'
            )
            self.run_and_check([
                ni_exec,
                '-cp', class_path,
                '--no-fallback',
                '--macro:truffle',
                '-H:+BuildOutputSilent',
                '-H:+ReportExceptionStackTraces',
                f'{self.main_java_class}',
                main_exec,
            ])

            # Run the compiled Java
            self.run_and_check([main_exec, project_path])


        # Run the test with the wanted mode
        if self.test_env['mode'] == 'graal_c_api':
            native_image_run()
        else:
            java_run()
