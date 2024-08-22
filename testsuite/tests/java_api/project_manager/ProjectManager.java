import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import com.adacore.libadalang.Libadalang;

public class ProjectManager {

    /** The directory which contains the projects */
    private static String projectPath;

    /**
     * Display the section header with the given name
     *
     * @param name The name of the section
     */
    private static void header(String name) {
        System.out.println("--- " + name + " ---");
    }

    /**
     * Display the footer of a section
     *
     * @param name The name of the section
     */
    private static void footer(String name) {
        System.out.println("----" + "-".repeat(name.length()) + "----\n");
    }

    /**
     * Util function to display all information about a project manager.
     */
    private static void projectInfo(
        Libadalang.ProjectManager project,
        String subproject
    ) {
        // Display the project manager diagnostics if any
        List<String> diagnostics = project.getDiagnostics();
        if (diagnostics.size() > 0) {
            System.out.println("Error during project opening:");
            System.out.println("  " + diagnostics);
        }

        String[] files = project.getFiles(
            Libadalang.SourceFileMode.ROOT_PROJECT,
            subproject == null ? null : new String[] {subproject}
        );

        // Create an analysis context with the project unit provider
        Libadalang.UnitProvider unitProvider =
            project.getProvider(subproject);

        // Create an event handler
        Libadalang.EventHandler eh = Libadalang.EventHandler.create(
            (ctx, name, from, found, notFoundIsError) -> {
                if (!found && notFoundIsError) {
                    System.out.println(
                        "Cannot find file " + new File(name).getName()
                    );
                }
            },
            null);

        try(
           Libadalang.AnalysisContext context
             = Libadalang.AnalysisContext.create(
                   null,
                   null,
                   unitProvider,
                   eh,
                   true,
                   8
               )
        ) {
            for(String file : files) {
                Libadalang.AnalysisUnit unit
                  = context.getUnitFromFile(file);
                System.out.println("File " + unit.getFileName(false));
                System.out.println("  root = " + unit.getRoot());
                System.out.println(
                    "  deps = " +
                    Arrays.toString(
                        ((Libadalang.CompilationUnit) unit.getRoot())
                            .pUnitDependencies()
                    )
                );
            }
        }
    }

    /** Simply open the given GPR file and display its source files. */
    private static void openProject(String gprFile) {
        openProject(
            gprFile,
            null
        );
    }

    /**
     * Open the given GPR file with a subproject to use, then display its
     * source files.
     */
    private static void openProject(
        String gprFile,
        String subproject
    ) {
        openProject(
            gprFile,
            subproject,
            null,
            true
        );
    }

    /**
     * Open a GPR project and list its source files.
     *
     * @param gprFile The gpr file
     * @param subproject The subproject to use. If null, use the root project
     *     in the given project path
     * @param scenarioVariables Scenario variuables to apply during project
     *     opening
     * @param lookInProjectPath If the function should look for the GPR file
     */
    private static void openProject(
        String gprFile,
        String subproject,
        Libadalang.ScenarioVariable[] scenarioVariables,
        boolean lookInProjectPath
    ) {
        openProject(
            gprFile,
            subproject,
            null,
            scenarioVariables,
            lookInProjectPath
        );
    }

    /**
     * Open a gpr project and list its source files.
     *
     * @param gprFile The gpr file
     * @param subproject The subproject to use. If null, use the root project
     *     in the given project path
     * @param configFile A configuration file to open the project with
     * @param scenarioVariables Scenario variuables to apply during project
     *     opening
     * @param lookInProjectPath If the function should look for the GPR file
     */
    private static void openProject(
        String gprFile,
        String subproject,
        String configFile,
        Libadalang.ScenarioVariable[] scenarioVariables,
        boolean lookInProjectPath
    ) {
        String headerMsg = "Open " + Paths.get(gprFile).getFileName();
        if (subproject != null) {
            headerMsg += " (" + subproject + ")";
        }
        if (configFile != null) {
            headerMsg += " with config " + configFile;
        }
        header(headerMsg);

        // Resolve the project file if needed
        if(lookInProjectPath) {
            gprFile = Paths.get(projectPath, gprFile).toString();
            configFile = configFile == null ?
                null :
                Paths.get(projectPath, configFile).toString();
        }

        try(
            Libadalang.ProjectManager project
              = Libadalang.ProjectManager.create(
                  gprFile,
                  scenarioVariables,
                  null,
                  null,
                  configFile
              )
        ) {
            projectInfo(project, subproject);
        } catch (Libadalang.ProjectManagerException e) {
            System.out.println(e.getMessage());
        } finally {
            footer(headerMsg);
        }
    }

    /**
     * Test opening a valid project.
     */
    private static void testValid() {
        openProject("p1.gpr");
        openProject(
            "p2.gpr",
            null,
            new Libadalang.ScenarioVariable[] {
                Libadalang.ScenarioVariable.create("SRC_DIR", "src2_1"),
                Libadalang.ScenarioVariable.create("USELESS", "useless")
            },
            true
        );
        openProject(
            "p2.gpr",
            null,
            new Libadalang.ScenarioVariable[] {
                Libadalang.ScenarioVariable.create("SRC_DIR", "src2_2")
            },
            true
        );
    }

    /**
     * Test opening an invalid project.
     */
    private static void testInvalid() {
        openProject("invalid.gpr");
    }

    /**
     * Test opening an inexistant project.
     */
    private static void testInexistant() {
        openProject("idonotexist.gpr", null, null, false);
    }

    /**
     * Test opening an aggregate project.
     */
    private static void testAggregate() {
        openProject("agg.gpr", "nosuchsubproject");
        openProject("agg.gpr", "p1");
        openProject("agg.gpr", "p2");
    }

    private static void testNoSuchTarget() {
        openProject("nosuchtarget.gpr");
    }

    /**
     * Test the opening of an implicit project.
     */
    private static void testImplicit() {
        String headerMsg = "Open implicit project";
        header(headerMsg);
        try (
            Libadalang.ProjectManager project =
                Libadalang.ProjectManager.createImplicit(
                    null,
                    null,
                    null
                )
        ) {
            projectInfo(project, null);
        } catch (Libadalang.ProjectManagerException e) {
            System.out.println(e.getMessage());
        } finally {
            footer(headerMsg);
        }
    }

    private static void testConfigFile() {
        openProject(
            "p1.gpr",
            null,
            "other_naming.cgpr",
            null,
            true
        );
    }

    private static void testValidConfigFile() {
        openProject("for_cgpr.gpr");
        openProject(
            Paths.get(projectPath, "for_cgpr.gpr").toString(),
            null,
            "custom.cgpr",
            null,
            false
        );
    }

    private static void testEmptyConfigFile() {
        openProject(
            "p1.gpr",
            null,
            "empty.cgpr",
            null,
            true
        );
    }

    private static void testInexistantConfigFile() {
        openProject(
            Paths.get(projectPath, "p1.gpr").toString(),
            null,
            "idonotexist.cgpr",
            null,
            false
        );
    }

    /**
     * Run the tests
     */
    public static void main(String[] args) {
        projectPath = args[0];
        testValid();
        testInvalid();
        testInexistant();
        testAggregate();
        testNoSuchTarget();
        testImplicit();
        testConfigFile();
        testValidConfigFile();
        testEmptyConfigFile();
        testInexistantConfigFile();
    }
}
