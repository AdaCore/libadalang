import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import com.adacore.libadalang.Libadalang;

public class Main {

    /** The direcotry which contains the projects */
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
     * Open a gpr project and list its source files
     *
     * @param gprFile The gpr file
     * @param lookInProjectPath If the function should look for the GPR file
     *     in the given project path
     * @param subproject The subproject to use. If null, use the root project
     */
    private static void openProject(
        String gprFile,
        Libadalang.ScenarioVariable[] scenarioVariables,
        boolean lookInProjectPath,
        String subproject
    ) {
        String headerMsg = "Open " + gprFile;
        if (subproject != null) {
            headerMsg += " (" + subproject + ")";
        }
        header(headerMsg);

        // Resolve the project file if needed
        String projectFile = gprFile;
        if(lookInProjectPath) {
            projectFile = Paths.get(projectPath, gprFile).toString();
        }

        try(
            Libadalang.ProjectManager project
              = Libadalang.ProjectManager.create(
                  projectFile,
                  scenarioVariables,
                  "",
                  ""
              )
        ) {
            String[] files = project.getFiles(
                Libadalang.SourceFileMode.ROOT_PROJECT,
                subproject == null ? null : new String[] {subproject}
            );

            // Create an analysis context with the project unit provider
            Libadalang.UnitProvider unitProvider =
                project.getProvider(subproject);

            try(
               Libadalang.AnalysisContext context
                 = Libadalang.AnalysisContext.create(
                       null,
                       null,
                       unitProvider,
                       null,
                       true,
                       8
                   )
            ) {
                for(String file : files) {
                    Libadalang.AnalysisUnit unit
                      = context.getUnitFromFile(file);
                    System.out.println("File " + unit.getFileName(false));
                    System.out.println("  root = " + unit.getRoot());
                }
            }
        } catch (Libadalang.ProjectManagerException e) {
            System.err.println(e.getMessage());
        } finally {
            footer("Open " + gprFile);
        }
    }

    /**
     * Test opening a valid project.
     */
    private static void testValid() {
        openProject("p1.gpr", null, true, null);
        openProject(
            "p2.gpr",
            new Libadalang.ScenarioVariable[] {
                Libadalang.ScenarioVariable.create("SRC_DIR", "src2_1"),
                Libadalang.ScenarioVariable.create("USELESS", "useless")
            },
            true,
            null
        );
        openProject(
            "p2.gpr",
            new Libadalang.ScenarioVariable[] {
                Libadalang.ScenarioVariable.create("SRC_DIR", "src2_2")
            },
            true,
            null
        );
    }

    /**
     * Test opening an invalid project.
     */
    private static void testInvalid() {
        openProject("invalid.gpr", null, true, null);
    }

    /**
     * Test opening an inexistant project.
     */
    private static void testInexistant() {
        openProject("idonotexist.gpr", null, false, null);
    }

    /**
     * Test opening an aggregate project.
     */
    private static void testAggregate() {
        openProject("agg.gpr", null, true, "p1.gpr");
        openProject("agg.gpr", null, true, "p2.gpr");
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
    }
}
