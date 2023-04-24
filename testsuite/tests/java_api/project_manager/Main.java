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
     * in the given project path
     */
    private static void openProject(
        String gprFile,
        boolean lookInProjectPath
    ) {
        header("Open " + gprFile);

        String projectFile = gprFile;
        if(lookInProjectPath) {
            projectFile = Paths.get(projectPath, gprFile).toString();
        }

        try(
            Libadalang.ProjectManager project
              = Libadalang.ProjectManager.create(projectFile, "", "")
        ) {
            String[] files = project.getFiles(
                Libadalang.SourceFileMode.ROOT_PROJECT
            );
            Libadalang.UnitProvider unitProvider = project.getProvider();
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

    private static void openProject(
        String gprFile
    ) {
        openProject(gprFile, true);
    }

    /**
     * Test opening a valid project.
     */
    private static void testValid() {
        openProject("p1.gpr");
        openProject("p2.gpr");
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
        openProject("idonotexist.gpr", false);
    }

    /**
     * Run the tests
     */
    public static void main(String[] args) {
        projectPath = args[0];
        testValid();
        testInvalid();
        testInexistant();
    }
}
