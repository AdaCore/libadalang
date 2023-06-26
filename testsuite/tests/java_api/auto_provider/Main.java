import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;

import com.adacore.libadalang.Libadalang;

/**
 * Test that auto providers work as expected
 */
public class Main {
    private static void doTest(String[] args, String charset) {
        final String projectPath = args[0];
        final String sourceA = Paths.get(projectPath, "a.ads").toString();
        final String sourceC = Paths.get(projectPath, "c.ads").toString();
        final String sourceD = Paths.get(projectPath, "d.ads").toString();

        // Create an auto provider with sources A and C and D:
        //  * B is not included on purpose to assert that nameres fails when a
        //    dependency is missing.
        //  * D designates a source file that does not exist. For now, no error
        //    are emitted, but the program doesn't crash.
        try (
            final Libadalang.UnitProvider unitProvider =
                Libadalang.createAutoProvider(
                    new String[] {sourceA, sourceC, sourceD},
                    charset
                );
            final Libadalang.AnalysisContext context =
                Libadalang.AnalysisContext.create(
                    null,
                    null,
                    unitProvider,
                    null,
                    true,
                    8
                )
        ) {
            final Libadalang.AnalysisUnit unit =
                context.getUnitFromFile(sourceC);
            final Libadalang.CompilationUnit root =
                (Libadalang.CompilationUnit) unit.getRoot();
            final Libadalang.PackageDecl pkg =
                (Libadalang.PackageDecl) root.pDecl();
            final Libadalang.AdaNodeList decls =
                (Libadalang.AdaNodeList) pkg.fPublicPart().fDecls();

            // Resolve the type of the two object declarations inside package
            // C: one of them refers to a type declared in package A (so
            // should resolve correctly), while the other refers to a type
            // declared in package B which is not part of the auto provider,
            // thus should fail to resolve.
            for (int i = 0; i < decls.getChildrenCount(); ++i) {
                final Libadalang.ObjectDecl obj =
                    (Libadalang.ObjectDecl) decls.getChild(i);
                final Libadalang.BaseTypeDecl type =
                    obj.fTypeExpr().pDesignatedTypeDecl();
                System.out.println(obj + " has type " + type);
            }
        }
    }

    public static void main(String[] args) {
        System.out.println("Using null charset:");
        doTest(args, null);
        System.out.println("Using utf-8 charset:");
        doTest(args, "utf-8");
    }
}

