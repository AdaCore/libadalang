import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

import com.adacore.libadalang.Libadalang;

/**
 * Test that the ProjectManager.createContext method work as expected.
 */
public class Main {
    private static void check(
        String label,
        String root_project,
        String project,
        Libadalang.EventHandler eventHandler,
        boolean withTrivia,
        int tabStop
    ) {
        System.out.println("== " + label + " ==");
        System.out.println("");

        // Load the requested project and create the analysis context from it.
        Libadalang.ProjectManager gpr = Libadalang.ProjectManager.create(
            root_project
        );

        Libadalang.AnalysisContext ctx;
        try {
            ctx = gpr.createContext(
                project,
                eventHandler,
                withTrivia,
                tabStop
            );
        } catch (Libadalang.LangkitException e) {
            if (e.kind
                != Libadalang.ExceptionKind.EXCEPTION_UNSUPPORTED_VIEW_ERROR)
                throw new RuntimeException();
            System.out.println("Unsupported_View_Error: " + e.getMessage());
            System.out.println("");
            return;
        }

        Libadalang.AnalysisUnit u = ctx.getUnitFromProvider(
            Libadalang.Text.create("pkg"),
            Libadalang.AnalysisUnitKind.UNIT_BODY
        );
        List<Libadalang.Diagnostic> diagnostics = u.getDiagnostics();
        if (!diagnostics.isEmpty())
        {
            for (Libadalang.Diagnostic d : diagnostics)
                System.out.println(d.toString());
            throw new RuntimeException();
        }

        // To show that With_Trivia / Tab_Stop are properly forwarded to the
        // analysis context constructor and that the default charset is
        // correctly determined, show the first token (or trivia).
        Libadalang.Token t = u.getFirstToken();
        System.out.print("pkg%b first token/trivia: ");
        System.out.print("<Token Kind=" + t.kind.name + " Text=\"");
        String text = t.getText();
        for (int i = 0; i < text.length(); ++i) {
            final char c = text.charAt(i);
            if (c < ' ' || c > '~')
                System.out.print(String.format("\\x%02x", (int) c));
            else
                System.out.print(c);
        }
        System.out.println("\">");
        System.out.println("pkg%b root node: " + u.getRoot());

        // To show that the unit provider works as expected, resolve the Pkg
        // package spec from its body.
        Libadalang.CompilationUnit cu =
            (Libadalang.CompilationUnit) u.getRoot();
        Libadalang.LibraryItem it = (Libadalang.LibraryItem) cu.fBody();
        Libadalang.PackageBody pkg = (Libadalang.PackageBody) it.fItem();
        Libadalang.AdaNode n = pkg.pPreviousPart(false);
        System.out.println("pkg%b previous part: " + n);

        // To show that configuration pragmas are properly detected from the
        // project, print their list.
        Libadalang.AdaNodeArray pragmas = cu.pAllConfigPragmas();
        for (int i = 0; i < pragmas.size(); ++i)
            System.out.println("Config pragma: " + pragmas.get(i));

        System.out.println("");
    }

    static boolean triggered = false;

    public static void main(String[] args) {
        check("Simple: defaults", "simple/p.gpr", null, null, true, 8);
        check("Simple: without trivia", "simple/p.gpr", null, null, false, 8);
        check("Simple: tab stop = 4", "simple/p.gpr", null, null, true, 4);

        check("UTF-8", "utf-8/p.gpr", null, null, true, 8);

        check(
            "Aggregate project (no specific view)",
            "aggregate/agg.gpr",
            null,
            null,
            true,
            8
        );
        check(
            "Aggregate project (specific view: p2)",
            "aggregate/agg.gpr",
            "p2",
            null,
            true,
            8
        );

        Libadalang.EventHandler.UnitRequestedCallback unitRequestedCallback = (
            Libadalang.AnalysisContext context,
            String name,
            Libadalang.AnalysisUnit from,
            boolean found,
            boolean isNotFoundError
        ) -> {
        };
        Libadalang.EventHandler.UnitParsedCallback unitParsedCallback = (
            Libadalang.AnalysisContext context,
            Libadalang.AnalysisUnit unit,
            boolean reparsed
        ) -> {
            if(!triggered) {
                triggered = true;
                System.out.println("Unit_Parsed_Callback invoked");
            }
        };
        check(
            "Simple: event handler",
            "simple/p.gpr",
            null,
            Libadalang.EventHandler.create(
                unitRequestedCallback,
                unitParsedCallback
            ),
            true,
            8
        );

        check(
            "Preprocessing (p1)",
            "preprocessing/p1.gpr",
            null,
            null,
            true,
            8
        );
        check(
            "Preprocessing (p2)",
            "preprocessing/p2.gpr",
            null,
            null,
            true,
            8
        );

        check(
            "Config pragmas (p1)",
            "config_pragmas/p1.gpr",
            null,
            null,
            true,
            8
        );
        check(
            "Config pragmas (p2)",
            "config_pragmas/p2.gpr",
            null,
            null,
            true,
            8
        );

        System.out.println("Done.");
    }
}

