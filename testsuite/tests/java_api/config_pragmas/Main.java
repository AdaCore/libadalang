import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.adacore.libadalang.Libadalang.*;

/**
 * Test that the AnalysisContext.setConfigPragmasMapping method works as
 * expected.
 */
public class Main {
    private static AnalysisContext ctx;

    private static String[] filenames = new String[] {"foo.adb", "bar.adb"};

    private static AnalysisUnit load(String filename) {
        AnalysisUnit u = ctx.getUnitFromFile(filename);
        List<Diagnostic> diagnostics = u.getDiagnostics();
        if (!diagnostics.isEmpty())
        {
            for (Diagnostic d : diagnostics)
                System.out.println(d.toString());
            throw new RuntimeException();
        }
        return u;
    }

    private static void check_pragmas() {
        for (String filename : filenames) {
            System.out.println("Pragmas for " + filename + ":");
            AnalysisUnit u = load(filename);
            CompilationUnit cu = (CompilationUnit) u.getRoot();
            AdaNodeArray pragmas;
            try {
                pragmas = cu.pAllConfigPragmas();
            } catch (LangkitException exc) {
                System.out.println(
                    "Got an exception (" + exc.kind + "): " + exc.getMessage()
                );
                continue;
            }
            for (int i = 0; i < pragmas.size(); ++i)
                System.out.println("Config pragma: " + pragmas.get(i));
        }
    }

    public static void main(String[] args) {
        ctx = AnalysisContext.create();

        final AnalysisUnit global = load("global.adc");
        final AnalysisUnit local1 = load("local_1.adc");
        final AnalysisUnit local2 = load("local_2.adc");
        final AnalysisUnit foo = load("foo.adb");
        final AnalysisUnit bar = load("bar.adb");

        final Map<AnalysisUnit, AnalysisUnit> emptyLocal =
            new HashMap<AnalysisUnit, AnalysisUnit>();
        final Map<AnalysisUnit, AnalysisUnit> singleLocal =
            new HashMap<AnalysisUnit, AnalysisUnit>();
        final Map<AnalysisUnit, AnalysisUnit> allLocals =
            new HashMap<AnalysisUnit, AnalysisUnit>();
        final Map<AnalysisUnit, AnalysisUnit> buggyKey =
            new HashMap<AnalysisUnit, AnalysisUnit>();
        final Map<AnalysisUnit, AnalysisUnit> buggyValue =
            new HashMap<AnalysisUnit, AnalysisUnit>();

        singleLocal.put(foo, local1);
        allLocals.put(foo, local1);
        allLocals.put(bar, local2);
        buggyKey.put(null, local1);
        buggyValue.put(foo, null);

        System.out.println("== Query config pragmas with no mappings set ==");
        check_pragmas();
        System.out.println("");

        System.out.println("== Set empty mappings ==");
        ctx.setConfigPragmasMapping(null, null);
        check_pragmas();
        System.out.println("");

        System.out.println("== Set global-only mapping ==");
        ctx.setConfigPragmasMapping(global, emptyLocal);
        check_pragmas();
        System.out.println("");

        System.out.println("== Set local mapping for foo.adb ==");
        ctx.setConfigPragmasMapping(global, singleLocal);
        check_pragmas();
        System.out.println("");

        System.out.println("== Set all mappings ==");
        ctx.setConfigPragmasMapping(global, allLocals);
        check_pragmas();
        System.out.println("");

        System.out.println("== Buggy key (error) ==");
        try {
            ctx.setConfigPragmasMapping(global, buggyKey);
        } catch (RuntimeException exc) {
            System.out.println("Got an exception: " + exc.getMessage());
        }
        check_pragmas();
        System.out.println("");

        System.out.println("== Buggy value (error) ==");
        try {
            ctx.setConfigPragmasMapping(global, buggyValue);
        } catch (RuntimeException exc) {
            System.out.println("Got an exception: " + exc.getMessage());
        }
        check_pragmas();
        System.out.println("");

        System.out.println("Done.");
    }
}

