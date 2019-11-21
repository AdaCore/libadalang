with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;

package App is

   procedure Process_Unit (Unit : Analysis_Unit);

   package App is new Libadalang.Helpers.App
     ("Example app. Will flag goto statements",
      Process_Unit => Process_Unit);

end App;
