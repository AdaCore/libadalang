with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;

package App is

   procedure Process_Unit (Context : App_Job_Context; Unit : Analysis_Unit);

   package App is new Libadalang.Helpers.App
     (Name         => "example",
      Description  => "Example app. Will flag goto statements",
      Process_Unit => Process_Unit);

end App;
