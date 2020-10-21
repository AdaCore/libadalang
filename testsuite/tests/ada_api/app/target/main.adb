with Libadalang.Analysis; use Libadalang.Analysis;
with Libadalang.Helpers;  use Libadalang.Helpers;

procedure Main is
   package App is new Libadalang.Helpers.App
     (Name        => "example",
      Description => "Example app.");
begin
   App.Run;
end Main;
