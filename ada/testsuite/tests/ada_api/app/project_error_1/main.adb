with Libadalang.Helpers;

procedure Main is
   package App is new Libadalang.Helpers.App
     (Name        => "example",
      Description => "Example app. Will flag goto statements");
begin
   App.Run;
end Main;
