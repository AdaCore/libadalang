pragma Ada_2022;

with Pkg.Child;

procedure Test is
begin
   Pkg.I := 0;
   pragma Test_Statement;

   Pkg.Child.I := 0;
   pragma Test_Statement;
exception
   when Exc : others =>
      declare
         S : constant String := Exc'Image;
         pragma Test_Statement;
      begin
         null;
      end;
end Test;
