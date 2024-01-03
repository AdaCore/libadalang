with Pkg;

procedure Test is
   package My_Pkg is new Pkg;
   pragma Test_Statement;
begin
   null;
end Test;
