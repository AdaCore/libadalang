with Test_Pkg;
pragma Test_Statement;

separate (Test_Sep)
procedure Bar is
   B : Integer := A + 12;
begin
   Test_Pkg.Pouet (B);
   pragma Test_Statement;
end Bar;
