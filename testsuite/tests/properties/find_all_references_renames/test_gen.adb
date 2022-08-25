procedure Test_Gen is
   generic
      I : Integer;
   package Generic_Pkg is
      function P (X : Integer) return Integer;
      pragma Find_All_References (Any, Follow_Renamings => True);
   end Generic_Pkg;

   package body Generic_Pkg is
      function P (X : Integer) return Integer
      is
      begin
         return I;
      end P;
      pragma Find_All_References (Any, Follow_Renamings => True);
   end Generic_Pkg;

   package Pkg is new Generic_Pkg (I => 42);

   function P_42 (X : Integer) return Integer renames Pkg.P;

   Ip : Integer := P_42 (34);
begin
   null;
end Test_Gen;
