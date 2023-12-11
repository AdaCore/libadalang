procedure Ptask is
   package Pkg is
      task type Parent is
         entry E (I : in out Integer);
      end Parent;

      function Id (X : Parent) return Integer;
   end Pkg;

   use Pkg;

   type T is new Parent;

   X : T;

   package body Pkg is
      task body Parent is
      begin
         null;
      end Parent;

      function Id (X : Parent) return Integer is (4);
   end pkg;

   I : Integer := 8;
begin
   I := Id (X);
   pragma Test_Statement;
end Ptask;
