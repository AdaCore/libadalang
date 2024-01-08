procedure Test is

   package Pkg is
      type T is tagged record
         I : Integer := 34;
      end record;

      function Get (X : T) return Integer;
      procedure Set (X    : in out T; V : Integer);
      procedure Set (X, Y : in out T; V : Integer);
   end Pkg;

   package body Pkg is
      function Get (X : T) return Integer is (X.I);
      procedure Set (X : in out T; V : Integer) is
      begin
         X.I := V;
      end Set;
      procedure Set (X, Y : in out T; V : Integer) is
      begin
         X.I := V;
         Y.I := V;
      end Set;

     procedure My_Set (A : in out T; X : Integer) renames Set;
     pragma Test_Statement;

   end Pkg;

   Obj : Pkg.T;

   function My_Get return Integer renames Obj.Get;
   pragma Test_Statement;

   procedure My_Set (X : Integer) renames Obj.Set;
   pragma Test_Statement;
begin
   null;
end Test;
