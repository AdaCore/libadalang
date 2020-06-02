procedure Test is
   type T is new Integer;
   type T_Access is access all T;

   function Foo (X : Integer) return T_Access;
   function Foo (X : Integer) return T;

   X : Integer;
begin
   X := Integer (Foo (42).all);
   pragma Test_Statement;
end Test;
