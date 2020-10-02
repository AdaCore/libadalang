procedure Test is
   package P is
      type T is range 1 .. 10;
   end P;

   generic
      with function F (X, Y : P.T) return P.T;
   procedure Foo;

   procedure Foo is null;

   procedure Bar is new Foo (F => P."+");
   pragma Test_Statement;
begin
   null;
end Test;
