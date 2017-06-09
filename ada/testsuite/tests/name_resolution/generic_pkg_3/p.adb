package body P is

   pragma Test (Foo);

   function Foo return T is
      V : T;
   begin
      return V;
   end Foo;

end P;
