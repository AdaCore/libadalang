package Test is

   function Foo (A : Integer; B : Integer; C : Integer) return Positive;
   function Foo (A : Integer; B, C: Natural) return Natural;

   type A is (B, C, D);

end Test;
