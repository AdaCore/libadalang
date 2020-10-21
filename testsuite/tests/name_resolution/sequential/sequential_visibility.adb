procedure Sequential_Visibility is
   type Int1 is range 1 .. 100;
   type Int2 is range 1 .. 10;
   B : Int1 := 9;

   function Foo (I : Int1) return Int1 is (I);
begin
   declare
      C : Int1 := Foo (B);
      --  What we want to test is that the above resolves to the outer
      --  definition of B, not the following one, according to sequential
      --  semantics of object declarations.
      B : Int1 := C;
   begin
      null;
   end;
   pragma Test_Block;
end Sequential_Visibility;
