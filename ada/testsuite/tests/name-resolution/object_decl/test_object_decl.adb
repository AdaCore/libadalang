procedure Test_Object_Decl is
   type Int is range 1 .. 100;

begin
   declare
      A : Int := 100;
   begin
      null;
   end;
   pragma Test_Block;
end Test_Object_Decl;
