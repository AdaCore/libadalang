procedure Test is
   type Integer is range 1 .. 1000;
   type Int_Access is access all Integer;

   procedure Foo (A : access Integer) is null;
   procedure Bar (A : Int_Access) is null;

   IA : Int_Access := null;
   AI : access Integer := null;
begin
   Foo (IA);
   pragma Test_Statement;

   Bar (AI);
   pragma Test_Statement;
end Test;
