procedure Test is
   type Integer is range 1 .. 1000;
   type Int_Access is access all Integer;
   IA : Int_Access := null;
   AI : access Integer := null;
begin
   IA := AI;
   pragma Test_Statement;

   AI := IA;
   pragma Test_Statement;
end Test;
