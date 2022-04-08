procedure Test is
   type Access_Proc is access procedure (Flag : Boolean);

   function Func return Access_Proc is (null);

   X : Access_Proc;
begin
   X := Func.all'Access;
   pragma Test_Statement;
end Test;
