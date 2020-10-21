procedure Cusbol is
   type My_Bool is new Boolean;

   A : My_Bool := True;
begin

   if A then
      null;
   end if;
   pragma Test_Statement;

end Cusbol;
