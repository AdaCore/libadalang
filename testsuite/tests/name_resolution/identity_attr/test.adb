with Ada.Exceptions; use Ada.Exceptions;
with Ada.Task_Identification; use Ada.Task_Identification;

procedure Test is
   task T;
   task body T is
   begin
      null;
   end T;

   task type TT;
   task body TT is
   begin
      null;
   end TT;

   X : TT;
begin
   if T'Identity = X'Identity then
      null;
   end if;
   pragma Test_Statement_UID;

   if Constraint_Error'Identity = Constraint_Error'Identity then
      null;
   end if;
   pragma Test_Statement_UID;
end Test;
