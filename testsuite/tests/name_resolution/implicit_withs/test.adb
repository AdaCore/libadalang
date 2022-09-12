with System;

procedure Test is
   --  Test that `Ada.Task_Identification` is correctly loaded
   task type Task_T;
   task body Task_T is
   begin
      null;
   end Task_T;

   X : Task_T;
   A : Boolean := (X'Identity = X'Identity);
   pragma Test_Statement_UID;

   --  Test that `Ada.Tags` is correctly loaded
   type Tagged_T is tagged null record;

   Y : Tagged_T'Class := Tagged_T'(null record);
   B : constant Boolean := (Y'Tag = Y'Tag);
   pragma Test_Statement_UID;
begin
   raise Constraint_Error;
exception
   -- Test that `Ada.Exceptions` is correctly loaded
   when Exc : Constraint_Error =>
      declare
         Dummy : constant System.Address := Exc'Address;
         pragma Test_Statement_UID;
      begin
         null;
      end;
end Test;
