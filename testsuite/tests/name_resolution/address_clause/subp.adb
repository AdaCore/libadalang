with System;

procedure Subp is
   type T is record
      A : System.Address;
      B : Float;
   end record;

   procedure P is
      X : T;
   begin
      declare
         procedure C (H : Integer);
         pragma Import (Ada, C);

         for C'Address use X.A;
         pragma Test_Statement;
      begin
         null;
      end;
   end P;
begin
   null;
end Subp;
