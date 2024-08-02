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
         if C'Code_Address /= C'Address then
            raise;
         end if;
         pragma Test_Statement;
      end;
   end P;
begin
   null;
end Subp;
