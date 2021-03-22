with Ada.Containers.Vectors;

procedure Test is
   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   subtype Int_Vector is Int_Vectors.Vector;

   X : Int_Vector;
   Y : Integer := X (2);
   pragma Test_Statement_UID;
begin
   null;
end Test;
