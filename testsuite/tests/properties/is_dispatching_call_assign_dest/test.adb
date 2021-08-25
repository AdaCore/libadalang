with Ada.Containers.Vectors;

procedure Test is
   package Int_Vectors is new Ada.Containers.Vectors (Positive, Integer);
   use Int_Vectors;

   X : Vector;
   O : Cursor;
begin
   Reference (X, O) := 2;
   --% node.f_dest.p_is_dispatching_call()
end Test;
