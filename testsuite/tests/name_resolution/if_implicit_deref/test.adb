with Ada.Containers.Vectors;

procedure Test is
   package My_Vectors is new Ada.Containers.Vectors (Positive, Boolean);
   use My_Vectors;

   V : My_Vectors.Vector;
begin
   if V (1) then
      null;
   end if;
   pragma Test_Statement_UID;
end Test;
