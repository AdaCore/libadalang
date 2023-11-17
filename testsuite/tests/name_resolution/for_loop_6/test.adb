with Ada.Containers.Hashed_Maps;

procedure Test is
   function Hash (K : Integer) return Ada.Containers.Hash_Type is (0);

   package My_Maps is new Ada.Containers.Hashed_Maps
     (Integer, Integer, Hash, "=", "=");

   use My_Maps;

   V : Map;
begin
   for It in V.Iterate when Key (It) > 2 loop
      null;
   end loop;
   pragma Test_Block;
end Test;
