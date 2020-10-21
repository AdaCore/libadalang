with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;

procedure Test is
   package EF is
      function "**" (L, R : Float) return Float;
      function Lol (L, R : Float) return Float;
   end EF;

   --  package  EF renames Ada.Numerics.Elementary_Functions;
   Arg : Float := 123.44444;
   use Ada.Numerics;
   F : Float;
begin
   F := EF."**"(e, 122.4);
   pragma Test_Statement_UID;
end Test;
