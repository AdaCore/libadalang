procedure Test is
   A : constant Character := 'l';
   B : constant Character := ASCII.NUL;

   E : constant String := "oiy";
   C : constant String := 'c' & "lol";
   D : constant String := C & "lol";
   F : constant String := A & D;
   G : constant String := "lol" & B;

   H : constant String := String ("lol" & G);

   I : constant String := "a" & "a" & "a";

   J : constant String := String ("lol" & G & "hihi");

   package With_Subtype is
      subtype T is String;

      K : constant T := "a" & "b";

      L : constant T := T (K & 'c' & K);

      M : constant T := T (K & K);
   end With_Subtype;
begin
   null;
end Test;
pragma Test_Block;
