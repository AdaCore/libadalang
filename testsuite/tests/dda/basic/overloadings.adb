package body Overloadings is

   -------
   -- P --
   -------

   procedure P (B : Boolean) is
      R : constant Boolean := not B;
   begin
      null;
   end P;

   -------
   -- P --
   -------

   procedure P (I : Integer) is
      subtype R is String (1 .. I);
   begin
      raise Program_Error with (R'Range => 'A');
   end P;

   -------
   -- P --
   -------

   procedure P (C : Character) is
      R : constant String := (1 .. 10 => C);
   begin
      null;
   end P;

end Overloadings;
