procedure Test is
   type Sequence is array (Natural range <>) of Integer;

   function Foo (S1, S2 : Sequence) return Boolean is
   begin
      return
        (for all K in S1'Range => S1(K) > 0) and then
        (for some E of S2 => E < 0);
      pragma Test_Statement;
   end Foo;

   procedure Bar (S : Sequence) is
      E : Integer;
   begin
      --  The `E` defined below should not leak in the outer scope
      pragma Assert (for all E of S => True);
      --  So the references to `E` below should refer to the object declared
      --  in the procedure.
      E := E + 1;
      pragma Test_Statement;
   end Bar;
begin
   null;
end Test;
