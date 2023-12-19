procedure Test_Invalid is
   type My_Bool is new Boolean;

   --  Preconditions and contract aspects in general do not accept bool type
   --  derivations, so nameres should fail.
   function Foo (X, Y : My_Bool) return My_Bool is (X)
      with Pre => X and not Y,
           Contract_Cases => (X and not Y => X and not Y,
                              others => X and not Y);
   pragma Test_Block;

   pragma Post (Foo, X and not Y);
   pragma Test_Statement;

   procedure Bar is null;

   X, Y : My_Bool;
   Z : Boolean;
begin
   --  Only bool types are expected for the condition of a ``Debug`` pragma
   pragma Debug (X or Y, Bar);
   pragma Test_Statement;

   --  Iterator filters seem to only accept standard booleans
   for K in 1 .. 3 when X and not Y loop
      null;
   end loop;
   pragma Test_Block;

   Z := (for all K in 1 ..3 => X and not Y);
   pragma Test_Statement;
end Test_Invalid;
