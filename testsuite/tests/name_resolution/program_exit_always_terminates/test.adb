pragma Assertion_Level (L1);
procedure Test is
   --  Simple boolean form
   procedure Foo
      with Program_Exit => True;
   pragma Test_Block;

   procedure Bar
      with Always_Terminates => True;
   pragma Test_Block;

   --  Assertion level form: (Level => predicate)
   procedure Baz
      with Program_Exit => (L1 => True);
   pragma Test_Block;

   procedure Qux
      with Always_Terminates => (L1 => True);
   pragma Test_Block;
   --  Bare form: no expression (defaults to True)
   procedure Corge
      with Always_Terminates;
   pragma Test_Block;

begin
   null;
end Test;
