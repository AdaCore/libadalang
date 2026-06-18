procedure Test is
   My_Exception : exception;

   --  Aspect form: guard is a Boolean expression, consequence is an
   --  exit kind identifier (SPARK keyword, no Ada declaration).
   procedure Foo (I : Integer)
      with Exit_Cases =>
        (I < 0  => Normal_Return,
         I > 5  => Exception_Raised,
         others => Program_Exit);
   pragma Test_Block;

   --  Aspect form: (Exception_Raised => exception_name) consequence;
   --  the exception name must be resolved.
   procedure Bar (I : Integer)
      with Exit_Cases =>
        (I = 0  => (Exception_Raised => My_Exception),
         others => Normal_Return);
   pragma Test_Block;

   --  Pragma form
   procedure Baz (I : Integer);
   pragma Exit_Cases
     ((I < 0 => Normal_Return));
   pragma Test_Statement;

begin
   null;
end Test;
