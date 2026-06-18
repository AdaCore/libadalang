procedure Test is
   My_Exception : exception;

   --  Aspect form: guards are exception names, consequence is a
   --  Boolean postcondition. Formal I must be in scope in consequences.
   procedure Foo (I : Integer)
      with Exceptional_Cases =>
        (My_Exception     => I > -100,
         Constraint_Error => False);
   pragma Test_Block;

   --  Aspect form: single others-guarded case
   procedure Bar (I : Integer)
      with Exceptional_Cases => (others => I > 0);
   pragma Test_Block;

   --  Pragma form
   procedure Baz (I : Integer);
   pragma Exceptional_Cases
     ((My_Exception => True));
   pragma Test_Statement;

begin
   null;
end Test;
