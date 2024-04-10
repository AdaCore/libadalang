procedure Test is
   type Int is range 1 .. 110;
   function C (I : Int) return Int;

   B : Int;
begin
   B := C (12);
   pragma Test_Statement (Expect_Fail => True);
   -- Check nameres output assuming this statement resolution is expected to
   -- fail (this is not the case, it's just for testing purpose).

   B := C (12);
   pragma Test_Statement (Expect_Fial => True);
   -- Check that unsupported argument are rejected

   C (12);
   pragma Test_Statement (Expect_Fail => True);
   -- Check that this statement's resolution fail, and reported as expected

   C (12);
   pragma Test_Statement (Expect_Fail => False);
   -- Check that nameres report a failure when resolving this statement

   C (12);
   pragma Test_Statement_UID (Expect_Fail => True);
   -- Check that this statement's resolution fail, and reported as expected
end Test;
pragma Test_Block (Expect_Fail => True);
-- Check that Expect_Fail is not supported for pragma Test_Block
