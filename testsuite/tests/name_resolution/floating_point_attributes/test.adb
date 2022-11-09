procedure Test is
   Denorm       : constant Boolean := Float'Denorm;
   pragma Test_Statement;
   Signed_Zeros : constant Boolean := Float'Signed_Zeros;
   pragma Test_Statement;

   Model       : constant := Float'Model (1.0);
   pragma Test_Statement;
   Model_Emin  : constant := Float'Model_Emin;
   pragma Test_Statement;

   Unbiased_Rounding : constant := Float'Unbiased_Rounding (1.0);
   pragma Test_Statement;
   Leading_Part      : constant := Float'Leading_Part (1.0);
   pragma Test_Statement;
begin
   null;
end Test;
