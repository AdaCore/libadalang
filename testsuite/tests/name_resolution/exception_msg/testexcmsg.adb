procedure Testexcmsg is
begin
   raise Constraint_Error with "Pouet" & Integer'Image (12);
   pragma Test_Statement;
end Testexcmsg;
