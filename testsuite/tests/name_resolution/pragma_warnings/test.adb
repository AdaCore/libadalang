procedure Test is
   pragma Warnings (GNATprove, On, "a warning");
   pragma Test_Statement;
   pragma Warnings (GNAT, Off, "a warning");
   pragma Test_Statement;
   pragma Warnings (Off, "a warning");
   pragma Test_Statement;
   pragma Warnings (Off);
   pragma Test_Statement;
begin
   null;
end Test;
