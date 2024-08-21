procedure Test is
   pragma Warnings (GNATprove, On, "a warning");
   pragma Test_Statement;
   pragma Warnings (GNAT, Off, "a warning");
   pragma Test_Statement;
   pragma Warnings (Off, "a warning");
   pragma Test_Statement;
   pragma Warnings (Off);
   pragma Test_Statement;

   procedure P;
   function F (I : Integer) return Boolean;
   pragma Warnings (GNAT, Off, P);
   pragma Test_Statement;
   pragma Warnings (On, F);
   pragma Test_Statement;

   procedure P is null;
   function F (I : Integer) return Boolean is (True);
begin
   null;
end Test;
