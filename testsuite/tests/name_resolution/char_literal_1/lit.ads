--  style: non-ascii

package Lit is

   type Enum is ('A', 'B');

   C1 : constant Character := 'A';
   pragma Test_Statement;

   C2 : constant Character := 'Z';
   pragma Test_Statement;

   C3 : constant Character := C1;
   pragma Test_Statement;

   E1 : constant Enum := 'A';
   pragma Test_Statement;

   E2 : constant Enum := 'B';
   pragma Test_Statement;

   function F1 return Character renames 'A';
   pragma Test_Statement;

   function F2 return Enum renames 'A';
   pragma Test_Statement;

   function F3 return Character renames 'Z';
   pragma Test_Statement;

   function F4 return Enum renames 'B';
   pragma Test_Statement;

   R1 : Character renames C1;
   pragma Test_Statement;

   R2 : Enum renames E1;
   pragma Test_Statement;

   WC1 : constant Wide_Character := '⺎';
   pragma Test_Statement;

   WWC1 : constant Wide_Wide_Character := '𠀤';
   pragma Test_Statement;

   function Fwc1 return Wide_Character renames '⺎';
   pragma Test_Statement;

   function Fwwc1 return Wide_Wide_Character renames '𠀤';
   pragma Test_Statement;

end Lit;
