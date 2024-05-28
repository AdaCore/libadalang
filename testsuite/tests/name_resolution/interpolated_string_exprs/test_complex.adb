with Ada.Text_IO; use Ada.Text_IO;

pragma Extensions_Allowed (On);

procedure Test_Complex is
   type UDS is record
      A : Integer;
   end record
      with String_Literal => From_String;

   function From_String (Value : Wide_Wide_String) return UDS
   is (A => Value'Length);

   A : UDS := f"Hello {12}";
   pragma Test_Statement;

   procedure P (S : String := f"Hello {12}") is null;
   pragma Test_Block;

   generic
      A : Integer := 12;
      with procedure P (S : String := f"Hello {A}") is null;
      S : String := f"Hello {A}";
   package Gen is
   end Gen;
   pragma Test_Block;

   type Rec is record
      A, B : Integer;
   end record;

   SS : String := f"Hello {Rec'(12, 15)}";
   pragma Test_Statement;

   B  : Natural := String'(f"Hello {Rec'(15, 17)}")'Length;
   pragma Test_Statement;
begin
   null;
end Test_Complex;
