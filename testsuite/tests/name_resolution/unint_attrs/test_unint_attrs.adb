procedure Test_Unint_Attrs is
   type R is record
      A, B, C : Integer;
   end record;
   Inst : R;

   type Arr is array (Positive range <>) of Positive;

   type Volt is delta 0.125 range 0.0 .. 255.0;

   type M is mod 2 ** 16;

   type E_Type is (E_X, E_Y, E_Z);
begin
   declare
      A  : constant Natural := Float'Machine_Mantissa;
      B  : constant Natural := Float'Mantissa;
      C  : constant Natural := Float'Model_Mantissa;
      D  : constant Integer := Inst.C'Position;
      D1 : constant Integer := Inst.C'First_Bit;
      D2 : constant Integer := Inst.C'Last_Bit;
      E  : constant Integer := Arr'Component_Size;
      F  : constant Integer := Integer'Width;
      G  : constant Integer := Float'Digits;
      H  : constant Integer := Volt'Aft;
      I  : constant Integer := Volt'Fore;
      J  : constant Natural := M'Modulus;
      K  : constant Natural := Standard'Address_Size;
      L  : constant Natural := Standard'System_Allocator_Alignment;
      M  : constant Natural := Inst'Finalization_Size;
      N  : constant Natural := R'Descriptor_Size;
      O  : constant Natural := Inst'Alignment;
      P  : constant Natural := Standard'Word_Size;
      Q  : constant Natural := Standard'Max_Integer_Size;
      R  : constant Natural := Standard'Maximum_Alignment;
      S  : constant Natural := Standard'Default_Bit_Order;
      T  : constant Natural := E_X'Enum_Rep;
      U  : constant Natural := Positive'Range_Length;
      V  : constant Natural := Standard'Storage_Unit;
      W  : constant Natural := Standard'Wchar_T_Size;
   begin
      null;
   end;
   pragma Test_Block;
end Test_Unint_Attrs;
