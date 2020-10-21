package Foo is
   Single_Digit_1 : constant := 0;
   Single_Digit_2 : constant := 1;
   Single_Digit_3 : constant := 1_2;

   Exponent_1 : constant := 2e1;
   Exponent_2 : constant := 2e+1;
   Exponent_3 : constant := 2e10;
   Exponent_4 : constant := 2e-10; --  Invalid
   Exponent_5 : constant := 2e1_0;

   Base2_1 : constant := 2#10#;
   Base2_2 : constant := 02#10#;
   Base2_3 : constant := 0_2#10#;
   Base2_4 : constant := 2#1010_1111#;
   Base2_5 : constant := 2:1010_1111:;
   Base2_6 : constant := 2#10#E20;
   Base2_7 : constant := 2#2#; --  Invalid

   Base7_1  : constant := 7#10#;
   Base16_1 : constant := 16#10#;
   Base16_2 : constant := 16#1f#;
   Base16_3 : constant := 16#Ff#;

   Base17_1 : constant := 17#10#; --  Invalid

   Base_OOR_1 : constant := 100000000000000000000#10#; --  Rejected, too big

end Foo;
