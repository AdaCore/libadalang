procedure Test is
   generic
      type Index_Type is range <>;
      type Array_Type is array (Index_Type) of Integer;
   procedure Foo (X : in out Array_Type; I : Index_Type);

   procedure Foo (X : in out Array_Type; I : Index_Type) is
   begin
      X (I) := 3;
   end Foo;

   type My_Index_Type is range 1 .. 1000;
   type My_Array_Type is array (My_Index_Type range <>) of Integer;

   subtype My_Index_Subtype is My_Index_Type range 2 .. 999;
   subtype My_Array_Subtype is My_Array_Type (My_Index_Subtype);

   procedure My_Foo is new Foo (My_Index_Subtype, My_Array_Subtype);
   pragma Test_Statement;

   My_Array : My_Array_Subtype := (others => 0);
begin
   My_Foo (My_Array, 2);
end Test;
