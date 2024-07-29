procedure Invalid is
   subtype Kursor is Integer;
   subtype Element_Type is Character;
   type My_String is array (Kursor range <>) of Element_Type;

   --  Type List doesn't provide the Element primitive in Iterable aspect. The
   --  ``for .. of`` loop kind is therefore not supported.

   type List is record S : My_String (-3 .. 2); end record with
     Iterable => (First       => First_Kursor,
                  Next        => Advance,
                  Has_Element => Kursor_Has_Element);

   function First_Kursor (L : List) return Kursor;
   function Advance (L : List; Position : Kursor) return Kursor;
   function Kursor_Has_Element (L : List; Position : Kursor) return Boolean;

   function First_Kursor (L : List) return Kursor is
   begin
      return Kursor (L.S'First);
   end First_Kursor;

   function Advance (L : List; Position : Kursor) return Kursor is
   begin
      return Position + 1;
   end Advance;

   function Kursor_Has_Element (L : List; Position : Kursor) return Boolean is
   begin
      return Position >= First_Kursor (L);
   end Kursor_Has_Element;

   My_List : List := (S => "Wouhou");

   procedure My_Put_Line (S : String) is null;
begin
   -- This is illegal Ada code, see comment above

   for E of My_List loop
     My_Put_Line (E'Image);
   end loop;
   pragma Test_Block (Expect_Fail => True);
end;
