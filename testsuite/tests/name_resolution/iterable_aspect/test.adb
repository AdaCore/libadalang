with Ada.Text_IO; use Ada.Text_IO;

procedure Test is

   subtype Kursor is Integer;
   subtype Element_Type is Character;
   type My_String is array (Kursor range <>) of Element_Type;

   type List is record S : My_String (-3 .. 2); end record with
     Iterable => (First       => First_Kursor,
                  Next        => Advance,
                  Has_Element => Kursor_Has_Element,
                  Element     => Get_Element, -- optional
                  Last        => Last_Kursor, -- optional
                  Previous    => Retreat); -- optional

   function First_Kursor (L : List) return Kursor;
   function Last_Kursor  (L : List) return Kursor;
   function Advance (L : List; Position : Kursor) return Kursor;
   function Retreat (L : List; Position : Kursor) return Kursor;
   function Kursor_Has_Element (L : List; Position : Kursor) return Boolean;
   function Get_Element (L : List; Position : Kursor) return Element_Type;

   function First_Kursor (L : List) return Kursor is
   begin
      return Kursor (L.S'First);
   end First_Kursor;

   function Last_Kursor (L : List) return Kursor is
   begin
      return Kursor (L.S'Last);
   end Last_Kursor;

   function Advance (L : List; Position : Kursor) return Kursor is
   begin
      return Position + 1;
   end Advance;

   function Retreat (L : List; Position : Kursor) return Kursor is
   begin
      return Position -1;
   end Retreat;

   function Kursor_Has_Element (L : List; Position : Kursor) return Boolean is
   begin
      return Position >= First_Kursor (L) and Position <= Last_Kursor (L);
   end Kursor_Has_Element;

   function Get_Element (L : List; Position : Kursor) return Element_Type is
   begin
      return L.S (Integer (Position));
   end Get_Element;

   My_List : List := (S => "Wouhou");

begin

   for E of My_List loop
     Put_Line (E'Image);
   end loop;
   pragma Test_Block;

   for E in My_List loop
     Put_Line (Get_Element (My_List, E)'Image);
   end loop;
   pragma Test_Block;

   for E of reverse My_List loop
      Put_Line (E'Image);
   end loop;
   pragma Test_Block;

   for E in reverse My_List loop
      Put_Line (Get_Element (My_List, E)'Image);
   end loop;
   pragma Test_Block;

end Test;
