procedure Calls is
   package My_Vectors is new Ada.Containers.Vectors (Index_Type => Positive, Element_Type => Integer);
   function To_Float is new Ada.Unchecked_Conversion (Source => Integer, Target => Float);
begin
   Move (Origin, 1, 2);
   Move (P => Origin, DX => 1, DY => 2);
   Move (Origin, DX => 1, DY => 2);
   Ada.Text_IO.Put_Line (Item => "Hi");
   Total := Add (A => 1, B => 2) + Add (1, 2);
   Outer (Inner (X => 1, Y => 2), 3);
   pragma Assert (Check => Total > 0, Message => "positive");
end Calls;
