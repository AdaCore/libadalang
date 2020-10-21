package body Foo is

   procedure Put_Line (S : String) is
   begin
      null;
   end Put_Line;

   procedure Print_Int_Gen (I : Integer) is
   begin
      Put_Line (Integer'Image (I));
   end Print_Int_Gen;

end Foo;
