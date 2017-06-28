package body Foo is

   function To_String (I : Integer) return String is
   begin
      return I'Image;
   end To_String;

   procedure Print_T (Inst : T) is
   begin
      Put_Line (To_String (I));
   end Print_Int_Gen;

end Foo;
