package body Foo is

   procedure Put_Line (S : String) is
   begin
      null;
   end Put_Line;

   procedure Print_T (Inst : T) is
   begin
      Put_Line (To_String (I));
   end Print_Int_Gen;

end Foo;
