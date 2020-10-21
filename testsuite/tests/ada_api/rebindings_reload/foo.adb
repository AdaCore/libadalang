with Options;
with Vectors;

procedure Foo is
   package Int_Options is new Options (Integer);

   V : Int_Options.Option_Vectors.Vector;
begin
   Int_Options.Option_Vectors.Clear (V);
end Foo;
