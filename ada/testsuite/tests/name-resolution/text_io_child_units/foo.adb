with Ada.Text_IO;

procedure Foo is
   pragma Config (Display_Short_Images => True);

   pragma Test (Ada.Text_IO.Integer_IO);
   pragma Test (Ada.Text_IO.Float_IO);
   pragma Test (Ada.Text_IO.Fixed_IO);
   pragma Test (Ada.Text_IO.Decimal_IO);
   pragma Test (Ada.Text_IO.Enumeration_IO);
begin
   null;
end Foo;
