with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Globals is
   A, B : Integer;
   Result, Result2 : Integer;

   procedure Add
      with Global => (Input => (A, B), Output => Result),
           Depends => (Result => (A, B));
   pragma Test_Block;

   procedure Add is
   begin
      Result := A + B;
   end Add;

   procedure Add2
      with Global => (Input => (A, B), Output => (Result, Result2)),
           Depends => ((Result, Result2) => (A, B));
   pragma Test_Block;

   procedure Add2 is
   begin
      Result := A + B;
      Result2 := A + B;
   end Add2;

   procedure Whatever
      with Global  => (Input => A, In_Out => (B, Result)),
           Depends => ((B, Result) => +A);
   pragma Test_Block;

   procedure Whatever is
   begin
      null;
   end Whatever;

begin
   A := 12;
   B := 15;
   Add;
   Put_Line (Result'Image);
end Test_Globals;
