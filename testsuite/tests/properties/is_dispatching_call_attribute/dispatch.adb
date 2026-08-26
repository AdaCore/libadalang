with Ada.Strings.Text_Buffers;
with Ada.Strings.Text_Buffers.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

procedure Dispatch is
   package Pkg is
      type T is tagged null record
         with Put_Image => Custom_Image;

      procedure Custom_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  : T);

      type U is new T with null record
         with Put_Image => Custom_Image;

      overriding procedure Custom_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  : U);
   end Pkg;

   package body Pkg is
      procedure Custom_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  : T)
      is
      begin
         Output.Put ("T");
      end Custom_Image;

      overriding procedure Custom_Image
        (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Value  : U)
      is
      begin
         Output.Put ("U");
      end Custom_Image;
   end Pkg;

   X      : constant Pkg.T'Class := Pkg.U'(null record);
   Buffer : Ada.Strings.Text_Buffers.Unbounded.Buffer_Type;

begin
   Pkg.T'Put_Image (Buffer, X);
   --% node.find(lal.CallExpr).p_is_dispatching_call()
   Put_Line (Buffer.Get);
end Dispatch;
