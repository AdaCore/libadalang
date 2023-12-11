with Ada.Strings.Text_Buffers;
with Ada.Text_IO;

procedure Test is
   X : Integer := 42;
   Y : String := X'Image;
   --% typ = node.f_default_expr.f_prefix.p_expression_type
   --% subp = typ.p_attribute_subprogram("image")
   --% spec = node.f_default_expr.p_called_subp_spec
   --% subp.p_subp_spec_or_null() == spec

   package Pkg is
      type Custom_Image is null record
         with Put_Image => Img;

      procedure Img
        (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Arg    : in Custom_Image);
   end Pkg;

   package body Pkg is
      procedure Img
        (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
         Arg    : in Custom_Image) is
      begin
         null;
      end Img;
   end Pkg;

   V : Pkg.Custom_Image;
   --% typ = node.f_type_expr.p_designated_type_decl
   --% subp = typ.p_attribute_subprogram("put_image")
   --% non_existant = typ.p_attribute_subprogram("non_existant")
begin
   Ada.Text_IO.Put_Line (V'Image);
end Test;
