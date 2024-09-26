pragma Extensions_Allowed (On);

with Ada.Strings.Text_Buffers;
with Ada.Text_IO;

procedure Test is
   type Source_Location is record
      Line   : Positive;
      Column : Positive;
   end record
     with Put_Image => My_Put_Image;

   procedure My_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Source_Location);

   procedure My_Put_Image
     (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
      Value  : Source_Location)
   is
      Line   : constant String := Value.Line'Image;
      Column : constant String := Value.Column'Image;
      Result : constant String :=
        Line (2 .. Line'Last) & ':' & Column (2 .. Column'Last);
   begin
       Output.Put (Result);
   end My_Put_Image;

   Line_10 : constant Source_Location := (Line => 10, Column => 1);

begin
   Ada.Text_IO.Put_Line (f"{Line_10} {Line_10.Line}");
end Test;
--% fstn = node.findall(lal.FormatStringChunk)
--% [n.p_image_subprogram for n in fstn]
