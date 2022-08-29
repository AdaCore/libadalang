with Ada.Containers.Vectors;

with Ada.Streams; use Ada.Streams;

procedure Test is

   Block_Size : constant Stream_Element_Offset := 256;
   type SEA_Block is array (1 .. Block_Size) of Stream_Element;

   package P_SEA_Block_Vector is new Ada.Containers.Vectors
     (Index_Type => Positive, Element_Type => SEA_Block);

   type Memory_Stream_Type is abstract new Root_Stream_Type with record
      Blocks               : P_SEA_Block_Vector.Vector;
      Current_Block        : Positive := 1;
   end record;

   procedure Write
     (Stream : in out Memory_Stream_Type;
      Item   : in Stream_Element_Array)
   is
      procedure Fill_Block (Offset : Stream_Element_Offset;
                            SEA    : Stream_Element_Array) is
      begin
         for I in SEA'Range loop
            Stream.Blocks (Stream.Current_Block) (Offset + I - SEA'First)
              := SEA (I);
            pragma Test_Statement;
         end loop;
      end Fill_Block;
   begin
      null;
   end Write;
begin
   null;
end Test;
