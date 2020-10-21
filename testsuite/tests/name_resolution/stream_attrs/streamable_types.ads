with Ada.Streams;

package Streamable_Types is
  use Ada;

  type Int is new  Integer;

  procedure Print (Stream : not null access Streams.Root_Stream_Type'Class;
                   Item   : Int);

  procedure Parse (Stream : not null access Streams.Root_Stream_Type'Class;
                   Item   : out Int);

  procedure Output (Stream : not null access Streams.Root_Stream_Type'Class;
                   Item   : Int);

  for Int'Read use Parse;
  pragma Test_Statement;

  for Int'Write use Print;
  pragma Test_Statement;

  for Int'Output use Output;
  pragma Test_Statement;

  Parsing_Error : exception;
end Streamable_Types;
