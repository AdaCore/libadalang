with Ada.Streams; use Ada.Streams;

procedure Test is
   type T is new Boolean;

   type D1 is new T;
   type D2 is new D1;

   procedure Write
     (Stream : access Root_Stream_Type'Class; Item : D1'Base) is null;
   procedure Write (Stream : access Root_Stream_Type'Class; Item : D2) is null;
   procedure Write
     (Stream : access Root_Stream_Type'Class; Item : Float) is null;

   for D1'Write use Write;
   pragma Test_Statement;
   for D2'Write use Write;
   pragma Test_Statement;
begin
   null;
end Test;
