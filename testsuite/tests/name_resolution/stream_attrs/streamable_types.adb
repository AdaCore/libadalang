with Ada.Streams; use Ada.Streams;
with Ada.Streams.Stream_IO;

package body Streamable_Types is
   procedure Main is
      X : Int;
      F : Ada.Streams.Stream_IO.File_Type;
      S : Ada.Streams.Stream_IO.Stream_Access;
   begin
      Int'Output (S, X);
      pragma Test_Statement;

      Int'Write (S, X);
      pragma Test_Statement;

      X := Int'Input (S);
      pragma Test_Statement;
   end Main;
end Streamable_Types;
