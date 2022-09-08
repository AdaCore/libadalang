with Ada.Text_IO; use Ada.Text_IO;
procedure Test is
   package P is
      type Object is tagged private;

      type Sub_Object is new Object with private;

      function Kind (O : access Object) return Integer is (1);

      procedure Main;
   private
      type Object is tagged record
         Kind : Integer;
      end record;

      type Sub_Object is new Object with null record;
   end P;

   package body P is
      procedure Main is
         X : Sub_Object;
      begin
         X.Kind := 41;
         pragma Test_Statement;
         Put_Line (X.Kind'Image);
         pragma Test_Statement;
      end Main;
   end P;
begin
   P.Main;
end Test;
