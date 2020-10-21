procedure Main is
   type My_Float is digits 10;
   type My_New_Float is new My_Float;
   subtype My_Sub_Float is My_New_Float range 0.0 .. 1.0;

   type My_Int is range 0 .. 10;
   type My_New_Int is new My_Int;
   subtype My_Sub_Int is My_New_Int range 0 .. 10;
begin
   null;
end Main;
