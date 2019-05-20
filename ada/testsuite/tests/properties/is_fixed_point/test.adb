procedure Main is
   type My_Fixed_Point is delta 0.1 range 0.0 .. 10.0;
   type My_New_Fixed_Point is new My_Fixed_Point;
   subtype My_Sub_Fixed_Point is My_New_Fixed_Point range 0.0 .. 1.0;

   type My_Int is range 0 .. 10;
   type My_New_Int is new My_Int;
   subtype My_Sub_Int is My_New_Int range 0 .. 10;
begin
   null;
end Main;
