procedure Test is

   pragma Compile_Time_Error (not Test'Library_Level, "Error");
   pragma Test_Statement;

   package P is
      Library_Level : Boolean := P'Library_Level;
      pragma Test_Statement;
   end P;

   package body P is
   end P;

   generic
      type T is private;
   package Q is
      pragma Compile_Time_Error (not Q'Library_Level,
         "Q can only be instantiated at library level");
      pragma Test_Statement;

      procedure P (X : T);
   end Q;

   package body Q is
      procedure P (X : T) is null;
   end Q;

begin
   null;
end Test;
