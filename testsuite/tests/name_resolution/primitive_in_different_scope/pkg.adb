package body Pkg is

   procedure Body_Prim (X : access T'Class) is
   begin
      null;
   end Body_Prim;

   procedure Classic_Prim (X : access T'Class) is
   begin
      X.Private_Prim;
      pragma Test_Statement;
   end Class_Prim;

   procedure Private_Prim (X : access T'Class) is
   begin
      X.Body_Prim;
      pragma Test_Statement;
   end Private_Prim;

end Pkg;
