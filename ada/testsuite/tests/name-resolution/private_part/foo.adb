package body Foo is
   procedure P is
      V : Object_Type'Class := Object_Type'(Value => <>);
   begin
      V.Value := Default_Value;
      pragma Test_Statement;
   end P;
end Foo;
