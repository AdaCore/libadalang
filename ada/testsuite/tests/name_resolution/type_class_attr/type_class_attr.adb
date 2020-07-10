with System.Aux_DEC; use System.Aux_DEC;

procedure Type_Class_Attr is
   procedure Foo (A : Boolean) is null;
   procedure Foo (A : Type_Class) is null;
begin
   Foo (Boolean'Type_Class);
   pragma Test_Statement_UID;
end Type_Class_Attr;
