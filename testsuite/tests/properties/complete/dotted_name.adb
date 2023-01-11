procedure Dotted_Name is

   package MI is

      type My_Int (D : Boolean) is tagged record
         A, B : Integer;
      end record;

      procedure Do_Nothing (Obj : My_Int; A : Integer; B : Integer) is null;
      procedure Do_Some (Obj : My_Int) is null;

      procedure Do_My_Best (Obj: My_Int);

   end MI;

   package body MI is

      procedure Do_My_Best (Obj: My_Int) is
      begin
         null;
      end Do_My_Best;

   end MI;

   Obj : MI.My_Int (True);
begin
   Obj.
   --% list(node.p_complete)
