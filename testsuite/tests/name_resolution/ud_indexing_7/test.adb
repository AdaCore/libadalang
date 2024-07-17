procedure Test is
   package String_Vector is
      type Virtual_String_Vector is tagged private with
         Constant_Indexing => Element;

      function Element
        (Self  : Virtual_String_Vector'Class;
         Index : Positive) return Character;
   private
      type Virtual_String_Vector is tagged record
         Data : Character;
      end record;
   end String_Vector;

   package body String_Vector is
      function Element
        (Self  : Virtual_String_Vector'Class;
         Index : Positive) return Character is
      begin
         return 'x';
      end;
   end String_Vector;

   use String_Vector;


   procedure P (X : Virtual_String_Vector'Class) is
      C : Character;
   begin
      C := X (1);
      pragma Test_Statement;
   end P;
begin
   null;
end;
