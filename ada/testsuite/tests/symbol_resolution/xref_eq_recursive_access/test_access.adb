procedure Test_Access is
   type Int is range 0 .. 1000000000;

   type Linked_List_Record;
   type Linked_List is access all Linked_List_Record;

   type Linked_List_Record is record
      A    : Int;
      Next : Linked_List;
   end record;

   R : Linked_List_Record;
   I : Int;
begin
   R.Next.Next.all.Next.A := I;
   pragma Test_Statement;
end Test_Access;
