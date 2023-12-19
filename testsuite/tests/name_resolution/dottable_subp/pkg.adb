with Foo; use Foo;

package body Pkg is

   type Slot_Record_T is new Foo.Record_T with record
      Dummy : Integer;
   end record;
   type Slot_T is access all Slot_Record_T;

   type Record_T is tagged record
      Slot : Slot_T := null;
   end record;

   procedure P (X : access Record_T'Class; B : Boolean := False);

   procedure P (X : access Record_T'Class; B : Boolean := False) is
   begin
     X.Slot.Init (Spacing => 5);
     pragma Test_Statement;
   end P;

end Pkg;
