procedure Testindexing is
   package P is
      type T is tagged null record
      with Variable_Indexing => Find_Ref;

      type Ref (Data : access Integer) is null record
      with Implicit_Dereference => Data;

      function Find (Self : T; Idx : Integer) return Integer;
      function Find_Ref (Self : T; Idx : Integer) return Ref;
   end P;

   package body P is
      function Find (Self : T; Idx : Integer) return Integer is (Idx);
      function Find_Ref (Self : T; Idx : Integer) return Ref
      is (Ref'(Data => new Integer'(Idx)));
   end P;

   use P;

   Inst : T;

   R : Ref := Ref'(Data => new Integer'(12));

   AA : Integer := R;

   A : Integer := Inst (12);
   pragma Test_Statement;
begin
   Inst (12) := A;
   pragma Test_Statement;
end Testindexing;
