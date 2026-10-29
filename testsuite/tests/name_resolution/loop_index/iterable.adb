procedure Iterable is
   package P is
      type Cursor is new Natural;

      type Seq is record
         Data : String (1 .. 4);
      end record
      with
        Iterable =>
          (First       => First,
           Next        => Next,
           Has_Element => Has_Element,
           Element     => Element);

      function First (S : Seq) return Cursor
      is (1);
      function Next (S : Seq; C : Cursor) return Cursor
      is (C + 1);
      function Has_Element (S : Seq; C : Cursor) return Boolean
      is (Natural (C) in S.Data'Range);
      function Element (S : Seq; C : Cursor) return Character
      is (S.Data (Natural (C)));

      type Rec_Cursor is record
         Pos : Natural;
      end record;

      type Rec_Seq is record
         Data : String (1 .. 4);
      end record
      with
        Iterable =>
          (First       => R_First,
           Next        => R_Next,
           Has_Element => R_Has_Element,
           Element     => R_Element);

      function R_First (S : Rec_Seq) return Rec_Cursor
      is ((Pos => 1));
      function R_Next (S : Rec_Seq; C : Rec_Cursor) return Rec_Cursor
      is ((Pos => C.Pos + 1));
      function R_Has_Element (S : Rec_Seq; C : Rec_Cursor) return Boolean
      is (C.Pos in S.Data'Range);
      function R_Element (S : Rec_Seq; C : Rec_Cursor) return Character
      is (S.Data (C.Pos));
   end P;
   use P;

   Q  : constant Seq := (Data => "data");
   RQ : constant Rec_Seq := (Data => "data");
begin
   --  The type of Loop_Index is the cursor type of the Iterable aspect

   for E of Q loop
      pragma Assert (Element (Q, E'Loop_Index) = E);
      pragma Test_Statement;
   end loop;

   --  Dotted access on a record cursor type

   for E of RQ loop
      pragma Assert (E'Loop_Index.Pos in 1 .. 4);
      pragma Test_Statement;
   end loop;
end Iterable;
