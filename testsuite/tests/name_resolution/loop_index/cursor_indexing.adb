procedure Cursor_Indexing is
   package One_Dimensional is
      type Cursor is array (1 .. 2) of Natural;
      type Seq is null record
      with
        Iterable =>
          (First       => First,
           Next        => Next,
           Has_Element => Has_Element,
           Element     => Element);
      function First (S : Seq) return Cursor
      is ((others => 1));
      function Next (S : Seq; C : Cursor) return Cursor
      is (C);
      function Has_Element (S : Seq; C : Cursor) return Boolean
      is (True);
      function Element (S : Seq; C : Cursor) return Integer
      is (1);
   end One_Dimensional;

   package Two_Dimensional is
      type Cursor is array (1 .. 2, 1 .. 2) of Natural;
      type Seq is null record
      with
        Iterable =>
          (First       => First,
           Next        => Next,
           Has_Element => Has_Element,
           Element     => Element);
      function First (S : Seq) return Cursor
      is ((others => (others => 1)));
      function Next (S : Seq; C : Cursor) return Cursor
      is (C);
      function Has_Element (S : Seq; C : Cursor) return Boolean
      is (True);
      function Element (S : Seq; C : Cursor) return Integer
      is (1);
   end Two_Dimensional;

   Q1 : One_Dimensional.Seq;
   Q2 : Two_Dimensional.Seq;
   D  : Integer := 1;
begin
   for E of Q1 loop
      --  Both suffixes index the cursor; unlike dimensions, indices need not
      --  be static.
      pragma Assert (E'Loop_Index (1) = E'Loop_Index (D));
      pragma Test_Statement;
   end loop;

   for E of Q2 loop
      --  Multiple cursor indices are legal; multiple dimension operands aren't
      pragma Assert (E'Loop_Index (1, D) = 1);
      pragma Test_Statement;
   end loop;
end Cursor_Indexing;
