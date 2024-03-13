procedure Private_View is
   package P is
      type Sequence is private with
        Iterable => (First       => Iter_First,
                     Has_Element => Iter_Has_Element,
                     Next        => Iter_Next);

      function Iter_First (Container : Sequence) return Integer;
      function Iter_Has_Element
        (Container : Sequence;
         Position  : Integer) return Boolean;
      function Iter_Next
        (Container : Sequence;
         Position  : Integer) return Integer;
   private

      type Sequence is record
         Content : String (1 .. 10);
      end record;

      function Iter_First (Container : Sequence) return Integer is
        (Container.Content'First);
      function Iter_Has_Element
        (Container : Sequence;
         Position  : Integer) return Boolean is
         (Position < Container.Content'Last);
      function Iter_Next
        (Container : Sequence;
         Position  : Integer) return Integer is (Position + 1);
   end P;

   package body P is
      function Ident (I : Integer) return Boolean is (True);

      --  Below, Sequence refers to the private view of Sequence, which has no
      --  aspect Iterable. `p_iterable_cursor_type` should also look for the
      --  Iterable aspect on the public part too.
      function "<" (Left : Sequence; Right : Sequence) return Boolean is
        (for all N in Left => Ident (N));
         pragma Test_Statement;

      type New_Sequence is new Sequence;

      --  Below, New_Sequence refers to the type declared above, inheriting the
      --  Iterable aspect from Sequence. `p_iterable_cursor_type` should also
      --  look for the Iterable aspect on the parent types.
      function "<" (Left : New_Sequence; Right : New_Sequence) return Boolean
      is (for all N in Left => Ident (N));
          pragma Test_Statement;
   end P;
begin
   null;
end Private_View;
