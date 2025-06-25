procedure Record_Rep_Clause is

   O1 : Integer := 1;

   type T  is
      record
         C1, C2 : Integer;
         C3 : Boolean;
         C4 : Float;
      end record;

   O2 : Integer := 2;

   for T use
      record
         --% list(node.p_complete)
