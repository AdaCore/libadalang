procedure Test is
   package P is
      type T1 is private;
      --% node.p_get_aspect('String_Literal')
      --% node.p_get_aspect('String_Literal', True)

      type T2 is private with String_Literal => SLT2;
      --% node.p_get_aspect('String_Literal')
      --% node.p_get_aspect('String_Literal', True)
      function SLT2 (X : Wide_Wide_String) return T2;
   private
      type T1 is null record with String_Literal => SLT1;
      --% node.p_get_aspect('String_Literal')
      --% node.p_get_aspect('String_Literal', True)
      function SLT1 (X : Wide_Wide_String) return T1;

      type T2 is null record;
      --% node.p_get_aspect('String_Literal')
      --% node.p_get_aspect('String_Literal', True)
   end P;

   package body P is
      GT1 : T1;
      GT2 : T2;

      function SLT1 (X : Wide_Wide_String) return T1 is (GT1);
      function SLT2 (X : Wide_Wide_String) return T2 is (GT2);

      type T3 is new T2;
      --% node.p_get_aspect('String_Literal')
      --% node.p_get_aspect('String_Literal', True)
   end P;
begin
   null;
end Test;
