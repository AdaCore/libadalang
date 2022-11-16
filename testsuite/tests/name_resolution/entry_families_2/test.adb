procedure Test is
   type Enum1 is (A, B);
   type Enum2 is (A, B);

   task T is
      entry E (1 .. 1);
      entry F (Enum2);
   end T;

   task body T is
   begin
      accept E (1);
      --% node.p_corresponding_entry()

      accept F (A);
      --% node.p_corresponding_entry()
   end T;
begin
   T.E (1);
   --% node.f_call.p_referenced_decl()

   T.F (B);
   --% node.f_call.p_referenced_decl()
end Test;
