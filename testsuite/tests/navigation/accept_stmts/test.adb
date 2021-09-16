procedure Test is
   task T is
      entry Start;

      entry Foo (X : Integer);
      entry Foo (X : Float);
   end T;

   task body T is
   begin
      accept Start do
         null;
      end Start;
      --% node.p_corresponding_entry()

      select
         accept Foo (X : Float) do
            null;
         end Foo;
         --% node.p_corresponding_entry()

         accept Foo (X : Integer) do
            null;
         end Foo;
         --% node.p_corresponding_entry()
      end select;
   end T;

   task U is
      entry Start;
   end U;

   task body U is separate;
begin
   null;
end Test;
