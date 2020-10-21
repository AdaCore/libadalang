package body B is

   procedure Boo is null;

   package C is
      procedure Foo;

      package D is
         procedure Bar;
      end D;
   private
      type Pouet is (A1, A2, A3, A4);
   end C;


   package body C is separate;
end B;
