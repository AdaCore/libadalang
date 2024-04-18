procedure Test is
   generic
   package Common is
      generic
         type T is private;
      package Shared_G is
      end Shared_G;

      generic
         with package Shared is new Shared_G (<>);
         with procedure Foo (X : Shared.T);
      package Operation is
         use Shared;

         procedure Bar (X : T);
      end Operation;
   end Common;

   package body Common is
      package body Operation is
         procedure Bar (X : T) is
         begin
            Foo (X);
         end Bar;
      end Operation;
   end Common;

   procedure F (X : Integer) is null;

   package C is new Common;
   package S is new C.Shared_G (Integer);
   package O is new C.Operation (S, F);
   pragma Test_Statement;
begin
   null;
end Test;

