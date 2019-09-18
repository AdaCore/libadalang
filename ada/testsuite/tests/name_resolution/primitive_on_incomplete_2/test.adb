package body Test is
   type T is tagged null record;

   procedure Foo (X : access T'Class) is null;
end Test;
