package Test is
private
   type T is tagged;

   procedure Foo (X : access T'Class);
end Test;
