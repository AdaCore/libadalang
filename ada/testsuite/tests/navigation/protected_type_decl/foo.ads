package Foo is
   type T is limited private;

private

   protected type T is
      procedure Test;
   private
      X : Integer := 0;
   end T;

end Foo;
