procedure Testinline is
   procedure Foo is
      pragma Inline (Foo);
   begin
      null;
   end Foo;

   package Test is
      procedure Bar;
   private
      pragma Inline (Bar);

      procedure Bar is null;
   end Test;

   protected type T is
      procedure Bar;
   private
      pragma Inline (Bar);

      procedure Bar is null;
   end T;
begin
   null;
end Testinline;
