package body Foo is
   subtype Int is Types.Int;

   procedure Bar (I : Int) is
   begin
      return 1;
   end Foo;

   procedure Bar (I : Types.Byte) is
   begin
      return 0;
   end Foo;

end Foo;
