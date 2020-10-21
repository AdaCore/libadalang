package body Foo is
   procedure Reset (A : out Array_Type) is
      pragma Test (Array_Type);
   begin
      for I in A'Range loop
         A (I) := 0;
      end loop;
   end Reset;
end Foo;
