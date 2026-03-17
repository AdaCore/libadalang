procedure Test is
   function Foo return Integer is (0);
   function Foo return Boolean is (True);

   procedure Bar (X : Integer) is null;
   procedure Bar (X : Boolean) is null;
begin
   Bar (2);
   Bar (Foo);
end Test;
