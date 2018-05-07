procedure Testcwconv is
   type T is tagged null record;

   procedure Bar (Self : T'Class) is null;

   function Foo return T is (null record);
begin
   Bar (T'Class (Foo));
   pragma Test_Statement;
end Testcwconv;
