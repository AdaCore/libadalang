procedure Test_3 is
   function Foo (X : Boolean) return Integer is (0);
   function Foo (X : Integer) return Boolean is (True);

   function Bar (X : String; Y : Integer; Z : Boolean) return Integer is (Y);
   function Bar (X : Integer; Y : Boolean; Z : String) return Integer is (X);
   function Bar (X : Boolean; Y : String; Z : Integer) return Integer is (Z);

   X : Integer := Bar
     (Foo (True),
      Bar (Foo (2), 2 + 2, True),
      "Hello world");
   pragma Test_Statement;
begin
   null;
end Test_3;
