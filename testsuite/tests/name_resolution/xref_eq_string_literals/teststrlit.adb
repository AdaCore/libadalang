procedure TestStrLit is
   type Char is ('a', 'b', 'c');
   type Int is range 0 .. 100;
   type Str is array (Int) of Char;

   S : Str;
   I : Int;

   function Foo (S : Str) return Int;
   procedure Foo (I : Int) return Int;
   function Foo (S : Str) return Str;
begin
   S := "abc";
   pragma Test_Statement;

   I := "abc";
   pragma Test_Statement;

   I := Foo ("abc");
   pragma Test_Statement;
end TestStrLit;
