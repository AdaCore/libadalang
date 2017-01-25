package body Foo is

   function "+" (S : String; I : Integer) return String is
   begin
      return S;
   end "+";

   function "-" (S : String; I : Integer) return String is
   begin
      return "";
   end "-";

end Foo;
