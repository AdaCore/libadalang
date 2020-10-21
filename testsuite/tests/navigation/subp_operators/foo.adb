package body Foo is

   function "+" (S : String; I : Integer) return String is
   begin
      return S;
   end "+";

   function "-" (S : String; I : Integer) return String is
   begin
      return "";
   end "-";

   function "AND" (S : String; I : Integer) return String is
   begin
      return "";
   end "AND";

end Foo;
