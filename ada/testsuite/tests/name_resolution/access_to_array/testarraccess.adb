procedure TestArrAccess is
   type Str_Access is access all String;
   function Prout (S : String) return Str_Access is
   begin
      return new String'(S);
   end Prout;

   A : Character;

   S : Str_Access := new String'("lolwat");

begin
   A := Prout ("lol").all (1);

   A := S.all (1);
end TestArrAccess;
pragma Test_Block;
