pragma Warnings (Off, "unrecognized pragma");

package B is
   type Typ is private;
   function "and" (L, R : Typ) return Typ is (L);
   function Wat (L, R : Typ) return Typ is (L);

private
   type Typ is array (1 .. 2) of Integer;

   I1 : Typ := (1, 2);

   Inst : Typ := I1 and (3, 4);
   pragma Test_Statement;

   Inst2 : Typ := Wat (I1, (3, 4));
   pragma Test_Statement;

end B;
