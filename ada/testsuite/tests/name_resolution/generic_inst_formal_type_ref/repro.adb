procedure Repro is

   generic
      type T is private;
   package Lol is
      Default : T;
   end Lol;

   generic
      with package My_Lol is new Lol (<>);
   package Wat is
      subtype TT is My_Lol.T;

      function Foo return TT is (My_Lol.Default);
   end Wat;

   package Lol_Inst is new Lol (Integer);
   package Wat_Inst is new Wat (Lol_Inst);

   A : Integer := Wat_Inst.Foo;
   pragma Test_Statement;
begin
   null;
end Repro;
