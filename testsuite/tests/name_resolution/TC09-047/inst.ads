with Gen;
with Root_Pack;
package Inst is

   package Gen_Inst is new Gen (Root_Pack.Root);

   type Whatever is new Gen_Inst.Counted_Type with null record;

end Inst;
