package B.C is
   --  Here, naming the type "Typ" used to cause hiding of the full view of
   --  B.Typ, from anywere where B.C.Typ is visible.
   type Typ is new B.Typ;
   procedure Foo;
end B.C;
