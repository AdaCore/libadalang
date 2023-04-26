separate (P.Subunit)
procedure Nested is

   --  Because of the relative placement for Before, Subunit and After
   --  declarations in p.adb, below references to Before should resolve to
   --  "type Before" in p.adb, while references to After should resolve to the
   --  one in types.ads.

   V1 : Before := 0;
   pragma Test_Statement;

   V2 : After := 0;
   pragma Test_Statement;
begin
   null;
end Nested;
