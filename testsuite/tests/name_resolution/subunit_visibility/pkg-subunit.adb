separate (Pkg)
procedure Subunit is

   --  Because of the relative placement for Before, Subunit and After
   --  declarations in pkg.adb, below references to Before should resolve to
   --  "type Before" in pkg.adb, while references to After should resolve to
   --  the one in types.ads.

   V1 : Before := 0;
   pragma Test_Statement;

   V2 : After := 0;
   pragma Test_Statement;
begin
   null;
end Subunit;
