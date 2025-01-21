procedure Test is
   generic
      type T is private;
   package Ptrs is
      type Ptr is tagged null record;

      procedure Set (Self : in out Ptr'Class; Data : T) is null;
   end Ptrs;

   package Pkg is
      type Data is null record;
      package Data_Ptrs is new Ptrs (Data);

      type Resource is new Data_Ptrs.Ptr with null record;
   end Pkg;

   X : Pkg.Resource;
   Y : Pkg.Data;
begin
   X.Set (Y);
   pragma Test_Statement;
end Test;
