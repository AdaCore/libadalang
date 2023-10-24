package body Pkg is

   function CRef (W : in Window; X, Y : in Natural) return Pixel is
   begin
      return W.Pixels (1);
   end CRef;

   function VRef (W : aliased in out Window;
                  X, Y : in Natural) return Ref_Rec is
   begin
      return (D => W.Pixels (1)'Access);
   end VRef;

   function CRef (W : in Window; N : in Name) return Pixel is
   begin
      return W.Pixels (1);
   end CRef;

   function VRef (W : aliased in out Window; N : in Name) return Ref_Rec is
   begin
      return (D => W.Pixels (1)'Access);
   end VRef;

end Pkg;
