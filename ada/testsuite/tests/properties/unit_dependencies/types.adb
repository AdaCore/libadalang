package body Types is
   function Convert (X : Int) return Integer is
   begin
      return Int_Conversions.Convert (X);
   end Convert;
end Types;
