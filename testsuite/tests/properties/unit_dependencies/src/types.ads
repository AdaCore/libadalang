with Conversions.Ranges;

package Types is
   type Int is private;
   function Convert (X : Int) return Integer;

private
   type Int is new Integer;

   package Int_Conversions is new Conversions.Ranges (T => Int);
end Types;
