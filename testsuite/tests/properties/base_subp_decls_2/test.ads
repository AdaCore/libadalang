with Ada.Finalization;

package Test is
   type A is limited private;
private
   procedure Finalize (X : in out A);
   --% node.p_base_subp_declarations()

   type A is new Ada.Finalization.Limited_Controlled with null record;
end Test;
