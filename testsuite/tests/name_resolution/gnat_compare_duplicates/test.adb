procedure Test is
   generic
      type T is (<>);
   procedure P (X : T);

   procedure P (X : T) is
   begin
      null;
   end;

   package K is
      procedure P1 is new P (Integer);
      procedure P2 is new P (Boolean);
   end K;
begin
   null;
end;

--  GNAT gives the following refs for the code above:
--
--   * 1U11*Test 1b11 17t4,
--   * 3E12 T 4r21 6r21,
--   * 4u14 P 4>17 6b14 9t7 12r27 13r27,
--   * 4*17 X{3E12} 6b17,
--   * 11K12 K 14l8 14e9,
--   * 12U17 P1[4] 6b14,
--   * 13U17 P2[4] 6b14.
--
-- P, P1, and P2 declarations have references to P's body (6b14).
--
-- When building xrefs list, gnat_compare will add the following references for
-- 6b14:
--   * 6b14 -> 4u16,
--   * 6b14 -> 12U17,
--   * 6b14 -> 13U17.
--
-- Then, gnat_compare needs to remove duplicates for 6b14, so it will sort the
-- vector containing all the references to keep only one value for 6b14, which
-- should be the first one in the source code (i.e. 6b14 -> 4u16).
