------------------------------------------------------------------------------
--                              C O D E P E E R                             --
--                                                                          --
--                     Copyright (C) 2008-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
--                                                                          --
-- The CodePeer technology was originally developed by SofCheck, Inc.       --
------------------------------------------------------------------------------

with BE.Checks;
with BE.Value_Numbers.Computations;
with BE.Value_Numbers.Value_Number_Sets;
with BE.Obj_Ids;

separate (BE.PVP.PVP_Pass.Prop)
procedure Propagate_Top_Down
  (Context              : Propagation_Context_Ptr;
   VN                   : Value_Number;
   If_Dead              : Action_If_Dead;
   Force_Down_To_Leaves : Boolean := False)
is
   --  Propagate information from VN to its children.
   --  E.g. if VN=VN2+VN3, we're propagating from VN to VN2 and VN3.
   --  This will call Constrain on VN's children, possibly causing them to
   --  [re]enter the work list.
   --  If Force_Down_To_Leaves is True, then will recurse on
   --  children directly unless is a Phi or a Kappa VN.
   --  This is used when doing postcond context.
   --  What about Is_Jump and empty sets?
   use SCIL.Expressions, PVP.Set_Arithmetic;
   --  PVP.Set_Arithmetic does all the range arithmetic.

   Is_Soft_Check : constant Boolean :=
      Vals_Depend_On_Soft_Checks (Context, VN);
   --  Whether current value set for VN is smaller due to
   --  a "soft" check/pecondition

   Is_Jump : constant Boolean :=
      If_Dead not in Dead_Check .. Dead_Check_Revert;
   --  True if this propagation is due to a conditional jump
   --  rather than a "check"

   procedure Constrain --  wrapper
     (VN            : Value_Number;
      Constraint    : Value_Set;
      If_Dead       : Action_If_Dead := Propagate_Top_Down.If_Dead;
      Is_Soft_Check : Boolean        := Propagate_Top_Down.Is_Soft_Check)
   is
   --  NOTE: The "VN" parameter to this routine will usually *not* be
   --       the same as the VN passed into the enclosing
   --       Propagate_Top_Down.

   begin
      if Context.Is_Postcond_Context then
         --  Make sure we are not killing off a basic block
         --  with this constraint
         if Vital_Values.Constraint_Would_Kill_Block
              (Cur_Proc,
               VN,
               Current_Vals => Possible_Vals (Context, VN),
               Constraint   => Constraint)
         then
            --  Ignore it
            if Debug_VN_Prop then
               Put_Line (" [Postcond-ctx constrain suppressed " &
                 "because would kill block]");
            end if;
            Prop.Stop_Propagation (Context);
            return;
         end if;
      end if;

      Constrain (Context, VN, Constraint, If_Dead, Is_Soft_Check);
      if Force_Down_To_Leaves then
         case VN_Kind (Propagate_Top_Down.VN) is
            when Phi_VN =>
               --  No need to keep going
               null;
            when others =>
               --  Recurse until we reach a phi
               Propagate_Top_Down
                 (Context,
                  VN,
                  If_Dead,
                  Force_Down_To_Leaves);
         end case;
      end if;
   end Constrain;

   Adjusted_Vitality_Map : constant
     array (Path_Vitality_Enum) of Path_Vitality_Enum :=
     (Dead_Jump             => Dead_Jump,
      Dead_Intentional_Fail => Dead_Intentional_Fail,
      Dead_Exit             => Dead_Exit,
      Live_Path_Vitality    => Live);

   function Adjusted_Vitality
     (State : Block_State)
      return  Path_Vitality_Enum
   is
      --  Return vitality of given state, but convert Dead_Check to Live,
      --  since we now treat "Live" and "Dead_Check" equivalently in
      --  many contexts.
      pragma Inline (Adjusted_Vitality);
   begin
      if Test_Dead_Check_Is_Live then
         return Adjusted_Vitality_Map (State.Vitality);
      else
         return State.Vitality;
      end if;
   end Adjusted_Vitality;

   Vals : constant Value_Set := Possible_Vals (Context, VN);

   procedure Do_Special_Phi_As_Kappa
     (VN : Value_Number;
      PAK_Special_Case : Phi_As_Kappa_Special_Case_Enum) is
   --  Handle special case phi-as-kappas for abs, -abs, 'Max, and 'Min
   --  Requires: VN_Kind(VN) = Phi_As_Kappa_Kind
   --            PAK_Special_Case /= Not_A_Special_Case
   --  TBD: Eventually, these might want to have their own operators.
      pragma Assert (VN_Kind (VN) = Phi_As_Kappa_VN);
      pragma Assert (PAK_Special_Case /= Not_A_Special_Case);

      New_Values   : constant Value_Numbers.Kappa_Component_Ptr :=
        Value_Numbers.Possible_New_Values (VN);
      First_Val    : constant Value_Number             :=
        Content_Value (Cur_Proc, New_Values.all);
      Second_Val   : constant Value_Number             :=
        Content_Value (Cur_Proc, New_Values.Next.all);
      Numeric_Vals : constant Number_Sets.Number_Set   :=
        To_Number_Set_Part (Vals);
      Kind_Is_Rat  : constant Boolean                  :=
        SCIL_Type_Kind_Of_VN (VN) = Big_Rat_Type_Kind;
      Negate_Ops   : constant array (Boolean) of Unary_Op_Enum :=
                        (Int_Negate_Op, Rat_Negate_Op);
      Negate_Op    : constant Unary_Op_Enum := Negate_Ops (Kind_Is_Rat);

      use Saturating_Big_Ints;
   begin
      case PAK_Special_Case is
      when Not_A_Special_Case =>
         --  Should not happen
         pragma Assert (False);

      when Abs_Phi_As_Kappa     => --  (if X in 0/1 .. +inf then X else -X)
         --  We take the appropriate "half" of the values (>= 0),
         --  and then duplicate them on the opposite side of zero.
         declare
            Non_Neg   : constant Value_Set :=
                          (if Kind_Is_Rat then
                             +Number_Sets.Non_Negative_Rat_Portion
                                (Numeric_Vals)
                           else
                             +Number_Sets.Non_Negative_Portion
                                (Numeric_Vals));
            Symmetric : constant Value_Set := Union (Non_Neg,
              Possible_Values (Negate_Op, Non_Neg));
         begin
            Constrain (First_Val, Include_Invalid_Value (Symmetric));
         end;

      when Neg_Abs_Phi_As_Kappa => --  (if X in -inf .. -1/0 then X else -X)
         --  We take the appropriate "half" of the values (<= 0),
         --  and then duplicate them on the opposite side of zero.
         declare
            Non_Pos   : constant Value_Set :=
                          (if Kind_Is_Rat then
                             +Number_Sets.Non_Positive_Rat_Portion
                                (Numeric_Vals)
                           else
                             +Number_Sets.Non_Positive_Portion
                                (Numeric_Vals));
            Symmetric : constant Value_Set := Union (Non_Pos,
              Possible_Values (Negate_Op, Non_Pos));
         begin
            Constrain (First_Val, Include_Invalid_Value (Symmetric));
         end;

      when Max_Phi_As_Kappa  => --  (if X-Y in 0/1 .. +inf then X else Y)
         --  X in XL .. XH, Y in YL .. YH, Max (X,Y) in L .. H ==>
         --    Max (XH, YH) = H, Max (XL, YL) = L ==>  X <= H, Y <= H,
         --  X in vals(X), Y in vals(Y), Max (X,Y) in vals(Max) ==>
         --      if YH' < H then all vals(X) > YH must be in vals(Max)
         --      if XH' < H then all vals(Y) > XH must be in vals(Max)
         declare
            X_Vals    : Number_Sets.Number_Set :=
                                To_Number_Set_Part
                                  (Possible_Vals (Context, First_Val));
            Y_Vals    : Number_Sets.Number_Set :=
                                To_Number_Set_Part
                                  (Possible_Vals (Context, Second_Val));
         begin
            if Number_Sets.Is_Empty_Set (X_Vals) then
               return;
            elsif Number_Sets.Is_Empty_Set (Y_Vals) then
               return;
            elsif Kind_Is_Rat then
               declare
                  Max_Rat : constant Big_Rat :=
                    Number_Sets.Max_Rat_Element (Numeric_Vals);
                  Below   : constant Number_Sets.Number_Set :=
                    (if Number_Sets.Max_Element_Open (Numeric_Vals) then
                       Number_Sets.Rat_Closed_Open_Range (First => -Inf,
                         After_Last => Max_Rat)
                     else
                       Number_Sets.Rat_Closed_Range
                         (First => -Inf, Last => Max_Rat));
               begin
                  --  Remove values above Max_Rat
                  Number_Sets.Intersect (X_Vals, Below);
                  Number_Sets.Intersect (Y_Vals, Below);

                  declare
                     X_Max : constant Big_Rat :=
                       Number_Sets.Max_Rat_Element (X_Vals);
                     Y_Max : constant Big_Rat :=
                       Number_Sets.Max_Rat_Element (Y_Vals);
                  begin
                     if Y_Max < Max_Rat and then Y_Max < X_Max then
                        --  vals(X) > Y_Max must be in Numeric_Vals
                        Number_Sets.Intersect (X_Vals,
                          Number_Sets.Union
                            (Number_Sets.Rat_Closed_Range
                                (First => -Inf, Last => Y_Max),
                             Numeric_Vals));
                     elsif X_Max < Max_Rat and then X_Max < Y_Max then
                        --  vals(Y) > X_Max must be in Numeric_Vals
                        Number_Sets.Intersect (Y_Vals,
                          Number_Sets.Union
                            (Number_Sets.Rat_Closed_Range
                                (First => -Inf, Last => X_Max),
                             Numeric_Vals));
                     end if;
                  end;
               end;
            else
               declare
                  Max_Int : constant Big_Int :=
                    Number_Sets.Max_Element (Numeric_Vals);
                  Below   : constant Number_Sets.Number_Set :=
                    Number_Sets.Int_Closed_Range
                      (First => -Inf, Last => Max_Int);
               begin
                  --  Remove values above Max_Int
                  Number_Sets.Intersect (X_Vals, Below);
                  Number_Sets.Intersect (Y_Vals, Below);

                  declare
                     X_Max : constant Big_Int :=
                       Number_Sets.Max_Element (X_Vals);
                     Y_Max : constant Big_Int :=
                       Number_Sets.Max_Element (Y_Vals);
                  begin
                     if Y_Max < Max_Int and then Y_Max < X_Max then
                        --  vals(X) > Y_Max must be in Numeric_Vals
                        Number_Sets.Intersect (X_Vals,
                          Number_Sets.Union
                            (Number_Sets.Int_Closed_Range
                                (First => -Inf, Last => Y_Max),
                             Numeric_Vals));
                     elsif X_Max < Max_Int and then X_Max < Y_Max then
                        --  vals(Y) > X_Max must be in Numeric_Vals
                        Number_Sets.Intersect (Y_Vals,
                          Number_Sets.Union
                            (Number_Sets.Int_Closed_Range
                                (First => -Inf, Last => X_Max),
                             Numeric_Vals));
                     end if;
                  end;
               end;
            end if;

            --  Now constrain X and Y
            Constrain (First_Val, Include_Invalid_Value (+X_Vals));
            Constrain (Second_Val, Include_Invalid_Value (+Y_Vals));
         end;

      when Min_Phi_As_Kappa  => --  (if X-Y in 0/1 .. +inf then Y else X)
         --  X in XL .. XH, Y in YL .. YH, Min (X,Y) in L .. H ==>
         --    Min (XH, YH) = H, Min (XL, YL) = L ==>  X >= L, Y >= L,
         --  X in vals(X), Y in vals(Y), Min (X,Y) in vals(Max) ==>
         --      if YL' > L then all vals(X) < YL must be in vals(Max)
         --      if XL' > L then all vals(Y) < XL must be in vals(Max)
         declare
            X_Vals    : Number_Sets.Number_Set :=
                                To_Number_Set_Part
                                  (Possible_Vals (Context, First_Val));
            Y_Vals    : Number_Sets.Number_Set :=
                                To_Number_Set_Part
                                  (Possible_Vals (Context, Second_Val));
         begin
            if Number_Sets.Is_Empty_Set (X_Vals) then
               return;
            elsif Number_Sets.Is_Empty_Set (Y_Vals) then
               return;
            elsif Kind_Is_Rat then
               declare
                  Min_Rat : constant Big_Rat :=
                    Number_Sets.Min_Rat_Element (Numeric_Vals);
                  Above   : constant Number_Sets.Number_Set :=
                    (if Number_Sets.Min_Element_Open (Numeric_Vals) then
                       Number_Sets.Rat_Open_Closed_Range
                         (Before_First => Min_Rat, Last => +Inf)
                     else
                       Number_Sets.Rat_Closed_Range
                         (First => Min_Rat, Last => +Inf));
               begin
                  --  Remove values below Min_Rat
                  Number_Sets.Intersect (X_Vals, Above);
                  Number_Sets.Intersect (Y_Vals, Above);

                  declare
                     X_Min : constant Big_Rat :=
                       Number_Sets.Min_Rat_Element (X_Vals);
                     Y_Min : constant Big_Rat :=
                       Number_Sets.Min_Rat_Element (Y_Vals);
                  begin
                     if Y_Min > Min_Rat and then Y_Min > X_Min then
                        --  vals(X) < Y_Min must be in Numeric_Vals
                        Number_Sets.Intersect (X_Vals,
                          Number_Sets.Union
                            (Number_Sets.Rat_Closed_Range
                               (First => Y_Min, Last => +Inf),
                             Numeric_Vals));
                     elsif X_Min > Min_Rat and then X_Min > Y_Min then
                        --  vals(Y) < X_Min must be in Numeric_Vals
                        Number_Sets.Intersect (Y_Vals,
                          Number_Sets.Union
                            (Number_Sets.Rat_Closed_Range
                               (First => X_Min, Last => +Inf),
                             Numeric_Vals));
                     end if;
                  end;
               end;
            else
               declare
                  Min_Int : constant Big_Int :=
                    Number_Sets.Min_Element (Numeric_Vals);
                  Above   : constant Number_Sets.Number_Set :=
                    Number_Sets.Int_Closed_Range
                      (First => Min_Int, Last => +Inf);
               begin
                  --  Remove values below Min_Int
                  Number_Sets.Intersect (X_Vals, Above);
                  Number_Sets.Intersect (Y_Vals, Above);

                  declare
                     X_Min : constant Big_Int :=
                       Number_Sets.Min_Element (X_Vals);
                     Y_Min : constant Big_Int :=
                       Number_Sets.Min_Element (Y_Vals);
                  begin
                     if Y_Min > Min_Int and then Y_Min > X_Min then
                        --  vals(X) < Y_Min must be in Numeric_Vals
                        Number_Sets.Intersect (X_Vals,
                          Number_Sets.Union
                            (Number_Sets.Int_Closed_Range
                               (First => Y_Min, Last => +Inf),
                             Numeric_Vals));
                     elsif X_Min > Min_Int and then X_Min > Y_Min then
                        --  vals(Y) < X_Min must be in Numeric_Vals
                        Number_Sets.Intersect (Y_Vals,
                          Number_Sets.Union
                            (Number_Sets.Int_Closed_Range
                               (First => X_Min, Last => +Inf),
                             Numeric_Vals));
                     end if;
                  end;
               end;
            end if;
            --  Now constrain X and Y
            Constrain (First_Val, Include_Invalid_Value (+X_Vals));
            Constrain (Second_Val, Include_Invalid_Value (+Y_Vals));
         end;

      end case;
   end Do_Special_Phi_As_Kappa;

   procedure Do_Kappa (VN : Value_Number; Is_Soft_Check : Boolean) is
      --  Push condition through Kappa
      New_Value        : Value_Numbers.Kappa_Component_Ptr :=
         Value_Numbers.Possible_New_Values (VN);
      Num_New_Values   : constant Natural                  :=
         Possible_Values_Count (New_Value);
      Old_Value        : Value_Numbers.Kappa_Component_Rec renames
        Value_Numbers.Old_Value (VN);
      Old_VN           : constant Value_Number             :=
         Content_Value (Cur_Proc, Old_Value);
      Ignore_Old_Value : Boolean                           :=
         not Has_Default_Value (VN)
        or else (Disjoint
                    (Possible_Vals (Context, Old_VN),
                     Vals,
                     Is_Ptr (VN))
                and then not Is_Empty_Invalid_VN (Old_VN));
      --  Some kappas don't use the "old" value part;
      --  Also, ignore old part if its value set is disjoint (but
      --       not if is an empty Invalid VN)
      Might_Be_True : array (1 .. Num_New_Values) of Boolean;
      --  Keep track of which predicates "might be true"
      Multiple_Objs : array (1 .. Num_New_Values) of Boolean :=
        (others => False);
      --  Keep track whether VN Represents_Multiple_Objects
      Existing_Vals : array (1 .. Num_New_Values) of Value_Set :=
        (others => Empty_Value_Set);
      --  Keep track of prior value set for new vals
      Num_Possible_Aliases     : Natural   := 0;
      I                        : Natural   := 1;
      All_New_Are_Presumptions : Boolean   := True;
      Some_Pred_Is_Soft        : Boolean   := False;
      New_Vals_Constraint      : Value_Set := Vals;
   --  This will be enlarged to include the whole "Valid" set
   --  for the new values of an Aliased_Kappa when the
   --  old value is possible.
   begin

      if VN_Kind (VN) = Final_Value_Of_Seq_Kappa_VN
        and then Is_Sequence_Variable (Associated_Phi (VN))
      then
         --  Never propagate down through a final-value kappa
         --  associated with a sequence variable,
         --  as these represent only the final value, and nothing
         --  about the other values the corresponding phi might take on.

         return;      -----------------

      end if;

      if Is_Just_Invalid_Set (Vals) then
         --  We have concluded object doesn't exist
         --  but it is hard to propagate that fact downward so
         --  just return.

         return;      -----------------

      end if;

      --  Push condition onto possible new values
      while New_Value /= null loop
         declare
            pragma Assert (Has_Predicate (Cur_Proc, New_Value.all));
            Pred_VN : constant Value_Number := Predicate
                                                 (Cur_Proc, New_Value.all);
            Predicate_Value_Set : Value_Set renames Possible_Vals
                                                      (Context, Pred_VN);
            Pred_Is_Soft : constant Boolean :=
              Vals_Depend_On_Soft_Checks (Context, Pred_VN);

            Must_Be_True : constant Boolean :=
              not Contains_Invalid_Value (Predicate_Value_Set) and then
              not Pred_Is_Soft
              and then not Int_Sets.Is_In
                             (Saturating_Big_Ints.Big_False,
                              To_Int_Set_Part (Predicate_Value_Set));
         begin
            if Must_Be_True
              and then (Num_Possible_Aliases = 0
                       or else VN_Kind (VN) /= Aliased_Kappa_VN)
            then

               --  This must be true; constrain it with passed in subset
               --  NOTE: As of 12/25/2009, we treat "must be true" as
               --       equivalent to "might be true" for an aliased-kappa
               --       if it is not the first possibly-true predicate
               --       since earlier predicates take precedence in
               --       a multi-component aliased kappa.

               if not Is_Jump
                 or else not BE.Checks.Represents_Multiple_Objects
                               (Address_Value (Cur_Proc, New_Value.all))
               then
                  if Debug_VN_Prop then
                     Put_Line
                       ("Top_Down.Do_Kappa: Predicate is True: " &
                        VN_Img (Predicate_Value (Cur_Proc, New_Value.all)) &
                        ", constraining: " &
                        VN_Img (Content_Value (Cur_Proc, New_Value.all)) &
                        " with " &
                        Set_Image (Vals, Is_Ptr (VN)));
                     Put_Line
                       ("  If_Dead = " & Action_If_Dead'Image (If_Dead));
                  end if;
                  Constrain (Content_Value (Cur_Proc, New_Value.all), Vals);
               end if;

               --  This predicate can't be false,
               --  so we can safely ignore the old value
               --  and other "might-be-trues".

               return; ---------- all done ------------

            end if;

            Some_Pred_Is_Soft := Some_Pred_Is_Soft or Pred_Is_Soft;

            Might_Be_True (I) := Pred_Is_Soft
                                   or else Int_Sets.Is_In
                                      (Saturating_Big_Ints.Big_True,
                                       To_Int_Set_Part (Predicate_Value_Set));
            --  NOTE: we are assuming false if predicate
            --       invalid, since we want to
            --       avoid inappropriate "push-throughs."
            --       A "soft" predicate of false is considered possibly true.

            if Might_Be_True (I) then
               --  Count number that might be true
               declare
                  New_VN            : constant Value_Number :=
                     Content_Value (Cur_Proc, New_Value.all);
                  New_Addr_VN       : constant Value_Number :=
                     Address_Value (Cur_Proc, New_Value.all);
                  Existing_New_Vals : constant Value_Set    :=
                     Possible_Vals (Context, New_VN);
                  New_Vals_Is_Soft  : constant Boolean      :=
                     Vals_Depend_On_Soft_Checks (Context, New_VN);
               begin
                  --  Remember the value set for later
                  Existing_Vals (I) := Existing_New_Vals;
                  if Is_Empty_Invalid_VN (New_VN) then
                     --  Always count empty invalid as a possibility
                     Num_Possible_Aliases := Num_Possible_Aliases + 1;
                     Existing_Vals (I)    := Empty_Value_Set;
                     --  No need to look at this again
                     Might_Be_True (I) := False;
                  elsif New_Vals_Is_Soft
                     or else Overlap
                           (Existing_New_Vals,
                            Vals,
                            Is_Ptr (New_VN))
                  then
                     --  Value sets overlap; this alternative is possible

                     --  bump count of aliases appropriately
                     Num_Possible_Aliases := Num_Possible_Aliases + 1;
                     if not VN_Equal (New_Addr_VN, null)
                       and then BE.Checks.Represents_Multiple_Objects
                                   (New_Addr_VN)
                     then
                        --  If "others" or wildcard, count as multiple aliases
                        Num_Possible_Aliases := Num_Possible_Aliases + 1;
                        Multiple_Objs (I)    := True;
                        if Debug_VN_Prop then
                           Put_Line
                             ("Top_Down.Do_Kappa: multi-object value: " &
                              VN_Img
                                (Content_Value (Cur_Proc, New_Value.all)));
                        end if;
                     end if;
                     if All_New_Are_Presumptions then
                        --  Check whether all aliases are presumptions
                        All_New_Are_Presumptions :=
                           Based_On_Presumptions (New_VN);
                     end if;
                  elsif Is_Just_Invalid_Set (Existing_New_Vals)
                    and then VN_Kind
                                (Computations.Skip_Over_Call_VNs (New_VN)) in
                       Kappa_VN
                  then
                     --  This might be a case where all possible alternatives
                     --  are "empty" invalids.  Treat it as such.
                     --  TBD: We need a better way to indicate these.

                     --  Always count empty invalid as a possibility
                     Num_Possible_Aliases := Num_Possible_Aliases + 1;
                     Existing_Vals (I)    := Empty_Value_Set;
                     --  No need to look at this again
                     Might_Be_True (I) := False;
                  else
                     --  value set of "new value" is disjoint with
                     --  required value set, so
                     --  Force the predicate to be false unless "soft"
                     --  or unless there are earlier possible alternatives
                     --  in an aliased kappa, since these take precedence.
                     if not Is_Soft_Check
                       and then not Pred_Is_Soft
                       and then (Num_Possible_Aliases = 0
                                or else VN_Kind (VN) /= Aliased_Kappa_VN)
                       and then SCIL_Type_Kind_Of_VN (VN) in
                         Elementary_Type_Kind
                     then
                        if Debug_VN_Prop then
                           Put_Line
                             ("Top_Down.Do_Kappa: Constrain predicate " &
                              VN_Img
                                 (Predicate_Value (Cur_Proc, New_Value.all)) &
                              " to be False");
                        end if;
                        Constrain
                          (Predicate_Value (Cur_Proc, New_Value.all),
                           +Number_Sets.Just_Zero_Int_Set,
                           If_Dead       => If_Dead,
                           Is_Soft_Check => Is_Soft_Check);
                     end if;
                     --  This one is ruled out
                     Might_Be_True (I) := False;
                  end if;
               end;
            else
               --  Just make sure it is initialized
               Existing_Vals (I) := Empty_Value_Set;
            end if;

            New_Value := New_Value.Next;
            I         := I + 1;

            if Must_Be_True then
               --  Can safely ignore remaining components in case
               --  when we are treating a "must be true" predicate
               --  as a "might be true" since it didn't come first.
               for J in I + 1 .. Num_New_Values loop
                  Might_Be_True (J) := False;
                  Existing_Vals (J) := Empty_Value_Set;
               end loop;

               Ignore_Old_Value := True;
               exit;  ---------- exit loop now
            end if;

         end;
      end loop;

      declare
         Action_For_Aliases : Action_If_Dead := Ignore;
         --  Default is to pass "If_Dead => Ignore" so path
         --  won't be marked dead.  We override this if
         --  there is exactly one that "might be true"

         Is_Soft_For_Aliases : Boolean :=
           Is_Soft_Check or else Some_Pred_Is_Soft;
            --  Whether condition being propagated down is "soft"

         PAK_Special_Case    : constant Phi_As_Kappa_Special_Case_Enum :=
           Phi_As_Kappa_Special_Case (Cur_Proc, VN);
             --  Indicates whether is a phi-as-kappa that is recognizable
             --  as "abs", 'Max, or 'Min.
         use Saturating_Big_Ints;
      begin
         --  Push condition onto other values

         if not Ignore_Old_Value then
            --  Include old value in count.
            Num_Possible_Aliases := Num_Possible_Aliases + 1;
         end if;

         if Num_Possible_Aliases = 1 then
            --  Pass through if-dead action from above
            --  since only one of the aliases is possible
            Action_For_Aliases := If_Dead;
         elsif PAK_Special_Case /= Not_A_Special_Case then
            Do_Special_Phi_As_Kappa (VN, PAK_Special_Case);
            return;   --  All done  --
         else
            --  Condition is "soft" on any particular alias
            Is_Soft_For_Aliases := True;
         end if;

         if Is_Soft_For_Aliases and then Can_Be_Precondition (VN) then
            --  As of 1/6/2013 we now allow Kappa VNs to be preconditions.
            --  If this is a precondition VN, then there is no point
            --  in propagating a "soft" check any further.

            return; -------------
         end if;

         if not Ignore_Old_Value
           and then not Is_Empty_Invalid_VN (Old_VN)
         then
            --  Propagate down to old value
            if Debug_VN_Prop then
               Put_Line ("Top_Down.Do_Kappa: Constrain old value");
            end if;
            Constrain
              (Content_Value (Cur_Proc, Old_Value),
               Vals,
               If_Dead       => Action_For_Aliases,
               Is_Soft_Check => Is_Soft_For_Aliases);

         end if;

         if Is_Jump and then Num_Possible_Aliases > 1
           and then PAK_Special_Case = Not_A_Special_Case
         then

            --  Multiple aliases, and a "jump" => don't push anything down.
            --  For example, given "if A[I] > 0 then," don't push down that
            --  "A[others] > 0" in the "then" part.
            --  On the other hand, given "assert(A[I] > 0)" it is reasonable
            --  to say "assert(A[others] > 0)".
            --  See failing_tests/too_many_reps.ada for another example.

            return; -------------

         end if;

         if VN_Kind (VN) = Aliased_Kappa_VN
           and then not Ignore_Old_Value
           and then not All_New_Are_Presumptions
         then
            --  Constrain new values only to be at least valid
            --  since further constraints may be inappropriate for
            --  a "random" alias referenced in an Aliased_Kappa.

            if Debug_VN_Prop then
               Put_Line ("Top_Down.Do_Kappa: Widen constraint to include all" &
                 " valid values");
            end if;
            New_Vals_Constraint :=
              Union (New_Vals_Constraint, Valid_Value_Set);
         end if;

         if not Is_Universal_Value_Set (New_Vals_Constraint) then
            --  The constraint is worth applying to the new values
            declare
               Use_Conditional_Vals : constant Boolean :=
                  VN_Kind (VN) = Phi_As_Kappa_VN;
            --  NOTE: We don't check "Kappa_Predicate_Affects_Value"
            --       as that seems to hurt output, e.g. in
            --       univ-int.Base10Value, post cond on Actual_Digits.
            begin
               for New_V in 1 .. Num_New_Values loop
                  if Might_Be_True (New_V) then
                     declare
                        New_Rec : constant Kappa_Component_Rec :=
                           Nth_Possible_Value (VN, New_V);
                        New_VN  : constant Value_Number        :=
                           Content_Value (Cur_Proc, New_Rec);
                     begin
                        if Is_Soft_For_Aliases
                          and then Multiple_Objs (New_V)
                          and then not Force_Down_To_Leaves
                          and then (Is_Descendant_Table_Others (New_VN)
                                   or else not Vital_Values.Fully_Initialized
                                   or else
                             Vital_Values.Constraint_Would_Kill_Block
                                (Cur_Proc,
                                 New_VN,
                                 Current_Vals => Existing_Vals (New_V),
                                 Constraint   => New_Vals_Constraint))
                        then

                           --  Suppress soft check, at least for now
                           if not Vital_Values.Fully_Initialized then
                              --  Make sure we revisit the block underlying
                              --  this context
                              declare
                                 Ctx : Propagation_Context_Ptr := Context;
                              begin
                                 while Ctx /= null loop
                                    if Ctx.BB /= Null_Basic_Block then
                                       --  Found underlying context;
                                       --  make sure it gets revisited.
                                       Pending_Blocks (Ctx.BB) := True;
                                       exit; ----- all done
                                    else
                                       Ctx := Ctx.Underlying_Context;
                                    end if;
                                 end loop;
                              end;
                           else
                              --  Ignore this soft check as it would
                              --  kill some block or constrain entire
                              --  descendant table
                              null;
                           end if;
                           if Debug_VN_Prop then
                              if Is_Descendant_Table_Others (New_VN) then
                                 Put_Line (" [Constrain suppressed because " &
                                      "is Descendant_Table[others]]");
                              elsif Vital_Values.Fully_Initialized then
                                 Put_Line (" [Constrain suppressed because " &
                                      "would kill block]");
                              else
                                 Put_Line (" [Constrain suppressed because " &
                                      "vital values not yet inited]");
                              end if;
                           end if;
                        else
                           declare
                              Pred : constant Value_Number :=
                                 Predicate_Value (Cur_Proc, New_Rec);
                           begin
                              if Num_Possible_Aliases = 1 then
                                 --  Force the value to conform
                                 --  and predicate to be true
                                 if Debug_VN_Prop then
                                    Put_Line ("Top_Down.Do_Kappa: " &
                                       "Constrain only value " &
                                       VN_Img (New_VN));
                                 end if;
                                 Constrain
                                   (New_VN,
                                    New_Vals_Constraint,
                                    If_Dead       => Action_For_Aliases,
                                    Is_Soft_Check => Is_Soft_For_Aliases);

                                 if not Is_Soft_For_Aliases
                                   or else not Has_Default_Value (VN)
                                 then
                                    --  Push down on predicate
                                    --  unless we have a soft check
                                    --  with a default -- see ptr6381
                                    if Debug_VN_Prop then
                                       Put_Line ("Top_Down.Do_Kappa: " &
                                          "Constrain only predicate " &
                                          VN_Img (Pred));
                                    end if;
                                    Constrain
                                      (Pred,
                                       +Number_Sets.Non_Zero_Int_Set,
                                       If_Dead       => Action_For_Aliases,
                                       Is_Soft_Check => Is_Soft_For_Aliases);
                                 end if;
                              elsif not Is_Subset
                                          (Existing_Vals (New_V),
                                           New_Vals_Constraint,
                                           Is_Ptr (New_VN))
                              then
                                 --  New_VN has some "bad" values
                                 if Use_Conditional_Vals then
                                    --  For "phi-as-kappa" we want to
                                    --  only remove "bad" values that aren't
                                    --  already screened out by the predicate
                                    declare
                                       Conditional_Existing_Vals : constant
                                         Value_Set :=
                                          Conditional_Vals
                                            (Context,
                                             Predicate => Pred,
                                             VN        => New_VN,
                                             Ignore_Recursion_Overflow =>
                                               PAK_Special_Case /=
                                                 Not_A_Special_Case);

                                       --  Values not screened out by predicate
                                       Bad_Vals : Value_Set :=
                                          Value_Set_Difference
                                            (Conditional_Existing_Vals,
                                             New_Vals_Constraint,
                                             Is_Ptr (New_VN));
                                    --  Bad values not screened out
                                    begin
                                       if not Contains_Invalid_Value
                                                (New_Vals_Constraint)
                                       then
                                          --  Make sure to remove "invalid" too
                                          Include_Invalid_Value (Bad_Vals);
                                       end if;
                                       --  Now remove the bad values
                                       if Debug_VN_Prop then
                                          Put_Line ("Top_Down.Do_Kappa: " &
                                             "Remove bad values " &
                                             Set_Image
                                                (Bad_Vals,
                                                 Is_Ptr (New_VN)) &
                                             " from " &
                                             VN_Img (New_VN));
                                       end if;
                                       Constrain
                                         (New_VN,
                                          Invert (Bad_Vals),
                                          If_Dead       => Action_For_Aliases,
                                          Is_Soft_Check =>
                                            Is_Soft_For_Aliases);
                                    end;
                                 else
                                    --  Just do the straightforward "constrain"
                                    if Debug_VN_Prop then
                                       Put_Line ("Top_Down.Do_Kappa: " &
                                          "Constrain new value " &
                                          (if Is_Soft_For_Aliases then
                                             "(soft) "
                                           else
                                             "") &
                                          VN_Img (New_VN));
                                    end if;
                                    Constrain
                                      (New_VN,
                                       New_Vals_Constraint,
                                       If_Dead       => Action_For_Aliases,
                                       Is_Soft_Check => Is_Soft_For_Aliases);
                                 end if;
                              end if;  --  Whether to constrain value or
                                       --  predicate
                           end;
                        end if;  --  Whether Is_Soft_For_Aliases and ...
                     end;
                  end if; --  Might_Be_True
               end loop;
            end;
         end if;
      end;
   end Do_Kappa;

   ----------------

   --  Here, and in Propagate_Bottom_Up.Do_Phi, we must remember that loop VNs
   --  represent a *sequence* of values, rather than a single value.  We must
   --  avoid assuming that a loop VN has the same value on two different
   --  iterations of the loop.

   procedure Do_Phi (VN : Value_Number; Is_Soft_Check : Boolean) is
      --  "Push" condition through non-sequence variable Phis.
      Into_BB      : constant BB_Id_Range := Phi_Associated_Block (VN);
      Edge_Into_BB : constant Edge_Id     :=
         Context_Edge_Into_BB (Context, Into_BB);
      --  See whether we are working in a (PIC or Edge) context which
      --  specifies one particular edge
      Is_Loop_Phi  : constant Boolean :=  --  Whether phi is a "loop" phi
        Loop_Detection.Is_Loop_Header (Cur_Proc, Into_BB);

      Adjusted_Vals : Value_Set := Vals;
      --  Make a copy of "Vals" so we can adjust it if we
      --  are propagating from a block inside a loop via
      --  an incoming edge, and the block is not executed
      --  on every iteration of the loop.
      --  The "adjustment" is to make it all but Invalid.
      --  Essentially, we can't require it have any particular
      --  initial value, but we can require it be initialized.

      Edge_On_All_Paths_In_Loop : Boolean := False;
   --  This will be True if the phi is a loop phi,
   --  and the current context is for an edge that is
   --  on all paths through the corresponding loop.

   begin --  Do_Phi

      if Debug_Phis and then not Block_Dominates_Context (Into_BB, Context)
      then
         Dbg ("Top-down Do_Phi on " &
               VN_Img (VN) &
               " in not dominated context: ");
            Put_Context_Chain (Context);
      end if;

      --  Don't push through a loop-VN unless the relevant BB or Edge is on all
      --  paths through the loop since the initial value might not be relevant
      --  otherwise.  Just push a requirement to be "valid" in that case.
      --  However, when doing postcond ctx, we go ahead and do the
      --  full "constrain" even if not on all paths.
      if Is_Loop_Phi then
         if (Context.Edge /= Null_Edge_Id
            and then not Edge_Is_On_All_Paths_Through_Loop
                           (Cur_Proc,
                            Context.Edge,
                            Loop_Header => Into_BB))
           or else (Context.Edge = Null_Edge_Id
                   and then Context.BB /= Null_Basic_Block
                   and then not Block_Is_On_All_Paths_Through_Loop
                                  (Cur_Proc,
                                   Context.BB,
                                   Loop_Header => Into_BB))
         then
            if Contains_Invalid_Value (Vals) then
               --  If Vals allows invalid, then give up;
               --  Otherwise, enlarge Vals to contain all valid values.
               if Debug_Phis then
                  Dbg
                    ("Top-down Do_Phi on loop-phi " &
                     VN_Img (VN) &
                     " ctxt not on all paths of loop " &
                     "so return immed; context: ");
                  Put_Context_Chain (Context);
               end if;
               return; ----------------
            else
               if Debug_Phis then
                  Dbg
                    ("Top-down Do_Phi on loop-phi " &
                     VN_Img (VN) &
                     " ctxt not on all paths of loop " &
                     "so only removing invalid; context: ");
                  Put_Context_Chain (Context);
               end if;
               Adjusted_Vals := Valid_Value_Set;
            end if;
         elsif Context.Edge /= Null_Edge_Id then
            --  We have a loop phi, and we are on an edge
            --  which is on all paths through the loop.
            Edge_On_All_Paths_In_Loop := True;
         end if;
      end if;

      if Edge_Into_BB /= Null_Edge_Id
        and then Context.Underlying_Context /= null
      then
         --  Only one edge is relevant (and this isn't a simple
         --  outgoing-edge context), only constrain that input
         if Debug_Phis then
            Dbg_Line
              ("Top-down Do_Phi on " &
               VN_Img (VN) &
               " -- Only edge " &
               Edge_Name (Cur_Proc, Edge_Into_BB) &
               " is relevant.");
         end if;
         if Edge_Kind (Edge_Into_BB) = Forward_Edge then
            --  Only do forward edges (to avoid infinite recursion,
            --  and to avoid assuming prior iteration has same value)
            declare
               PIC           : constant Propagation_Context_Ptr :=
                  Get_PIC (Context, Edge_Into_BB);
               Incoming_VN   : constant Value_Number            :=
                  Nth_Parameter
                    (Cur_Proc,
                     VN,
                     To_Index (Cur_CFG, Edge_Into_BB));
               Incoming_Vals : constant Value_Set               :=
                  Possible_Vals (PIC, Incoming_VN);
               Bad_Vals      : constant Value_Set               :=
                  Value_Set_Difference
                    (Incoming_Vals,
                     Adjusted_Vals,
                     Is_Ptr (Incoming_VN));
            begin
               Constrain (Incoming_VN, Invert (Bad_Vals));
            end;
         elsif Debug_Phis then
            Dbg_Line (" Only edge is non-fwd edge => return immed.");
         end if;
         return; -------------------------
      end if;

      declare

         Max_Vitality : Path_Vitality_Enum := Dead_Jump;
         --  Relevant Vitality -- max (most live) of incoming PICs.

         Only_Edge_Index : In_Edges_Length := 0;
         --  Unique edge with given max_vitality

         Is_First_Forward_Edge : Boolean := True;

         Forward_Edge_Pic : Propagation_Context_Ptr := null;
         --  PIC for forward edge, if there is only one.

         Only_Forward_Edge : Edge_Id := Null_Edge_Id;
         --  Forward edge, if there is only one.

         Recompute_Phis_And_Control_VN : Boolean := False;
         --  Indicates whether some edge became "less vital"
         --  and hence we need to recompute *all* the phis,
         --  as well as the dominating control VN

         Action_For_Inputs  : Action_If_Dead := Ignore;
         Is_Soft_For_Inputs : Boolean        := True;
      --  Options to pass when constraining inputs
      begin
         --  Calculate Max:
         for Edge_Index in 1 .. Num_Parameters (VN) loop
            declare
               Edge                   : constant Edge_Id            :=
                  Nth_In_Edge (Cur_CFG, Into_BB, Edge_Index);
               Adjusted_Edge_Vitality : constant Path_Vitality_Enum :=
                  Adjusted_Vitality (Edge_States.A (Edge));
            --  Treat Live and Dead_Check as the same
            begin
               if Edge_Kind (Edge) /= Forward_Edge then
                  if Adjusted_Edge_Vitality > Max_Vitality then
                     --  Update max vitality
                     Max_Vitality := Adjusted_Edge_Vitality;
                     if not Is_Jump
                       and then Based_On_Presumptions
                                   (Nth_Parameter (Cur_Proc, VN, Edge_Index))
                     then
                        --  Back edge, but based on presumptions
                        --  Remember first max-vitality edge in case it is
                        --  the only one.
                        Only_Edge_Index := Edge_Index;
                     else
                        --  Zero out rather than remember first max-vitality
                        --  edge, since is back edge
                        Only_Edge_Index := 0;
                     end if;
                  elsif Adjusted_Edge_Vitality = Max_Vitality then
                     --  Prevent treating some other edge as the "only"
                     --  vital edge
                     Only_Edge_Index := 0;
                  end if;
               else
                  --  Forward edge
                  declare
                     PIC                   : constant Propagation_Context_Ptr
                        :=
                        Get_PIC (Context, Edge);
                     Adjusted_Pic_Vitality : constant Path_Vitality_Enum :=
                        Adjusted_Vitality (PIC.State.all);
                  begin
                     if Adjusted_Pic_Vitality < Adjusted_Edge_Vitality
                       and then Adjusted_Pic_Vitality <
                                Adjusted_Vitality (Context.State.all)
                     then
                        --  We have A PIC whose vitality is lower than
                        --  the incoming vitality of the edge and the context.
                        --  We should recompute all phis defined in the
                        --  Into_BB, as well as the incoming value for
                        --  the control VN of the immediate dominator of
                        --  Into_BB (by unioning across the live inputs).
                        Recompute_Phis_And_Control_VN := True;
                     end if;

                     --  Remember "max" Vitality
                     if Adjusted_Pic_Vitality > Max_Vitality then
                        Max_Vitality    := Adjusted_Pic_Vitality;
                        Only_Edge_Index := Edge_Index;
                     elsif Adjusted_Pic_Vitality = Max_Vitality then
                        --  There is more than one edge with "Max_Vitality"
                        Only_Edge_Index := 0;
                     end if;

                     if Is_First_Forward_Edge then
                        --  Remember forward edge PIC if is only one.
                        Only_Forward_Edge     := Edge;
                        Forward_Edge_Pic      := PIC;
                        Is_First_Forward_Edge := False;
                     else
                        --  We have more than one forward edge
                        Only_Forward_Edge := Null_Edge_Id;
                        Forward_Edge_Pic  := null;
                     end if;
                  end;
               end if;  --  if forward edge
            end;
         end loop;

         --  Check whether there is only one edge with Max_Vitality
         if Only_Edge_Index > 0 then
            --  Only one edge, so not necessarily a soft check
            if Debug_Phis then
               Dbg_Line
                 ("Only one edge " &
                  Edge_Name
                     (Cur_Proc,
                      Nth_In_Edge (Cur_CFG, Into_BB, Only_Edge_Index)) &
                  " still alive in top-down Do_Phi");
               if Edge_On_All_Paths_In_Loop then
                  Dbg_Line (" (this edge is on all paths of loop)");
               end if;
            end if;

            if Is_Loop_Phi and then Only_Forward_Edge /= Null_Edge_Id then
               --  Don't constrain loop-phi if all back edges are dead
               --  as this can produce instability.
               if Debug_Phis then
                  Dbg_Line
                    ("Quit because only edge is forward edge for loop phi.");
               end if;
               return; --------------

            end if;
            Action_For_Inputs  := Propagate_Top_Down.If_Dead;
            Is_Soft_For_Inputs := Is_Soft_Check;
         elsif Is_Jump then
            --  Multiple edges, and is a jump, so all we want to do
            --  is make sure the inputs are valid.
            --  TBD: Adjusted_Vals := Valid_Value_Set;
            --  TBD: For now, we are just going to return

            --  But first, check special case of edge on all paths of loop,
            --  to see if the phi's new value set has no overlap with
            --  the incoming value set, implying that the loop
            --  will be exited immediately.
            if Edge_On_All_Paths_In_Loop
              and then Forward_Edge_Pic /= null
              and then If_Dead = Dead_Jump
            then
               --  See whether loop body is dead
               declare
                  Incoming_VN   : constant Value_Number :=
                     Nth_Parameter
                       (Cur_Proc,
                        VN,
                        To_Index (Cur_CFG, Only_Forward_Edge));
                  Incoming_Vals : constant Value_Set    :=
                     Possible_Vals (Forward_Edge_Pic, Incoming_VN);
                  pragma Unreferenced (Incoming_Vals);
               begin
                  if Debug_Phis then
                     Dbg_Line
                       ("Top_Down.Do_Phi: Jump constraint on " &
                        "loop phi with only one forward edge into loop, " &
                        "context is for edge on all paths through loop.");
                  end if;

                  --  This may trigger "Mark_Dead" if no overlap
                  Constrain (Incoming_VN, Adjusted_Vals);

                  if Debug_Phis
                    and then Context.State.Vitality = Dead_Jump
                  then
                     --  Edge is dead on first iteration, so is dead
                     --  on all future iterations, since it is only
                     --  way to get to future iterations.
                     if Debug_Phis then
                        Dbg_Line
                          ("Top_Down.Do_Phi: Loop body is dead " &
                           "due to conflict with incoming val of loop phi.");
                     end if;

                  end if;
               end;
            end if;

            if Debug_Phis then
               Dbg_Line
                 ("Top_Down.Do_Phi: Multiple live edges " &
                  "and is jump constraint, so returning immediately.");
               if Edge_On_All_Paths_In_Loop then
                  Dbg_Line (" (context is for edge on all paths of loop)");
               end if;
            end if;
            return; --------------
         end if;

         if Only_Edge_Index > 0
           and then Is_Jump
           and then not Is_Soft_Check
         then
            --  We have a situation where we are doing a propagation on a
            --  Phi as part of creating an outgoing edge, and we have
            --  discovered that there is only one input to this Phi
            --  whose PIC is alive.  We also know that the block
            --  where the Phi is defined dominates this outgoing edge,
            --  (or is equal to its successor block).
            --  Hence, we can merge the PIC into the current context
            --  rather than simply propagating the constraint to the
            --  input VN.  The PIC already represents such a merge,
            --  so in fact we can simply use the state from the PIC.
            declare
               Only_Edge : constant Edge_Id :=
                  Nth_In_Edge (Cur_CFG, Into_BB, Only_Edge_Index);
            begin
               if Edge_Kind (Only_Edge) = Forward_Edge then
                  --  Merge PIC into current context.
                  --  NOTE: We only try to optimize forward edges
                  declare
                     PIC : constant Propagation_Context_Ptr :=
                        Get_PIC (Context, Only_Edge);
                  begin
                     if Debug_Phis then
                        Dbg_Line
                          ("Only one edge " &
                           Edge_Name
                              (Cur_Proc,
                               Nth_In_Edge
                                  (Cur_CFG,
                                   Into_BB,
                                   Only_Edge_Index)) &
                           " alive in top-down Do_Phi for edge ctxt." &
                           " Merging in PIC.");
                        Dbg_Line
                           ("postcond_context => " &
                            Boolean'Image (Context.Is_Postcond_Context));
                     end if;
                     --  TBD: There might be a faster way to incorporate PIC.
                     Constrain_Stored_Values_By_Block
                       (Context,
                        PIC.State.all,
                        If_Dead => Dead_Jump);
                     --  NOTE: We are presuming a propagation will take place
                     --       eventually.
                     return; ---------
                  end;
               end if;
            end;
         end if;

         --  Loop through incoming, ignoring edges with irrelevant vitality,
         --  and ignoring back edges.  For each, constrain the Param in the
         --  *current* context to not be in the "bad values" of the Phi with
         --  respect to the incoming PIC.  ("Bad values" implies we're talking
         --  about checks, but it could just as well be a jump.)

         for Edge_Index in 1 .. Num_Parameters (VN) loop
            declare
               Edge        : constant Edge_Id      :=
                  Nth_In_Edge (Cur_CFG, Into_BB, Edge_Index);
               Incoming_VN : constant Value_Number :=
                  Nth_Parameter (Cur_Proc, VN, Edge_Index);
            begin
               if VN_Equal (Incoming_VN, VN) then --  Optimization;
                  --  ignore self-reference.
                  null;
               elsif Edge_Kind (Edge) /= Forward_Edge then
                  --  Ignore back/cross (unless is "presumption" VN).
                  --  We can't propagate to a loop-back edge, because that
                  --  would constitute imposing this condition on prior
                  --  iterations, which is wrong.
                  if Adjusted_Vitality (Edge_States.A (Edge)) =
                     Max_Vitality
                    and then not Is_Jump
                    and then Based_On_Presumptions (Incoming_VN)
                  then
                     --  This is based on a presumption, so it is OK
                     --  to constrain it (but only if a "check")
                     --  Constrain away the "bad values" coming
                     --  in via this edge.
                     declare
                        Incoming_Vals : constant Value_Set :=
                           PVP.SCIL_Extension.Possible_Vals
                             (Edge_States.A (Edge),
                              Unique_VN_Id (Incoming_VN),
                              Fall_Back);
                        Bad_Vals      : Value_Set          :=
                           Value_Set_Difference
                             (Incoming_Vals,
                              Adjusted_Vals,
                              Is_Ptr (Incoming_VN));
                     begin
                        if Context.Stable_Backup /= Context.State then
                           --  Eliminate from Bad_Vals those values
                           --  already missing in Stable_Backup for VN,
                           --  since we might be in a conditional block
                           --  where some of the "bad values" have already
                           --  been eliminated.
                           Intersect
                             (Bad_Vals,
                              PVP.SCIL_Extension.Possible_Vals
                                 (Context.Stable_Backup.all,
                                  Unique_VN_Id (VN),
                                  Fall_Back),
                              Is_Ptr (VN));
                        end if;

                        if Debug_Phis then
                           Dbg_Line
                             ("Propagating through back edge " &
                              Edge_Name (Cur_Proc, Edge) &
                              " because " &
                              VN_Img (Incoming_VN) &
                              " is based on presumptions");
                           Dbg_Line
                             (" Incoming_Vals: " &
                              Set_Image (Incoming_Vals, Incoming_VN) &
                              ", Bad_Vals: " &
                              Set_Image (Bad_Vals, Incoming_VN));
                        end if;
                        if not Is_Empty_Value_Set (Bad_Vals) then
                           --  Remove bad values from VN in *current*
                           --  context.
                           --  NOTE: We constrain stored values since
                           --       it may not be possible to do a
                           --       bottom-up computation of Incoming_VN
                           --       in current context, if Incoming_VN
                           --       is itself a phi.
                           Constrain_Stored_Values
                             (Context,
                              Incoming_VN,
                              Invert (Bad_Vals),
                              If_Dead       => Action_For_Inputs,
                              Is_Soft_Check => Is_Soft_For_Inputs);

                        end if;
                     end;
                  else
                     --  Back edge and not based on presumptions,
                     --  so ignore it.
                     null;
                  end if;
               else
                  --  Forward Edge
                  declare
                     PIC : constant Propagation_Context_Ptr :=
                        Get_PIC (Context, Edge);
                  begin
                     if PIC = Context
                       and then Context.Level = Max_PIC_Level
                       and then not Is_Loop_Phi
                     then
                        --  Return immediately if we discover
                        --  we have reached the "Max" PIC level
                        --  and we get back our own Context,
                        --  and it is not a forward edge into a loop header
                        return;
                     end if;

                     if not Is_Jump
                       and then Adjusted_Vitality (PIC.State.all) =
                                Max_Vitality
                     then
                        --  Constrain away the "bad values" coming
                        --  in via this edge.
                        --  NOTE: We do this only if is a "check"
                        declare
                           Incoming_Vals : constant Value_Set :=
                              PVP.SCIL_Extension.Possible_Vals
                                (Edge_States.A (Edge),
                                 Unique_VN_Id (Incoming_VN),
                                 Fall_Back);
                           Bad_Vals      : constant Value_Set :=
                              Value_Set_Difference
                                (Incoming_Vals,
                                 Adjusted_Vals,
                                 Is_Ptr (Incoming_VN));
                        begin
                           if not Is_Empty_Value_Set (Bad_Vals) then
                              --  Remove bad values from VN in *current*
                              --  context.

                              if Bad_Vals = Incoming_Vals
                                and then PIC /= Context
                              then
                                 --  But first, if *no* values from
                                 --  this edge are acceptable,
                                 --  kill off PIC itself as well
                                 --  (unless it is same as current context)
                                 if Debug_Phis then
                                    Dbg_Line
                                      ("Killing PIC because " &
                                       "no good values coming from " &
                                       Edge_Name (Cur_Proc, Edge) &
                                       " in " &
                                       VN_Img (Incoming_VN));
                                 end if;
                                 Constrain
                                   (PIC,
                                    Incoming_VN,
                                    Invert (Incoming_Vals),
                                    If_Dead       => If_Dead,
                                    Is_Soft_Check => Is_Soft_Check);
                              end if;

                              --  TBD: Worry about vital values if
                              --      incoming VN represents multiple
                              --      objects?
                              if Debug_Phis then
                                 Dbg_Line
                                   ("Removing bad values " &
                                    Set_Image
                                       (Bad_Vals,
                                        Is_Ptr (Incoming_VN)) &
                                    " coming on edge " &
                                    Edge_Name (Cur_Proc, Edge) &
                                    " from " &
                                    VN_Img (Incoming_VN));
                              end if;
                              Constrain
                                (Incoming_VN,
                                 Invert (Bad_Vals),
                                 If_Dead       => Action_For_Inputs,
                                 Is_Soft_Check => Is_Soft_For_Inputs);
                           else
                              if Debug_Phis then
                                 Dbg_Line
                                   ("No bad values from edge " &
                                    Edge_Name (Cur_Proc, Edge) &
                                    " in " &
                                    VN_Img (Incoming_VN));
                              end if;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end;
         end loop;

         if Recompute_Phis_And_Control_VN then
            --  Recompute Control_VN from live PICs
            --  TBD: Recompute all other active Phis of Into_BB
            declare
               Control_VN    : constant Value_Number :=
                  Jump_Value
                    (Cur_Proc,
                     Dominator_Trees.Immediate_Dominator (Cur_Proc, Into_BB));
               Live_PIC_Vals : Value_Set             := Empty_Value_Set;
            begin
               for Edge_Index in 1 .. Num_Parameters (VN) loop
                  declare
                     Edge : constant Edge_Id :=
                        Nth_In_Edge (Cur_CFG, Into_BB, Edge_Index);
                  begin
                     if Edge_Kind (Edge) = Forward_Edge then
                        declare
                           PIC : constant Propagation_Context_Ptr :=
                              Get_PIC (Context, Edge);
                        begin
                           if Adjusted_Vitality (PIC.State.all) =
                              Max_Vitality
                           then
                              Union
                                (Live_PIC_Vals,
                                 Possible_Vals (PIC, Control_VN));
                           end if;
                        end;
                     end if;
                  end;
               end loop;

               if Debug_Phis then
                  Dbg_Line ("Constraining control VN " & VN_Img (Control_VN));
               end if;
               --  Constrain control VN to new union from live PICs
               Constrain (Control_VN, Live_PIC_Vals);  --  TBD: Is_Soft_Check?
            end;
         end if;
      end;
   end Do_Phi;

   procedure Do_Table_Element (VN : Value_Number; Is_Soft_Check : Boolean) is
   --  "Push" condition through table element VN
   begin
      null; --  TBD
   end Do_Table_Element;

begin --  Propagate_Top_Down

   if Is_Just_Invalid_Set (Vals) then
      --  Don't propagate down from simply "invalid".
      --  This tends to happen when an Others_VN has
      --  no values associated with it.
      return;
   end if;

   --  Push the condition down into subexpressions.
   case VN_Kind (VN) is
      when Membership_VN =>
         declare
            Arg : constant Value_Number := Tested_VN (VN);
         begin
            Constrain
              (Arg,
               Membership_Arg_Possible_Values
                  (Res  => Vals,
                   Sets => +Value_Numbers.Sets (VN)));
         end;

      when Known_Call_VN =>
         declare
            PCV         : constant Value_Number := Pre_Call_Value (VN);
            Expanded_VN : constant Value_Number := Expanded_Call_Site_VN (VN);
         begin
            if not VN_Equal (PCV, null)
              and then not Contains_Invalid_Value (Vals)
            then
               --  Push validity condition on pre-call-value.
               Constrain (PCV, Valid_Value_Set);
            end if;

            if not VN_Equal (Expanded_VN, null) then
               --  Recurse down on Expanded_Call_Site_VN if non-null
               Constrain (Expanded_VN, Vals);
            end if;
         end;

      when Unexpr_VN =>
         declare
            Op  : constant Unary_Op_Enum := Operator (VN);
            Arg : constant Value_Number  := First_Operand (VN);
         begin
            --  NOTE: We include the "invalid" value here because
            --       we don't want to exclude the invalid value in
            --       this top-down propagation.  That should happen
            --       as part of a "check" inserted at the point of a
            --       "fetch" of any object, and that is the place
            --       where we want the invalid value to be effectively
            --       "removed."  Removing invalid via top-down propagation
            --       means we end up with more false-positives and our
            --       "input" annotations don't include all the
            --       fetched objects.
            --       Also see PVP.Mess.Select_Valid_Live_On_Entry.
            Constrain
              (Arg,
               Include_Invalid_Value (Arg_Possible_Values (Op, Res => Vals)));
         end;

      when Binexpr_VN =>
         --  Note: In determining the possible values of each
         --  operand, we use the saved value set of the
         --  *other* operand, or "worst case" if none.
         --  For literals, "worst case" is also "best case,"
         --  that is, it gives the one and only value.
         declare
            Op   : constant Binary_Op_Enum := Operator (VN);
            Arg1 : constant Value_Number   := First_Operand (VN);
            Arg1_Vals : constant Value_Set :=
               Possible_Vals (Context, Arg1, Check_For_Top_Down => False);
            Arg2 : constant Value_Number   := Second_Operand (VN);
            Arg2_Vals : Value_Set          := Universal_Value_Set;
            New_Arg1_Vals : Value_Set;
            New_Arg2_Vals : Value_Set;
         begin

            --  Disable top-down propagation if one operand is a Kappa_VN
            --  defined in a loop and the value of the other operand can be
            --  changed in the loop.
            --
            --  The reason for doing this is that for an access to an array
            --  index inside a loop using a sequence variable (e.g., A (I)
            --  where I is a sequence variable) there can be more kappa value
            --  numbers representing this access created - one kappa for each
            --  possible value of the sequence variable (e.g., kappa K1 for
            --  A (0) and K2 for A (1)). If there is a binary expression
            --  involving the access to the array index (e.g, "A (I) - B")
            --  there will be more value numbers representing this binary
            --  expression - one for each kappa (e.g, "V1: K1 - VB" and
            --  "V2: K2 - VB" where VB is a value number representing value of
            --  B). Now assume that V1, V2, K1, and K2 have the following value
            --  sets: V1 = {0}, V2 = {0}, K1 = {0}, K2 = {1}.
            --  Propagating value sets for V1 and V2 top-down leads to the
            --  conclusion that the value set for VB is empty. This is correct
            --  only if the value of VB is always the same in every loop
            --  iteration. This may be not true if VB is Call_VN, Phi_VN, and
            --  Phi_As_Kappa_VN and that's why we disable top-down propagation
            --  in these cases.
            --
            --  Note that the case when both operands are kappas is handled
            --  correctly in SSA - binary expressions where the operands
            --  represent matching array accesses are created
            --  (e.g., "V1: A (0) - B (0)", "V2: A (1) - B (1)").
            --
            --  See the ticket P601-021 for more information.
            declare
               function Disable_Top_Down
                 (Arg1, Arg2 : Value_Number) return Boolean
               is
                  function Check_If_First_Is_Kappa
                    (First, Second : Value_Number) return Boolean is
                     --  Check if First is a kappa active inside a loop and
                     --  the value of Second can be different in different loop
                     --  iterations.
                  begin
                     if (VN_Kind (First) in Kappa_VN) and then
                       Loop_Detection.Is_Within_Loop
                         (Cur_Proc, Default_Active_Block (First)) and then
                       (VN_Kind (Second) in Call_VN or else
                        VN_Kind (Second) = Phi_VN or else
                        VN_Kind (Second) = Phi_As_Kappa_VN) and then
                       Loop_Detection.Is_Within_Loop
                         (Cur_Proc, Default_Active_Block (Second))
                     then
                        return True;
                     else
                        return False;
                     end if;
                  end Check_If_First_Is_Kappa;
               begin
                  return Check_If_First_Is_Kappa (Arg1, Arg2)
                    or else Check_If_First_Is_Kappa (Arg2, Arg1);
               end Disable_Top_Down;

            begin
               if Disable_Top_Down (Arg1, Arg2) then
                  return;
               end if;
            end;

            if Is_Short_Circuit (VN)
              and then Arg1_Vals /=
                +Number_Sets.Int_Singleton_Set (Logical_Op_Identity (Op))
              and then Vals /=
                +Number_Sets.Int_Singleton_Set (Logical_Op_Identity (Op))
              and then not VN_Is_Active_Or_Constrained
                (Cur_Proc, Arg2, Context)
            then
               --  We have a short-circuit where the second operand might
               --  not always be evaluated; use conditional vals.

               if Arg1_Vals =
                 +Number_Sets.Int_Singleton_Set (Logical_Op_Zero (Op))
               then
                  --  Arg1 is the "zero" of the logical operator,
                  --  so we aren't going to learn anything new.
                  null;
               elsif Is_Subset (+Number_Sets.Boolean_Set, Vals,
                 Is_Ptr => False)
               then
                  --  Result is True or False, so no point in going further
                  null;
               elsif Vals =
                 +Number_Sets.Int_Singleton_Set (Logical_Op_Identity (Op))
               then
                  --  Result is identity, so both operands must be as well.
                  Constrain
                    (Arg1,
                     Include_Invalid_Value (Vals),
                     Is_Soft_Check => Is_Soft_Check);
               elsif Vals =
                 +Number_Sets.Int_Singleton_Set (Logical_Op_Zero (Op))
               then
                  --  Arg1 might be True or False.
                  --  Result is zero of op, so at most one operand can be the
                  --  identity, so if Arg2 cannot be the zero
                  --  when we assume Arg1 is the identity, then that is
                  --  a contradiction, and Arg1 must be the zero.
                  --  First compute Arg2 presuming Arg1 is the identity of
                  --  the operator.
                  Arg2_Vals := Conditional_Vals (Context,
                    Predicate                 => Arg1,
                    VN                        => Arg2,
                    Assume_Predicate_Is_False => (Op = Logical_Or_Op));

                  if not Number_Sets.Int_Is_In
                    (Logical_Op_Zero (Op), Down (Arg2_Vals))
                  then
                     --  Arg2 is not the zero, so Arg1 must be the "zero"
                     --  of the operator, to match the result.
                     Constrain
                       (Arg1,
                        Include_Invalid_Value (Vals),
                        Is_Soft_Check => Is_Soft_Check);
                  end if;

               end if;

            else
               --  Not a short-circuit, or it is safe to eval Arg2

               Arg2_Vals :=
                 Possible_Vals (Context, Arg2, Check_For_Top_Down => False);

               New_Arg1_Vals := Arg1_Possible_Values
                (Op, Res  => Vals, Arg2 => Arg2_Vals);

               New_Arg2_Vals := Arg2_Possible_Values
                (Op, Res  => Vals, Arg1 => Arg1_Vals);

               --  NOTE: See NOTE on Unexpr_VN above to understand
               --       why we call Include_Invalid_Value here.

               Constrain
                 (Arg1,
                  Include_Invalid_Value (New_Arg1_Vals),
                  Is_Soft_Check => Is_Soft_Check
                    or else Vals_Depend_On_Soft_Checks (Context, Arg2));

               Constrain
                 (Arg2,
                  Include_Invalid_Value (New_Arg2_Vals),
                  Is_Soft_Check => Is_Soft_Check
                    or else Vals_Depend_On_Soft_Checks (Context, Arg1));

               if Op in Logical_And_Or_Op
                 and then Vals =
                    +Number_Sets.Int_Singleton_Set (Logical_Op_Zero (Op))
                 and then Is_Subset (+Number_Sets.Boolean_Set, Arg1_Vals,
                    Is_Ptr => False)
                 and then Is_Subset (+Number_Sets.Boolean_Set, Arg2_Vals,
                    Is_Ptr => False)
               then
                  --  We have logical and/or, and the result is the "zero"
                  --  of the operator, but both operands are unknown.
                  --  See if conditional_vals can tell us anything.
                  --  Result is zero of op, so at most one operand can be the
                  --  identity, so if Arg2 cannot be the zero
                  --  when we assume Arg1 is the identity, then that is
                  --  a contradiction, and Arg1 must be the zero.
                  --  First compute Arg2 presuming Arg1 is the identity of
                  --  the operator.
                  New_Arg2_Vals := Conditional_Vals (Context,
                    Predicate                 => Arg1,
                    VN                        => Arg2,
                    Assume_Predicate_Is_False => (Op = Logical_Or_Op));

                  if not Number_Sets.Int_Is_In
                    (Logical_Op_Zero (Op), Down (Arg2_Vals))
                  then
                     --  Arg2 is not the zero, so Arg1 must be the "zero"
                     --  of the operator, to match the result.
                     Constrain
                       (Arg1,
                        Include_Invalid_Value (Vals),
                        Is_Soft_Check => Is_Soft_Check);
                  end if;

                  --  Now assume Arg2 is the identity, and see whether Arg1
                  --  can be the zero.
                  New_Arg1_Vals := Conditional_Vals (Context,
                    Predicate                 => Arg2,
                    VN                        => Arg1,
                    Assume_Predicate_Is_False => (Op = Logical_Or_Op));

                  if not Number_Sets.Int_Is_In
                    (Logical_Op_Zero (Op), Down (Arg1_Vals))
                  then
                     --  Arg1 is not the zero, so Arg2 must be the "zero"
                     --  of the operator, to match the result.
                     Constrain
                       (Arg2,
                        Include_Invalid_Value (Vals),
                        Is_Soft_Check => Is_Soft_Check);
                  end if;
               end if;  --  is logical and/or

            end if; --  whether is a short-circuit
         end;

      when Length_Expr_VN =>
         declare
            Args : constant Value_Number_Id_Seqs.Seq := Operands (VN);
            use Value_Number_Id_Seqs;
            Arg1 : constant Value_Number             :=
               Id_To_VN (Nth (Args, 1));
            Arg1_Vals : constant Value_Set           :=
               Possible_Vals (Context, Arg1);
            Arg2 : constant Value_Number             :=
               Id_To_VN (Nth (Args, 2));
            Arg2_Vals : constant Value_Set           :=
               Possible_Vals (Context, Arg2);
         begin
            --  NOTE: See NOTE on Unexpr_VN above to understand
            --       why we call Include_Invalid_Value here.
            Constrain
              (Arg1,
               Include_Invalid_Value
                 (Arg1_Possible_Values
                    (Length_Op,
                     Res  => Vals,
                     Arg2 => Arg2_Vals)),
               Is_Soft_Check => Is_Soft_Check
                 or else Vals_Depend_On_Soft_Checks (Context, Arg2));

            Constrain
              (Arg2,
               Include_Invalid_Value
                 (Arg2_Possible_Values
                    (Length_Op,
                     Res  => Vals,
                     Arg1 => Arg1_Vals)),
               Is_Soft_Check => Is_Soft_Check
                 or else Vals_Depend_On_Soft_Checks (Context, Arg1));
         end;

      when Kappa_VN =>
         Do_Kappa (VN, Is_Soft_Check);

         --  We have just finished a top-down propagation,
         --  so no need to do it again right away.
         Value_Number_Sets.Exclude (Context.Kappa_Predicate_Has_Shrunk, VN);

      when Phi_VN =>
         Do_Phi (VN, Is_Soft_Check);

      when Table_Element_VN =>
         Do_Table_Element (VN, Is_Soft_Check);

      --  For the rest, they don't have any operands, or we don't
      --  have any easy way for pushing conditions down to them.
      when Literal_VN      |
           Invalid_VN      |
           External_VN     |
           Table_VN        |
           Address_VN      |
           Others_VN       |
           Call_Effect_VN  |
           Unknown_Call_VN =>
         null;
   end case;

end Propagate_Top_Down;
