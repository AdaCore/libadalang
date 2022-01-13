--  Enumerate all tagged types that are derived and count how many derivations
--  they have.

with Ada.Containers.Hashed_Maps;
with Ada.Text_IO;

with Libadalang.Analysis;
with Libadalang.Common;

with Helpers;

procedure Derivation_Count is

   package TIO renames Ada.Text_IO;
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   use type LALCO.Ada_Node_Kind_Type;

   function Hash (TD : LAL.Base_Type_Decl) return Ada.Containers.Hash_Type is
     (LAL.Ada_Node'Class (TD).Hash);

   package Derivation_Histogram_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => LAL.Base_Type_Decl,
      Element_Type    => Positive,
      Hash            => Hash,
      Equivalent_Keys => LAL."=");

   Histogram : Derivation_Histogram_Maps.Map;
   --  Mapping of tagged type declarations to the number of times this type
   --  declaration is derived.

   function Process_Node (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status;

   ------------------
   -- Process_Node --
   ------------------

   function Process_Node (Node : LAL.Ada_Node'Class) return LALCO.Visit_Status
   is
      use Derivation_Histogram_Maps;

      Base     : LAL.Base_Type_Decl;
      Derived  : LAL.Derived_Type_Def;
      Cur      : Cursor;
      Inserted : Boolean;
   begin
      --  Skip all nodes that are not the derivation of a tagged type, or that
      --  are "with private".
      if Node.Kind /= LALCO.Ada_Derived_Type_Def then
         return LALCO.Into;
      end if;

      Derived := Node.As_Derived_Type_Def;
      if Derived.F_Has_With_Private.P_As_Bool
        or else not Derived.Parent.As_Type_Decl.P_Is_Tagged_Type
      then
         return LALCO.Into;
      end if;

      --  Get the canonical view for the type from which Derived is derived
      Base :=
        Derived.F_Subtype_Indication.P_Designated_Type_Decl.P_Canonical_Type;

      --  Increment its derivation count
      Histogram.Insert (Base, 1, Cur, Inserted);
      if not Inserted then
         Histogram.Replace_Element (Cur, Element (Cur) + 1);
      end if;

      return LALCO.Into;
   end Process_Node;

   Units : Helpers.Unit_Vectors.Vector;

   Ctx : constant LAL.Analysis_Context :=
     Helpers.Initialize ("material.gpr", Units);

   pragma Unreferenced (Ctx);
begin
   for Unit of Units loop
      LAL.Root (Unit).Traverse (Process_Node'Access);
   end loop;

   for Cur in Histogram.Iterate loop
      declare
         use Derivation_Histogram_Maps;
      begin
         TIO.Put_Line (Key (Cur).Image & " is derived"
                       & Positive'Image (Element (Cur)) & " times");
      end;
   end loop;
end Derivation_Count;
