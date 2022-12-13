--  Rename controlling parameters of primitive subprograms to "Self". Write the
--  result in ".new" suffixed files.

with Ada.Text_IO;

with Langkit_Support.Text, Libadalang.Analysis, Libadalang.Common,
     Libadalang.Iterators, Libadalang.Rewriting;

with Helpers;

procedure Rewrite_Self_Arg is

   package TIO renames Ada.Text_IO;
   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package LAL_RW renames Libadalang.Rewriting;
   package LAL_It renames Libadalang.Iterators;

   use type LALCO.Ada_Node_Kind_Type;

   Units : Helpers.Unit_Vectors.Vector;

   Ctx : constant LAL.Analysis_Context :=
     Helpers.Initialize ("material.gpr", Units);

   Handle : LAL_RW.Rewriting_Handle := LAL_RW.Start_Rewriting (Ctx);

   function Is_Subp (N : LAL.Ada_Node) return Boolean is
     (LAL.Kind (N) = LALCO.Ada_Subp_Spec);

begin

   for Unit of Units loop
      for Node of LAL_It.Find (LAL.Root (Unit), Is_Subp'Access).Consume loop
         declare
            use type LAL_RW.Node_Rewriting_Handle;

            Subp_Spec : constant LAL.Subp_Spec := Node.As_Subp_Spec;
            --  Subprogram specification to rewrite

            Primitive_Of : LAL.Base_Type_Decl;
            --  Subp_Spec is a primitive of this

            Id : LAL_RW.Node_Rewriting_Handle
              := LAL_RW.No_Node_Rewriting_Handle;
            --  Formal identifier to rewrite into Self
         begin

            Primitive_Of := Subp_Spec.P_Primitive_Subp_Types (1);

            if not Primitive_Of.Is_Null
              and then not Subp_Spec.F_Subp_Params.Is_Null
            then
               --  Look for a parameter whose type is the same as Primitive_Of.
               --  Abort rewriting if there are more than one such parameter.
               Primitive_Of := Primitive_Of.P_Canonical_Type;

               for Param_Spec of Subp_Spec.F_Subp_Params.F_Params.Children loop
                  declare
                     use type LAL.Base_Type_Decl;

                     PS         : constant LAL.Param_Spec :=
                       Param_Spec.As_Param_Spec;
                     Param_Type : constant LAL.Base_Type_Decl :=
                       PS.F_Type_Expr.P_Designated_Type_Decl.P_Canonical_Type;
                  begin
                     if Param_Type = Primitive_Of
                       and then Id = LAL_RW.No_Node_Rewriting_Handle
                       and then PS.F_Ids.Children_Count = 1
                     then
                        Id := LAL_RW.Handle
                          (PS.F_Ids.Child (1).As_Defining_Name.F_Name);
                     end if;
                  end;
               end loop;

               --  Rename the formal!
               if Id /= LAL_RW.No_Node_Rewriting_Handle then
                  LAL_RW.Set_Text (Id, "Self");
               end if;

            end if;
         end;
      end loop;
   end loop;

   --  Write results

   declare
      Units  : Helpers.Unit_Vectors.Vector;
      Result : LAL_RW.Apply_Result;
   begin
      --  Remember which analysis units are to be rewritten. Keep track of them
      --  using Analysis_Unit values, since the call to LAL_RW.Apply will make
      --  all rewriting handles invalid.

      for Unit_Handle of LAL_RW.Unit_Handles (Handle) loop
         Units.Append (LAL_RW.Unit (Unit_Handle));
      end loop;

      Result := LAL_RW.Apply (Handle);

      if not Result.Success then
         TIO.Put_Line ("Error during rewriting...");
      end if;

      --  Go through all rewritten units and generate a ".new" source file to
      --  contain the rewritten sources.

      for U of Units loop
         declare
            Filename : constant String := LAL.Get_Filename (U) & ".new";
            Charset  : constant String := LAL.Get_Charset (U);

            --  Retreive rewritten text, and encode it using the same encoding
            --  as in the original file.
            Content_Text  : constant Langkit_Support.Text.Text_Type :=
              LAL.Text (U);
            Content_Bytes : constant String :=
              Langkit_Support.Text.Encode (Content_Text, Charset);

            Output_File : TIO.File_Type;
         begin
            TIO.Create (Output_File, TIO.Out_File, Filename);
            TIO.Put (Output_File, Content_Bytes);
            TIO.Close (Output_File);
         end;
      end loop;
   end;

end Rewrite_Self_Arg;
