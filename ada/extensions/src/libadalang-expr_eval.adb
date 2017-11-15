package body Libadalang.Expr_Eval is

   ---------------
   -- Expr_Eval --
   ---------------

   function Expr_Eval (E : LAL.Expr) return Eval_Result is

      ---------------
      -- Eval_Decl --
      ---------------

      function Eval_Decl (D : LAL.Basic_Decl) return Eval_Result is
      begin
         case D.Kind is
            when LAL.Ada_Enum_Literal_Decl =>

               --  An enum literal declaration evaluates to itself
               return (Enum_Lit,
                       D.As_Enum_Literal_Decl.P_Enum_Type,
                       D.As_Enum_Literal_Decl);

            when LAL.Ada_Number_Decl =>

               --  A number declaration evaluates to the evaluation of its
               --  expression.
               return Expr_Eval (D.As_Number_Decl.F_Expr);

            when others =>
               raise LAL.Property_Error;
         end case;
      end Eval_Decl;

   begin
      case E.Kind is
         when LAL.Ada_Base_Id =>

            return Eval_Decl (E.As_Base_Id.P_Referenced_Decl);

         when LAL.Ada_Int_Literal =>
            return (Int,
                    E.P_Universal_Int_Type.As_Base_Type_Decl,
                    Long_Integer'Value (LAL.Text (E.As_Int_Literal.F_Tok)));

         when LAL.Ada_Real_Literal =>
            return (Real,
                    E.P_Universal_Real_Type.As_Base_Type_Decl,
                    Long_Float'Value (LAL.Text (E.As_Real_Literal.F_Tok)));

         when LAL.Ada_Bin_Op =>
            raise LAL.Property_Error;
         when others =>
            raise LAL.Property_Error;
      end case;
   end Expr_Eval;

   ------------
   -- As_Int --
   ------------

   function As_Int (Self : Eval_Result) return Integer is
   begin
      case Self.Kind is
         when Int => return Integer (Self.Int_Result);
         when Real => raise LAL.Property_Error;
         when Enum_Lit => return Self.Enum_Result.Child_Index + 1;
      end case;
   end As_Int;

   -----------
   -- Image --
   -----------

   function Image (Self : Eval_Result) return String is
   begin
      return "<Eval_Result "
        & Self.Kind'Image & " "
        & (case Self.Kind is
              when Int => Self.Int_Result'Image,
              when Real => Self.Real_Result'Image,
              when Enum_Lit => Self.Enum_Result.Short_Image) & ">";
   end Image;

end Libadalang.Expr_Eval;
