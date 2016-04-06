## vim: ft=makoada

with Ada.Finalization;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Unchecked_Deallocation;

with Langkit_Support.Symbols; use Langkit_Support.Symbols;
with Langkit_Support.Token_Data_Handler;
use Langkit_Support.Token_Data_Handler;

--  The purpose of this package is to provide parsing and evaluation facilities
--  for a tiny DSL used to explore a Libadalang analysis unit.
--
--  This tiny DSL is currently re-using the parser for Ada expressions and we
--  implement here a custom evaluator for the resulting expressions. Here is
--  an example to show how to use this package::
--
--    Expr : Expression := Parse_Expression (Code_As_A_String);
--    if Expr /= No_Expression then
--       Value : Eval_Result := Eval (Expr, Analysis_Unit_Root_Node)
--
--       if Value.Kind = Error_Value then
--          Put_Line ("error: " & To_String (Value.Message));
--       else
--          Put_Line (Value);
--       end if;
--       Destroy (Expr);
--    end if;

package Libadalang.AST.Types.Parsers.Test is

   type Expression is private;
   --  Holder for a parsed DSL expression

   No_Expression : constant Expression;
   --  Constant representing the absence of expression (no resources
   --  allocated).

   function Parse_Expression (Buffer : String) return Expression;
   --  Try to parse the DSL expression in Buffer and return the corresponding
   --  Expresion. It is up to the caller to invoke Destroy on the return value
   --  when done with it, in order to free the allocated resources.
   --
   --  If Buffer triggers parsing errors, No_Expression is returned and the
   --  corresponding diagnostics are output on standard output.

   procedure Destroy (E : in out Expression);
   --  Free the resources allocated to the E expression and set it to
   --  No_Expression.

   type Eval_Result_Kind is
     (Boolean_Value,
      --  Value for mere boolean values

      Integer_Value,
      --  Value for mere integer values

      % for cls in eval_types:
         ${enum_for_type(cls)},
      % endfor

      Ada_Node_Value,
      --  Value for generic AST nodes

      Ada_Node_Iterator_Value,
      --  Value for iterators that yield AST nodes

      Token_Value,
      --  Value for tokens

      Lexical_Env_Value,
      --  Value for lexical environments

      Field_Access_Value,
      --  Value for fields that still have to be called (i.e. properties which
      --  take explicit arguments).

      Find_Builtin_Value,
      --  Value for the .Find builtin method

      Symbol_Value,
      --  Value for symbols

      Error_Value
      --  Value resulting from an error during evaluation
     );
   --  Discriminants for Eval_Result_Record: each value designates a specific
   --  kind of value (AST node, Token, array, ...).

   type Eval_Result_Record (Kind : Eval_Result_Kind) is limited record
      Ref_Count : Natural;
      --  Eval_Result is supposed to be dynamically allocated and shared by
      --  reference. This is a count of the number of references to a
      --  particular value so that we know when to deallocate it.

      case Kind is
      when Boolean_Value  => Bool : Boolean;
      when Integer_Value  => Int  : Integer;

      % for cls in eval_types:
         when ${enum_for_type(cls)} =>
            ${field_for_type(cls)} : ${cls.name()};
      % endfor

      when Ada_Node_Value          => Node        : Ada_Node;
      when Ada_Node_Iterator_Value =>
         Node_Iter   : Ada_Node_Iterators.Iterator_Access;
      when Token_Value             =>
        Unit  : Analysis_Unit_Interface;
        Index : Token_Index;
      when Lexical_Env_Value       => Lexical_Env : AST_Envs.Lexical_Env;
      when Field_Access_Value      =>
        Field_Node  : Ada_Node;
        Field_Name  : Symbol_Type;

      when Find_Builtin_Value      => Find_Root   : Ada_Node;

      when Symbol_Value =>       Symbol      : Symbol_Type;
      when Error_Value =>
         Sub_Expr : Ada_Node;
         --  Smallest subexpression whose evaluation triggered an error

         Message  : Unbounded_String;
         --  Message describing the error that occured
      end case;
   end record;
   --  Data structure for the result of the evaluation of a DSL expression

   type Eval_Result_Access is access Eval_Result_Record;

   procedure Destroy (Value : in out Eval_Result_Access);
   --  Deallocate all resources associated to Value

   type Eval_Result is new Ada.Finalization.Controlled with record
      Value : Eval_Result_Access;
   end record;
   --  Reference to the result of the evaluation of a DSL expression. Thanks to
   --  the fact that this is controlled, reference counting is automatic and
   --  thus resources are automatically deallocated when nobody has references
   --  to it anymore.
   --
   --  Evaluation returns only reference so that the values are shared: in the
   --  general case, it is not possible to duplicate a value (for instance
   --  because of iterators).

   function Eval (E : Expression; Root : Ada_Node) return Eval_Result;
   --  Try to evaluate the E expression on the analysis unit whose root AST
   --  node is Root.
   --
   --  Note that this function is not supposed to raise an exception: if an
   --  error occurs, a Error_Value Eval_Result object is returned.

   overriding
   procedure Adjust (V : in out Eval_Result);
   overriding
   procedure Finalize (V : in out Eval_Result);

private

   type Expression_Type is record
      Ctx  : Analysis_Context;
      Unit : Analysis_Unit;
   end record;

   type Expression is access Expression_Type;
   No_Expression : constant Expression := null;

end Libadalang.AST.Types.Parsers.Test;
