## vim: ft=makoada

with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Wide_Wide_Characters.Handling; use Ada.Wide_Wide_Characters.Handling;

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;
with Langkit_Support.Text;        use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

package body Libadalang.AST.Types.Parsers.Test is

   function "+" (S : String) return Unbounded_String
      renames To_Unbounded_String;
   function "+" (S : Unbounded_String) return String
      renames To_String;

   function "+" (S : Wide_Wide_String) return Unbounded_Wide_Wide_String
      renames To_Unbounded_Wide_Wide_String;
   function "+" (S : Unbounded_Wide_Wide_String) return Wide_Wide_String
      renames To_Wide_Wide_String;

   type Eval_Result_Array is array (Natural range <>) of Eval_Result;

   ---------------
   -- Kind_Name --
   ---------------

   function Kind_Name (K : Eval_Result_Kind) return String is
     (case K is
      when Boolean_Value  => "boolean",
      when Integer_Value  => "integer",

      % for cls in ctx.sorted_types(ctx.enum_types):
         when ${enum_for_type(cls)} => "${cls.name()} enumeration",
      % endfor

      % for cls in ctx.sorted_types(ctx.struct_types):
         when ${enum_for_type(cls)} => "${cls.name()} structure",
      % endfor

      % for cls in ctx.sorted_types(ctx.array_types):
         when ${enum_for_type(cls)} => "${cls.name()} array",
      % endfor

      when Ada_Node_Value          => "AST node",
      when Ada_Node_Iterator_Value => "AST node iterator",
      when Token_Value             => "token",
      when Lexical_Env_Value       => "lexical environment",
      when Field_Access_Value      => "access to field requiring arguments",
      when Find_Builtin_Value      => ".Find builtin method",
      when Symbol_Value            => "symbol",
      when Logic_Var_Value         => "logic variable",
      when Equation_Value          => "logic equation",
      when Error_Value             => raise Program_Error);

   function Create (V : Eval_Result_Access) return Eval_Result;
   --  Initialize the refcount for V and return a reference to it

   type Identifier_Filter is new Ada_Node_Predicate_Type with record
      Name : Unbounded_Wide_Wide_String;
   end record;
   --  Predicate that returns whether a node is an identifier and has some
   --  specific associated text.

   function Token_Text (Token : Token_Type) return Text_Type is
     (Data (Token).Text.all);

   function Evaluate
     (P : access Identifier_Filter;
      N : Ada_Node)
      return Boolean
   is
     (Kind (N) = Ada_Identifier
      and then Token_Text (F_Tok (Single_Tok_Node (N))) = +P.Name);

   ------------
   -- Create --
   ------------

   function Create (V : Eval_Result_Access) return Eval_Result is
   begin
      V.Ref_Count := 1;
      return (Ada.Finalization.Controlled with
              Value => V);
   end Create;

   ----------------------
   -- Parse_Expression --
   ----------------------

   function Parse_Expression (Buffer : String) return Expression is
      Expr   : Expression := new Expression_Type;
   begin
      Expr.Ctx := Create;
      Expr.Unit := Get_From_Buffer
        (Context  => Expr.Ctx,
         Filename => "<input>",
         Buffer   => Buffer,
         Rule     => Expression_Rule);

      if Has_Diagnostics (Expr.Unit) then
         Put_Line ("Parsing failed:");
         for D of Diagnostics (Expr.Unit) loop
            Put_Line (To_Pretty_String (D));
         end loop;
         Destroy (Expr);
      end if;

      return Expr;
   end Parse_Expression;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (E : in out Expression) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Expression_Type, Expression);
   begin
      if E /= null then
         Destroy (E.Ctx);
         Free (E);
      end if;
   end Destroy;

   ----------
   -- Eval --
   ----------

   function Eval (E : Expression; Root : Ada_Node) return Eval_Result is

      --  For convenience, all expression evaluation code is embedded in this
      --  function. On the other hand, this root Eval function is basically
      --  just a wrapper for the inner Eval one. This inner function dispatches
      --  the evaluation to other Eval_* functions according to the type of the
      --  root DSL expression node it is provided.

      Evaluation_Error : exception;
      --  Internal exception: see Raise_Error

      Error : Eval_Result_Record (Error_Value);
      --  Internal error holder: see Raise_Error

      procedure Raise_Error
        (Expr    : access Ada_Node_Type'Class;
         Message : String)
         with No_Return => True;
      --  When one of the Eval* functions below notifies an error in the input
      --  expression (invalid identifier, out-of-bounds access, etc.), it must
      --  call Raise_Error in order to abort the expression evaluation and
      --  return to the caller the Error_Value Eval_Result object that
      --  describes what happened.
      --
      --  The implementation is straightforward: Raise_Error creates the
      --  Eval_Result instance and stores it in the Error local variable. Then,
      --  it raises a Evaluation_Error exception, which is supposed to be
      --  caught in the top-level Eval function. This Eval function discards
      --  processing and just returns the Error variable.

      function Eval (Expr : access Ada_Node_Type'Class) return Eval_Result;
      --  Evaluation entry point for arbitrary sub-expressions, return the
      --  corresponding result or invoke Raise_Error.

      function Eval_Call (Expr : Call_Expr) return Eval_Result;
      --  Return a call expression evaluation (method invocation or array
      --  subscript) or invoke Raise_Error.

      function Eval_Find
        (Expr   : Ada_Node;
         Root   : Ada_Node;
         Params : Param_List)
         return Eval_Result;
      --  Given a Expr sub-expression which is a Find method invocation, the
      --  Root node used as the root for the AST node lookup and Params, the
      --  Param_List associated to this Find method call, return the evaluation
      --  of the Find method or invoke Raise_Error.

      function Eval_Identifier (Expr : Identifier) return Eval_Result;
      --  Return a mere identifier expression evaluation or invoke Raise_Error

      function Eval_Dotted_Name (Expr : Dotted_Name) return Eval_Result;
      --  Return a dotted name (X.Y) expression evaluation or invoke Raise_Error

      function Eval_Node_Kind
        (Expr : access Ada_Node_Type'Class)
         return Ada_Node_Kind_Type;
      --  Assuming Expr is an expression that only contains a name, try to turn
      --  it into an AST node kind. If there is no such kind of if Expr is
      --  anything else, invoke Raise_Error.

      --  The following functions evaluate a field access for the field
      --  "Field_Cmp" (lower case name of the field) in the Prefix AST node. In
      --  the context of the Expr expression evaluation.
      --
      --  Two cases: either 1) Param_Value is empty, then it's a mere field
      --  access (no additional arguments), either 2) it is not empty, then
      --  it's a method evaluation with explicit arguments.
      --
      --  In the case of 1), If the field does not exist, Raise_Error is
      --  invoked. If the field really takes no argument, then the evaluation
      --  goes until completion. Otherwise, it returns a Field_Access_Value
      --  Eval_Result to defer evaluation until a call expression provides
      --  arguments.
      --
      --  In the case of 2), receiving a field that does not exist or that
      --  takes no argument is a bug (a Program_Error is raised). Otherwise the
      --  evaluation is done as usual.

      % for cls in ctx.astnode_types:
         % if not cls.abstract:
            function Eval_${cls.name()}_Field_Access
              (Expr         : access Ada_Node_Type'Class;
               Prefix_Node  : ${cls.name()};
               Field        : Symbol_Type;
               Field_Cmp    : Wide_Wide_String;
               Param_Values : Eval_Result_Array)
               return Eval_Result;
         % endif
      % endfor

      function Eval_Node_Field_Access
        (Expr         : access Ada_Node_Type'Class;
         Prefix_Node  : Ada_Node;
         Field        : Symbol_Type;
         Field_Cmp    : Wide_Wide_String;
         Param_Values : Eval_Result_Array)
         return Eval_Result;
      --  Likewise, but take any kind of node as the Prefix. Depending on the
      --  concrete node kind, it dispatches to one of the above.

      procedure Check_In_Array_Bound
        (Expr        : access Ada_Node_Type'Class;
         Index       : Natural;
         First, Last : Natural);
      --  Check that Index is in First .. Last. If it's not, invoke Raise_Error
      --  with Expr as the context expression.

      ----------
      -- Eval --
      ----------

      function Eval (Expr : access Ada_Node_Type'Class) return Eval_Result is
      begin
         case Kind (Expr) is
         when Ada_Call_Expr =>
            return Eval_Call (Call_Expr (Expr));
         when Ada_Identifier =>
            return Eval_Identifier (Identifier (Expr));
         when Ada_Int_Literal =>
            declare
               Text : constant Text_Type :=
                  Token_Text (F_Tok (Single_Tok_Node (Expr)));
            begin
               return Create (new Eval_Result_Record'
                 (Kind      => Integer_Value,
                  Ref_Count => <>,
                  Int       => Integer'Value (Image (Text))));
            end;
         when Ada_Dotted_Name =>
            return Eval_Dotted_Name (Dotted_Name (Expr));
         when others =>
            Raise_Error (Expr, "Unhandled expression: " & Kind_Name (Expr));
         end case;
      end Eval;

      ---------------
      -- Eval_Call --
      ---------------

      function Eval_Call (Expr : Call_Expr) return Eval_Result is

         function Get_Single_Index (Params : Param_List) return Integer;
         --  If Params is anything else than a list of exactly one integer
         --  parameter without any designator, raise an error. Otherwise,
         --  evaluate this integer and return it.

         function Eval_Params (Params : Param_List) return Eval_Result_Array;
         --  Evaluate each actual in Params and return an array for these.
         --  This raises an error if any parameter has a designator (we don't
         --  support them).

         ----------------------
         -- Get_Single_Index --
         ----------------------

         function Get_Single_Index (Params : Param_List) return Integer is
            Index_Expr : Ada_Node;
            Exists     : Boolean;
         begin
            --  Invoke Raise_Error if we have more or less that 1 parameter in
            --  Params.

            if Child_Count (Params) /= 1 then
               Raise_Error (Params, "Exactly one index is expected");
            end if;

            Params.F_Params.Get_Child (1, Exists, Index_Expr);
            pragma Assert (Exists);

            --  Likewise if the kind of the parameter is unexpected or if
            --  it's not a simple form (i.e. X => Y instead of Y).

            if Kind (Index_Expr) /= Ada_Param_Assoc then
               Raise_Error (Params,
                            "Invalid index: " & Kind_Name (Index_Expr));
            elsif Param_Assoc (Index_Expr).F_Designator /= null then
               Raise_Error (Params, "No designator allowed for subscript");
            end if;

            --  Now, try to get an integer out of this expression

            Index_Expr := Ada_Node (Param_Assoc (Index_Expr).F_Expr);
            declare
               Index : constant Eval_Result := Eval (Index_Expr);
            begin
               if Index.Value.Kind /= Integer_Value then
                  Raise_Error (Index_Expr,
                               "Invalid index: "
                               & Kind_Name (Index.Value.Kind));
               end if;
               return Index.Value.Int;
            end;
         end Get_Single_Index;

         -----------------
         -- Eval_Params --
         -----------------

         function Eval_Params (Params : Param_List) return Eval_Result_Array is
            Result : Eval_Result_Array (1 .. Params.F_Params.Child_Count);
         begin
            for I in Result'Range loop
               declare
                  Assoc  : Ada_Node;
                  Exists : Boolean;
               begin
                  Params.F_Params.Get_Child (I, Exists, Assoc);
                  pragma Assert (Exists);

                  if Kind (Assoc) /= Ada_Param_Assoc then
                     Raise_Error
                       (Assoc, "Invalid parameter: " & Kind_Name (Assoc));
                  elsif Param_Assoc (Assoc).F_Designator /= null then
                     Raise_Error (Assoc, "Designator not allowed here");
                  end if;

                  Result (I) := Eval (Param_Assoc (Assoc).F_Expr);
               end;
            end loop;
            return Result;
         end Eval_Params;

         Name   : constant Eval_Result := Eval (Expr.F_Name);
         Params : Param_List;
      begin
         --  This is more like a sanity check: for Call_Expr nodes, we don't
         --  expect anything else than a Param_List suffix.

         if Kind (Expr.F_Suffix) /= Ada_Param_List then
            Raise_Error (Expr,
                         "Invalid " & Kind_Name (Expr.F_Suffix)
                         & " suffix (ParamList expected)");
         end if;

         Params := Param_List (Expr.F_Suffix);

         --  What this expression really do depend on the kind of the name
         --  (aka. "prefix"): it can be either a call or an array subscript.

         case Name.Value.Kind is

            --  If it's an array, try to fetch the Nth element

            % for cls in ctx.sorted_types(ctx.array_types):
               when ${enum_for_type(cls)} =>
                  declare
                     Index  : Integer := Get_Single_Index (Params);
                     A      : ${cls.api_name()} renames
                                 Name.Value.${field_for_type(cls)}.Items;
                  begin
                     Check_In_Array_Bound (Expr, Index, A'First, A'Last);
                     return Create (new Eval_Result_Record'
                       (Kind      => ${enum_for_type(cls.element_type())},
                        Ref_Count => <>,
                        ${field_for_type(cls.element_type())} =>
                           <% value = 'A (Index)' %>
                           % if is_ast_node(cls.element_type()):
                              Ada_Node (${value})
                           % else:
                              ${value}
                           % endif
                           ));
                  end;
            % endfor

            --  If it's an AST node, try to fetch the Nth child

            when Ada_Node_Value =>
               declare
                  Index  : constant Integer := Get_Single_Index (Params);
                  Result : Ada_Node;
                  Exists : Boolean;
               begin
                  Name.Value.Node.Get_Child (Index, Exists, Result);
                  if not Exists then
                     Raise_Error
                       (Expr,
                        "Out of bounds index: "
                        & Integer'Image (Index)
                        & " not in "
                        & Integer'Image (1) & " .. "
                        & Integer'Image (Child_Count (Name.Value.Node)));
                  end if;
                  return Create (new Eval_Result_Record'
                    (Kind      => Ada_Node_Value,
                     Ref_Count => <>,
                     Node      => Result));
               end;

            --  If it's an AST node iterator, try to fetch the Nth yielded node

            when Ada_Node_Iterator_Value =>
               declare
                  It       : Ada_Node_Iterators.Iterator'Class renames
                     Name.Value.Node_Iter.all;
                  Index    : constant Integer := Get_Single_Index (Params);
                  Result   : Ada_Node;
                  Has_Next : Boolean := True;
               begin
                  if Index < 0 then
                     Raise_Error
                       (Params,
                        "Invalid iterator element index: "
                        & Integer'Image (Index));
                  end if;

                  for I in 0 .. Index loop
                     Has_Next := It.Next (Result);
                     if not Has_Next then
                        Raise_Error
                          (Params,
                           "Iterator stopped after yielding"
                           & Integer'Image (I) & " elements, the"
                           & Integer'Image (Index) & "th one was expected");
                     end if;
                  end loop;

                  return Create (new Eval_Result_Record'
                    (Kind      => Ada_Node_Value,
                     Ref_Count => <>,
                     Node      => Result));
               end;

            when Field_Access_Value =>
               return Eval_Node_Field_Access
                 (Expr,
                  Name.Value.Field_Node,
                  Name.Value.Field_Name,
                  To_Lower (Name.Value.Field_Name.all),
                  Eval_Params (Params));

            when Find_Builtin_Value =>
               return Eval_Find
                 (Ada_Node (Expr), Name.Value.Find_Root, Params);

            when others =>
               Raise_Error
                 (Expr, "Cannot subscript a " & Kind_Name (Name.Value.Kind));
         end case;
      end Eval_Call;

      ---------------
      -- Eval_Find --
      ---------------

      function Eval_Find
        (Expr   : Ada_Node;
         Root   : Ada_Node;
         Params : Param_List)
         return Eval_Result
      is
         Expected_Arg : constant String :=
           ("the name of the kind for searched nodes or a string to look for a"
            & " SingleTokNode");

         Param_Assoc_Node : Ada_Node;
         Param_Expr       : Libadalang.AST.Types.Expr;
         Exists           : Boolean;
         Filter           : Ada_Node_Predicate;
      begin
         --  Invoke Raise_Error if we have more or less that 1 parameter in
         --  Params.

         if Child_Count (Params) /= 1 then
            Raise_Error (Params, "Exactly one argument is expected: "
                                 & Expected_Arg);
         end if;

         Params.F_Params.Get_Child (1, Exists, Param_Assoc_Node);
         pragma Assert (Exists);

         --  Likewise if the kind of the parameter is unexpected or if
         --  it's not a simple form (i.e. X => Y instead of Y).

         if Kind (Param_Assoc_Node) /= Ada_Param_Assoc then
            Raise_Error (Params,
                         "Invalid argument: " & Kind_Name (Param_Assoc_Node));
         elsif Param_Assoc (Param_Assoc_Node).F_Designator /= null then
            Raise_Error (Params, "No designator allowed for .Find methods");
         end if;
         Param_Expr := Param_Assoc (Param_Assoc_Node).F_Expr;

         --  Now, yield a filter predicate depending on the type of this
         --  argument.

         case Kind (Param_Expr) is
            when Ada_Identifier =>
               Filter := new Ada_Node_Kind_Filter'
                 (Kind => Eval_Node_Kind (Param_Expr));

            when Ada_String_Literal =>
               declare
                  Str : Text_Type renames
                     Token_Text (F_Tok (Single_Tok_Node (Param_Expr)));
               begin
                  --  Assume that the first and last characters are quotes and
                  --  strip them.

                  Filter := new Identifier_Filter'
                    (Name => +Str (Str'First + 1 .. Str'Last - 1));
               end;

            when others =>
               Raise_Error
                 (Expr, "Invalid Find argument: got " & Kind_Name (Param_Expr)
                        & " but expected " & Expected_Arg);
         end case;

         return Create (new Eval_Result_Record'
           (Kind      => Ada_Node_Iterator_Value,
            Ref_Count => <>,
            Node_Iter => new Find_Iterator'(Find (Root, Filter))));
      end Eval_Find;

      ---------------------
      -- Eval_Identifier --
      ---------------------

      function Eval_Identifier (Expr : Identifier) return Eval_Result is
         Ident     : constant Wide_Wide_String :=
            Token_Text (F_Tok (Single_Tok_Node (Expr)));
         Ident_Cmp : constant Wide_Wide_String := To_Lower (Ident);
      begin
         --  The only identifier available so far is the analysis unit root
         --  node.

         if Ident_Cmp = "root" then
            return Create (new Eval_Result_Record'
              (Kind => Ada_Node_Value, Ref_Count => <>, Node => Root));

         else
            Raise_Error
              (Expr, "Undefined identifier: " & Image (Ident));
         end if;
      end Eval_Identifier;

      ----------------------
      -- Eval_Dotted_Name --
      ----------------------

      function Eval_Dotted_Name (Expr : Dotted_Name) return Eval_Result is
         Pref  : constant Eval_Result := Eval (Expr.F_Prefix);
         Ident : Symbol_Type;
      begin
         --  The only dotted name  form we handle here is X.Y where X is any
         --  valid expression and Y is a static name.

         if Kind (Expr.F_Suffix) /= Ada_Identifier then
            Raise_Error (Expr,
                         "Invalid " & Kind_Name (Expr.F_Suffix)
                         & " suffix (Identifier expected)");
         end if;
         Ident := Get_Symbol (F_Tok (Single_Tok_Node (Expr.F_Suffix)));

         declare
            --  We want to be case insensitive, so keep Ident_Cmp to perform
            --  lower case string comparisons.

            Ident_Cmp : constant Wide_Wide_String :=
               To_Lower (Ident.all);
         begin
            --  Now, field access (validation) completely depends of the prefix
            --  used in the expression.

            case Pref.Value.Kind is

            --  If the prefix is a structure, then we know directly the set of
            --  valid fields.

            % for cls in ctx.sorted_types(ctx.struct_types):
               when ${enum_for_type(cls)} =>
                  <% fields = cls.get_abstract_fields(
                                  include_inherited=True) %>
                  if Ident_Cmp = "" then
                     ## This should not happen, this is just a handy case
                     ## for code generation.
                     raise Program_Error;
                  % for f in fields:
                     <%
                        field_access = 'Pref.Value.{}.{}'.format(
                           field_for_type(cls), f.name
                        )
                     %>
                     elsif Ident_Cmp = "${f.name.lower}" then
                     % if is_ast_node(f.type):
                        return Create (new Eval_Result_Record'
                          (Kind      => Ada_Node_Value,
                           Ref_Count => <>,
                           Node      => Ada_Node (${field_access})));
                     % elif is_token_type(f.type):
                        return Create (new Eval_Result_Record'
                          (Kind      => ${enum_for_type(f.type)},
                           Ref_count => <>,
                           Unit      => Root.Unit,
                           Index     => ${field_access});
                     % else:
                        return Create (new Eval_Result_Record'
                          (Kind      => ${enum_for_type(f.type)},
                           Ref_count => <>,
                           ${field_for_type(f.type)} => ${field_access}));
                     % endif
                  % endfor
                  else
                     Raise_Error
                       (Expr.F_Suffix,
                        "${cls.name()} has no " & Image (Ident.all)
                        & " field; valid ones are:"
                        % for f in fields:
                           & " ${f.name}"
                        % endfor
                        );
                  end if;
            % endfor

            --  If the prefix is an AST node, we have first to get the kind of
            --  the node in order to know what (possibly inheritted) fields are
            --  available.

            when Ada_Node_Value =>

               --  Special case: "Find" is not a field, but we use this syntax
               --  to perform AST node lookup.

               if Ident_Cmp = "find" then
                  return Create (new Eval_Result_Record'
                    (Kind      => Find_Builtin_Value,
                     Ref_Count => <>,
                     Find_Root => Pref.Value.Node));

               elsif Ident_Cmp = "parent" then
                  return Create (new Eval_Result_Record'
                    (Kind      => Ada_Node_Value,
                     Ref_Count => <>,
                     Node      => Pref.Value.Node.Parent));

               else
                  return Eval_Node_Field_Access
                    (Expr,
                     Pref.Value.Node,
                     Ident,
                     Ident_Cmp,
                     (1 .. 0 => <>));
               end if;

            when Error_Value =>
               raise Program_Error;

            when others =>
               Raise_Error
                 (Expr, Kind_Name (Pref.Value.Kind) & " have no field");
            end case;
         end;
      end Eval_Dotted_Name;

      --------------------
      -- Eval_Node_Kind --
      --------------------

      function Eval_Node_Kind
        (Expr : access Ada_Node_Type'Class)
         return Ada_Node_Kind_Type
      is
      begin
         if Kind (Expr) /= Ada_Identifier then
            Raise_Error
              (Expr,
               "Invalid argument: identifier expected but got "
               & Kind_Name (Expr) & " instead");
         end if;

         declare
            Ident     : constant Symbol_Type :=
               Get_Symbol (F_Tok (Single_Tok_Node (Expr)));
            Ident_Cmp : constant Wide_Wide_String := To_Lower (Ident.all);
         begin
            if Ident_Cmp = "" then
               ## This should not happen, this is just a handy case
               ## for code generation.
               raise Program_Error;
            % for cls in ctx.astnode_types:
               % if not cls.abstract:
                  elsif Ident_Cmp = "${cls.name().lower}" then
                     return ${cls.ada_kind_name()};
               % endif
            % endfor
            else
               Raise_Error (Expr, "Invalid node kind: " & Image (Ident.all));
            end if;
         end;
      end Eval_Node_Kind;

      % for cls in ctx.astnode_types:
         % if not cls.abstract:

      function Eval_${cls.name()}_Field_Access
        (Expr         : access Ada_Node_Type'Class;
         Prefix_Node  : ${cls.name()};
         Field        : Symbol_Type;
         Field_Cmp    : Wide_Wide_String;
         Param_Values : Eval_Result_Array)
         return Eval_Result
      is
         Params : Param_List;
      begin
         ## Do not handle "parent" fields are they are common to all nodes.
         ## They are handled directly in Eval_Dotted_Name.
         <%
            fields = [f
                      for f in cls.get_abstract_fields(include_inherited=True)
                      if f.name.lower != 'parent']
         %>
         if Field_Cmp = "" then
            ## This should not happen, this is just a handy
            ## case for code generation.
            raise Program_Error;

         % for f in fields:
         elsif Field_Cmp = "${f.name.lower}" then

            <%
               args = f.explicit_arguments
               result_kind = enum_for_type(f.type)
               result_field = field_for_type(f.type)
               field_access_base = 'Prefix_Node.{}'.format(f.name)

               def field_access():
                   if is_ast_node(f.type):
                       return 'Ada_Node ({})'.format(field_access_base)
                   elif f.is_property:
                       return field_access_base
                   else:
                       return f.type.extract_from_storage_expr(
                           'Prefix_Node',
                           field_access_base
                       )
            %>

            if Param_Values'Length = 0 then
               % if f.explicit_arguments:
                  --  This field expects arguments that we don't have here:
                  --  defer the evaluation.

                  return Create (new Eval_Result_Record'
                    (Kind       => Field_Access_Value,
                     Ref_Count  => <>,
                     Field_Node => Ada_Node (Prefix_Node),
                     Field_Name => Field));

               % else:
                  return Create (new Eval_Result_Record'
                    (Kind            => ${result_kind},
                     Ref_Count       => <>,
                     ${result_field} => ${field_access()}));
               % endif
            end if;

            ## If we reach this point, we have explicit arguments: either the
            ## field really accepts arguments, either we have a bug somewhere,
            ## since fields with no explicit arguments should never yield
            ## Field_Access_Value Eval_Result.
            Params := Param_List (Call_Expr (Expr).F_Suffix);

            % if args:
               ## Make sure there are exactly the number of arguments expected
               if Param_Values'Length /= ${len(args)} then
                  Raise_Error
                    (Params,
                     "Invalid number of arguments: ${len(args)} expected but"
                     & " got " & Natural'Image (Param_Values'Length));
               end if;

               declare
                  % for n, t, _ in args:
                     Arg_${n} : ${t.name()};
                  % endfor
               begin
                  ## ... and make sure they have the expected types
               % for i, (n, t, _) in enumerate(args, 1):

                  ## If this is anything else than an AST node, checking the
                  ## Eval_Result_Kind is enough. Otherwise, we also must check
                  ## the tag.
                  if Param_Values (${i}).Value.Kind /= ${enum_for_type(t)}
                  % if is_ast_node(t):
                        or else
                     (Param_Values (${i}).Value.Node /= null
                         and then
                      not (Param_Values (${i}).Value.Node.all
                           in ${t.name()}_Type'Class))
                  % endif
                  then
                     Raise_Error
                       (Params.F_Params.Child (${i}),
                        "Expected ${t.name()} but got "
                        & Kind_Name (Param_Values (${i}).Value.Kind));
                  else
                     Arg_${n} := ${t.name()}
                       (Param_Values (${i}).Value.${field_for_type(t)});
                  end if;
               % endfor

                  ## Fine, arguments are fine, now let's just evaluate the
                  ## field itself.
                  <%
                     field_access_base = '{} (Prefix_Node).{} ({})'.format(
                        cls.name(), f.name,
                        ', '.join(
                           '{} => Arg_{}'.format(n, n)
                           for n, t, _ in args
                        )
                     )
                  %>
                  return Create (new Eval_Result_Record'
                    (Kind            => ${result_kind},
                     Ref_Count       => <>,
                     ${result_field} => ${field_access()}));
               end;

            % else:
               raise Program_Error;
            %endif
         % endfor

         else
            ## Since the only way to get this error is to evaluate a Prefix
            ## expression, the conversion below should never raise an error.
            Raise_Error
              (Dotted_Name_Type (Expr.all).F_Suffix,
               "${cls.name()} has no " & Image (Field.all)
               & " field; valid ones are:"
               % for f in fields:
                  & " ${f.name}"
               % endfor
               );
         end if;
      end Eval_${cls.name()}_Field_Access;

         % endif
      % endfor

      ----------------------------
      -- Eval_Node_Field_Access --
      ----------------------------

      function Eval_Node_Field_Access
        (Expr         : access Ada_Node_Type'Class;
         Prefix_Node  : Ada_Node;
         Field        : Symbol_Type;
         Field_Cmp    : Wide_Wide_String;
         Param_Values : Eval_Result_Array)
         return Eval_Result
      is
      begin
         case Kind (Prefix_Node) is
         when Ada_List =>
            Raise_Error (Expr, "Lists have no field");

         % for cls in ctx.astnode_types:
            % if not cls.abstract:
         when ${cls.ada_kind_name()} =>
            return Eval_${cls.name()}_Field_Access
              (Expr, ${cls.name()} (Prefix_Node),
               Field, Field_Cmp, Param_Values);
            % endif
         % endfor

         when others =>
            --  We handle all concrete node types, so this should not happen
            raise Program_Error;
         end case;
      end Eval_Node_Field_Access;

      --------------------------
      -- Check_In_Array_Bound --
      --------------------------

      procedure Check_In_Array_Bound
        (Expr        : access Ada_Node_Type'Class;
         Index       : Natural;
         First, Last : Natural)
      is
      begin
         if Index not in First .. Last then
            Raise_Error
              (Expr,
               "Out of bounds index: "
               & Integer'Image (Index)
               & " not in "
               & Integer'Image (First) & " .. "
               & Integer'Image (Last));
         end if;
      end Check_In_Array_Bound;

      -----------------
      -- Raise_Error --
      -----------------

      procedure Raise_Error
        (Expr    : access Ada_Node_Type'Class;
         Message : String)
      is
      begin
         Error.Sub_Expr := Ada_Node (Expr);
         Error.Message := +Message;
         raise Evaluation_Error;
      end Raise_Error;

   begin
      return Eval (Libadalang.Analysis.Root (E.Unit));
   exception
      when Evaluation_Error =>
         return Create (new Eval_Result_Record'
           (Kind      => Error_Value,
            Ref_Count => <>,
            Sub_Expr  => Error.Sub_Expr,
            Message   => Error.Message));
   end Eval;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Value : in out Eval_Result_Access) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Eval_Result_Record, Eval_Result_Access);
   begin
      case Value.Kind is
         % for cls in ctx.sorted_types(ctx.array_types):
            when ${enum_for_type(cls)} =>
               Dec_Ref (Value.${field_for_type(cls)});
         % endfor

         when Ada_Node_Iterator_Value =>
            Ada_Node_Iterators.Destroy (Value.Node_Iter);

         when others => null;
      end case;
      Free (Value);
   end Destroy;

   ------------
   -- Adjust --
   ------------

   overriding
   procedure Adjust (V : in out Eval_Result) is
   begin
      if V.Value /= null then
         V.Value.Ref_Count := V.Value.Ref_Count + 1;
      end if;
   end Adjust;

   --------------
   -- Finalize --
   --------------

   overriding
   procedure Finalize (V : in out Eval_Result) is
   begin
      if V.Value = null then
         return;
      end if;

      declare
         Ref_Count : Natural renames V.Value.Ref_Count;
      begin
         Ref_Count := Ref_Count - 1;
         if Ref_Count = 0 then
            Destroy (V.Value);
         end if;
      end;
   end Finalize;

end Libadalang.AST.Types.Parsers.Test;
