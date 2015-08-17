## vim: filetype=makoada

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;

with Interfaces; use Interfaces;

with GNATCOLL.Symbols; use GNATCOLL.Symbols;

with Langkit_Support.AST;                use Langkit_Support.AST;
with Langkit_Support.AST.List;
with Langkit_Support.Diagnostics;        use Langkit_Support.Diagnostics;
with Langkit_Support.Tokens;             use Langkit_Support.Tokens;
with Langkit_Support.Token_Data_Handler; use Langkit_Support.Token_Data_Handler;

package ${_self.ada_api_settings.lib_name} is

   procedure Initialize
     with Export        => True,
          Convention    => C,
          External_Name =>
             "${get_context().ada_api_settings.lib_name.lower()}_initialize";
   --  Initialize the library. Must be called before anything else from this
   --  library and from Langkit_Support.

   ----------------------
   -- Analysis context --
   ----------------------

   type Analysis_Context_Type;
   type Analysis_Unit_Type;

   type Analysis_Context is access all Analysis_Context_Type;
   type Analysis_Unit is access all Analysis_Unit_Type;

   package Units_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Unbounded_String,
      Element_Type    => Analysis_Unit,
      Hash            => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => "=");

   type Analysis_Context_Type is record
      Units_Map : Units_Maps.Map;
      Symbols   : Symbol_Table_Access;
   end record;

   type Analysis_Unit_Type is record
      Context     : Analysis_Context;
      Ref_Count   : Natural;
      AST_Root    : AST_Node;
      File_Name   : Unbounded_String;
      TDH         : aliased Token_Data_Handler;
      Diagnostics : Diagnostics_Vectors.Vector;
      With_Trivia : Boolean;
   end record;

   function Create return Analysis_Context;
   --  Create a new Analysis_Context. When done with it, invoke Destroy on it.

   function Get_From_File (Context     : Analysis_Context;
                           Filename    : String;
                           Reparse     : Boolean := False;
                           With_Trivia : Boolean := False) return Analysis_Unit;
   --  Create a new Analysis_Unit for Filename or return the existing one if
   --  any. If Reparse is true and the analysis unit already exists, reparse it
   --  from Filename.
   --
   --  The result is owned by the context: the caller must increase its ref.
   --  count in order to keep a reference to it.
   --
   --  On file opening failure, raise a Name_Error exception and in this case,
   --  if the analysis unit did not exist yet, do not register it. In this
   --  case, if the analysis unit was already existing, this preserves the AST.
   --
   --  When With_Trivia is true, the parsed analysis unit will contain trivias.
   --  Already existing analysis units are reparsed if needed.

   function Get_From_Buffer (Context     : Analysis_Context;
                             Filename    : String;
                             Buffer      : String;
                             With_Trivia : Boolean := False) return Analysis_Unit;
   --  Create a new Analysis_Unit for Filename or return the existing one if
   --  any. Whether the analysis unit already exists or not, (re)parse it from
   --  the source code in Buffer.
   --
   --  The result is owned by the context: the caller must increase its ref.
   --  count in order to keep a reference to it.
   --
   --  When With_Trivia is true, the parsed analysis unit will contain trivias.
   --  Already existing analysis units are reparsed if needed.

   procedure Remove (Context   : Analysis_Context;
                     File_Name : String);
   --  Remove the corresponding Analysis_Unit from this context. If someone
   --  still owns a reference to it, it remains available but becomes
   --  context-less.
   --
   --  If there is no such Analysis_Unit, raise a Constraint_Error exception.

   procedure Destroy (Context : in out Analysis_Context);
   --  Invoke Remove on all the units Context contains and free Context

   procedure Inc_Ref (Unit : Analysis_Unit);
   procedure Dec_Ref (Unit : Analysis_Unit);

   procedure Reparse (Unit : Analysis_Unit);
   --  Reparse an analysis unit from the associated file.
   --
   --  On file opening failure, raise a Name_Error exception and in this case,
   --  if the analysis unit did not exist yet, do not register it.  In this
   --  case, preserve any existing AST and diagnostics.

   procedure Reparse (Unit : Analysis_Unit; Buffer : String);
   --  Reparse an analysis unit from a buffer

   procedure Print (Unit : Analysis_Unit);
   --  Debug helper: output the AST and eventual diagnostic for this unit on
   --  standard output

   procedure PP_Trivia (Unit : Analysis_Unit);
   --  Debug helper: output a minimal AST with mixed trivias

   -----------------------
   -- Enumeration types --
   -----------------------

   ## Output constants so that all concrete AST_Node subclasses get their own
   ## AST_Node_Kind. Nothing can be an instance of an abstract subclass, so
   ## these do not need their own kind. Note that we start from 2 because 1 is
   ## reserved for all lists.
   List_Kind : constant AST_Node_Kind := 1;
   % for cls in _self.astnode_types:
      % if not cls.abstract:
         ${cls.name()}_Kind : constant AST_Node_Kind :=
            ${ctx.node_kind_constants[cls]};
      % endif
   % endfor

   function Image (Value : Boolean) return String is
     (if Value then "True" else "False");

   % for decl in _self.enum_declarations:
   ${decl.public_part}
   % endfor

   ---------------------------
   -- ASTNode derived types --
   ---------------------------

   % for decl in _self.incomplete_types_declarations:
   ${decl.public_part}
   % endfor

   % for decl in _self.list_types_declarations:
   ${decl.public_part}
   % endfor

   % for decl in _self.types_declarations:
   ${decl.public_part}
   % endfor

private

   % for decl in _self.types_declarations:
   ${decl.private_part}
   % endfor

   % for decl in _self.list_types_declarations:
   ${decl.private_part}
   % endfor

end ${_self.ada_api_settings.lib_name};
