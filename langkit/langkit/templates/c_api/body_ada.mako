## vim: filetype=makoada

with Ada.Containers;        use Ada.Containers;
with Ada.IO_Exceptions;     use Ada.IO_Exceptions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;

with Langkit_Support.AST;        use Langkit_Support.AST;
with Langkit_Support.Extensions; use Langkit_Support.Extensions;
with Langkit_Support.Tokens;     use Langkit_Support.Tokens;

package body ${_self.ada_api_settings.lib_name}.C is

   function Wrap (S : Source_Location) return ${sloc_type} is
     ((S.Line, S.Column));
   function Unwrap (S : ${sloc_type}) return Source_Location is
     ((S.Line, S.Column));

   function Wrap (S : Source_Location_Range) return ${sloc_range_type} is
     ((Start_S => (S.Start_Line, S.Start_Column),
       End_S   => (S.End_Line,   S.End_Column)));
   function Unwrap (S : ${sloc_range_type}) return Source_Location_Range is
     ((S.Start_S.Line, S.End_S.Line,
       S.Start_S.Column, S.End_S.Column));

   --  The following conversions are used only at the interface between Ada and
   --  C (i.e. as parameters and return types for C entry points) for access
   --  types.  All read/writes for the pointed values are made through the
   --  access values and never through the System.Address values.  Thus, strict
   --  aliasing issues should not arise for these.
   --
   --  See <https://gcc.gnu.org/onlinedocs/gnat_ugn/
   --       Optimization-and-Strict-Aliasing.html>.

   pragma Warnings (Off, "possible aliasing problem for type");

   function Wrap is new Ada.Unchecked_Conversion
     (Token_Access, ${token_type});
   function Unwrap is new Ada.Unchecked_Conversion
     (${token_type}, Token_Access);

   function Wrap is new Ada.Unchecked_Conversion
     (Analysis_Context, ${analysis_context_type});
   function Unwrap is new Ada.Unchecked_Conversion
     (${analysis_context_type}, Analysis_Context);

   function Wrap is new Ada.Unchecked_Conversion
     (Analysis_Unit, ${analysis_unit_type});
   function Unwrap is new Ada.Unchecked_Conversion
     (${analysis_unit_type}, Analysis_Unit);

   function Wrap is new Ada.Unchecked_Conversion
     (AST_Node, ${node_type});
   function Unwrap is new Ada.Unchecked_Conversion
     (${node_type}, AST_Node);

   function Convert is new Ada.Unchecked_Conversion
     (${capi.get_name("node_extension_destructor")},
      Extension_Destructor);
   function Convert is new Ada.Unchecked_Conversion
     (chars_ptr, System.Address);

   pragma Warnings (Off, "possible aliasing problem for type");

   -------------------------
   -- Analysis primitives --
   -------------------------

   function ${capi.get_name("create_analysis_context")}
      return ${analysis_context_type}
   is
   begin
      return Wrap (Create);
   end ${capi.get_name("create_analysis_context")};

   procedure ${capi.get_name("destroy_analysis_context")}
     (Context : ${analysis_context_type})
   is
      C : Analysis_Context := Unwrap (Context);
   begin
      Destroy (C);
   end ${capi.get_name("destroy_analysis_context")};

   function ${capi.get_name("get_analysis_unit_from_file")}
     (Context  : ${analysis_context_type};
      Filename : chars_ptr;
      Reparse  : int) return ${analysis_unit_type}
   is
      Ctx : constant Analysis_Context := Unwrap (Context);
      Unit : Analysis_Unit;
   begin
      begin
         Unit := Get_From_File (Ctx, Value (Filename), Reparse /= 0);
      exception
         when Name_Error =>
            Unit := null;
      end;
      return Wrap (Unit);
   end ${capi.get_name("get_analysis_unit_from_file")};

   function ${capi.get_name("get_analysis_unit_from_buffer")}
     (Context     : ${analysis_context_type};
      Filename    : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t) return ${analysis_unit_type}
   is
      Ctx : constant Analysis_Context := Unwrap (Context);
      Unit : Analysis_Unit;

      Buffer_Str : String (1 .. Positive (Buffer_Size));
      for Buffer_Str'Address use Convert (Buffer);
   begin
      Unit := Get_From_Buffer (Ctx, Value (Filename), Buffer_Str);
      return Wrap (Unit);
   end ${capi.get_name("get_analysis_unit_from_buffer")};

   function ${capi.get_name("remove_analysis_unit")}
     (Context  : ${analysis_context_type};
      Filename : chars_ptr) return int
   is
      Ctx : constant Analysis_Context := Unwrap (Context);
   begin
      begin
         Remove (Ctx, Value (Filename));
      exception
         when Constraint_Error =>
            return 0;
      end;
      return 1;
   end ${capi.get_name("remove_analysis_unit")};

   function ${capi.get_name("unit_root")} (Unit : ${analysis_unit_type})
                                           return ${node_type}
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      return Wrap (U.AST_Root);
   end ${capi.get_name("unit_root")};

   function ${capi.get_name("unit_diagnostic_count")}
     (Unit : ${analysis_unit_type}) return unsigned
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      return unsigned (U.Diagnostics.Length);
   end ${capi.get_name("unit_diagnostic_count")};

   function ${capi.get_name("unit_diagnostic")}
     (Unit         : ${analysis_unit_type};
      N            : unsigned;
      Diagnostic_P : ${diagnostic_type}_Ptr) return int
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      if N < unsigned (U.Diagnostics.Length) then
         declare
            D_In  : Diagnostic renames U.Diagnostics (Natural (N));
            D_Out : ${diagnostic_type} renames Diagnostic_P.all;
         begin
            D_Out.Sloc_Range := Wrap (D_In.Sloc_Range);
            D_Out.Message := New_String (To_String (D_In.Message));
            return 1;
         end;
      else
         return 0;
      end if;
   end ${capi.get_name("unit_diagnostic")};

   function ${capi.get_name("unit_incref")}
     (Unit : ${analysis_unit_type}) return ${analysis_unit_type}
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      Inc_Ref (U);
      return Unit;
   end ${capi.get_name("unit_incref")};

   procedure ${capi.get_name("unit_decref")} (Unit : ${analysis_unit_type})
   is
      U : Analysis_Unit := Unwrap (Unit);
   begin
      Dec_Ref (U);
   end ${capi.get_name("unit_decref")};

   function ${capi.get_name("unit_reparse_from_file")}
     (Unit : ${analysis_unit_type})
      return int
   is
      U : constant Analysis_Unit := Unwrap (Unit);
   begin
      begin
         Reparse (U);
      exception
         when Name_Error =>
            return 0;
      end;
      return 1;
   end ${capi.get_name("unit_reparse_from_file")};

   procedure ${capi.get_name("unit_reparse_from_buffer")}
     (Unit        : ${analysis_unit_type};
      Buffer      : chars_ptr;
      Buffer_Size : size_t)
   is
      U : constant Analysis_Unit := Unwrap (Unit);
      Buffer_Str : String (1 .. Positive (Buffer_Size));
      for Buffer_Str'Address use Convert (Buffer);
   begin
      Reparse (U, Buffer_Str);
   end ${capi.get_name("unit_reparse_from_buffer")};

   procedure ${capi.get_name("free_str")} (Str : chars_ptr) is
      S : chars_ptr := Str;
   begin
      Free (S);
   end ${capi.get_name("free_str")};


   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   Node_Kind_Names : constant array (Positive range <>) of Unbounded_String :=
     (To_Unbounded_String ("list")
      % for astnode in _self.astnode_types:
         % if not astnode.abstract:
            , To_Unbounded_String ("${astnode.name().camel}")
         % endif
      % endfor
      );

   function ${capi.get_name("node_kind")} (Node : ${node_type})
      return ${node_kind_type}
   is
      N : constant AST_Node := Unwrap (Node);
   begin
      return ${node_kind_type} (Kind (N));
   end ${capi.get_name("node_kind")};

   function ${capi.get_name("kind_name")} (Kind : ${node_kind_type})
                                           return chars_ptr
   is
      Name : Unbounded_String renames Node_Kind_Names (Natural (Kind));
   begin
      return New_String (To_String (Name));
   end ${capi.get_name("kind_name")};

   procedure ${capi.get_name("node_sloc_range")}
     (Node         : ${node_type};
      Sloc_Range_P : ${sloc_range_type}_Ptr)
   is
      N : constant AST_Node := Unwrap (Node);
   begin
      Sloc_Range_P.all := Wrap (Sloc_Range (N));
   end ${capi.get_name("node_sloc_range")};

   function ${capi.get_name("lookup_in_node")}
     (Node : ${node_type};
      Sloc : ${sloc_type}_Ptr) return ${node_type}
   is
      N : constant AST_Node := Unwrap (Node);
      S : constant Source_Location := Unwrap (Sloc.all);
   begin
      return Wrap (Lookup (N, S));
   end ${capi.get_name("lookup_in_node")};

   function ${capi.get_name("node_parent")} (Node : ${node_type})
                                             return ${node_type}
   is
      N : constant AST_Node := Unwrap (Node);
   begin
      return Wrap (N.Parent);
   end ${capi.get_name("node_parent")};

   function ${capi.get_name("node_child_count")} (Node : ${node_type})
                                                  return unsigned
   is
      N : constant AST_Node := Unwrap (Node);
   begin
      return unsigned (Child_Count (N));
   end ${capi.get_name("node_child_count")};

   function ${capi.get_name("node_child")}
     (Node    : ${node_type};
      N       : unsigned;
      Child_P : ${node_type}_Ptr) return int
   is
      Nod    : constant AST_Node := Unwrap (Node);
      Result : AST_Node;
      Exists : Boolean;
   begin
      if N > unsigned (Natural'Last) then
         return 0;
      end if;
      Get_Child (Nod, Natural (N), Exists, Result);
      if Exists then
         Child_P.all := Wrap (Result);
         return 1;
      else
         return 0;
      end if;
   end ${capi.get_name("node_child")};

   function ${capi.get_name("token_text")} (Token : ${token_type})
                                            return chars_ptr
   is
      T : Langkit_Support.Tokens.Token renames Unwrap (Token).all;
   begin
      if T.Text = null then
         return Null_Ptr;
      end if;
      return New_String (T.Text.all);
   end ${capi.get_name("token_text")};


   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   % for astnode in _self.astnode_types:
       % for primitive in _self.c_astnode_primitives[astnode]:
           ${primitive.implementation}
       % endfor
   % endfor


   -------------------------
   -- Extensions handling --
   -------------------------

   function ${capi.get_name("register_extension")} (Name : chars_ptr)
      return unsigned
   is
   begin
      return unsigned (Register_Extension (Value (Name)));
   end ${capi.get_name("register_extension")};

   function ${capi.get_name("node_extension")}
     (Node   : ${node_type};
      Ext_Id : unsigned;
      Dtor   : ${capi.get_name("node_extension_destructor")})
      return System.Address
   is
      N  : constant AST_Node := Unwrap (Node);
      ID : constant Extension_ID := Extension_Id (Ext_Id);
      D  : constant Extension_Destructor := Convert (Dtor);
   begin
      return Get_Extension (N, ID, D).all'Address;
   end ${capi.get_name("node_extension")};

end ${_self.ada_api_settings.lib_name}.C;
