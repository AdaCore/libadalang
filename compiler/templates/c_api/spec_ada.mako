## vim: filetype=makoada

with System;

with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package ${_self.ada_api_settings.lib_name}.C is

   type ${analysis_context_type} is new System.Address;
   --  Context for all source analysis

   type ${analysis_unit_type} is new System.Address;
   --  Context for the analysis of a single compilation unit. References are
   --  ref-counted.

   type ${node_type} is new System.Address;
   --  Data type for all AST nodes. AST nodes are assembled to make up a tree.
   --  See the AST node primitives below to inspect such trees. References are
   --  ref-counted.

   type ${node_kind_type} is new int;
   --  Kind of AST nodes in parse trees

   type ${token_type} is new System.Address;
   --  Data type for tokens. Tokens always come from AST node and have the
   --  same lifetime than the AST node they come from.

   --  Helper data structures for source location handling

   type ${sloc_type} is record
      Line   : Unsigned_32;
      Column : Unsigned_16;
   end record
      with Convention => C;

   type ${sloc_range_type} is record
      Start_S, End_S : ${sloc_type};
   end record
      with Convention => C;

   type ${diagnostic_type} is record
      Sloc_Range : ${sloc_range_type};
      Message    : chars_ptr;
      --  When the API returns a diagnostic, it is up to the caller to free the
      --  message string.
   end record
     with Convention => C;
   --  Analysis unit diagnostics

   % for type_name in (node_type, token_type, sloc_type, sloc_range_type, diagnostic_type):
      type ${type_name}_Ptr is access ${type_name};
   % endfor

   type int_Ptr is access int;

   % for chunk in _self.c_astnode_field_types_ada.values():
       ${chunk}
   % endfor


   -------------------------
   -- Analysis primitives --
   -------------------------

   function ${capi.get_name("create_analysis_context")}
      return ${analysis_context_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("create_analysis_context")}";
   --  Create and return an analysis context. The caller is responsible to
   --  destroy it when done with it.

   procedure ${capi.get_name("destroy_analysis_context")}
     (Context : ${analysis_context_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("destroy_analysis_context")}";
   --  Destroy an analysis context. Any analysis units it contains may survive
   --  if there are still references to it.

   function ${capi.get_name("get_analysis_unit_from_file")}
     (Context  : ${analysis_context_type};
      Filename : chars_ptr;
      Reparse  : int) return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name("get_analysis_unit_from_file")}";
   --  Create a new analysis unit for Filename or return the existing one if
   --  any. If Reparse is true and the analysis unit already exists, reparse it
   --  from Filename.
   --
   --  The result is owned by the context: the caller must increase its ref.
   --  count in order to keep a reference to it.
   --
   --  On file opening failure, return a null address and in this case, if the
   --  analysis unit did not exist yet, do not register it.

   function ${capi.get_name("get_analysis_unit_from_buffer")}
     (Context     : ${analysis_context_type};
      Filename    : chars_ptr;
      Buffer      : chars_ptr;
      Buffer_Size : size_t) return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name =>
              "${capi.get_name("get_analysis_unit_from_buffer")}";
   --  Create a new analysis unit for Filename or return the existing one if
   --  any. Whether the analysis unit already exists or not, (re)parse it from
   --  the source code in Buffer.
   --
   --  The result is owned by the context: the caller must increase its ref.
   --  count in order to keep a reference to it.
   --
   --  On file opening failure, return a null address and in this case, if the
   --  analysis unit did not exist yet, do not register it.

   function ${capi.get_name("remove_analysis_unit")}
     (Context  : ${analysis_context_type};
      Filename : chars_ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("remove_analysis_unit")}";
   --  Remove the corresponding analysis unit from this context. Note that if
   --  someone still owns a reference to this unit, it is still available.
   --  Return whether the removal was successful (i.e. whether the analysis
   --  unit existed).

   function ${capi.get_name("unit_root")} (Unit : ${analysis_unit_type})
                                           return ${node_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_root")}";
   --  Return the root AST node for this unit, or NULL if there is none

   function ${capi.get_name("unit_diagnostic_count")}
     (Unit : ${analysis_unit_type}) return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_diagnostic_count")}";
   --  Return the number of diagnostics associated to this unit.

   function ${capi.get_name("unit_diagnostic")}
     (Unit         : ${analysis_unit_type};
      N            : unsigned;
      Diagnostic_P : ${diagnostic_type}_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_diagnostic")}";
   --  Get the Nth diagnostic in this unit and store it into Diagnostic_P.all.
   --  Return zero on failure (when N is too big).

   function ${capi.get_name("unit_incref")}
     (Unit : ${analysis_unit_type}) return ${analysis_unit_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_incref")}";
   --  Increase the reference count to an analysis unit. Return the reference
   --  for convenience.

   procedure ${capi.get_name("unit_decref")} (Unit : ${analysis_unit_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("unit_decref")}";
   --  Decrease the reference count to an analysis unit

   procedure ${capi.get_name("free_str")} (Str : chars_ptr)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("free_str")}";
   --  Free "str".  This is a convenience function for bindings.


   ---------------------------------
   -- General AST node primitives --
   ---------------------------------

   function ${capi.get_name("node_kind")} (Node : ${node_type})
      return ${node_kind_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_kind")}";
   --  Get the kind of an AST node

   function ${capi.get_name("kind_name")} (Kind : ${node_kind_type})
                                           return chars_ptr
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("kind_name")}";
   --  Helper for textual dump: return the name of a node kind. The returned
   --  string is a copy and thus must be free'd by the caller.

   procedure ${capi.get_name("node_sloc_range")}
     (Node         : ${node_type};
      Sloc_Range_P : ${sloc_range_type}_Ptr)
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_sloc_range")}";
   --  Get the spanning source location range for an AST node

   function ${capi.get_name("lookup_in_node")}
     (Node : ${node_type};
      Sloc : ${sloc_type}_Ptr) return ${node_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("lookup_in_node")}";
   --  Return the bottom-most AST node from NODE that contains SLOC, or NULL if
   --  there is none.

   function ${capi.get_name("node_parent")} (Node : ${node_type})
                                             return ${node_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_parent")}";
   --  Return the lexical parent of NODE, if any. Return NULL for the root AST
   --  node or for AST nodes for which no one has a reference to the parent.

   function ${capi.get_name("node_child_count")} (Node : ${node_type})
                                                  return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_child_count")}";
   --  Return the number of AST node in NODE's fields

   function ${capi.get_name("node_child")}
     (Node    : ${node_type};
      N       : unsigned;
      Child_P : ${node_type}_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_child")}";
   --  Get the Nth child AST node in NODE's fields and store it into
   --  CHILD_P.all. Return zero on failure (when N is too big).

   function ${capi.get_name("node_incref")}
     (Node : ${node_type}) return ${node_type}
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_incref")}";
   --  Increase the reference count to an AST node. Return the reference for
   --  convenience.

   procedure ${capi.get_name("node_decref")} (Node : ${node_type})
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_decref")}";
   --  Decrease the reference count to an AST node

   function ${capi.get_name("token_text")} (Token : ${token_type})
                                            return chars_ptr
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("token_text")}";
   --  Get the text of the given token. The caller is responsible to free the
   --  returned string.


   ---------------------------------------
   -- Kind-specific AST node primitives --
   ---------------------------------------

   --  All these primitives return their result through an OUT parameter. They
   --  return a boolean telling whether the operation was successful (it can
   --  fail if the node does not have the proper type, for instance). When an
   --  AST node is returned, its ref-count is left as-is.

   % for astnode in _self.astnode_types:
       % for primitive in _self.c_astnode_primitives[astnode]:
           ${primitive.declaration}
       % endfor
   % endfor


   -------------------------
   -- Extensions handling --
   -------------------------

   --  The following functions make it possible to attach arbitrary data to AST
   --  nodes: these are extensions. Each data is associated with both an
   --  extension ID and a destructor. AST nodes can have either none or only
   --  one extension for a given ID. The destructor is called when the AST
   --  node is about to be destroyed itself.

   --  This mechanism is inteded to ease annotating trees with analysis data
   --  but also to host node wrappers for language bindings.

   type ${capi.get_name("node_extension_destructor")} is
      access procedure (Node      : ${node_type};
                        Extension : System.Address)
      with Convention => C;
   --  Type for extension destructors. The parameter are the "node" the
   --  extension was attached to and the "extension" itself.

   function ${capi.get_name("register_extension")} (Name : chars_ptr)
      return unsigned
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("register_extension")}";
   --  Register an extension and return its identifier. Multiple calls with
   --  the same name will return the same identifier.

   function ${capi.get_name("node_extension")}
     (Node   : ${node_type};
      Ext_Id : unsigned;
      Dtor   : ${capi.get_name("node_extension_destructor")})
      return System.Address
      with Export        => True,
           Convention    => C,
           External_name => "${capi.get_name("node_extension")}";
   --  Create an extension slot in "node". If this node already contains an
   --  extension for "ext_id", return the existing slot. If not, create such a
   --  slot, associate the "dtor" destructor to it and initialize the slot to
   --  NULL. Return a pointer to the slot.
   --
   --  Note that the pointer is not guaranteed to stay valid after further
   --  calls to this function.

end ${_self.ada_api_settings.lib_name}.C;
