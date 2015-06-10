## vim: filetype=makoada

% if not private_part:

   --
   --  ${cls.name()}
   --

   type ${cls.name()}_Type is ${"abstract" if cls.abstract else "" }
      new ${base_name}_Type with private;
   type ${cls.name()}_Access is access all ${cls.name()}_Type;
   type ${cls.name()} is access all ${cls.name()}_Type'Class;

   % if not cls.abstract:

      overriding
      function Kind (Node : access ${cls.name()}_Type) return AST_Node_Kind;
      overriding
      function Kind_Name (Node : access ${cls.name()}_Type) return String;
      overriding
      function Image (Node : access ${cls.name()}_Type) return String;


      overriding
      function Child_Count (Node : access ${cls.name()}_Type) return Natural;
      overriding
      procedure Get_Child (Node  : access ${cls.name()}_Type;
                           Index : Natural;
                           Exists : out Boolean;
                           Result : out AST_Node);

      overriding
      procedure Compute_Indent_Level (Node : access ${cls.name()}_Type);
      overriding
      procedure Print (Node  : access ${cls.name()}_Type;
                       Level : Natural := 0);
      overriding
      procedure Validate (Node   : access ${cls.name()}_Type;
                          Parent : AST_Node := null);

      overriding
      function Lookup_Children (Node : access ${cls.name()}_Type;
                                Sloc : Source_Location;
                                Snap : Boolean := False) return AST_Node;

   % endif

   overriding
   procedure Free (Node : access ${cls.name()}_Type);

   procedure Inc_Ref (Node : in out ${cls.name()});
   pragma Inline (Inc_Ref);

   procedure Dec_Ref (Node : in out ${cls.name()});
   pragma Inline (Dec_Ref);

% else:

   type ${cls.name()}_Type is ${"abstract" if cls.abstract else "" }
      new ${base_name}_Type with
   % if cls_field_decls:
      record
          % for t, f in cls_field_decls:
               F_${f.name} : aliased ${decl_type(t)}
                  := ${t.nullexpr()};
          % endfor
      end record;
   % else:
      null record;
   % endif

% endif
