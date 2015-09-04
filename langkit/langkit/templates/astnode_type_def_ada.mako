## vim: filetype=makoada

<%namespace name="exts" file="extensions.mako" />

<% type_name = '{}_Type'.format(cls.name()) %>

% if not private_part:

   --
   --  ${cls.name()}
   --

   type ${cls.name()}_Type is ${"abstract" if cls.abstract else "" }
      new ${base_name}_Type with private;

   % if not cls.abstract:

      overriding
      function Kind (Node : access ${type_name}) return AST_Node_Kind;
      overriding
      function Kind_Name (Node : access ${type_name}) return String;
      overriding
      function Image (Node : access ${type_name}) return String;


      overriding
      function Child_Count (Node : access ${type_name}) return Natural;
      overriding
      procedure Get_Child (Node  : access ${type_name};
                           Index : Natural;
                           Exists : out Boolean;
                           Result : out AST_Node);

      overriding
      procedure Print (Node  : access ${type_name};
                       Level : Natural := 0);
      overriding
      procedure Validate (Node   : access ${type_name};
                          Parent : AST_Node := null);

      overriding
      function Lookup_Children (Node : access ${type_name};
                                Sloc : Source_Location;
                                Snap : Boolean := False) return AST_Node;

      overriding procedure Destroy
        (Node : access ${cls.name()}_Type);
   % endif

   ## Attribute getters

   % for field in cls.get_fields(include_inherited=False):
       function ${field.name}
         (Node : ${cls.name()}) return ${decl_type(field.type)};
   % endfor

% else:
   <%
      fields = cls.get_fields(include_inherited=False)
      ext = ctx.ext("nodes", cls.name(), "components")
   %>
   type ${type_name} is ${"abstract" if cls.abstract else ""}
      new ${base_name}_Type with

   % if fields or ext:
      record
       % for f in fields:
            ${f.name} : aliased ${decl_type(f.type)}
               := ${f.type.nullexpr()};
       % endfor
         ${exts.include_extension(ext)}
      end record;
   % else:
      null record;
   % endif

   % if not cls.abstract:
      package ${cls.name()}_Alloc is
        new Tagged_Alloc (${cls.name()}_Type, ${cls.name()}_Access);
   % endif
% endif
