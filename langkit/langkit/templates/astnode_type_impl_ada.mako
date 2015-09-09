## vim: filetype=makoada

   --
   --  Primitives for ${cls.name()}
   --

<%
# Keep a list of ASTNode fields
astnode_fields = cls.get_fields(lambda f: is_ast_node(f.type))

# Keep a list of fields that are annotated with repr
repr_fields = cls.get_fields(lambda f: f.repr)
%>

% if not cls.abstract:

   ----------
   -- Kind --
   ----------

   overriding
   function Kind (Node : access ${cls.name()}_Type) return AST_Node_Kind is
   begin
      return ${cls.name()}_Kind;
   end Kind;

   ---------------
   -- Kind_Name --
   ---------------

   overriding
   function Kind_Name (Node : access ${cls.name()}_Type) return String is
   begin
      return "${cls.repr_name()}";
   end Kind_Name;

   -----------
   -- Image --
   -----------

   overriding
   function Image (Node : access ${cls.name()}_Type) return String is
      Result : Unbounded_String;
   begin
      Append (Result, Kind_Name (Node));
      Append (Result, '[');
      Append (Result, Image (Sloc_Range (AST_Node (Node))));
      Append (Result, "](");

      % for i, field in enumerate(repr_fields):
          % if i > 0:
              Append (Result, ", ");
          % endif

          % if field.type.is_ptr:
              if Node.F_${field.name} /= null then
          % endif

          % if is_ast_node(field.type):
             Append (Result, Image (AST_Node (Node.F_${field.name})));
          % else:
             Append (Result, Image (Node.F_${field.name}));
          % endif

          % if field.type.is_ptr:
              else
                 Append (Result, "None");
              end if;
          % endif
      % endfor

      Append (Result, ')');
      return To_String (Result);
   end Image;

   -----------------
   -- Child_Count --
   -----------------

   overriding
   function Child_Count (Node : access ${cls.name()}_Type) return Natural is
   begin
      return ${len(astnode_fields)};
   end Child_Count;

   ---------------
   -- Get_Child --
   ---------------

   overriding
   procedure Get_Child (Node  : access ${cls.name()}_Type;
                        Index : Natural;
                        Exists : out Boolean;
                        Result : out AST_Node) is
      ## Some ASTnodes have no ASTNode child: avoid the "unused parameter"
      ## compilation warning for them.
      % if not astnode_fields:
          pragma Unreferenced (Node);
          pragma Unreferenced (Result);
      % endif
   begin
      case Index is
          % for i, field in enumerate(astnode_fields):
              when ${i} =>
                  Result := AST_Node (Node.F_${field.name});
                  Exists := True;
          % endfor
          when others =>
             Exists := False;
             Result := null;
      end case;
   end Get_Child;

   -----------
   -- Print --
   -----------

   overriding
   procedure Print (Node  : access ${cls.name()}_Type;
                    Level : Natural := 0)
   is
      Nod : constant AST_Node := AST_Node (Node);
   begin
      Put_Line (Level, Kind_Name (Nod) & "[" & Image (Sloc_Range (Nod)) & "]");

      % for i, field in enumerate(repr_fields):
         % if field.type.is_ptr:
            if Node.F_${field.name} /= null
               and then not Is_Empty_List (Node.F_${field.name})
            then
               Put_Line (Level + 1, "${field.name.lower}:");
               Node.F_${field.name}.Print (Level + 2);
            end if;
         % else:
            Put_Line (Level + 1, "${field.name.lower}: " & Image (Node.F_${field.name}));
         % endif
      % endfor

   end Print;

   --------------
   -- Validate --
   --------------

   overriding
   procedure Validate (Node   : access ${cls.name()}_Type;
                       Parent : AST_Node := null)
   is
      Nod : constant AST_Node := AST_Node (Node);
   begin
      if Node.Parent /= Parent then
         raise Program_Error;
      end if;

      % for field in astnode_fields:
         if Node.F_${field.name} /= null then
            Node.F_${field.name}.Validate (AST_Node (Node));
         end if;
      % endfor
   end Validate;

   ---------------------
   -- Lookup_Children --
   ---------------------

   overriding
   function Lookup_Children (Node : access ${cls.name()}_Type;
                             Sloc : Source_Location;
                             Snap : Boolean := False) return AST_Node is
      ## For this implementation helper (i.e. internal primitive), we can
      ## assume that all lookups fall into this node's sloc range.

      Nod : constant AST_Node := AST_Node (Node);
      pragma Assert(Compare (Sloc_Range (Nod, Snap), Sloc) = Inside);

      Child : AST_Node;
      Pos   : Relative_Position;
      ## Some ASTnodes have no ASTNode child: avoid the "unused parameter"
      ## compilation warning for them.
      % if not astnode_fields:
          pragma Unreferenced (Child);
          pragma Unreferenced (Pos);
      % endif

   begin
      ## Look for a child node that contains Sloc (i.e. return the most
      ## precise result).

      % for i, field in enumerate(astnode_fields):
         ## Note that we assume here that child nodes are ordered so
         ## that the first one has a sloc range that is before the
         ## sloc range of the second child node, etc.

         if Node.F_${field.name} /= null then
            Lookup_Relative (AST_Node (Node.F_${field.name}), Sloc,
                             Pos, Child,
                             Snap);
            case Pos is
               when Before =>
                   ## If this is the first node, Sloc is before it, so
                   ## we can stop here.  Otherwise, Sloc is between the
                   ## previous child node and the next one...  so we can
                   ## stop here, too.
                   return Nod;

               when Inside =>
                   return Child;

               when After =>
                   ## Sloc is after the current child node, so see with
                   ## the next one.
                   null;
            end case;
         end if;
      % endfor

      ## If we reach this point, we found no children that covers Sloc,
      ## but Node still covers it (see the assertion).

      return Nod;
   end Lookup_Children;
% endif

## Body of attribute getters

% for field in cls.get_fields(include_inherited=False):
   function F_${field.name}
     (Node : ${cls.name()}) return ${decl_type(field.type)}
   is
   begin
      return ${decl_type(field.type)} (${cls.name()}_Type (Node.all).F_${field.name});
   end F_${field.name};
% endfor

   ----------
   -- Free --
   ----------

   overriding
   procedure Free (Node : access ${cls.name()}_Type) is
   begin
      % for f in cls.get_fields(include_inherited=False):
         % if f.type.is_ptr:
            Dec_Ref (AST_Node (Node.F_${f.name}));
         % endif
      % endfor

      --  Let the base class destructor take care of inheritted fields

      Free (${base_name}_Access (Node));
   end Free;

   procedure Inc_Ref (Node : in out ${cls.name()}) is
   begin
      if Node /= null then
         Inc_Ref (AST_Node (Node));
      end if;
   end Inc_Ref;

   procedure Dec_Ref (Node : in out ${cls.name()}) is
   begin
      if Node /= null then
         Dec_Ref (AST_Node (Node));
      end if;
   end Dec_Ref;
