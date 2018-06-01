from __future__ import absolute_import, division, print_function

from langkit.diagnostics import check_source_language
from langkit.dsl import (
    AnalysisUnitKind, AnalysisUnitType, Annotations, ASTNode, BoolType,
    EnumNode, EquationType, Field, LexicalEnvType, LogicVarType, LongType,
    Struct, SymbolType, T, UserField, abstract, env_metadata,
    has_abstract_list, synthetic
)
from langkit.envs import (
    EnvSpec, add_to_env, add_env, call_env_hook, handle_children, do,
    reference, set_initial_env
)
from langkit.expressions import (
    AbstractKind, AbstractProperty, And, ArrayLiteral as Array, BigInteger,
    Bind, Cond, DynamicVariable, EmptyEnv, Entity, If, Let, Literal, No, Not,
    Or, Property, PropertyError, Self, Var, ignore, langkit_property
)
from langkit.expressions.analysis_units import UnitBody, UnitSpecification
from langkit.expressions.logic import LogicFalse, LogicTrue, Predicate


env = DynamicVariable('env', LexicalEnvType)
origin = DynamicVariable('origin', T.AdaNode)


def entity_no_md(type, node, rebindings):
    return Let(lambda n=node: type.entity.new(
        el=n,
        info=T.entity_info.new(
            rebindings=rebindings,
            md=No(T.env_md)
        )
    ))


def TypeBind(*args, **kwargs):
    check_source_language(
        'eq_prop' not in kwargs.keys(),
        "You cannot pass an eq_prop to TypeBind"
    )
    kwargs['eq_prop'] = BaseTypeDecl.matching_type
    return Bind(*args, **kwargs)


def bool_bind(type_var):
    """
    Decouple the logic of binding to a Boolean type. We use
    matching_formal_prim_type because in name resolution, Ada expects any type
    derived from bool.
    """
    return Bind(type_var, Self.bool_type,
                eq_prop=BaseTypeDecl.matching_formal_prim_type)


def universal_int_bind(type_var):
    """
    Return an equation that will bind type_var to any integer value,
    corresponding to the notion of universal_integer in the Ada RM.
    """
    return TypeBind(type_var, Self.universal_int_type)


def universal_real_bind(type_var):
    return TypeBind(type_var, Self.universal_real_type)


def ref_used_packages():
    """
    If Self is a library item or a subunit, reference the environments for
    packages that are used at the top-level here. See
    UsePackageClause's ref_env_nodes for the rationale.
    """
    return reference(
        Self.top_level_use_package_clauses,
        through=T.Name.use_package_name_designated_env,
        cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
    )


def ref_generic_formals():
    """
    If Self is a generic package/subprogram and not a library item,
    then the generic formals are not available in parent
    environments. Make them available with ref_envs.
    """
    return reference(
        Self.cast(T.AdaNode).to_array,
        through=T.AdaNode.nested_generic_formal_part,
        cond=Not(Self.is_library_item)
    )


def add_to_env_kv(key, val, *args, **kwargs):
    """
    Wrapper around envs.add_to_env, that takes a key and a val expression, and
    creates the intermediate env_assoc Struct.
    """
    return add_to_env(
        T.env_assoc.new(key=key, val=val), *args, **kwargs
    )


def env_mappings(defining_names, entity):
    """
    Creates an env mapping array from a list of BaseId to be used as keys, and
    an entity to be used as value in the mappings.
    """
    return defining_names.map(
        lambda n: T.env_assoc.new(key=n.name_symbol, val=entity)
    )


@env_metadata
class Metadata(Struct):
    dottable_subp = UserField(
        BoolType, doc="Whether the stored element is a subprogram accessed "
                      "through the dot notation"
    )
    primitive = UserField(
        T.AdaNode,
        doc="The type for which this subprogram is a primitive, if any"
    )
    primitive_real_type = UserField(
        T.AdaNode,
        doc="The type for which this subprogram is a primitive, if any"
    )
    access_entity = UserField(
        BoolType,
        doc="Whether the accessed entity is an anonymous access to it or not."
    )


@abstract
class AdaNode(ASTNode):
    """
    Root node class for the Ada syntax tree.
    """

    annotations = Annotations(
        generic_list_type='AdaList',
        warn_on_node=True
    )

    expression_type = Property(
        No(T.BaseTypeDecl.entity),
        type=T.BaseTypeDecl.entity,
        public=True,
        doc="""
        Return the declaration corresponding to the type of this expression
        after name resolution.
        """
    )

    empty_env = Property(
        Self.parents.find(lambda p: p.is_a(T.CompilationUnit))
        .cast(T.CompilationUnit).get_empty_env,
    )

    @langkit_property(return_type=BoolType)
    def is_children_env(parent=LexicalEnvType, current_env=LexicalEnvType):
        """
        Static property. Will return True if current_env is a children of
        parent.
        """
        return Cond(
            current_env == parent, True,
            current_env.is_null, False,
            Self.is_children_env(parent, current_env.env_parent)
        )

    @langkit_property(return_type=T.AdaNode.entity)
    def trigger_access_entity(val=T.BoolType):
        """
        Return Self as an entity, but with the ``access_entity`` field set to
        val. Helper for the 'Unrestricted_Access machinery.
        """
        new_md = Var(Metadata.new(
            dottable_subp=Entity.info.md.dottable_subp,
            primitive=Entity.info.md.primitive,
            primitive_real_type=Entity.info.md.primitive_real_type,
            access_entity=val
        ))

        return AdaNode.entity.new(
            el=Entity.el, info=T.entity_info.new(
                rebindings=Entity.info.rebindings,
                md=new_md
            )
        )

    @langkit_property(public=True)
    def referenced_decl():
        """
        Return the declaration this node references after name resolution.
        """
        return Entity.referenced_decl_internal(False)

    @langkit_property(public=True, return_type=T.DefiningName.entity)
    def xref():
        """
        Return a cross reference from this node to a defining identifier.
        """
        return No(T.DefiningName.entity)

    @langkit_property(public=True)
    def referenced_decl_internal(try_immediate=BoolType):
        """
        Return the declaration this node references. Try not to run name res if
        already resolved. INTERNAL USE ONLY.
        """
        # TODO: remove from public API
        ignore(try_immediate)
        return No(T.BasicDecl.entity)

    @langkit_property(public=True)
    def generic_instantiations():
        """
        Return the potentially empty list of generic package/subprogram
        instantiations that led to the creation of this entity. Outer-most
        instantiations appear last.
        """
        return Self.generic_instantiations_internal(Entity.info.rebindings)

    @langkit_property(return_type=T.GenericInstantiation.entity.array)
    def generic_instantiations_internal(r=T.EnvRebindingsType):
        return If(
            r == No(T.EnvRebindingsType),
            No(T.GenericInstantiation.entity.array),

            Let(lambda
                head=(r.rebindings_new_env.env_node
                      .cast_or_raise(T.GenericInstantiation).as_bare_entity),
                tail=Self.generic_instantiations_internal(
                    r.rebindings_parent
                ):
                head.singleton.concat(tail))
        )

    @langkit_property()
    def logic_val(from_node=T.AdaNode.entity, lvar=LogicVarType,
                  try_immediate=(BoolType, False)):
        success = Var(If(
            try_immediate & Not(lvar.get_value.is_null),
            True,
            from_node.parents.find(lambda p: p.xref_entry_point).resolve_names
        ))

        return If(success, lvar.get_value, No(T.AdaNode.entity))

    @langkit_property(return_type=T.AdaNode.entity)
    def semantic_parent_helper(env=LexicalEnvType):
        return env.then(lambda env: env.env_node.as_entity._or(
            Entity.semantic_parent_helper(env.env_parent)
        ))

    @langkit_property(public=True)
    def semantic_parent():
        """
        Return the semantic parent for this node, if applicable, null
        otherwise.
        """
        return Entity.semantic_parent_helper(Entity.node_env)

    @langkit_property(
        return_type=AnalysisUnitType, external=True, uses_entity_info=False,
        uses_envs=False,
        call_non_memoizable_because='Getting an analysis unit cannot appear'
                                    ' in a memoized context'
    )
    def get_unit(name=SymbolType.array, kind=AnalysisUnitKind,
                 load_if_needed=BoolType):
        """
        Return the analysis unit for the given "kind" corresponding to this
        Name. Return null if this is an illegal unit name, or if
        "load_if_needed" is false and the unit is not loaded yet.
        """
        pass

    @langkit_property(return_type=T.AdaNode, uses_entity_info=False,
                      ignore_warn_on_node=True, call_memoizable=True)
    def get_compilation_unit(name=SymbolType.array, kind=AnalysisUnitKind):
        """
        If the corresponding analysis unit is loaded, return the compilation
        unit node for the given analysis unit "kind" and correpsonding to the
        name "name". If it's not loaded, return none.
        """
        # Because we don't load the unit when it's not already there, it is
        # safe to use this property in a memoized context.
        u = Var(Self.get_unit(name, kind, False))

        return u._.root._.match(
            lambda cu=T.CompilationUnit: cu.body.match(
                lambda su=T.Subunit: su.body,
                lambda li=T.LibraryItem: li.item,
                lambda _: No(T.AdaNode)
            ),
            lambda _: No(T.AdaNode),
        )

    @langkit_property(return_type=T.AspectSpec.entity, public=True)
    def node_aspects():
        """
        Return the list of aspects that are attached to this node.
        """
        return No(T.AspectSpec.entity)

    @langkit_property(return_type=T.AspectAssoc.entity)
    def get_aspect(name=SymbolType):
        return Entity.node_aspects._.aspect_assocs.find(
            lambda asp: asp.id.cast(T.BaseId).sym == name
        )

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      return_type=EquationType, dynamic_vars=[env, origin])
    def xref_equation():
        """
        This is the base property for constructing equations that, when solved,
        will resolve names and types for every sub expression of the expression
        you call it on. Note that if you call that on any expression, in some
        context it might lack full information and return multiple solutions.
        If you want completely precise resolution, you must call that on the
        outermost node that supports xref_equation.
        """
        pass

    xref_stop_resolution = Property(False)
    stop_resolution_equation = Property(LogicTrue())

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def sub_equation():
        """
        Wrapper for xref_equation, meant to be used inside of xref_equation
        when you want to get the sub equation of a sub expression. It is
        used to change the behavior when xref_equation is called from
        another xref_equation call, or from the top level, so that we can do
        resolution in several steps.
        """
        return If(Entity.xref_stop_resolution,
                  Entity.stop_resolution_equation,
                  Entity.xref_equation)

    @langkit_property(return_type=BoolType, dynamic_vars=[env, origin])
    def resolve_names_internal(initial=BoolType,
                               additional_equation=EquationType):
        """
        Internal helper for resolve_names, implementing the recursive logic.
        """

        solve_xref = Var(initial | Self.xref_stop_resolution)

        i = Var(If(
            solve_xref,
            (Entity.xref_equation & additional_equation)._.solve,
            True)
        )

        j = Var(Self.children.all(lambda c: c.then(
            # Do not explore nodes that are xref entry points, and are not the
            # initial node.
            lambda c: If(
                c.xref_entry_point,
                True,
                c.as_entity.resolve_names_internal(False, LogicTrue()),
            ),
            default_val=True
        )))

        return i & j

    xref_entry_point = Property(
        False,
        public=True,
        doc="""
        Designates entities that are entry point for the xref solving
        infrastructure. If this returns true, then resolve_names can be called
        on it.
        """
    )

    @langkit_property(return_type=BoolType, public=True,
                      memoized=True, call_memoizable=True)
    def resolve_names():
        """
        This will resolve names for this node. If the operation is successful,
        then type_var and ref_var will be bound on appropriate subnodes of the
        statement.
        """
        return env.bind(
            Entity.node_env,
            origin.bind(Self, Entity.resolve_names_internal(True, LogicTrue()))
        )

    # TODO: Navigation properties are not ready to deal with units containing
    # multiple packages.

    body_unit = Property(
        Self.top_level_item(Self.unit)._.match(
            lambda body=T.Body: body.unit,
            lambda decl=T.BasicDecl:
                decl.as_bare_entity.defining_name.referenced_unit(UnitBody),
        ),

        public=True, doc="""
        If this unit has a body, fetch and return it.
        """
    )

    spec_unit = Property(
        Self.top_level_item(Self.unit)
        .cast(T.Body)._.as_bare_entity.defining_name
        .referenced_unit_or_null(UnitSpecification),

        public=True, doc="""
        If this unit has a spec, fetch and return it. Return the null analysis
        unit otherwise. Note that this returns null for specs, as they don't
        have another spec themselves.
        """
    )

    @langkit_property(return_type=LexicalEnvType)
    def parent_unit_env_helper(unit=AnalysisUnitType, env=LexicalEnvType):
        return env.env_parent.then(lambda parent_env: parent_env.env_node.then(
            lambda parent_node: If(
                parent_node.unit == unit,
                Self.parent_unit_env_helper(unit, parent_env),
                parent_env
            )
        ))

    @langkit_property()
    def parent_unit_env(env=LexicalEnvType):
        """
        Given env's AnalysisUnit, return the first env that has a different
        analysis unit in the env parent chain.
        """
        return env.then(
            lambda env: Self.parent_unit_env_helper(env.env_node.unit, env)
        )

    @langkit_property(return_type=T.AnalysisUnitType, public=True,
                      external=True, uses_entity_info=False, uses_envs=False)
    def standard_unit():
        """
        Static method. Return the analysis unit corresponding to the Standard
        package.
        """
        pass

    std = Property(
        Self.standard_unit.root.cast(T.CompilationUnit)
        .body.cast(T.LibraryItem).item.as_bare_entity,
        doc="""
        Retrieves the package corresponding to the Standard unit. Used to
        access standard types.
        """
    )

    std_env = Property(
        Self.std.children_env,
        doc="Get the children env of the Standard package."
    )

    std_entity = Property(
        lambda sym=SymbolType: Self.std_env.get_first(sym),
        doc="Return an entity from the standard package with name `sym`"
    )

    bool_type = Property(
        Self.std_entity('Boolean'), public=True, doc="""
        Static method. Return the standard Boolean type.
        """
    )
    int_type = Property(
        Self.std_entity('Integer'), public=True, doc="""
        Static method. Return the standard Integer type.
        """
    )
    universal_int_type = Property(
        Self.std_entity('Universal_Int_Type_'), public=True, doc="""
        Static method. Return the standard Universal Integer type.
        """
    )
    universal_real_type = Property(
        Self.std_entity('Universal_Real_Type_'), public=True, doc="""
        Static method. Return the standard Universal Real type.
        """
    )

    ext_id_type = Property(
        Self
        .get_compilation_unit(['Ada', 'Exceptions'], UnitSpecification)
        ._.children_env.get_first('Exception_Id', recursive=False)
        .cast(T.BaseTypeDecl), doc="""
        Return the type Ada.Exceptions.Exception_Id.
        """

    )

    task_id_type = Property(
        Self.get_compilation_unit(['Ada', 'Task_Identification'],
                                  UnitSpecification)
        ._.children_env.get_first('Task_Id', recursive=False)
        .cast(T.BaseTypeDecl), doc="""
        Return the type Ada.Task_Identification.Task_Id.
        """
    )

    @langkit_property(return_type=BoolType)
    def has_with_visibility(refd_unit=AnalysisUnitType):
        """
        Return whether Self's unit has "with visibility" on "refd_unit".

        In other words, whether Self's unit has a WITH clause on "refd_unit",
        or if its spec, or one of its parent specs has one.
        """
        return Or(
            refd_unit.is_referenced_from(Self.unit),
            Self.parent_unit_env(
                # Here we go and explicitly grab the top level item, rather
                # than use Self's children env, because of use clauses, that
                # can be at the top level but semantically belong to the env of
                # the top level item.
                Self.top_level_item(Self.unit).children_env
            )
            .env_node._.has_with_visibility(refd_unit)
        )

    @langkit_property()
    def resolve_generic_actual():
        """
        Helper property to resolve the actuals of generic instantiations.
        """
        return Entity.match(
            lambda te=T.TypeExpr.entity: origin.bind(Self, te.designated_type),

            # TODO: depending on the formal that matches this actual, this name
            # can be both an object or a type. For now, we assume it's a type
            # but we should handle objects too.
            lambda n=T.Name.entity: n.name_designated_type.cast(T.entity)._or(
                # If we don't find a type, find something else
                env.bind(n.children_env, n.env_elements.at(0))
            ),

            lambda _: No(T.entity),
        )

    @langkit_property()
    def top_level_use_package_clauses():
        """
        If Self is a library item or a subunit, return a flat list of all names
        for top-level UsePackageClause nodes. See
        UsePackageClause.env_spec.ref_envs for more details.
        """
        return (
            Self.parent.parent.cast_or_raise(T.CompilationUnit)
            .prelude
            .filter(lambda p: p.is_a(UsePackageClause))
            .mapcat(
                lambda p: p.cast_or_raise(UsePackageClause).packages.map(
                    lambda n: n.cast(AdaNode)
                )
            )
        )

    @langkit_property()
    def use_packages_in_spec_of_subp_body():
        """
        If Self is a library-level SubpBody, fetch the environments USE'd in
        its declaration.
        """
        return Let(lambda subpb=Self.cast(T.SubpBody): If(
            subpb.parent.is_a(T.LibraryItem),

            subpb.as_bare_entity.decl_part_entity.then(
                lambda subp_decl: subp_decl.top_level_use_package_clauses.map(
                    lambda use_name:
                    origin.bind(use_name, env.bind(
                        use_name.node_env,
                        use_name.cast_or_raise(T.Name)
                        .as_bare_entity.designated_env
                    ))
                ).env_group(),
                default_val=EmptyEnv
            ),

            EmptyEnv
        ))

    @langkit_property()
    def nested_generic_formal_part():
        """
        Assuming Self is a generic entity's body that is nested (not a library
        item), return the lexical environment for the corresponding
        GenericPackageDecl (or GenericSubpDecl) node. Return an empty
        environment in all other cases.

        This is a helper for generic formals visibility in generic bodies. See
        the use in the child_unit macro.

        The following property is evaluated each time we make a recursive
        lexical environment lookup on a child unit. As it does itself a lot of
        lookups, memoizing it is very important.
        """
        gen_decl = Var(Self.as_bare_entity.match(
            lambda pkg_body=T.PackageBody:
                pkg_body.decl_part_entity.then(
                    lambda d: d.el.parent.cast(T.GenericPackageDecl)
                ),
            lambda bod=T.BaseSubpBody:
                # We're only searching for generics. We look at index 1 and
                # 2, because if self is a subunit, the first entity we find
                # will be the separate declaration. NOTE: We don't use
                # decl_part/previous_part on purpose: They can cause env
                # lookups, hence doing an infinite recursion.
                bod.children_env.env_parent.get(bod.name_symbol).then(
                    lambda results:
                    results.at(1).el.cast(T.GenericSubpDecl)._or(
                        results.at(2).el.cast(T.GenericSubpDecl)
                    )
                ).cast(T.AdaNode),
            lambda _: No(T.AdaNode)
        ))

        return gen_decl.then(
            lambda gd: gd.children_env, default_val=Self.empty_env
        )

    @langkit_property()
    def is_library_item():
        """
        Property helper to determine if an entity is the root entity for its
        unit.
        """
        return Self.parent.then(lambda p: p.match(
            lambda _=T.LibraryItem: True,
            lambda gen_pkg_decl=T.GenericPackageDecl:
                gen_pkg_decl.parent.then(lambda p: p.is_a(LibraryItem)),
            lambda _: False,
        ))

    @langkit_property()
    def is_package():
        """
        Property helper to determine if an entity is a package or not.
        """
        return Self.is_a(PackageDecl, PackageBody, GenericPackageInstantiation,
                         PackageRenamingDecl)

    @langkit_property()
    def initial_env():
        """
        Provide a lexical environment to use in EnvSpec's initial_env.
        """
        return Self.parent.then(lambda p: p.children_env,
                                default_val=Self.children_env)

    @langkit_property(ignore_warn_on_node=True)
    def top_level_item(unit=AnalysisUnitType):
        """
        Property helper to get the top-level item in "unit".

        This is the body of a Subunit, or the item of a LibraryItem.
        """
        return unit._.root.then(
            lambda root:
                root.cast_or_raise(T.CompilationUnit).body.match(
                    lambda li=T.LibraryItem: li.item,
                    lambda su=T.Subunit: su.body,
                    lambda _: No(T.BasicDecl),
                )
        )

    @langkit_property()
    def unpack_formals(formal_params=T.BaseFormalParamDecl.entity.array):
        """
        Couples (identifier, param spec) for all parameters.
        """
        return formal_params.mapcat(
            lambda spec: spec.identifiers.map(lambda id: SingleFormal.new(
                name=id, spec=spec
            ))
        )

    @langkit_property(return_type=T.ParamMatch.array, dynamic_vars=[env])
    def match_formals(formal_params=T.BaseFormalParamDecl.entity.array,
                      params=T.AssocList,
                      is_dottable_subp=BoolType):
        """
        For each ParamAssoc in a AssocList, return whether we could find a
        matching formal in Self, and whether this formal is optional (i.e. has
        a default value).
        """
        def matches(formal, actual):
            return ParamMatch.new(has_matched=True,
                                  formal=formal, actual=actual)

        unpacked_formals = Var(Self.unpack_formals(formal_params))

        return params.then(lambda p: p.unpacked_params.map(lambda i, a: If(
            a.name.is_null,

            Let(lambda idx=If(is_dottable_subp, i + 1, i):
                # Positional parameter case: if this parameter has no
                # name association, make sure we have enough formals.
                unpacked_formals.at(idx).then(lambda sp: matches(sp, a))),

            # Named parameter case: make sure the designator is
            # actualy a name and that there is a corresponding
            # formal.
            a.name.then(lambda id: (
                unpacked_formals.find(lambda p: p.name.matches(id)).then(
                    lambda sp: matches(sp, a)
                )
            ))
        )))

    @langkit_property(public=True)
    def gnat_xref():
        """
        Return a cross reference from this name to a defining identifier,
        trying to mimic GNAT's xrefs as much as possible.
        """

        bd = Var(
            Entity.parents.find(lambda p: p.is_a(T.DefiningName))
            .cast(T.DefiningName).then(
                lambda dn: dn.basic_decl
            )
        )

        return Cond(
            bd.then(lambda bd: bd.is_a(T.ParamSpec))
            & bd.semantic_parent.is_a(T.SubpDecl, T.ExprFunction,
                                      T.GenericSubpInternal,
                                      T.BaseTypeDecl),
            bd.semantic_parent.cast(T.BasicDecl).defining_name,

            bd.then(lambda bd: bd.is_a(T.DiscriminantSpec)),
            bd.semantic_parent.cast(T.BasicDecl).defining_name,

            bd.then(lambda bd: bd.is_a(T.ParamSpec))
            & bd.semantic_parent.is_a(T.AbstractSubpDecl, T.FormalSubpDecl,
                                      T.NullSubpDecl),
            bd.semantic_parent.cast(T.BasicDecl).defining_name,

            bd.then(lambda bd: bd.is_a(T.AbstractSubpDecl)),
            bd.cast(T.AbstractSubpDecl).subp_decl_spec
            .primitive_subp_of.defining_name,

            bd.then(lambda bd: bd.is_a(T.BasicSubpDecl)),
            bd.cast(T.BasicSubpDecl).subp_decl_spec.primitive_subp_of.then(
                lambda prim_typ:
                prim_typ.is_tagged_type.then(
                    lambda _: prim_typ.private_completion.then(
                        lambda pc: pc.defining_name
                    )._or(prim_typ.defining_name)
                )
            ),

            Let(lambda ret=Entity.xref: Let(lambda dbd=ret.basic_decl: Cond(
                dbd.is_a(T.ParamSpec),
                dbd.cast(T.ParamSpec).decl_param(ret),

                dbd.is_a(T.GenericSubpInternal, T.GenericPackageInternal),
                dbd.generic_instantiations.at(0).then(
                    lambda gi: gi.cast_or_raise(T.BasicDecl).defining_name,
                    default_val=ret
                ),

                dbd.is_a(T.ObjectDecl),
                dbd.cast(T.ObjectDecl).public_part_decl.then(
                    lambda ppd: ppd.defining_name
                )._or(ret),


                dbd.is_a(T.BaseSubpBody),
                dbd.cast(T.BaseSubpBody)
                .decl_part_entity._or(dbd).defining_name,

                ret
            )))
        )


def child_unit(name_expr, scope_expr, dest_env=None,
               transitive_parent=False, more_rules=[]):
    """
    This macro will add the properties and the env specification necessary
    to make a node implement the specification of a library child unit in
    Ada, so that you can declare new childs to an unit outside of its own
    scope.

    :param AbstractExpression name_expr: The expression that will retrieve
        the name symbol for the decorated node.

    :param AbstractExpression scope_expr: The expression that will retrieve the
        scope node for the decorated node. If the scope node is not found, it
        should return EmptyEnv: in this case, the actual scope will become the
        root environment.

    :rtype: EnvSpec
    """
    more_rules = list(more_rules)

    add_to_env_expr = (
        add_to_env_kv(name_expr, Self, dest_env=dest_env)
        if dest_env else add_to_env_kv(name_expr, Self)
    )

    return EnvSpec(
        call_env_hook(Self),
        set_initial_env(
            env.bind(Self.initial_env, Let(
                lambda scope=scope_expr: If(scope == EmptyEnv, env, scope)
            ))
        ),
        add_to_env_expr,
        add_env(transitive_parent=transitive_parent),
        ref_used_packages(),
        ref_generic_formals(),
        *more_rules
    )


@abstract
class BasicDecl(AdaNode):

    is_in_private_part = Property(Self.parent.parent.is_a(T.PrivatePart))

    @langkit_property(return_type=BoolType)
    def subp_decl_match_signature(other=T.BasicDecl.entity):
        return (
            Entity.subp_spec_or_null
            .cast_or_raise(T.BaseSubpSpec).match_signature(
                other.subp_spec_or_null.cast_or_raise(T.SubpSpec),
                False
            )
        )

    annotations = Annotations(custom_short_image=True)

    defining_names = AbstractProperty(
        type=T.DefiningName.entity.array, public=True,
        doc="""
        Get all the names of this basic declaration.
        """
    )

    defining_name = Property(
        Entity.defining_names.at(0), public=True, ignore_warn_on_node=True,
        doc="""
        Get the name of this declaration. If this declaration has several
        names, it will return the first one.
        """
    )

    defining_env = Property(
        EmptyEnv,
        dynamic_vars=[origin],
        doc="""
        Return a lexical environment that contains entities that are accessible
        as suffixes when Self is a prefix.
        """
    )

    @langkit_property(dynamic_vars=[origin], return_type=T.BaseTypeDecl.entity)
    def identity_type():
        return Entity.match(
            lambda _=T.ExceptionDecl: Self.ext_id_type,
            lambda _=T.TaskTypeDecl: Self.task_id_type,
            lambda _=T.TaskBody: Self.task_id_type,
            lambda _: No(T.BaseTypeDecl.entity)
        )

    @langkit_property(dynamic_vars=[origin], return_type=LongType)
    def array_ndims():
        return Entity.expr_type.array_ndims

    is_array = Property(Entity.array_ndims > 0, dynamic_vars=[origin])

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[origin])
    def expr_type():
        """
        Return the type declaration corresponding to this basic declaration
        has when it is used in an expression context. For example, for this
        basic declaration::

            type Int is range 0 .. 100;

            A : Int := 12;

        the declaration of the Int type will be returned. For this
        declaration::

            type F is delta 0.01 digits 10;

            function B return F;

        expr_type will return the declaration of the type F.
        """
        ret = Var(Entity.type_expression.then(lambda te: te.designated_type))

        # If the entity is actually an anonymous access to the decl rather than
        # the decl itself, return an anonymous access type pointing to the type
        # of the decl.
        ret_2 = Var(If(
            Entity.info.md.access_entity,
            ret.anonymous_access_type,
            ret
        ))

        return If(
            # If this basic decl has been found through a primitive env
            # (md.primitive is set) and this primitive env is the one of the
            # return type (md.primitive == return type), then we want to return
            # the derived type through which we found this primitive
            # (md.primitive_real_type).

            Not(ret_2.is_null) & (Entity.info.md.primitive == ret_2.el),
            entity_no_md(
                BaseTypeDecl,
                Entity.info.md.primitive_real_type.cast(BaseTypeDecl),
                Entity.info.rebindings
            ),
            ret_2
        )

    type_expression = Property(
        No(T.TypeExpr).as_entity,
        type=T.TypeExpr.entity,
        doc="""
        Return the type expression for this BasicDecl if applicable, a null
        otherwise.
        """
    )

    @langkit_property(return_type=T.BaseFormalParamHolder.entity)
    def subp_spec_or_null(follow_generic=(BoolType, False)):
        """
        If node is a Subp, returns the specification of this subprogram.
        """
        return Entity.match(
            lambda subp=BasicSubpDecl:  subp.subp_decl_spec,
            lambda subp=BaseSubpBody:       subp.subp_spec,
            lambda subp=SubpBodyStub:   subp.subp_spec,
            lambda entry=EntryDecl:     entry.spec,
            lambda gsp=GenericSubpDecl:
            If(follow_generic, gsp.subp_decl.subp_spec, No(SubpSpec.entity)),
            lambda _:                   No(SubpSpec.entity),
        )

    @langkit_property(return_type=BoolType)
    def is_subprogram():
        return Self.is_a(BasicSubpDecl, BaseSubpBody, SubpBodyStub, EntryDecl)

    @langkit_property(return_type=BoolType)
    def is_stream_subprogram_for_type(typ=T.BaseTypeDecl.entity,
                                      return_obj=BoolType):
        root_stream_type = Var(
            Entity
            .get_compilation_unit(['Ada', 'Streams'], UnitSpecification)
            ._.children_env.get_first('Root_Stream_Type', recursive=False)
            .cast(T.BaseTypeDecl).classwide_type.cast(T.BaseTypeDecl)
        )
        params = Var(Entity.subp_spec_or_null._.unpacked_formal_params)

        return origin.bind(
            Self,
            Self.is_subprogram
            & params.at(0).spec.type.is_access_to(root_stream_type)
            & If(
                return_obj,
                Entity.subp_spec_or_null.return_type.matching_formal_type(typ),
                params.at(1).spec.type.matching_formal_type(typ),
            )
        )

    @langkit_property(return_type=BoolType)
    def can_be_paramless():
        """
        Return true if entity can be a paramless subprogram entity, when used
        in an expression context.
        """
        return Entity.subp_spec_or_null.then(
            lambda ss: ss.paramless(Entity.info.md, can_be=True),
            default_val=True
        )

    @langkit_property(return_type=BoolType)
    def is_paramless():
        """
        Return true if entity is a paramless subprogram entity, when used
        in an expression context.
        """
        return Entity.subp_spec_or_null.then(
            lambda ss: ss.paramless(Entity.info.md, can_be=False),
            default_val=True
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[origin])
    def constrain_prefix(prefix=T.Expr):
        """
        This method is used when self is a candidate suffix in a dotted
        expression, to express the potential constraint that the suffix could
        express on the prefix.

        For example, given this code::

            1 type P is record
            2     A, B : Integer;
            3 end record;
            4
            5 P_Inst : P;
            7
            8 P_Inst.A;
              ^^^^^^^^

        A references the A ComponentDecl at line 2, and the constraint that we
        want to express on the prefix (P_Inst), is that it needs to be of type
        P.
        """
        # Default implementation returns logic true => does not add any
        # constraint to the xref equation.
        ignore(prefix)
        return LogicTrue()

    declarative_scope = Property(
        Self.parents.find(
            lambda p: p.is_a(T.DeclarativePart)
        ).cast(T.DeclarativePart),
        doc="Return the scope of definition of this basic declaration.",
        ignore_warn_on_node=True
    )

    relative_name = Property(Entity.defining_name.relative_name)
    name_symbol = Property(Self.as_bare_entity.relative_name.symbol)

    @langkit_property()
    def body_part_entity():
        """
        Return the body corresponding to this node if applicable.
        """
        ignore(Var(Self.body_unit))
        return If(
            Self.is_a(T.GenericSubpInternal), Entity.parent.children_env,
            Entity.children_env
        ).get_first('__body', recursive=False).cast(T.Body)

    @langkit_property(dynamic_vars=[env])
    def decl_scope(follow_private=(BoolType, True)):
        scope = Var(Entity.defining_name.parent_scope)

        # If this the corresponding decl is a generic, go grab the internal
        # package decl. Then If the package has a private part, then get the
        # private part, else return the public part.
        return Let(
            lambda public_scope=scope.env_node.cast(T.GenericPackageDecl).then(
                lambda gen_pkg_decl: If(
                    Self.is_a(FormalSubpDecl),
                    scope,
                    gen_pkg_decl.package_decl.children_env,
                ),
                default_val=scope
            ): If(
                And(
                    follow_private,
                    public_scope.env_node._.is_a(
                        T.BasePackageDecl, T.SingleProtectedDecl,
                        T.ProtectedTypeDecl,
                    )
                ),
                # Don't try to go to private part if we're not in a package
                # decl.

                public_scope.get('__privatepart', recursive=False).at(0).then(
                    lambda pp: pp.children_env, default_val=public_scope
                ),
                public_scope
            )
        )

    @langkit_property(public=True)
    def fully_qualified_name():
        """
        Return the fully qualified name corresponding to this declaration.
        """
        # TODO: handle non-library items
        name = Var(Self.match(
            lambda subp_decl=T.ClassicSubpDecl:
                subp_decl.subp_spec.subp_name,
            lambda pkg_decl=T.BasePackageDecl:
                pkg_decl.package_name,
            lambda subp_inst=T.GenericSubpInstantiation:
                subp_inst.subp_name,
            lambda pkg_inst=T.GenericPackageInstantiation:
                pkg_inst.name,
            lambda pkg_renam=T.PackageRenamingDecl:
                pkg_renam.name,
            lambda gen_subp=T.GenericSubpDecl:
                gen_subp.subp_decl.subp_spec.subp_name,
            lambda gen_pkg=T.GenericPackageDecl:
                gen_pkg.package_decl.package_name,
            lambda subp_body=T.SubpBody:
                subp_body.subp_spec.subp_name,
            lambda pkg_body=T.PackageBody:
                pkg_body.package_name,
            lambda _: PropertyError(T.Name),
        ))
        return name.as_symbol_array


class ErrorDecl(BasicDecl):
    defining_names = Property(No(T.DefiningName.entity.array))


@abstract
class Body(BasicDecl):

    @langkit_property()
    def body_decl_scope():
        """
        Return the scope of this body's decl.
        """
        return env.bind(
            Self.initial_env,
            Entity.body_scope(True, True)
        )

    @langkit_property()
    def previous_part():
        """
        Return the decl corresponding to this body. For convenience, the
        default implemention is for subprograms, overloaded in other kinds of
        bodies.
        """
        return If(
            Self.is_library_item & Not(Self.is_subunit),

            # If library item, we just return the spec. We don't check if it's
            # a valid and matching subprogram because that's an error case.
            Self.top_level_item(Self.spec_unit).as_entity,

            # If not a library item, find the matching subprogram spec in the
            # env.
            Entity.children_env.env_parent.get(Entity.name_symbol)
            .find(lambda sp: And(Not(sp.is_null), Not(sp.el == Self), sp.match(
                # If this body completes a generic subprogram, then we just
                # return it (no need to match the signature).
                lambda _=T.GenericSubpDecl: True,

                lambda subp_decl=T.BasicSubpDecl:
                subp_decl.subp_decl_spec.match_signature(
                    Entity.subp_spec_or_null.cast(T.SubpSpec), True
                ),

                lambda subp_stub=T.SubpBodyStub:
                subp_stub.subp_spec.match_signature(
                    Entity.subp_spec_or_null.cast(T.SubpSpec), True
                ),

                lambda _: False
            ))).cast_or_raise(T.BasicDecl.entity)
        )

    @langkit_property()
    def decl_part_entity():
        return Entity.previous_part.then(
            lambda prev_part: prev_part.match(
                lambda subp_stub=T.SubpBodyStub:
                subp_stub.previous_part,
                lambda other: other
            )
        )

    decl_part = Property(
        Entity.decl_part_entity,
        public=True,
        doc="""
        Return the decl corresponding to this node if applicable.
        """

    )

    @langkit_property()
    def is_subunit():
        return Not(Self.parent.cast(T.Subunit).is_null)

    @langkit_property(ignore_warn_on_node=True)
    def subunit_root():
        """
        If self is a subunit, return the body in which it is rooted.
        """
        return Self.parent.cast(T.Subunit).then(lambda su: Self.top_level_item(
            su.name.referenced_unit_or_null(UnitBody)
        ))

    @langkit_property(dynamic_vars=[env])
    def body_scope(follow_private=BoolType, force_decl=(BoolType, False)):
        """
        Return the scope for this body.
        If follow_private, then returns the private part if possible.

        If force_decl, then returns the corresponding declaration's scope,
        rather than the parent body's scope.
        """

        # Subunits always appear at the top-level in package bodies. So if
        # this is a subunit, the scope is the same as the scope of the
        # corresponding "is separate" decl, hence: the defining env of this
        # top-level package body.
        scope = Var(Self.subunit_root.then(
            lambda su: su.children_env,

            # In case this is a library level subprogram that has no spec
            # (which is legal), we'll register this body in the parent scope.
            default_val=Cond(
                Self.is_subprogram & Self.is_library_item,
                Let(lambda dns=Entity.defining_name.scope:
                    # If the scope is self's scope, return parent scope, or
                    # else we'll have an infinite recursion.
                    If(dns.is_null | (dns.env_node == Self),
                       Entity.defining_name.parent_scope,
                       dns)),

                Self.is_library_item | force_decl, Entity.defining_name.scope,

                Self.parent.children_env,

            )
        ))

        # If this the corresponding decl is a generic, go grab the internal
        # package decl.
        public_scope = Var(scope.env_node.cast(T.GenericPackageDecl).then(
            lambda gen_pkg_decl: gen_pkg_decl.package_decl.children_env,
            default_val=scope
        ))

        # If the package has a private part, then get the private part,
        # else return the public part.
        return If(
            And(
                follow_private,
                public_scope.env_node._.is_a(
                    T.BasePackageDecl, T.SingleProtectedDecl,
                    T.ProtectedTypeDecl,
                )
            ),
            public_scope.get('__privatepart', recursive=False).at(0).then(
                lambda pp: pp.children_env, default_val=public_scope
            ),
            public_scope
        )


@abstract
class BodyStub(Body):
    pass


@abstract
class BaseFormalParamDecl(BasicDecl):
    """
    Base class for formal parameter declarations. This is used both for records
    components and for subprogram parameters.
    """
    identifiers = Property(Entity.defining_names.map(lambda e: e.el))
    is_mandatory = Property(False)

    type = Property(
        origin.bind(
            Self, Entity.type_expression._.designated_type
        )
    )

    el_type = Property(
        origin.bind(
            Self, Entity.type_expression._.element_type
        )
    )


class DiscriminantSpec(BaseFormalParamDecl):
    ids = Field(type=T.DefiningName.list)
    type_expr = Field(type=T.TypeExpr)
    default_expr = Field(type=T.Expr)

    env_spec = EnvSpec(add_to_env(env_mappings(Self.ids, Self)))

    defining_names = Property(Self.ids.map(lambda id: id.as_entity))
    defining_env = Property(Entity.type_expr.defining_env)

    type_expression = Property(Entity.type_expr)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.type_expr.sub_equation,
            Entity.default_expr.then(
                lambda de:
                de.sub_equation
                & Bind(de.el.type_var,
                       Entity.expr_type,
                       eq_prop=BaseTypeDecl.matching_assign_type),
                default_val=LogicTrue()
            )
        )


@abstract
class BaseFormalParamHolder(AdaNode):
    """
    Base class for lists of formal parameters. This is used both for subprogram
    specifications and for records, so that we can share the matching and
    unpacking logic.
    """

    abstract_formal_params = AbstractProperty(
        type=BaseFormalParamDecl.entity.array,
        doc="Return the list of abstract formal parameters for this holder."
    )

    unpacked_formal_params = Property(
        Self.unpack_formals(Entity.abstract_formal_params),
        doc="""
        Couples (identifier, param spec) for all parameters
        """
    )

    @langkit_property(return_type=T.ParamMatch.array, dynamic_vars=[env])
    def match_param_list(params=T.AssocList,
                         is_dottable_subp=BoolType):
        return Self.match_formals(
            Entity.abstract_formal_params, params, is_dottable_subp
        )

    nb_min_params = Property(
        Self.as_bare_entity.unpacked_formal_params.filter(
            lambda p: p.spec.is_mandatory
        ).length,
        type=LongType, doc="""
        Return the minimum number of parameters this subprogram can be called
        while still being a legal call.
        """
    )

    nb_max_params = Property(
        Self.as_bare_entity.unpacked_formal_params.length, type=LongType,
        doc="""
        Return the maximum number of parameters this subprogram can be called
        while still being a legal call.
        """
    )

    @langkit_property(return_type=BoolType)
    def paramless(md=Metadata, can_be=(BoolType, True)):
        """
        Utility function. Given a subprogram spec and its associated metadata,
        determine if it can be called without parameters (and hence without a
        callexpr).
        """
        nb_params = Var(If(can_be, Self.nb_min_params, Self.nb_max_params))
        return Or(
            md.dottable_subp & (nb_params == 1),
            nb_params == 0
        )

    @langkit_property(return_type=BoolType)
    def match_formal_params(other=T.BaseFormalParamHolder.entity,
                            match_names=(BoolType, True)):
        # Check that there is the same number of formals and that each
        # formal matches.

        self_params = Var(Entity.unpacked_formal_params)
        other_params = Var(other.unpacked_formal_params)
        return And(
            self_params.length == other_params.length,
            self_params.all(lambda i, p: And(
                p.name.matches(other_params.at(i).name) | Not(match_names),
                origin.bind(
                    Self,
                    p.spec.type._.matching_type(other_params.at(i).spec.type)
                )
            ))
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[env])
    def is_matching_param_list(params=T.AssocList, is_dottable_subp=BoolType):
        """
        Return whether a AssocList is a match for this SubpSpec, i.e.
        whether the argument count (and designators, if any) match.
        """
        bare = Var(Self.as_bare_entity)
        match_list = Var(bare.match_param_list(params, is_dottable_subp))
        nb_max_params = If(is_dottable_subp, bare.nb_max_params - 1,
                           bare.nb_max_params)
        nb_min_params = If(is_dottable_subp, bare.nb_min_params - 1,
                           bare.nb_min_params)

        return And(
            params.length <= nb_max_params,
            match_list.all(lambda m: m.has_matched),
            match_list.filter(
                lambda m: m.formal.spec.is_mandatory
            ).length == nb_min_params,
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity, dynamic_vars=[origin])
    def return_type():
        """
        Returns the return type of Self, if applicable (eg. if Self is a
        subprogram). Else, returns null.
        """
        return No(T.BaseTypeDecl.entity)


@abstract
class DiscriminantPart(BaseFormalParamHolder):

    @langkit_property()
    def abstract_formal_params():
        return No(T.BaseFormalParamDecl.entity.array)


class KnownDiscriminantPart(DiscriminantPart):
    discr_specs = Field(type=T.DiscriminantSpec.list)

    @langkit_property()
    def abstract_formal_params():
        return Self.discr_specs.map(
            lambda e: e.cast(BaseFormalParamDecl).as_entity
        )


class UnknownDiscriminantPart(DiscriminantPart):
    pass


@abstract
class TypeDef(AdaNode):

    @langkit_property(return_type=T.DiscreteRange)
    def discrete_range():
        """
        Return the discrete range for this type def, if applicable.
        """
        return No(DiscreteRange)

    array_ndims = Property(
        Literal(0),
        doc="""
        If this designates an array type, return its number of dimensions.
        Return 0 otherwise.
        """,
        dynamic_vars=[origin]
    )

    is_real_type = Property(False, doc="Whether type is a real type or not.")

    @langkit_property(dynamic_vars=[origin])
    def is_discrete_type():
        return Entity.is_int_type | Entity.is_enum_type | Entity.is_char_type

    @langkit_property(dynamic_vars=[origin])
    def is_int_type():
        """Whether type is an integer type or not."""
        return False

    is_access_type = Property(False, uses_entity_info=False,
                              doc="Whether type is an access type or not.")
    is_char_type = Property(False)
    is_enum_type = Property(False)
    is_record_type = Property(False)

    @langkit_property(dynamic_vars=[origin])
    def accessed_type():
        return No(BaseTypeDecl.entity)

    @langkit_property(public=True)
    def is_tagged_type():
        """
        Return whether this type is tagged.
        """
        return False

    base_type = Property(
        No(T.BaseTypeDecl.entity), doc="""
        Return the base type entity for this derived type definition.
        """
    )

    base_types = Property(
        Entity.base_type.singleton.concat(Entity.base_interfaces), doc="""
        Return all the base types for this type (base type + base interfaces)
        """
    )

    base_interfaces = Property(
        No(T.BaseTypeDecl.entity.array), doc="""
        Return the interfaces this type derives from
        """
    )

    @langkit_property(dynamic_vars=[origin])
    def defining_env():
        return EmptyEnv

    containing_type = Property(
        Entity.parent.cast_or_raise(T.TypeDecl), doc="""
        Return the TypeDecl containing this TypeDef
        """
    )

    previous_part = Property(Entity.containing_type.previous_part(True))

    previous_part_env = Property(Entity.previous_part._.match(
        lambda _=T.IncompleteTypeDecl: EmptyEnv,
        lambda t: t.children_env
    ))


class Variant(AdaNode):
    choices = Field(type=T.AlternativesList)
    components = Field(type=T.ComponentList)

    @langkit_property(return_type=BoolType)
    def choice_match(choice=T.AdaNode.entity, val=T.BigIntegerType):
        """
        Checks whether val matches choice.
        """
        return choice.match(

            # If choice is a binop, it is either a range, or a static
            # arithmetic expression.
            lambda bo=T.BinOp: If(
                # If choice is a range, then check that val is in the range
                bo.op.is_a(Op.alt_double_dot),

                And(val >= bo.left.eval_as_int,
                    val <= bo.right.eval_as_int),

                val == bo.eval_as_int,
            ),

            # If choice is a name, it is either a subtype name, either a
            # constant number name.
            lambda n=T.Name: n.name_designated_type._.discrete_range.then(
                lambda dr: And(val >= dr.low_bound, val <= dr.high_bound),
                default_val=(val == n.eval_as_int)
            ),

            # If choice is a subtype indication, then get the range
            lambda st=T.SubtypeIndication: st.discrete_range.then(
                lambda dr: And(val >= dr.low_bound,
                               val <= dr.high_bound)
            ),

            # If it is an expr, then just check for equality
            lambda e=T.Expr: val == e.eval_as_int,

            # If 'others', always return true
            lambda _=T.OthersDesignator: True,

            lambda _: False,
        )

    @langkit_property(return_type=BoolType)
    def matches(expr=T.Expr):
        """
        Check if any choice in the choice list matches expr's value.
        """
        # Statically evaluate expr
        expr_val = Var(expr.eval_as_int)

        return Self.choices.any(
            lambda c: Self.choice_match(c.as_entity, expr_val)
        )


class VariantPart(AdaNode):
    discr_name = Field(type=T.Identifier)
    variant = Field(type=T.Variant.list)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        ignore(Var(
            Entity.discr_name.resolve_names_internal(True, LogicTrue()))
        )

        return Entity.variant.logic_all(lambda var: (
            var.choices.logic_all(lambda c: c.match(
                # Expression case
                lambda e=T.Expr:
                TypeBind(e.type_var, Self.discr_name.type_val)
                & e.sub_equation,

                # TODO: Bind other cases: SubtypeIndication and Range
                lambda _: LogicTrue()
            ))
        ))

    @langkit_property(return_type=T.BaseFormalParamDecl.entity.array)
    def get_components(discriminants=T.ParamMatch.array):
        """
        Get components for this variant part, depending on the values of
        discriminants.
        """
        # Get the specific discriminant this variant part depends upon
        discr = Var(discriminants.find(
            lambda d: d.formal.name.name_symbol == Self.discr_name.symbol
        ))

        # Get the variant branch with a choice that matches the discriminant's
        # value.
        variant = Var(Self.variant.find(
            lambda v: v.as_entity.matches(discr.actual.assoc.expr)
        ).as_entity)

        # Get the components for this variant branch. We're passing down
        # discriminants, because there might be a nested variant part in this
        # variant branch.
        return variant.components.abstract_formal_params_impl(
            discriminants, False, False
        )


class ComponentDecl(BaseFormalParamDecl):
    ids = Field(type=T.DefiningName.list)
    component_def = Field(type=T.ComponentDef)
    default_expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    env_spec = EnvSpec(add_to_env(env_mappings(Self.ids, Self)))

    defining_env = Property(
        Entity.component_def.type_expr.defining_env,
        doc="See BasicDecl.defining_env"
    )

    defining_names = Property(Self.ids.map(lambda i: i.as_entity))

    array_ndims = Property(Entity.component_def.type_expr.array_ndims)

    type_expression = Property(Self.component_def.type_expr.as_entity)

    @langkit_property(return_type=EquationType)
    def constrain_prefix(prefix=T.Expr):
        # Simple type equivalence
        return Bind(prefix.type_var,
                    Entity.container_type,
                    eq_prop=BaseTypeDecl.matching_prefix_type)

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def container_type():
        """
        Return the defining container type for this component declaration.
        """
        return Self.parents.find(
            lambda p: p.is_a(BaseTypeDecl)
        ).cast(BaseTypeDecl).as_entity

    @langkit_property()
    def xref_equation():
        typ = Var(Entity.expr_type)
        return (
            Entity.component_def.type_expr.sub_equation
            & Entity.default_expr.then(
                lambda de:
                de.sub_equation
                & Bind(de.type_var, typ,
                       eq_prop=BaseTypeDecl.matching_assign_type),
                default_val=LogicTrue()
            )
        )

    xref_entry_point = Property(True)


class ComponentList(BaseFormalParamHolder):
    components = Field(type=T.AdaNode.list)
    variant_part = Field(type=T.VariantPart)

    type_def = Property(Self.parent.parent.cast(T.TypeDef).as_entity)
    type_decl = Property(Entity.type_def.parent.cast(T.TypeDecl))

    parent_component_list = Property(
        Entity.type_def.cast(T.DerivedTypeDef)._.base_type.record_def._.comps
    )

    @langkit_property(return_type=BaseFormalParamDecl.entity.array)
    def abstract_formal_params_impl(
        discriminants=T.ParamMatch.array,
        include_discriminants=(BoolType, True),
        recurse=(BoolType, True)
    ):

        # Get self's components. We pass along discriminants, to get variant
        # part's components too.
        self_comps = Var(Entity.components.keep(BaseFormalParamDecl).concat(
            Entity.variant_part._.get_components(discriminants)
        ))

        # Append parent's components.
        # TODO: The parent could have a variant part too, using the explicit
        # discriminants mappings. We need to handle that too.
        ret = Var(If(
            recurse,
            Entity.parent_component_list.then(
                lambda pcl: pcl.abstract_formal_params_impl(
                    No(T.ParamMatch.array), False
                )
                .concat(self_comps),
                default_val=self_comps
            ),
            self_comps
        ))

        return If(
            include_discriminants,
            Entity.type_decl._.discriminants_list.concat(ret),
            ret
        )

    @langkit_property()
    def abstract_formal_params():
        return Entity.abstract_formal_params_impl(No(T.ParamMatch.array))


@abstract
class BaseRecordDef(AdaNode):
    components = Field(type=T.ComponentList)

    # TODO: Kludge, to remove when Q619-018 is implemented
    comps = Property(Self.components.as_entity)


class RecordDef(BaseRecordDef):
    pass


class NullRecordDef(BaseRecordDef):
    pass


class Tagged(EnumNode):
    qualifier = True


class Abstract(EnumNode):
    qualifier = True


class Limited(EnumNode):
    qualifier = True


class Private(EnumNode):
    qualifier = True


class Aliased(EnumNode):
    qualifier = True


class NotNull(EnumNode):
    qualifier = True


class Constant(EnumNode):
    qualifier = True


class All(EnumNode):
    qualifier = True


class Abort(EnumNode):
    qualifier = True


class Reverse(EnumNode):
    qualifier = True


class WithPrivate(EnumNode):
    qualifier = True


class Until(EnumNode):
    qualifier = True


class Synchronized(EnumNode):
    qualifier = True


class Protected(EnumNode):
    qualifier = True


class RecordTypeDef(TypeDef):
    has_abstract = Field(type=Abstract)
    has_tagged = Field(type=Tagged)
    has_limited = Field(type=Limited)
    record_def = Field(type=T.BaseRecordDef)

    defining_env = Property(
        Array([
            Entity.children_env, Entity.previous_part_env
        ]).env_group(),
        type=LexicalEnvType
    )

    is_tagged_type = Property(Self.has_tagged.as_bool)
    is_record_type = Property(True)

    xref_equation = Property(LogicTrue())


@abstract
class RealTypeDef(TypeDef):
    is_real_type = Property(True)
    xref_equation = Property(LogicTrue())


class DiscreteRange(Struct):
    """
    Represents the range of a discrete type or subtype. The bounds are already
    evaluated, so the type of the fields is LongType.
    """
    low_bound = UserField(type=T.BigIntegerType)
    high_bound = UserField(type=T.BigIntegerType)


@abstract
class BaseTypeDecl(BasicDecl):
    name = Field(type=T.DefiningName)

    env_spec = EnvSpec(add_to_env_kv(Entity.name_symbol, Self))

    defining_names = Property(Entity.name.singleton)

    @langkit_property(return_type=T.BaseTypeDecl.entity, memoized=True)
    def anonymous_access_type():
        return T.SynthAnonymousTypeDecl.new(
            name=Self.name,
            discriminants=No(T.DiscriminantPart),
            type_def=T.AnonymousTypeAccessDef.new(
                has_not_null=T.NotNullAbsent.new(),
                type_decl=Self
            ),
            aspects=No(T.AspectSpec),
            prims_env=No(T.LexicalEnvType)
        ).cast(T.BaseTypeDecl).as_entity

    @langkit_property(
        return_type=T.BaseTypeDecl.entity, public=True, memoized=True
    )
    def private_completion():
        """
        Return the private completion for this type, if there is one.
        """
        return (
            Entity.declarative_scope.cast(T.PublicPart)
            ._.parent.cast(BasePackageDecl)
            ._.private_part._.decls._.find(
                lambda d: d.cast(T.BaseTypeDecl).then(
                    lambda pp:
                    pp.name_symbol == Entity.name_symbol
                )
            ).cast(T.BaseTypeDecl).as_entity
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def model_of_type():
        """
        Return the type for which this type is a model, if applicable.
        """
        return (
            Entity.get_aspect('Model_Of')
            .expr.cast_or_raise(T.Name).name_designated_type
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def modeled_type(from_unit=AnalysisUnitType):
        """
        Return model type for this type if applicable.
        """
        types_with_models = (Self.top_level_item(from_unit)
                             .cast_or_raise(T.PackageDecl).public_part
                             .types_with_models)

        return types_with_models.find(lambda t: t.model_of_type == Entity)

    @langkit_property(return_type=BoolType)
    def is_view_of_type(comp_view=T.BaseTypeDecl.entity):
        """
        Predicate that will return true if comp_view is a more complete view of
        type typ, or if it is the same view of type typ.
        """
        typ = Var(Entity)
        return Cond(
            comp_view.is_null, False,
            comp_view == typ, True,
            typ.is_view_of_type(comp_view.previous_part(True))
        )

    @langkit_property(dynamic_vars=[origin], return_type=BoolType)
    def is_array_or_rec():
        return Entity.is_array | Entity.is_record_type

    is_record_type = Property(False)
    is_task_type = Property(False, doc="Whether type is a task type")
    is_real_type = Property(False, doc="Whether type is a real type or not.")
    is_enum_type = Property(False)
    is_classwide = Property(False)
    is_access_type = Property(
        False, public=True, doc="Whether Self is an access type or not"
    )

    is_implicit_deref = Property(
        Entity.is_access_type | Not(Entity.get_imp_deref.is_null),
        doc="Whether Self is an implicitly dereferenceable type or not"
    )

    has_ud_indexing = Property(
        False, doc="Whether self has user defined indexing or not"
    )

    constant_indexing_fns = Property(
        No(T.BasicDecl.entity.array),
        doc="""
        For a type with user defined indexing, return the set of all
        Constant_Indexing functions.
        """
    )
    variable_indexing_fns = Property(
        No(T.BasicDecl.entity.array),
        doc="""
        For a type with user defined indexing, return the set of all
        Variable_Indexing functions.
        """
    )

    get_imp_deref = Property(
        No(T.AspectAssoc.entity),
        doc="Whether Self has an Implicit_Dereference aspect or not"
    )

    access_def = Property(No(T.AccessDef.entity))

    is_char_type = Property(False,
                            doc="Whether type is a character type or not")

    classwide_type = Property(If(
        Entity.is_tagged_type,
        Self.classwide_type_node.as_entity,
        No(T.ClasswideTypeDecl.entity)
    ))

    @langkit_property(dynamic_vars=[origin], return_type=LongType)
    def array_ndims():
        return Literal(0)

    @langkit_property(return_type=DiscreteRange)
    def discrete_range():
        """
        Return the discrete range for this type decl, if applicable.
        """
        return No(DiscreteRange)

    @langkit_property(dynamic_vars=[origin], memoized=True)
    def is_iterator_type():
        iifcs = Var(Entity.get_compilation_unit(
            ['Ada', 'Iterator_Interfaces'], UnitSpecification
        ))
        typ = Var(Entity.cast(T.ClasswideTypeDecl).then(
            lambda cw: cw.typedecl, default_val=Entity)
        )
        return typ.semantic_parent.semantic_parent.el == iifcs

    @langkit_property(dynamic_vars=[origin])
    def is_discrete_type():
        return Entity.is_int_type | Entity.is_enum_type

    @langkit_property(dynamic_vars=[origin])
    def is_int_type():
        """Whether type is an integer type or not."""
        return False

    @langkit_property(dynamic_vars=[origin])
    def is_str_type_or_null():
        return Self.is_null | (
            Entity.is_array & Entity.comp_type._.is_char_type
        )

    is_not_null_char_type = Property(Not(Self.is_null) & Entity.is_char_type)

    @langkit_property(dynamic_vars=[origin])
    def accessed_type():
        return No(T.BaseTypeDecl.entity)

    @langkit_property(dynamic_vars=[origin], return_type=T.BaseTypeDecl.entity)
    def final_accessed_type(first_call=(BoolType, True)):
        """
        Call accessed_type recursively until we get the most nested accessed
        type. For example, for the following code::

            type A is access Integer;
            type AA is access A;
            type AAA is access AA;

        ``AAA``'s final_accessed_type is Integer.
        """
        return Entity.accessed_type.then(
            lambda at: at.final_accessed_type(False),
            default_val=If(first_call, No(T.BaseTypeDecl.entity), Entity)
        )

    @langkit_property(dynamic_vars=[origin])
    def is_access_to(typ=T.BaseTypeDecl.entity):
        access_type = Var(Entity)
        return access_type.accessed_type.matching_formal_type(typ)

    @langkit_property(dynamic_vars=[origin])
    def is_subp_access_of(entity=T.BasicDecl.entity):
        """
        Returns whether self is an access type whose accessed type matches
        other.
        """
        access_type = Var(Entity)
        return access_type.access_def.cast(AccessToSubpDef).then(
            lambda sa: sa.subp_spec.match_signature(
                entity.subp_spec_or_null.cast(T.SubpSpec), False
            )
        )

    is_tagged_type = Property(False, doc="Whether type is tagged or not")
    base_type = Property(
        No(T.BaseTypeDecl.entity), doc="""
        Return the base type entity for this derived type declaration.
        """, public=True
    )

    base_types = Property(
        Entity.base_type.singleton.concat(Entity.base_interfaces)
    )

    base_interfaces = Property(No(T.BaseTypeDecl.entity.array))

    array_def = Property(No(T.ArrayTypeDef.entity))
    record_def = Property(No(T.BaseRecordDef.entity))

    @langkit_property(dynamic_vars=[origin])
    def array_def_with_deref():
        """
        Return the array definition corresponding to type `Self` in the context
        of array-indexing, e.g. implicitly dereferencing if `Self` is an
        access.
        """
        return Cond(
            Entity.is_array, Entity.array_def,

            Entity.is_implicit_deref,
            Entity.accessed_type.then(lambda c: c.array_def),

            No(T.ArrayTypeDef.entity)
        )

    @langkit_property(dynamic_vars=[origin], return_type=T.BaseTypeDecl.entity)
    def comp_type(is_subscript=(BoolType, False)):
        """
        Return the component type of `Self`, if applicable. The component type
        is the type you'll get if you call a value whose type is `Self`.  So it
        can either be:

            1. The component type for an array.
            2. The return type for an access to function.
        """
        return Entity.then(
            lambda e: Let(
                lambda ad=If(is_subscript,
                             Entity.array_def_with_deref,
                             Entity.array_def):
                ad.then(lambda ad: ad.comp_type)._or(
                    e.access_def._.match(
                        lambda asd=T.AccessToSubpDef:
                        asd.subp_spec.return_type,
                        lambda tad=T.BaseTypeAccessDef: tad.accessed_type
                    )
                )
            )
        )

    @langkit_property(dynamic_vars=[origin])
    def index_type(dim=LongType):
        return Entity.array_def_with_deref.then(lambda ad: ad.index_type(dim))

    # A BaseTypeDecl in an expression context corresponds to a type conversion,
    # so its type is itself.
    expr_type = Property(Entity)

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def is_derived_type(other_type=T.BaseTypeDecl.entity):
        """
        Whether Self is derived from other_type.
        """
        entity_can = Var(Entity.canonical_type)
        other_can = Var(other_type.canonical_type)
        return Or(
            entity_can == other_can,
            And(Not(Entity.classwide_type.is_null),
                entity_can.classwide_type == other_can.classwide_type),
            Entity.base_types.any(lambda bt: bt._.is_derived_type(other_type))
        )

    is_iterable_type = Property(
        False,
        doc="""
        Whether Self is a type that is iterable in a for .. of loop
        """,
        dynamic_vars=[origin]
    )

    @langkit_property(dynamic_vars=[origin])
    def iterable_comp_type():
        return No(T.BaseTypeDecl.entity)

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_prefix_type(container_type=T.BaseTypeDecl.entity):
        """
        Given a dotted expression A.B, where container_type is the container
        type for B, and Self is a potential type for A, returns whether Self is
        a valid type for A in the dotted expression.
        """
        cont_type = Var(container_type)
        return Or(
            # Derived type case
            Entity.matching_formal_prim_type(cont_type),

            # Access to derived type case
            Entity.final_accessed_type._.matching_formal_prim_type(cont_type),
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_access_type(expected_type=T.BaseTypeDecl.entity):
        """
        Whether self is a matching access type for expected_type.
        """
        actual_type = Var(Entity)
        return expected_type.match(
            lambda atd=T.AnonymousTypeDecl.entity:
            atd.access_def_matches(actual_type),
            lambda _: actual_type.match(
                lambda atd2=T.AnonymousTypeDecl.entity:
                atd2.access_def_matches(expected_type),
                lambda _: False
            )
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_formal_prim_type(formal_type=T.BaseTypeDecl.entity):
        return Entity.matching_formal_type_impl(formal_type, True)

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_formal_type_inverted(formal_type=T.BaseTypeDecl.entity):
        return formal_type.matching_formal_type_impl(Entity)

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_formal_type(formal_type=T.BaseTypeDecl.entity):
        return Entity.matching_formal_type_impl(formal_type)

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_formal_type_impl(formal_type=T.BaseTypeDecl.entity,
                                  accept_derived=(BoolType, False)):
        actual_type = Var(Entity)
        return Or(
            And(formal_type.is_classwide | accept_derived,
                actual_type.is_derived_type(formal_type)),

            And(actual_type.is_classwide,
                actual_type.is_derived_type(formal_type)),

            # Matching of access types parameters
            actual_type.accessed_type.then(
                lambda actual_access:
                formal_type.accessed_type.then(
                    lambda formal_access:
                    And(actual_access.is_classwide | accept_derived,
                        actual_access.is_derived_type(formal_access))
                )
            ),

            And(Not(actual_type.get_imp_deref.is_null),
                actual_type
                .accessed_type.matching_formal_type(formal_type)),

            actual_type.matching_type(formal_type)
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_assign_type(expected_type=T.BaseTypeDecl.entity):
        actual_type = Var(Entity)
        return Or(
            Entity.matching_type(expected_type),

            And(
                expected_type.is_classwide,
                actual_type.matching_formal_prim_type(expected_type)
            ),

            And(
                Not(actual_type.get_imp_deref.is_null),
                actual_type
                .accessed_type.matching_assign_type(expected_type)
            )
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_type(expected_type=T.BaseTypeDecl.entity):
        actual_type = Var(Entity)
        return Or(
            And(actual_type == Self.universal_int_type,
                expected_type.is_int_type),

            And(expected_type == Self.universal_int_type,
                actual_type.is_int_type),

            And(actual_type == Self.universal_real_type,
                expected_type.is_real_type),

            And(expected_type == Self.universal_real_type,
                actual_type.is_real_type),

            And(Not(expected_type.is_null),
                Not(actual_type.is_null),
                Or(actual_type.canonical_type == expected_type.canonical_type,
                   actual_type.matching_access_type(expected_type)))
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_allocator_type(allocated_type=T.BaseTypeDecl.entity):
        return And(
            Entity.is_access_type,
            allocated_type.matching_formal_type(Entity.accessed_type)
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[(origin, No(T.AdaNode))], public=True)
    def canonical_type():
        """
        Return the canonical type declaration for this type declaration. For
        subtypes, it will return the base type declaration.
        """
        return Entity.canonical_part.as_entity

    @langkit_property(memoized=True, memoize_in_populate=True,
                      ignore_warn_on_node=True)
    def classwide_type_node():
        return T.ClasswideTypeDecl.new(name=Self.name)

    @langkit_property(public=True, return_type=T.BaseTypeDecl.entity,
                      memoized=True)
    def previous_part(go_to_incomplete=BoolType):
        """
        Returns the previous part for this type decl.
        """
        return Self.name.then(
            lambda type_name:

            Self.children_env.get(
                type_name.name_symbol, from_node=Self
            ).then(lambda previous_parts: previous_parts.find(lambda pp: Or(
                And(Entity.is_in_private_part,
                    pp.cast(T.BaseTypeDecl)._.is_private),
                And(go_to_incomplete,
                    pp.is_a(T.IncompleteTypeDecl)),
            )).cast(T.BaseTypeDecl))
        )

    @langkit_property(return_type=T.BaseTypeDecl, ignore_warn_on_node=True)
    def canonical_part():
        return Entity.previous_part(True).then(
            lambda pp: pp.canonical_part,
            default_val=Self,
        )

    is_private = Property(False)

    discriminants_list = AbstractProperty(
        type=BaseFormalParamDecl.entity.array
    )


@synthetic
class ClasswideTypeDecl(BaseTypeDecl):
    """
    Synthetic node (not parsed, generated from a property call). Refers to the
    classwide type for a given tagged type. The aim is that those be mostly
    equivalent to their non-classwide type, except for some resolution rules.
    """
    # We don't want to add the classwide type to the environment
    env_spec = EnvSpec()

    typedecl = Property(Self.parent.cast(BaseTypeDecl).as_entity)

    is_classwide = Property(True)

    is_tagged_type = Property(True)
    base_type = Property(Entity.typedecl.base_type)
    base_interfaces = Property(Entity.typedecl.base_interfaces)
    record_def = Property(Entity.typedecl.record_def)
    classwide_type = Property(Entity)
    is_iterable_type = Property(Entity.typedecl.is_iterable_type)
    iterable_comp_type = Property(Entity.typedecl.iterable_comp_type)
    defining_env = Property(Entity.typedecl.defining_env)
    node_aspects = Property(Entity.typedecl.node_aspects)
    is_private = Property(Entity.typedecl.is_private)
    is_in_private_part = Property(Entity.typedecl.is_in_private_part)

    discriminants_list = Property(Entity.typedecl.discriminants_list)

    @langkit_property(public=True, return_type=T.BaseTypeDecl.entity,
                      memoized=True)
    def previous_part(go_to_incomplete=BoolType):
        return Entity.typedecl.previous_part(go_to_incomplete).then(
            lambda pp: pp.classwide_type
        )

    canonical_type = Property(Entity.typedecl.canonical_type)


class TypeDecl(BaseTypeDecl):
    discriminants = Field(type=T.DiscriminantPart)
    type_def = Field(type=T.TypeDef)
    aspects = Field(type=T.AspectSpec)
    prims_env = UserField(type=T.LexicalEnvType, public=False)

    is_iterable_type = Property(
        # TODO: Need to implement on:
        #
        #   * Spark iterable types (Iterable aspect).
        Or(
            Entity.is_array,
            Not(Entity.get_aspect('Iterator_Element').is_null),
            Not(Entity.get_aspect('Iterable').is_null),
            Entity.type_def.match(
                lambda dtd=T.DerivedTypeDef:
                dtd.base_type.then(lambda bt: bt.is_iterable_type),
                lambda _: False
            ),
            Entity.previous_part(False).then(lambda pp: pp.is_iterable_type)
        ),
        doc="""
        Whether Self is a type that is iterable in a for .. of loop
        """,
        dynamic_vars=[origin]
    )

    @langkit_property()
    def iterable_comp_type():
        ie = Var(Entity.get_aspect('Iterator_Element'))
        it = Var(Entity.get_aspect('Iterable'))

        return Cond(
            Entity.is_array, Entity.comp_type,
            Not(ie.is_null), ie.expr.cast(T.Name).name_designated_type,

            Not(it.is_null),
            it.expr.cast(T.Aggregate).assocs.unpacked_params.find(
                lambda sa: sa.name.name_symbol == 'Element'
            ).assoc.expr.as_entity.referenced_decl.expr_type,

            Entity.type_def.match(
                lambda dtd=T.DerivedTypeDef:
                dtd.base_type.then(lambda bt: bt.iterable_comp_type),
                lambda _: No(T.BaseTypeDecl.entity)
            ),
        )._or(Entity.previous_part(False)
              .then(lambda pp: pp.iterable_comp_type))

    node_aspects = Property(Entity.aspects)

    @langkit_property()
    def discrete_range():
        return Entity.type_def.discrete_range

    @langkit_property()
    def discriminants_list():
        base_type = Var(Entity.base_type)
        self_discs = Var(Entity.discriminants.then(
            lambda d: d.abstract_formal_params)
        )

        return Cond(
            Entity.is_access_type,
            origin.bind(Self, Entity.accessed_type.discriminants_list),

            self_discs.length > 0, self_discs,
            Not(base_type.is_null), Entity.base_type.discriminants_list,
            No(T.BaseFormalParamDecl.entity.array)
        )

    @langkit_property(external=True, uses_entity_info=False, uses_envs=True,
                      return_type=LexicalEnvType)
    def primitives():
        pass

    array_ndims = Property(Entity.type_def.array_ndims)

    is_record_type = Property(Entity.type_def.is_record_type)
    is_real_type = Property(Entity.type_def.is_real_type)
    is_int_type = Property(Entity.type_def.is_int_type)
    is_access_type = Property(Self.as_bare_entity.type_def.is_access_type)

    @langkit_property()
    def accessed_type():
        imp_deref = Var(Entity.get_imp_deref)

        return If(
            imp_deref.is_null,
            Entity.type_def.accessed_type,

            # Here, we need to call defining_env on TypeDef, in order to not
            # recurse for ever (accessed_type is called by defining_env).
            Entity.type_def.defining_env.get_first(
                imp_deref.expr.cast(T.Name).name_symbol
            )

            # We cast to BaseFormalParamDecl. Following Ada's legality rule,
            # you need to implicit deref on a discriminant, but I see no reason
            # to enforce that here.
            .cast_or_raise(T.BaseFormalParamDecl).type.accessed_type
        )

    access_def = Property(Entity.type_def.match(
        lambda ad=T.AccessDef: ad,
        lambda dtd=T.DerivedTypeDef: dtd.base_type.access_def,
        lambda _: No(T.AccessDef.entity)
    ))

    is_tagged_type = Property(Entity.type_def.is_tagged_type)
    base_type = Property(Entity.type_def.base_type)
    base_interfaces = Property(Entity.type_def.base_interfaces)
    is_char_type = Property(Entity.type_def.is_char_type)
    is_enum_type = Property(Entity.type_def.is_enum_type)
    is_private = Property(
        Self.type_def.is_a(T.PrivateTypeDef)
        | Self.type_def.cast(T.DerivedTypeDef).then(
            lambda dtd: dtd.has_with_private.as_bool
        )
    )

    array_def = Property(Entity.type_def.match(
        lambda atd=T.ArrayTypeDef: atd,
        lambda dtd=T.DerivedTypeDef: dtd.base_type.array_def,
        lambda _: No(T.ArrayTypeDef.entity)
    ))

    @langkit_property()
    def defining_env():
        imp_deref = Var(Entity.get_imp_deref)

        # Evaluating in type env, because the defining environment of a type
        # is always its own.
        self_env = Var(
            env.bind(Entity.children_env, Entity.type_def.defining_env)
        )

        return Cond(
            Not(imp_deref.is_null),
            Array([self_env, Entity.accessed_type.defining_env]).env_group(),

            Entity.has_ud_indexing,
            Entity.constant_indexing_fns
            .concat(Entity.variable_indexing_fns)
            .map(lambda fn: fn.defining_env)
            .concat([self_env]).env_group(),

            self_env,
        )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env(),
        handle_children(),
        reference(
            Self.cast(AdaNode).singleton,
            through=T.TypeDecl.parent_primitives_env,
            transitive=True,
            dest_env=Self.node_env,
            cond=Self.type_def.is_a(T.DerivedTypeDef)
        ),
    )

    record_def = Property(
        Self.type_def.match(
            lambda r=T.RecordTypeDef: r.record_def,
            lambda d=T.DerivedTypeDef: d.record_extension,
            lambda _: No(T.BaseRecordDef)
        ).as_entity
    )

    xref_entry_point = Property(True)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        # TODO: Handle discriminants
        return Entity.type_def.sub_equation

    is_discrete_type = Property(Entity.type_def.is_discrete_type)

    @langkit_property(return_type=LexicalEnvType.array)
    def own_primitives_envs():
        # If self has a previous part, it might have primitives too
        return Entity.previous_part(False).cast(T.TypeDecl).then(
            lambda pp: Array([Self.primitives, pp.primitives]),
            default_val=Self.primitives.singleton
        )

    @langkit_property(return_type=LexicalEnvType.array)
    def primitives_envs(include_self=(BoolType, False)):
        return Entity.base_types.mapcat(lambda t: t.cast(T.TypeDecl).then(
            lambda bt: bt.own_primitives_envs.concat(
                bt.primitives_envs
            )
        )).concat(
            If(include_self,
               Entity.own_primitives_envs, No(LexicalEnvType.array))
        )

    @langkit_property(memoized=True)
    def parent_primitives_env():
        return Self.type_def.match(
            lambda _=T.DerivedTypeDef: Entity.primitives_envs.env_group(
                with_md=Metadata.new(
                    dottable_subp=False,
                    primitive=No(T.AdaNode),
                    primitive_real_type=Self,
                    access_entity=False,
                )
            ),
            lambda _: Self.empty_env
        )

    @langkit_property(memoized=True)
    def primitives_env():
        return Entity.primitives_envs(include_self=True).env_group()

    get_imp_deref = Property(Entity.get_aspect('Implicit_Dereference'))

    has_ud_indexing = Property(
        Not(Entity.get_aspect('Constant_Indexing').is_null)
        | Not(Entity.get_aspect('Variable_Indexing').is_null)
    )

    @langkit_property()
    def constant_indexing_fns():
        return Entity.get_aspect('Constant_Indexing').then(
            lambda a:
            a.expr.cast_or_raise(T.Name).all_env_elements(seq=False).filtermap(
                lambda e: e.cast(T.BasicDecl),
                lambda env_el:
                env_el.cast_or_raise(T.BasicDecl).subp_spec_or_null.then(
                    lambda ss:
                    origin.bind(
                        Self,
                        ss.unpacked_formal_params.at(0)
                        ._.spec.type.matching_formal_type(Entity)
                    )
                )
            )
        )

    @langkit_property()
    def variable_indexing_fns():
        return origin.bind(Self, Entity.get_aspect('Variable_Indexing').then(
            lambda a:
            a.expr.cast_or_raise(T.Name).all_env_elements(seq=False).filtermap(
                lambda e: e.cast(T.BasicDecl),
                lambda env_el:
                env_el.cast_or_raise(T.BasicDecl).subp_spec_or_null.then(
                    lambda ss:
                    ss.unpacked_formal_params.at(0)
                    ._.spec.type.matching_formal_type(Entity)
                    & ss.return_type.is_implicit_deref
                )
            )
        ))


class AnonymousTypeDecl(TypeDecl):

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def access_def_matches(other=BaseTypeDecl.entity):
        """
        Returns whether:
        1. Self and other are both access types.
        2. Their access def matches structurally.
        """

        # If the anonymous type is an access type definition, then verify if
        #  the accessed type corresponds to other's accessed type.
        return Entity.type_def.cast(AccessDef)._.match(
            lambda asp=AccessToSubpDef:
            other.access_def.cast(AccessToSubpDef).then(
                lambda sa: sa.subp_spec.match_signature(
                    asp.subp_spec, False
                )
            ),
            lambda ad:
            ad.accessed_type.then(
                lambda ast: ast.matching_type(other.accessed_type)
            )
        )

    xref_entry_point = Property(False)

    # We don't want to add anonymous type declarations to the lexical
    # environments, so we reset the env spec.
    env_spec = EnvSpec()


@synthetic
class SynthAnonymousTypeDecl(AnonymousTypeDecl):
    """
    Synthetic anonymous type decl. Used to generate anonymous access types.
    """
    pass


class EnumTypeDef(TypeDef):
    enum_literals = Field(type=T.EnumLiteralDecl.list)

    is_char_type = Property(Self.enum_literals.any(
        lambda lit: lit.name.name.is_a(T.CharLiteral)
    ))

    is_enum_type = Property(True)

    xref_equation = Property(LogicTrue())


class FloatingPointDef(RealTypeDef):
    num_digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)


class OrdinaryFixedPointDef(RealTypeDef):
    delta = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)


class DecimalFixedPointDef(RealTypeDef):
    delta = Field(type=T.Expr)
    digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)


@abstract
class BaseAssoc(AdaNode):
    """
    Abstract class for a key -> value association, where the value is an
    expression.
    """
    assoc_expr = AbstractProperty(
        type=T.Expr.entity, public=True,
        doc="Returns the expression side of this assoc node."
    )


@abstract
class Constraint(AdaNode):
    pass


class RangeConstraint(Constraint):
    range = Field(type=T.RangeSpec)

    xref_equation = Property(Entity.range.sub_equation)


class DigitsConstraint(Constraint):
    digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)

    xref_equation = Property(
        Entity.digits.sub_equation & Entity.range.sub_equation
    )


class DeltaConstraint(Constraint):
    digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)

    xref_equation = Property(
        Entity.digits.sub_equation & Entity.range.sub_equation
    )


class IndexConstraint(Constraint):
    constraints = Field(type=T.ConstraintList)

    xref_equation = Property(
        Entity.constraints.logic_all(lambda c: c.xref_equation)
    )


class DiscriminantConstraint(Constraint):
    constraints = Field(type=T.AssocList)

    @langkit_property()
    def xref_equation():
        typ = Var(Self.parent.cast_or_raise(T.SubtypeIndication)
                  .as_entity.designated_type)

        return If(
            # Due to ambiguities in the grammar, this can actually be parsed as
            # a DiscriminantConstraint but be an index constraint.
            typ.is_array,

            # Index constraints imply no overloading
            Entity.constraints.logic_all(
                lambda c: c.expr.as_entity.sub_equation
            ),

            # Regular discriminant constraint case
            Self.match_formals(
                typ.discriminants_list, Self.constraints, False
            ).logic_all(
                lambda pm: pm.actual.assoc.expr.as_entity.xref_equation
                & TypeBind(
                    pm.actual.assoc.expr.type_var, pm.formal.spec.type
                )
            )
        )


@abstract
@has_abstract_list
class BasicAssoc(AdaNode):
    expr = AbstractProperty(type=T.Expr, ignore_warn_on_node=True)
    names = AbstractProperty(type=T.AdaNode.array)


class DiscriminantAssoc(BasicAssoc):
    ids = Field(type=T.DiscriminantChoiceList)
    discr_expr = Field(type=T.Expr)

    expr = Property(Self.discr_expr)
    names = Property(Self.ids.map(lambda i: i.cast(T.AdaNode)))


class DerivedTypeDef(TypeDef):
    has_abstract = Field(type=Abstract)
    has_limited = Field(type=Limited)
    has_synchronized = Field(type=Synchronized)
    subtype_indication = Field(type=T.SubtypeIndication)
    interfaces = Field(type=T.ParentList)
    record_extension = Field(type=T.BaseRecordDef)
    has_with_private = Field(type=WithPrivate)

    array_ndims = Property(Entity.base_type.array_ndims)

    # TODO: this origin bind is erroneous
    base_type = Property(
        origin.bind(Self, Entity.subtype_indication.designated_type)
    )

    base_interfaces = Property(
        Entity.interfaces.map(lambda i: i.name_designated_type)
    )

    is_real_type = Property(Entity.base_type.is_real_type)
    is_int_type = Property(Entity.base_type.is_int_type)
    is_access_type = Property(Self.as_bare_entity.base_type.is_access_type)
    is_char_type = Property(Entity.base_type.is_char_type)
    accessed_type = Property(Entity.base_type.accessed_type)
    is_tagged_type = Property(
        Not(Entity.record_extension.is_null)
        | Not(Entity.has_with_private.is_null)
    )

    is_enum_type = Property(Entity.base_type.is_enum_type)
    is_record_type = Property(
        Entity.is_tagged_type | Entity.base_type.is_record_type
    )

    defining_env = Property(
        Entity.base_types.map(
            lambda bt: bt._.defining_env
        ).concat(
            Array([Entity.children_env, Entity.previous_part_env])
        ).env_group()
    )

    @langkit_property(return_type=EquationType)
    def xref_equation():
        # We want to make discriminants accessible, so need to evaluate this in
        # Self's children_env.
        return env.bind(Self.children_env, (
            Entity.subtype_indication.xref_equation
            & Entity.interfaces.logic_all(lambda ifc: ifc.xref_equation)
        ))

    @langkit_property()
    def discrete_range():
        return Entity.subtype_indication.discrete_range


class PrivateTypeDef(TypeDef):
    has_abstract = Field(type=Abstract)
    has_tagged = Field(type=Tagged)
    has_limited = Field(type=Limited)

    is_tagged_type = Property(Self.has_tagged.as_bool)

    xref_equation = Property(LogicTrue())

    defining_env = Property(Entity.children_env, type=LexicalEnvType)


class SignedIntTypeDef(TypeDef):
    range = Field(type=T.RangeSpec)
    is_int_type = Property(True)

    xref_equation = Property(
        # We consider that the range expression is of the type we're defining.
        # Not sure how good of an idea this is but works in most cases.
        TypeBind(Entity.range.range.type_var, Entity.containing_type)
        & Entity.range.xref_equation
    )

    @langkit_property()
    def discrete_range():
        return Entity.range.range.discrete_range


class ModIntTypeDef(TypeDef):
    expr = Field(type=T.Expr)
    is_int_type = Property(True)

    xref_equation = Property(Entity.expr.sub_equation)

    @langkit_property()
    def discrete_range():
        return DiscreteRange.new(low_bound=BigInteger(0),
                                 high_bound=Self.expr.eval_as_int)


@abstract
class ArrayIndices(AdaNode):
    ndims = AbstractProperty(
        type=LongType,
        doc="""Number of dimensions described in this node."""
    )

    @langkit_property(return_type=EquationType, dynamic_vars=[origin],
                      kind=AbstractKind.abstract)
    def constrain_index_expr(index_expr=T.Expr, dim=LongType):
        """
        Add a constraint on an expression passed as the index of an array
        access expression.

        For example::

            type A is array (Integer range 1 .. 10) of Integer;

            A_Inst : A;

            A_Inst (2);
            --      ^ Will add constraint on lit that it needs to be of type
            --      Integer.
        """
        pass

    @langkit_property(dynamic_vars=[origin], kind=AbstractKind.abstract,
                      return_type=T.BaseTypeDecl.entity)
    def index_type(dim=LongType):
        pass


class UnconstrainedArrayIndices(ArrayIndices):
    types = Field(type=T.UnconstrainedArrayIndex.list)
    ndims = Property(Self.types.length)

    @langkit_property(return_type=EquationType)
    def constrain_index_expr(index_expr=T.Expr, dim=LongType):
        return TypeBind(index_expr.type_var, Entity.index_type(dim))

    @langkit_property()
    def index_type(dim=LongType):
        return Entity.types.at(dim).designated_type


class ConstrainedArrayIndices(ArrayIndices):
    list = Field(type=T.ConstraintList)

    ndims = Property(Self.list.length)

    @langkit_property(return_type=EquationType)
    def constrain_index_expr(index_expr=T.Expr, dim=LongType):
        return TypeBind(index_expr.type_var, Entity.index_type(dim))

    @langkit_property()
    def xref_equation():
        return Entity.list.logic_all(
            lambda index:
            index.sub_equation
            & index.cast(T.Expr).then(
                lambda expr:
                TypeBind(expr.type_var, Self.int_type)
                | Predicate(BaseTypeDecl.is_discrete_type, expr.type_var),
                default_val=LogicTrue()
            )
        )

    @langkit_property(dynamic_vars=[origin])
    def index_type(dim=LongType):
        # We might need to solve self's equation to get the index type
        ignore(Var(Self.parents.find(
            lambda p: p.xref_entry_point).as_entity.resolve_names
        ))

        return Entity.list.at(dim)._.match(
            lambda st=T.SubtypeIndication: st.designated_type,
            lambda e=T.Expr: e.type_val.cast(T.BaseTypeDecl.entity),
            lambda _: No(T.BaseTypeDecl.entity)
        )


class ComponentDef(AdaNode):
    has_aliased = Field(type=Aliased)
    has_constant = Field(type=Constant)
    type_expr = Field(type=T.TypeExpr)

    @langkit_property()
    def xref_equation():
        return Entity.type_expr.sub_equation


class ArrayTypeDef(TypeDef):
    indices = Field(type=T.ArrayIndices)
    component_type = Field(type=T.ComponentDef)

    @langkit_property(dynamic_vars=[origin])
    def comp_type():
        """Returns the type stored as a component in the array."""
        return (Entity.component_type.type_expr.designated_type)

    @langkit_property(dynamic_vars=[origin])
    def index_type(dim=LongType):
        return Entity.indices.index_type(dim)

    array_ndims = Property(Self.indices.ndims)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.indices.sub_equation,
            Entity.component_type.sub_equation
        )

    defining_env = Property(Entity.comp_type.defining_env)

    xref_entry_point = Property(True)


class InterfaceKind(EnumNode):
    alternatives = ["limited", "task", "protected", "synchronized"]


class InterfaceTypeDef(TypeDef):
    interface_kind = Field(type=InterfaceKind)
    interfaces = Field(type=T.ParentList)

    is_tagged_type = Property(True)

    base_interfaces = Property(
        Entity.interfaces.map(lambda i: i.name_designated_type)
    )

    defining_env = Property(Entity.children_env)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return Entity.interfaces.logic_all(lambda ifc: ifc.xref_equation)


class SubtypeDecl(BaseTypeDecl):
    subtype = Field(type=T.SubtypeIndication)
    aspects = Field(type=T.AspectSpec)

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def from_type_bound():
        # TODO: This is a hack, to avoid making all of the predicates on types
        # take an origin. But ultimately, for semantic correctness, it will be
        # necessary to remove this, and migrate every property using it to
        # having a dynamic origin parameter.
        return origin.bind(Self, Entity.from_type)

    @langkit_property(return_type=T.BaseTypeDecl.entity, dynamic_vars=[origin])
    def from_type():
        return Entity.subtype.designated_type.match(
            lambda st=T.SubtypeDecl: st.from_type,
            lambda t: t
        )

    array_ndims = Property(Entity.subtype.array_ndims)
    defining_env = Property(Entity.subtype.defining_env)

    canonical_type = Property(Entity.from_type.canonical_type)
    record_def = Property(Entity.from_type.record_def)
    accessed_type = Property(Entity.from_type.accessed_type)
    is_int_type = Property(Entity.from_type.is_int_type)
    is_discrete_type = Property(Entity.from_type.is_discrete_type)
    is_real_type = Property(Entity.from_type_bound.is_real_type)
    is_enum_type = Property(Entity.from_type_bound.is_enum_type)
    is_access_type = Property(Entity.from_type_bound.is_access_type)
    access_def = Property(Entity.from_type_bound.access_def)
    is_char_type = Property(Entity.from_type_bound.is_char_type)
    is_tagged_type = Property(Entity.from_type_bound.is_tagged_type)
    base_type = Property(Entity.from_type_bound.base_type)
    array_def = Property(Entity.from_type_bound.array_def)
    record_def = Property(Entity.from_type_bound.record_def)
    is_classwide = Property(Entity.from_type_bound.is_classwide)
    discriminants_list = Property(Entity.from_type_bound.discriminants_list)
    is_iterable_type = Property(Entity.from_type.is_iterable_type)
    iterable_comp_type = Property(Entity.from_type.iterable_comp_type)
    is_record_type = Property(Entity.from_type_bound.is_record_type)
    is_private = Property(Entity.from_type_bound.is_private)

    @langkit_property()
    def discrete_range():
        return Entity.subtype.discrete_range

    @langkit_property()
    def xref_equation():
        return Entity.subtype.sub_equation

    xref_entry_point = Property(True)


class TaskDef(AdaNode):
    interfaces = Field(type=T.ParentList)
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)
    end_name = Field(type=T.EndName)


class ProtectedDef(AdaNode):
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)
    end_name = Field(type=T.EndName)


class TaskTypeDecl(BaseTypeDecl):
    discriminants = Field(type=T.DiscriminantPart)
    aspects = Field(type=T.AspectSpec)
    definition = Field(type=T.TaskDef)
    is_task_type = Property(True)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env()
    )

    defining_env = Property(Entity.children_env)

    discriminants_list = Property(Entity.discriminants.abstract_formal_params)


class SingleTaskTypeDecl(TaskTypeDecl):
    env_spec = EnvSpec(
        # In this case, we don't want to add this type to the env, because it's
        # the single task that contains this type decl that will be added to
        # the env. So we don't call the inherited env spec.
        add_env()
    )


class ProtectedTypeDecl(BaseTypeDecl):
    discriminants = Field(type=T.DiscriminantPart)
    aspects = Field(type=T.AspectSpec)
    interfaces = Field(type=T.ParentList)
    definition = Field(type=T.ProtectedDef)

    discriminants_list = Property(Entity.discriminants.abstract_formal_params)

    defining_env = Property(Entity.children_env)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env()
    )


@abstract
class AccessDef(TypeDef):
    has_not_null = Field(type=NotNull)

    is_access_type = Property(True)
    defining_env = Property(Entity.accessed_type.defining_env)


class AccessToSubpDef(AccessDef):
    has_protected = Field(type=Protected)
    subp_spec = Field(type=T.SubpSpec)

    xref_equation = Property(LogicTrue())

    accessed_type = Property(Entity.subp_spec.return_type)


@abstract
class BaseTypeAccessDef(AccessDef):
    pass


class TypeAccessDef(BaseTypeAccessDef):
    has_all = Field(type=All)
    has_constant = Field(type=Constant)
    subtype_indication = Field(type=T.SubtypeIndication)

    accessed_type = Property(Entity.subtype_indication.designated_type)
    xref_equation = Property(Entity.subtype_indication.xref_equation)


@synthetic
class AnonymousTypeAccessDef(BaseTypeAccessDef):
    """
    Synthetic type access, that will directly reference a type decl. It is used
    to generate synthetic anonymous access types.
    """
    type_decl = Field(type=T.BaseTypeDecl)

    accessed_type = Property(Entity.type_decl)


class FormalDiscreteTypeDef(TypeDef):
    xref_equation = Property(LogicTrue())

    is_discrete_type = Property(True)


class NullComponentDecl(AdaNode):
    pass


class WithClause(AdaNode):
    has_limited = Field(type=Limited)
    has_private = Field(type=Private)
    packages = Field(type=T.Name.list)

    env_spec = EnvSpec(do(Self.packages.map(
        lambda package_name:
        # First fetch the spec
        package_name.referenced_unit(UnitSpecification)
        # If no spec exists, maybe it is a library level subprogram with
        # just a body, so fetch the body.
        .root._or(package_name.referenced_unit(UnitBody).root)
    )))

    xref_entry_point = Property(True)
    xref_equation = Property(
        Entity.packages.logic_all(lambda p: p.xref_no_overloading)
    )


@abstract
class UseClause(AdaNode):
    xref_entry_point = Property(True)


class UsePackageClause(UseClause):
    packages = Field(type=T.Name.list)

    env_spec = EnvSpec(reference(
        Self.packages.map(lambda n: n.cast(AdaNode)),
        T.Name.use_package_name_designated_env,

        # We don't want to process use clauses that appear in the top-level
        # scope here, as they apply to the library item's environment,
        # which is not processed at this point yet. See CompilationUnit's
        # ref_env_nodes.
        cond=Not(Self.parent.parent.is_a(T.CompilationUnit))
    ))

    @langkit_property(return_type=LexicalEnvType.array)
    def designated_envs():
        """
        Return the array of designated envs corresponding to each package name.

        It is very important for this property to be memoized, as it is used a
        lot during lexical environment lookups.
        """
        return Self.packages.map(
            lambda n:
            env.bind(Self.node_env,
                     origin.bind(n, n.as_bare_entity.designated_env))
        )

    xref_equation = Property(
        Entity.packages.logic_all(lambda p: p.xref_no_overloading)
    )


class UseTypeClause(UseClause):
    has_all = Field(type=All)
    types = Field(type=T.Name.list)

    env_spec = EnvSpec(
        handle_children(),
        reference(
            Self.types.map(lambda n: n.cast(AdaNode)),
            T.Name.name_designated_type_env,
            dest_env=Self.node_env,
            # We don't want to process use clauses that appear in the top-level
            # scope here, as they apply to the library item's environment,
            # which is not processed at this point yet. See CompilationUnit's
            # ref_env_nodes.
            cond=Not(Self.parent.parent.is_a(T.CompilationUnit))
        ),
    )

    xref_equation = Property(
        Entity.types.logic_all(lambda p: p.xref_no_overloading)
    )


@abstract
class TypeExpr(AdaNode):
    """
    A type expression is an abstract node that embodies the concept of a
    reference to a type.

    Since Ada has both subtype_indications and anonymous (inline) type
    declarations, a type expression contains one or the other.
    """

    array_ndims = Property(
        origin.bind(Self, Entity.designated_type.array_ndims)
    )

    type_name = Property(
        Entity.cast(T.SubtypeIndication).then(lambda sti: sti.name),
        doc="Return the name node for this type expression, "
        "if applicable, else null",
        public=True
    )

    @langkit_property(dynamic_vars=[origin])
    def accessed_type():
        return Entity.designated_type._.accessed_type

    @langkit_property(dynamic_vars=[origin])
    def defining_env():
        return Entity.designated_type.defining_env

    designated_type = AbstractProperty(
        type=BaseTypeDecl.entity,
        dynamic_vars=[origin],
        doc="""
        Return the type designated by this type expression.
        """
    )

    designated_type_decl = Property(
        origin.bind(Self, Entity.designated_type),
        public=True,
        doc="""
        Returns the type declaration designated by this type expression.
        """
    )

    @langkit_property(return_type=BaseTypeDecl.entity, public=True)
    def designated_type_decl_from(origin_node=T.AdaNode.entity):
        """
        Return the type declaration designated by this type expression as
        viewed from the node given by origin_node.
        """
        return origin.bind(origin_node.el, Entity.designated_type)

    @langkit_property(return_type=BaseTypeDecl.entity, dynamic_vars=[origin])
    def element_type():
        """
        If self is an anonymous access, return the accessed type. Otherwise,
        return the designated type.
        """
        d = Entity.designated_type
        return If(
            d.cast(AnonymousTypeDecl)._.type_def.cast(AccessDef).is_null,
            d,
            Entity.accessed_type,
        )

    @langkit_property(return_type=BaseTypeDecl.entity, dynamic_vars=[origin])
    def canonical_type():
        return Entity.designated_type._.canonical_type


class AnonymousType(TypeExpr):
    """
    Container for inline anonymous array and access types declarations.
    """
    type_decl = Field(type=T.AnonymousTypeDecl)

    designated_type = Property(Entity.type_decl)
    xref_equation = Property(Entity.type_decl.sub_equation)


class SubtypeIndication(TypeExpr):
    has_not_null = Field(type=NotNull)
    name = Field(type=T.Name)
    constraint = Field(type=T.Constraint)

    # The name for this type has to be evaluated in the context of the
    # SubtypeIndication node itself: we don't want to use whatever lexical
    # environment the caller is using. However we need to inherit the
    # visibility (origin node) of the caller.
    designated_type = Property(
        env.bind(Entity.node_env,
                 Entity.name.designated_type_impl)
    )

    @langkit_property()
    def xref_equation():
        # Called by allocator.xref_equation, since the suffix can be either a
        # qual expr or a subtype indication.
        return And(
            Entity.name.subtype_indication_equation,
            Entity.constraint.then(
                lambda c: c.sub_equation, default_val=LogicTrue()
            )
        )

    @langkit_property()
    def discrete_range():
        rc = Var(Entity.constraint.cast_or_raise(RangeConstraint))
        return rc.range.range.discrete_range


class ConstrainedSubtypeIndication(SubtypeIndication):
    pass


class DiscreteSubtypeIndication(SubtypeIndication):
    pass


class Mode(EnumNode):
    alternatives = ["in", "out", "in_out", "default"]


class ParamSpec(BaseFormalParamDecl):
    ids = Field(type=T.DefiningName.list)
    has_aliased = Field(type=Aliased)
    mode = Field(type=Mode)
    type_expr = Field(type=T.TypeExpr)
    default_expr = Field(type=T.Expr)

    is_mandatory = Property(Self.default_expr.is_null)
    defining_names = Property(Self.ids.map(lambda id: id.as_entity))

    env_spec = EnvSpec(
        add_to_env(env_mappings(Self.ids, Self))
    )

    type_expression = Property(Entity.type_expr)

    @langkit_property()
    def defining_env():
        return Entity.type_expr.defining_env

    @langkit_property()
    def xref_equation():
        typ = Var(Entity.expr_type)
        return (
            Entity.type_expr.sub_equation

            & Entity.default_expr.then(
                lambda de: de.sub_equation
                & Bind(de.type_var, typ,
                       eq_prop=BaseTypeDecl.matching_assign_type),
                default_val=LogicTrue()
            )
        )

    xref_entry_point = Property(True)

    @langkit_property(return_type=T.DefiningName.entity)
    def decl_param(param=T.DefiningName.entity):
        """
        If self is a ParamSpec of a subprogram body, go fetch the equivalent
        spec in the subprogram decl.
        """
        return If(
            Entity.semantic_parent.is_a(T.BaseSubpBody),

            Entity.semantic_parent.cast_or_raise(T.BaseSubpBody)
            .decl_part_entity.then(
                lambda decl:
                decl.subp_spec_or_null(follow_generic=True)
                .unpacked_formal_params.find(
                    lambda sf: sf.name.name_symbol == param.name_symbol
                ).name.as_entity,
                default_val=param
            ),

            param
        )


class AspectSpec(AdaNode):
    aspect_assocs = Field(type=T.AspectAssoc.list)


class Overriding(EnumNode):
    alternatives = ["overriding", "not_overriding", "unspecified"]


@abstract
class BasicSubpDecl(BasicDecl):

    defining_names = Property(Entity.subp_decl_spec.name.as_entity.singleton)

    defining_env = Property(Entity.subp_decl_spec.defining_env)

    type_expression = Property(
        Entity.subp_decl_spec.returns, doc="""
        The expr type of a subprogram declaration is the return type of the
        subprogram if the subprogram is a function.
        """
    )

    @langkit_property()
    def constrain_prefix(prefix=T.Expr):
        return If(
            # If self is a dottable subprogram, then we want to constrain the
            # prefix so that it's type is the type of the first parameter of
            # self.
            Entity.info.md.dottable_subp,
            Bind(prefix.type_var,
                 Entity.subp_decl_spec
                 .unpacked_formal_params.at(0)._.spec.type,
                 eq_prop=BaseTypeDecl.matching_prefix_type),
            LogicTrue()
        )

    @langkit_property()
    def expr_type():

        return Entity.type_expression.then(lambda te: te.designated_type)

    subp_decl_spec = AbstractProperty(
        type=T.SubpSpec.entity, public=True,
        doc='Return the specification for this subprogram'
    )

    @langkit_property(public=True)
    def body_part():
        """
        Return the BaseSubpBody corresponding to this node.
        """
        return Entity.body_part_entity.cast(BaseSubpBody)

    env_spec = EnvSpec(
        # Call the env hook to parse eventual parent unit
        call_env_hook(Self),

        set_initial_env(
            env.bind(Self.initial_env, Entity.decl_scope)
        ),

        add_to_env_kv(
            Entity.name_symbol, Self,
            dest_env=env.bind(
                Self.initial_env,
                Self.as_bare_entity.subp_decl_spec.name.parent_scope
            )
        ),
        add_env(),
        ref_used_packages(),

        handle_children(),

        # Adding subp to the type's environment if the type is tagged and self
        # is a primitive of it.
        add_to_env(
            T.env_assoc.new(key=Entity.name_symbol, val=Self),
            dest_env=Self.as_bare_entity.subp_decl_spec
            .dottable_subp_of._.children_env,
            # We pass custom metadata, marking the entity as a dottable
            # subprogram.
            metadata=Metadata.new(dottable_subp=True,
                                  primitive=No(T.AdaNode),
                                  primitive_real_type=No(T.AdaNode),
                                  access_entity=False)
        ),

        # Adding subp to the primitives env if the subp is a primitive. TODO:
        # Ada allows a subprogram to be a primitive of several types. This is
        # not handled for the moment, due to the limitations of the current env
        # spec format. We could modify dest_env to take an array optionally,
        # but that's one more kludge to the pile.

        add_to_env(
            T.env_assoc.new(key=Entity.name_symbol, val=Self),
            dest_env=(Self.as_bare_entity.subp_decl_spec
                      .primitive_subp_of.cast(T.TypeDecl)._.primitives),
            metadata=Metadata.new(
                dottable_subp=False,
                primitive=(Self.as_bare_entity.subp_decl_spec
                           .primitive_subp_of.cast(T.AdaNode).el),
                primitive_real_type=No(T.AdaNode),
                access_entity=False
            )
        )
    )


@abstract
class ClassicSubpDecl(BasicSubpDecl):
    """
    This is an intermediate abstract class for subprogram declarations with a
    common structure: overriding indicator, subp_spec, aspects, <other fields>.
    """
    overriding = Field(type=Overriding)
    subp_spec = Field(type=T.SubpSpec)

    subp_decl_spec = Property(Entity.subp_spec)


class SubpDecl(ClassicSubpDecl):
    aspects = Field(type=T.AspectSpec)


class NullSubpDecl(ClassicSubpDecl):
    aspects = Field(type=T.AspectSpec)


class AbstractSubpDecl(ClassicSubpDecl):
    aspects = Field(type=T.AspectSpec)


class SubpRenamingDecl(ClassicSubpDecl):
    renames = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)

    xref_entry_point = Property(True)
    xref_equation = Property(Or(
        And(
            Entity.renames.renamed_object.xref_no_overloading(all_els=True),
            Predicate(BasicDecl.subp_decl_match_signature,
                      Entity.renames.renamed_object.ref_var,
                      Entity.cast(T.BasicDecl))
        ),
        # Operators might be built-in, so if we cannot find a reference, we'll
        # just abandon resolution...
        If(Entity.renames.renamed_object.is_operator_name,
           LogicTrue(), LogicFalse())
    ))


class Pragma(AdaNode):
    id = Field(type=T.Identifier)
    args = Field(type=T.BaseAssoc.list)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        return Cond(
            Entity.id.name_symbol == 'Assert',
            Let(lambda expr=Entity.args.at(0).assoc_expr:
                expr.sub_equation & bool_bind(expr.type_var)),

            Entity.id.name_symbol == 'Unreferenced',
            Entity.args.logic_all(
                lambda assoc:
                assoc.assoc_expr.cast_or_raise(T.Name).xref_no_overloading
            ),

            LogicTrue(),
        )


class PragmaArgumentAssoc(BaseAssoc):
    id = Field(type=T.Identifier)
    expr = Field(type=T.Expr)
    assoc_expr = Property(Entity.expr)


@abstract
class AspectClause(AdaNode):
    xref_entry_point = Property(True)
    xref_equation = Property(LogicTrue())


class EnumRepClause(AspectClause):
    type_name = Field(type=T.Name)
    aggregate = Field(type=T.BaseAggregate)


class AttributeDefClause(AspectClause):
    attribute_expr = Field(type=T.Name)
    expr = Field(type=T.Expr)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        attr = Var(Entity.attribute_expr.cast_or_raise(T.AttributeRef))
        rel_name = Var(attr.attribute.name_symbol)

        return Cond(
            rel_name.any_of('Read', 'Write', 'Input', 'Output'),
            Entity.expr.cast_or_raise(T.Name).xref_no_overloading
            & Predicate(
                BasicDecl.is_stream_subprogram_for_type,
                Entity.expr.cast(T.Name).ref_var,
                attr.prefix.name_designated_type,
                rel_name == 'Input',
            ),

            Entity.expr.sub_equation
        ) & attr.then(
            lambda ar: ar.prefix.sub_equation, default_val=LogicTrue()
        )


class ComponentClause(AdaNode):
    id = Field(type=T.Identifier)
    position = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)


class RecordRepClause(AspectClause):
    name = Field(type=T.Name)
    at_expr = Field(type=T.Expr)
    components = Field(type=T.ComponentClause.list)


class AtClause(AspectClause):
    name = Field(type=T.BaseId)
    expr = Field(type=T.Expr)


class SingleTaskDecl(BasicDecl):
    task_type = Field(type=T.SingleTaskTypeDecl)

    defining_names = Property(Entity.task_type.defining_names)
    expr_type = Property(Entity.task_type)

    env_spec = EnvSpec(add_to_env_kv(Self.name_symbol, Self))


class SingleProtectedDecl(BasicDecl):
    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    interfaces = Field(type=T.ParentList)
    definition = Field(type=T.ProtectedDef)

    defining_names = Property(Entity.name.singleton)

    defining_env = Property(Entity.children_env)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env()
    )


class AspectAssoc(AdaNode):
    id = Field(type=T.Name)
    expr = Field(type=T.Expr)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        target = Var(Self.parent.parent.parent)
        return Cond(
            # Iterable aspect
            Entity.id.name_symbol == 'Iterable',
            Entity.expr.cast(T.Aggregate).assocs.unpacked_params.logic_all(
                lambda sa:
                sa.assoc.expr.as_entity
                .cast_or_raise(T.Name).xref_no_overloading(sequential=False)
            ),

            # Contracts
            target.is_a(BasicSubpDecl, BaseSubpBody)
            & Entity.id.name_symbol.any_of(
                'Pre', 'Post', 'Model_Pre', 'Model_Post'
            ),

            Entity.expr.sub_equation
            & bool_bind(Self.expr.type_var),

            # Model_Of aspect on types
            target.is_a(T.BaseTypeDecl)
            & Entity.id.name_symbol.any_of('Model_Of'),
            Bind(Self.expr.cast_or_raise(T.Name).ref_var,
                 Entity.expr.cast_or_raise(T.Name).name_designated_type),

            # Model_Of aspect on subprograms
            target.is_a(T.BasicSubpDecl)
            & Entity.id.name_symbol.any_of('Model_Of'),
            Bind(
                Self.expr.cast_or_raise(T.Name).ref_var,
                Entity.expr.cast_or_raise(T.Name).all_env_elements.find(
                    lambda e:
                    e.is_a(T.BasicSubpDecl)
                    & e.cast(T.BasicSubpDecl).subp_decl_spec
                    .cast_or_raise(T.BaseSubpSpec).match_signature(
                        target.as_entity.cast_or_raise(T.BasicSubpDecl)
                        .subp_decl_spec,
                        False
                    )
                )
            ),

            LogicTrue()
        )


class NumberDecl(BasicDecl):
    ids = Field(type=T.DefiningName.list)
    expr = Field(type=T.Expr)

    defining_names = Property(Entity.ids.map(lambda id: id))

    env_spec = EnvSpec(add_to_env(env_mappings(Self.ids, Self)))

    @langkit_property(call_memoizable=True)
    def expr_type():
        p = Var(If(Self.expr.type_val.is_null,
                   Entity.expr.resolve_names,
                   True))

        typ = Var(If(p,
                     Self.expr.type_val.cast_or_raise(BaseTypeDecl.entity),
                     No(BaseTypeDecl.entity)))

        return If(typ.is_int_type,
                  Self.universal_int_type,
                  Self.universal_real_type).cast(BaseTypeDecl.entity)

    xref_entry_point = Property(True)

    xref_equation = Property(Entity.expr.sub_equation)


class ObjectDecl(BasicDecl):
    ids = Field(type=T.DefiningName.list)
    has_aliased = Field(type=Aliased)
    has_constant = Field(type=Constant)
    mode = Field(type=Mode)
    type_expr = Field(type=T.TypeExpr)
    default_expr = Field(type=T.Expr)
    renaming_clause = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)

    env_spec = EnvSpec(add_to_env(env_mappings(Self.ids, Self)))

    defining_names = Property(Entity.ids.map(lambda id: id))
    defining_env = Property(Entity.type_expr.defining_env)
    type_expression = Property(Entity.type_expr)

    @langkit_property()
    def xref_equation():
        typ = Var(Entity.expr_type)
        return (
            Entity.type_expr.sub_equation
            & Entity.default_expr.then(
                lambda de:
                de.sub_equation
                & Bind(de.el.type_var,
                       typ,
                       eq_prop=BaseTypeDecl.matching_assign_type),
                default_val=LogicTrue()
            )
            & Entity.renaming_clause.then(
                lambda rc:
                rc.renamed_object.sub_equation
                & Bind(rc.renamed_object.el.type_var, typ,
                       eq_prop=BaseTypeDecl.matching_assign_type),
                default_val=LogicTrue()
            )
        )

    @langkit_property(public=True)
    def public_part_decl():
        """
        If this object decl is the constant completion of an object decl in the
        public part, return the object decl from the public part.
        """

        return If(
            Entity.is_in_private_part & Self.has_constant.as_bool,
            Self.declarative_scope.parent
            .cast(T.BasePackageDecl).public_part.children_env
            .get_first(Self.name_symbol, recursive=False).cast(T.BasicDecl),
            No(T.BasicDecl.entity)
        )

    xref_entry_point = Property(True)


class ExtendedReturnStmtObjectDecl(ObjectDecl):
    pass


class DeclarativePart(AdaNode):
    annotations = Annotations(snaps=True)

    decls = Field(type=T.AdaNode.list)

    @langkit_property(memoized=True)
    def types_with_models():
        return Self.as_bare_entity.decls.filtermap(
            lambda d: d.cast(T.BaseTypeDecl),
            lambda d:
            d.is_a(BaseTypeDecl) & Not(d.get_aspect('Model_Of').is_null)
        )


class PrivatePart(DeclarativePart):
    env_spec = EnvSpec(
        add_to_env_kv('__privatepart', Self),
        add_env(transitive_parent=True)
    )


class PublicPart(DeclarativePart):
    pass


@abstract
class BasePackageDecl(BasicDecl):
    """
    Package declarations. Concrete instances of this class
    will be created in generic package declarations. Other non-generic
    package declarations will be instances of PackageDecl.

    The behavior is the same, the only difference is that BasePackageDecl
    and PackageDecl have different behavior regarding lexical environments.
    In the case of generic package declarations, we use BasePackageDecl
    which has no env_spec, and the environment behavior is handled by the
    GenericPackageDecl instance.
    """
    package_name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.package_name.singleton)
    defining_env = Property(Entity.children_env)

    @langkit_property(public=True)
    def body_part():
        """
        Return the PackageBody corresponding to this node.
        """
        return Entity.body_part_entity.cast(T.PackageBody)


class PackageDecl(BasePackageDecl):
    """
    Non-generic package declarations.
    """
    env_spec = child_unit(
        Entity.name_symbol, Entity.decl_scope,
        dest_env=env.bind(Self.parent.node_env, Entity.decl_scope(False))
    )


class ExceptionDecl(BasicDecl):
    """
    Exception declarations.
    """
    ids = Field(type=T.DefiningName.list)
    renames = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)
    defining_names = Property(Entity.ids.map(lambda id: id))

    env_spec = EnvSpec(add_to_env(env_mappings(Self.ids, Self)))


@abstract
class GenericInstantiation(BasicDecl):
    """
    Instantiations of generics.
    """

    inst_env = UserField(type=T.LexicalEnvType, public=False)

    @langkit_property(external=True, uses_entity_info=False, uses_envs=True,
                      return_type=LexicalEnvType)
    def instantiation_env():
        pass

    generic_entity_name = AbstractProperty(
        type=T.Name.entity, doc="""
        Return the name of the generic entity designated by this generic
        instantiation.
        """
    )

    generic_inst_params = AbstractProperty(
        type=T.AssocList.entity, doc="""
        Return the parameters of this generic instantiation
        """
    )

    is_any_formal = Property(
        Entity.generic_inst_params._.at(0)._.expr._.is_a(T.BoxExpr)
    )

    nonbound_generic_decl = Property(
        env.bind(
            Self.node_env,
            Self.as_bare_entity.generic_entity_name
            .all_env_elements(seq=True, seq_from=Self).at(0)
        )._.match(
            lambda b=Body: b.decl_part_entity,
            lambda rd=T.GenericRenamingDecl: rd.resolve,
            lambda d=BasicDecl: d,
            lambda _: No(T.GenericDecl.entity)
        )._.cast(T.GenericDecl),
        doc="""
        Return the formal package designated by the right hand part of this
        generic package instantiation.
        """
    )

    designated_generic_decl = AbstractProperty(
        type=T.BasicDecl.entity, public=True, doc="""
        Return the generic decl entity designated by this instantiation,
        containing the generic context. This is equivalent to the expanded
        generic unit in GNAT.
        """
    )

    xref_entry_point = Property(True)

    xref_equation = Property(
        Bind(Entity.generic_entity_name.ref_var,
             Entity.nonbound_generic_decl)
        & Entity.generic_entity_name.match(
            lambda dn=T.DottedName: dn.prefix.xref_no_overloading,
            lambda _: LogicTrue()
        ) & If(
            Entity.is_any_formal,
            LogicTrue(),

            Entity.designated_generic_decl.cast_or_raise(T.GenericDecl)
            ._.formal_part.match_param_list(
                Entity.generic_inst_params.el, False
            ).logic_all(lambda pm: Let(
                lambda actual_name=pm.actual.assoc.expr.as_entity.cast(T.Name):
                pm.formal.spec.cast(T.GenericFormal).decl.match(
                    lambda _=T.TypeDecl: actual_name.xref_no_overloading,

                    lambda subp_decl=T.FormalSubpDecl.entity:
                    Or(
                        actual_name.xref_no_overloading
                        & Predicate(BasicDecl.subp_decl_match_signature,
                                    actual_name.ref_var,
                                    subp_decl.cast(T.BasicDecl)),
                        LogicTrue()
                    ),

                    lambda obj_decl=T.ObjectDecl:
                    pm.actual.assoc.expr.as_entity.sub_equation
                    & TypeBind(pm.actual.assoc.expr.type_var,
                               obj_decl.expr_type),

                    lambda _: LogicTrue(),
                ) & pm.actual.name.then(
                    lambda n:
                    Bind(n.ref_var, pm.formal.name.as_bare_entity.basic_decl),
                    default_val=LogicTrue()
                )
            ))
        )
    )


class GenericSubpInstantiation(GenericInstantiation):
    overriding = Field(type=Overriding)
    kind = Field(type=T.SubpKind)
    subp_name = Field(type=T.DefiningName)
    generic_subp_name = Field(type=T.Name)
    params = Field(type=T.AssocList)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.subp_name.singleton)

    generic_entity_name = Property(Entity.generic_subp_name)
    generic_inst_params = Property(Entity.params)

    @langkit_property()
    def designated_subp():
        """
        Return the subprogram decl designated by this instantiation.
        """
        return Self.nonbound_generic_decl.then(
            lambda p: BasicSubpDecl.entity.new(
                el=p.el.cast(GenericSubpDecl).subp_decl,
                info=T.entity_info.new(
                    md=p.info.md,
                    rebindings=p.info.rebindings.append_rebinding(
                        p.el.children_env, Self.instantiation_env
                    )
                )
            ).cast(T.entity)
        )

    designated_generic_decl = Property(
        Entity.designated_subp.parent.cast(T.BasicDecl)
    )

    env_spec = EnvSpec(
        call_env_hook(Self),

        add_env(),
        ref_used_packages(),

        handle_children(),
        add_to_env(
            env.bind(
                Self.initial_env,
                Self.nonbound_generic_decl._.formal_part.match_param_list(
                    Self.params, False
                ).map(lambda pm: T.env_assoc.new(
                    key=pm.formal.name.name_symbol, val=pm.actual.assoc.expr
                ))
            ),
            dest_env=Self.instantiation_env,
            resolver=AdaNode.resolve_generic_actual,
        ),
        add_to_env_kv(
            Entity.name_symbol, Self,
            resolver=T.GenericSubpInstantiation.designated_subp,
            dest_env=Self.node_env
        ),
    )


class GenericPackageInstantiation(GenericInstantiation):
    name = Field(type=T.DefiningName)
    generic_pkg_name = Field(type=T.Name)
    params = Field(type=T.AssocList)
    aspects = Field(type=T.AspectSpec)

    generic_entity_name = Property(Entity.generic_pkg_name)
    generic_inst_params = Property(Entity.params)

    @langkit_property()
    def designated_package():
        return Self.nonbound_generic_decl.then(
            lambda p: BasePackageDecl.entity.new(
                el=p.el.cast(GenericPackageDecl).package_decl,
                info=T.entity_info.new(
                    md=p.info.md,

                    # Take the rebindings from the current context
                    rebindings=Entity.info.rebindings

                    # Append the rebindings from the decl
                    .concat_rebindings(p._.decl.info.rebindings)

                    # Append the rebindings for the current instantiation.
                    # NOTE: We use the formal env to create rebindings. There,
                    # we purposefully want the children env of the P node, with
                    # no rebindings associated, since the rebinding indication
                    # concerns the *naked* generic. Hence we use
                    # p.el.children_env.
                    .append_rebinding(p.el.children_env,
                                      Self.instantiation_env)
                )
            )
        )

    designated_generic_decl = Property(
        Entity.designated_package.parent.cast(T.BasicDecl)
    )

    @langkit_property(return_type=LexicalEnvType)
    def defining_env():
        dp = Var(Entity.designated_package)
        return If(
            Self.is_formal_pkg,
            Array([dp.children_env, dp.parent.children_env]).env_group(),
            dp.children_env
        )

    defining_names = Property(Entity.name.singleton)

    is_formal_pkg = Property(Self.parent.is_a(T.GenericFormalPackage))

    env_spec = EnvSpec(
        call_env_hook(Self),

        set_initial_env(env.bind(
            Self.initial_env,
            If(Self.is_formal_pkg, Self.initial_env, Entity.decl_scope)
        )),
        add_to_env_kv(Entity.name_symbol, Self),
        add_env(),
        ref_used_packages(),

        handle_children(),
        add_to_env(
            env.bind(
                Self.initial_env,
                If(Entity.is_any_formal,
                   No(T.env_assoc.array),
                   Self.nonbound_generic_decl._.formal_part.match_param_list(
                       Self.params, False
                   ).map(lambda pm: T.env_assoc.new(
                       key=pm.formal.name.name_symbol, val=pm.actual.assoc.expr
                   )))
            ),
            dest_env=Self.instantiation_env,
            resolver=AdaNode.resolve_generic_actual,
        )
    )


class RenamingClause(AdaNode):
    """
    Renaming clause, used everywhere renamings are valid.
    """
    renamed_object = Field(type=T.Name)


class PackageRenamingDecl(BasicDecl):
    name = Field(type=T.DefiningName)
    renames = Field(type=RenamingClause)
    aspects = Field(type=T.AspectSpec)

    env_spec = child_unit(Entity.name_symbol, Self.name.parent_scope)

    defining_names = Property(Entity.name.singleton)
    defining_env = Property(env.bind(
        Entity.node_env,
        Entity.renames.renamed_object.env_elements.at(0)
        ._.cast(BasicDecl).defining_env
    ))

    xref_entry_point = Property(True)
    xref_equation = Property(
        Entity.renames.renamed_object.xref_no_overloading
    )


@abstract
class GenericRenamingDecl(BasicDecl):
    """
    Base node for all generic renaming declarations.
    """
    renaming_name = AbstractProperty(type=T.Name.entity)

    resolve = Property(env.bind(
        Entity.node_env,
        Entity.renaming_name.env_elements.at(0)._.match(
            lambda gd=T.GenericDecl: gd,
            lambda grd=T.GenericRenamingDecl: grd.resolve,
            lambda _: No(T.GenericDecl.entity)
        )
    ), type=T.GenericDecl.entity, doc="""
    Resolve the GenericDecl this renaming decl is pointing at
    """)


class GenericPackageRenamingDecl(GenericRenamingDecl):
    name = Field(type=T.DefiningName)
    renames = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)
    defining_env = Property(Entity.resolve.defining_env)
    renaming_name = Property(Entity.renames)

    env_spec = child_unit(Entity.name_symbol, Self.name.parent_scope)


class SubpKind(EnumNode):
    alternatives = ["procedure", "function"]


class GenericSubpRenamingDecl(GenericRenamingDecl):
    env_spec = child_unit(Entity.name_symbol, Self.name.parent_scope)

    kind = Field(type=T.SubpKind)
    name = Field(type=T.DefiningName)
    renames = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)
    renaming_name = Property(Entity.renames)


@abstract
class FormalSubpDecl(ClassicSubpDecl):
    """
    Formal subprogram declarations, in generic declarations formal parts.
    """
    default_expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.subp_spec.name.as_entity.singleton)


class ConcreteFormalSubpDecl(FormalSubpDecl):
    pass


class AbstractFormalSubpDecl(FormalSubpDecl):
    pass


class GenericFormalPart(BaseFormalParamHolder):
    decls = Field(type=T.AdaNode.list)

    abstract_formal_params = Property(
        Entity.decls.keep(BaseFormalParamDecl)
    )


@abstract
class GenericFormal(BaseFormalParamDecl):
    decl = Field(T.BasicDecl)
    defining_names = Property(Entity.decl.defining_names)


class GenericFormalObjDecl(GenericFormal):
    pass


class GenericFormalTypeDecl(GenericFormal):
    pass


class GenericFormalSubpDecl(GenericFormal):
    pass


class GenericFormalPackage(GenericFormal):
    pass


class GenericSubpInternal(BasicSubpDecl):
    subp_spec = Field(type=T.SubpSpec)
    aspects = Field(type=T.AspectSpec)

    subp_decl_spec = Property(Entity.subp_spec)
    env_spec = EnvSpec(add_env())


@abstract
class GenericDecl(BasicDecl):
    formal_part = Field(type=T.GenericFormalPart)
    decl = AbstractProperty(type=T.BasicDecl.entity)

    annotations = Annotations(rebindable=True)


class GenericSubpDecl(GenericDecl):
    env_spec = child_unit(Entity.name_symbol,
                          Self.subp_decl.subp_spec.name.parent_scope)

    subp_decl = Field(type=T.GenericSubpInternal)

    defining_names = Property(
        Self.subp_decl.subp_spec.name.as_entity.singleton)

    @langkit_property(public=True)
    def body_part():
        """
        Return the BaseSubpBody corresponding to this node.
        """
        return Entity.body_part_entity.cast(BaseSubpBody)

    env_spec = EnvSpec(
        # Process eventual parent unit
        call_env_hook(Self),

        set_initial_env(
            env.bind(Self.initial_env, Entity.defining_name.parent_scope)
        ),
        add_to_env_kv(Entity.name_symbol, Self),
        add_env(),
        ref_used_packages(),
    )

    decl = Property(Entity.subp_decl)


class GenericPackageInternal(BasePackageDecl):
    """
    This class denotes the internal package contained by a GenericPackageDecl.
    """
    # Implementation note: This exists so that we can insert an environment to
    # distinguish between formal parameters and the package's contents.

    env_spec = EnvSpec(add_env())


class GenericPackageDecl(GenericDecl):
    env_spec = child_unit(
        Entity.name_symbol,
        Entity.decl_scope,
        dest_env=env.bind(Self.parent.node_env, Entity.decl_scope(False))
    )

    package_decl = Field(type=GenericPackageInternal)

    defining_env = Property(Entity.package_decl.defining_env)

    defining_names = Property(
        Self.package_decl.package_name.as_entity.singleton)

    @langkit_property(public=True)
    def body_part():
        """
        Return the PackageBody corresponding to this node, or null if there is
        none.
        """
        return Entity.package_decl.body_part

    decl = Property(Entity.package_decl)


@abstract
@has_abstract_list
class Expr(AdaNode):

    type_var = UserField(LogicVarType, public=False)
    type_val = Property(Self.type_var.get_value)

    expression_type = Property(
        Self.logic_val(Entity, Self.type_var).cast_or_raise(T.BaseTypeDecl)
    )

    @langkit_property(external=True, uses_entity_info=False, uses_envs=False,
                      return_type=T.BigIntegerType, public=True)
    def eval_as_int():
        """
        Statically evaluates self, and returns the value of the evaluation as
        an integer.
        """
        pass

    @langkit_property(return_type=DiscreteRange)
    def discrete_range():
        """
        Return the discrete range for this expression, if applicable.
        """
        return Entity.match(
            # TODO: This won't handle array objects
            lambda ar=T.AttributeRef: ar.prefix.discrete_range,

            lambda n=T.Name: n.name_designated_type.then(
                lambda dt: dt.discrete_range
            ),

            lambda bo=T.BinOp: DiscreteRange.new(
                low_bound=bo.left.eval_as_int, high_bound=bo.right.eval_as_int
            ),
            lambda _: No(DiscreteRange)
        )

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      return_type=LexicalEnvType, dynamic_vars=[env, origin])
    def designated_env():
        """
        Returns the lexical environment designated by this name.

        If this name involves overloading, this will return a combination of
        the various candidate lexical environments.
        """
        pass

    env_elements = Property(
        Entity.env_elements_impl.filter(lambda e: (
            Not(e.is_library_item)
            | Self.has_with_visibility(e.el.unit)
        )),
        dynamic_vars=[env]
    )

    @langkit_property(return_type=AdaNode.entity.array,
                      kind=AbstractKind.abstract_runtime_check,
                      dynamic_vars=[env])
    def env_elements_impl():
        """
        Returns the list of annotated elements in the lexical environment
        that can statically be a match for expr before overloading analysis.
        """
        pass

    @langkit_property(return_type=AdaNode.entity.array, public=True)
    def matching_nodes():
        """
        Return the list of AST nodes that can be a match for this expression
        before overloading analysis.
        """
        return env.bind(Self.node_env, Entity.env_elements)


class ContractCaseAssoc(BaseAssoc):
    guard = Field(type=T.AdaNode)
    consequence = Field(type=T.Expr)

    assoc_expr = Property(Entity.consequence)


class ContractCases(Expr):
    contract_cases = Field(ContractCaseAssoc.list)


class ParenExpr(Expr):
    expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return (
            Entity.expr.sub_equation
            & TypeBind(Self.expr.type_var, Self.type_var)
        )


class Op(EnumNode):
    """
    Operation in a binary expression.

    Note that the ARM does not consider "double_dot" ("..") as a binary
    operator, but we process it this way here anyway to keep things simple.
    """
    alternatives = ["and", "or", "or_else", "and_then", "xor", "in",
                    "not_in", "abs", "not", "pow", "mult", "div", "mod",
                    "rem", "plus", "minus", "concat", "eq", "neq", "lt",
                    "lte", "gt", "gte", "double_dot"]

    subprograms = Property(
        lambda: Self.node_env.get(Self.match(
            lambda _=Op.alt_and: '"and"',
            lambda _=Op.alt_or: '"or"',
            lambda _=Op.alt_xor: '"xor"',
            lambda _=Op.alt_abs: '"abs"',
            lambda _=Op.alt_not: '"not"',
            lambda _=Op.alt_pow: '"**"',
            lambda _=Op.alt_mult: '"*"',
            lambda _=Op.alt_div: '"/"',
            lambda _=Op.alt_mod: '"mod"',
            lambda _=Op.alt_rem: '"rem"',
            lambda _=Op.alt_plus: '"+"',
            lambda _=Op.alt_minus: '"-"',
            lambda _=Op.alt_concat: '"&"',
            lambda _=Op.alt_eq: '"="',
            lambda _=Op.alt_neq: '"="',
            lambda _=Op.alt_lt: '"<"',
            lambda _=Op.alt_lte: '"<="',
            lambda _=Op.alt_gt: '">"',
            lambda _=Op.alt_gte: '">="',
            lambda _: '<<>>',
        )).filtermap(
            lambda e: e.cast_or_raise(T.BasicDecl),
            lambda e: e.cast_or_raise(T.BasicDecl).is_subprogram
        ),
        doc="""
        Return the subprograms corresponding to this operator accessible in the
        lexical environment.
        """
    )

    ref_var = UserField(type=LogicVarType, public=False)

    @langkit_property(public=True, return_type=T.DefiningName.entity)
    def xref():
        return Entity.parent.referenced_decl.defining_name


class UnOp(Expr):
    op = Field(type=Op)
    expr = Field(type=T.Expr)

    @langkit_property()
    def referenced_decl_internal(try_immediate=BoolType):
        return Self.logic_val(
            Entity, Self.op.ref_var, try_immediate
        ).cast_or_raise(T.BasicDecl)

    @langkit_property()
    def xref_equation():
        subps = Var(Entity.op.subprograms.filter(
            lambda s: s.subp_spec_or_null.nb_max_params == 1
        ))
        return Entity.expr.sub_equation & (subps.logic_any(lambda subp: Let(
            lambda ps=subp.subp_spec_or_null.unpacked_formal_params:

            # The subprogram's first argument must match Self's left
            # operand.
            TypeBind(Self.expr.type_var, ps.at(0).spec.type)

            # The subprogram's return type is the type of Self
            & TypeBind(Self.type_var,
                       subp.subp_spec_or_null.return_type)

            # The operator references the subprogram
            & Bind(Self.op.ref_var, subp)
        )) | TypeBind(Self.type_var, Self.expr.type_var))


class BinOp(Expr):
    left = Field(type=T.Expr)
    op = Field(type=Op)
    right = Field(type=T.Expr)

    @langkit_property()
    def referenced_decl_internal(try_immediate=BoolType):
        # TODO: Factor all those implems that do the same thing
        return Self.logic_val(
            Entity, Self.op.ref_var, try_immediate
        ).cast_or_raise(T.BasicDecl)

    @langkit_property()
    def xref_equation():
        subps = Var(Entity.op.subprograms.filter(
            lambda s: s.subp_spec_or_null.nb_max_params == 2
        ))
        return (
            Entity.left.sub_equation
            & Entity.right.sub_equation
        ) & (subps.logic_any(lambda subp: Let(
            lambda ps=subp.subp_spec_or_null.unpacked_formal_params:

            # The subprogram's first argument must match Self's left
            # operand.
            TypeBind(Self.left.type_var, ps.at(0).spec.type)

            # The subprogram's second argument must match Self's right
            # operand.
            & TypeBind(Self.right.type_var, ps.at(1).spec.type)

            # The subprogram's return type is the type of Self
            & TypeBind(Self.type_var,
                       subp.subp_spec_or_null.return_type)

            # The operator references the subprogram
            & Bind(Self.op.ref_var, subp)
        )) | Self.no_overload_equation)

    @langkit_property(dynamic_vars=[origin])
    def no_overload_equation():
        """
        When no subprogram is found for this node's operator, use this property
        to construct the xref equation for this node.
        """
        return Self.op.match(
            lambda _=Op.alt_pow:

            TypeBind(Self.right.type_var, Self.universal_int_type)
            & TypeBind(Self.left.type_var, Self.type_var),

            lambda _=Op.alt_concat: Or(
                TypeBind(Self.type_var, Self.left.type_var)
                & TypeBind(Self.type_var, Self.right.type_var),

                TypeBind(Self.type_var, Self.left.type_var)
                & TypeBind(Self.left.type_var, Self.right.type_var,
                           conv_prop=BaseTypeDecl.comp_type),

                TypeBind(Self.type_var, Self.right.type_var)
                & TypeBind(Self.right.type_var, Self.left.type_var,
                           conv_prop=BaseTypeDecl.comp_type),

                TypeBind(Self.right.type_var, Self.left.type_var)
                & TypeBind(Self.type_var, Self.right.type_var,
                           conv_prop=BaseTypeDecl.comp_type)

            ),

            lambda _: Or(
                # Regular case: Both operands and binop are of the same type
                TypeBind(Self.type_var, Self.left.type_var)
                & TypeBind(Self.type_var, Self.right.type_var),

                # Universal real with universal int case: Implicit conversion
                # of the binop to universal real.
                Or(
                    universal_int_bind(Self.left.type_var)
                    & universal_real_bind(Self.right.type_var),

                    universal_real_bind(Self.left.type_var)
                    & universal_int_bind(Self.right.type_var)
                ) & universal_real_bind(Self.type_var)
            )
        )


class RelationOp(BinOp):
    no_overload_equation = Property(
        TypeBind(Self.left.type_var, Self.right.type_var)
        & bool_bind(Self.type_var)
    )


class MembershipExpr(Expr):
    """
    Represent a membership test (in/not in operators).

    Note that we don't consider them as binary operators since multiple
    expressions on the right hand side are allowed.
    """
    expr = Field(type=T.Expr)
    op = Field(type=Op)
    membership_exprs = Field(type=T.ExprAlternativesList)

    xref_equation = Property(
        bool_bind(Self.type_var)
        & Entity.expr.sub_equation
        &
        Entity.membership_exprs.logic_all(
            lambda m: m.cast(T.Name)._.name_designated_type.then(
                # Tagged type check
                lambda _: m.cast(T.Name).xref_no_overloading,

                # Regular membership check
                default_val=m.sub_equation & TypeBind(
                    Entity.expr.type_var, m.type_var
                )
            )
        ),
    )


@abstract
class BaseAggregate(Expr):
    ancestor_expr = Field(type=T.Expr)
    assocs = Field(type=T.AssocList)


class Aggregate(BaseAggregate):

    xref_stop_resolution = Property(True)

    # An aggregate is resolved separately from the rest of an expression,
    # however, resolution of the containing expression can leverage the
    # knowledge that self is an aggregate, by accepting only type that can be
    # represented by an aggregate (eg. records and arrays).
    stop_resolution_equation = Property(origin.bind(
        Self, Predicate(BaseTypeDecl.is_array_or_rec, Self.type_var)
    ))

    @langkit_property(return_type=T.BaseAggregate, ignore_warn_on_node=True)
    def root_direct_parent_aggregate():
        return Self.direct_parent_aggregate.then(
            lambda p: p.root_direct_parent_aggregate,
            default_val=Self
        )

    @langkit_property(return_type=T.LongType)
    def sub_aggregate_rank(r=(LongType, 0)):
        return Self.direct_parent_aggregate.then(
            lambda p: p.sub_aggregate_rank(r + 1),
            default_val=r
        )

    @langkit_property(ignore_warn_on_node=True)
    def direct_parent_aggregate():
        return Self.parent.parent.parent.cast(T.Aggregate)

    @langkit_property()
    def xref_equation():
        return If(
            Self.parent.is_a(AspectClause, AspectAssoc),
            LogicTrue(),
            Entity.general_xref_equation
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def general_xref_equation():

        # Self might be a sub-aggregate in a multi-dimensional array aggregate.
        # So we'll go and grab the root direct parent aggregate, which is the
        # aggregate that is really an aggregate if this corresponds to a
        # multi-dimensional array.
        rtd = Var(Self.root_direct_parent_aggregate
                  .type_val.cast(BaseTypeDecl.entity))

        # We take the number of dimensions of this root rype, if it exists
        is_multidim = Var(rtd.then(lambda rtd: rtd.array_ndims > 1))

        # If the root type def exists is multi-dimensional, then take it. Else,
        # this is a regular aggregate. In this case grab the type in type_val.
        td = Var(If(is_multidim, rtd, Self.type_val.cast(BaseTypeDecl.entity)))

        # If this is a sub aggregate, compute the rank of it in the dimensions
        rank = Var(If(is_multidim, Self.sub_aggregate_rank, 0))

        atd = Var(td.array_def)
        return Cond(
            atd.is_null,

            # First case, aggregate for a record
            Entity.ancestor_expr.then(
                lambda ae: ae.sub_equation, default_val=LogicTrue()
            )
            & Entity.record_equation(td),

            # Second case, aggregate for an array
            Entity.assocs.logic_all(
                lambda assoc:
                If(
                    Or(Not(is_multidim),
                       rank == rtd.array_ndims - 1),

                    assoc.expr.as_entity.sub_equation
                    & TypeBind(assoc.expr.type_var, atd.comp_type),

                    LogicTrue()
                )

                & assoc.names.logic_all(
                    lambda n:
                    n.as_entity.sub_equation
                    & n.cast(T.Expr).then(
                        lambda n: TypeBind(n.type_var, atd.index_type(rank)),
                        default_val=LogicTrue()
                    )
                )
            )
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def assoc_equation(match=T.ParamMatch):
        """
        Helper for record_equation. Returns the equation for one discriminant
        assoc. Meant to be passed as additional equation to resolve_names.
        """
        return And(
            TypeBind(match.actual.assoc.expr.type_var,
                     match.formal.spec.type_expression.designated_type),
            If(match.actual.name.is_null,
               LogicTrue(),
               Bind(match.actual.name.ref_var, match.formal.spec))
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def record_equation(td=BaseTypeDecl.entity):
        """
        Equation for the case where this is an aggregate for a record
        type.
        """
        discriminants = Var(td.discriminants_list)

        # Get param matches for discriminants only
        discriminants_matches = Var(Self.match_formals(
            td.discriminants_list, Self.assocs, False
        ).filter(
            lambda pm: Not(discriminants
                           .find(lambda d: d == pm.formal.spec)
                           .is_null)
        ))

        # We run resolution for discriminants, because need ref and type
        # information to statically evaluate their values.
        ignore(Var(discriminants_matches.map(
            lambda dm: dm.actual.assoc.expr.as_entity.resolve_names_internal(
                True, Self.assoc_equation(dm)
            )
        )))

        # Get param matches for all aggregates' params. Here, we use and pass
        # down the discriminant matches, so that abstract_formal_params_impl is
        # able to calculate the list of components belonging to variant parts,
        # depending on the static value of discriminants.
        all_params = Var(td.record_def.comps.abstract_formal_params_impl(
            discriminants=discriminants_matches
        ))

        # Match formals to actuals, and compute equations
        return Self.match_formals(all_params, Self.assocs, False).logic_all(
            lambda pm:
            TypeBind(pm.actual.assoc.expr.type_var,
                     pm.formal.spec.type_expression.designated_type)
            & pm.actual.assoc.expr.as_entity.sub_equation
            & pm.actual.name.then(lambda n: Bind(n.ref_var, pm.formal.spec),
                                  LogicTrue())
        )


class NullRecordAggregate(BaseAggregate):
    @langkit_property()
    def xref_equation():
        return LogicTrue()


@abstract
class Name(Expr):

    parent_scope = AbstractProperty(
        type=LexicalEnvType, runtime_check=True,
        dynamic_vars=[env],
        doc="""
        Returns the lexical environment that is the scope in which the
        entity designated by this name is defined/used.
        """
    )

    @langkit_property(public=True, return_type=T.DefiningName.entity)
    def referenced_id(ref_decl=T.BasicDecl.entity):
        """
        Like ``referenced_decl``, but will return the defining identifier for
        the decl, rather than the basic declaration node itself.
        """
        return ref_decl.then(lambda ref_decl: Entity.name_symbol.then(
            lambda rel_name: ref_decl.defining_names.find(
                lambda dn: dn.name_symbol == rel_name
            )
        )._or(ref_decl.defining_name), default_val=No(T.DefiningName.entity))

    @langkit_property(public=True, return_type=T.DefiningName.entity)
    def xref():
        dn = Var(Entity.parents.find(lambda p: p.is_a(T.DefiningName))
                 .cast(T.DefiningName))
        bd = Var(dn.then(lambda dn: dn.basic_decl))

        return Cond(
            bd.then(lambda bd: bd.is_a(T.ParamSpec)),
            bd.cast(T.ParamSpec).decl_param(dn),

            bd.then(lambda bd: bd.is_a(Body)),
            bd.cast(T.Body).decl_part_entity.defining_name,

            bd.then(lambda bd: bd.is_a(BaseTypeDecl)
                    & bd.cast(T.BaseTypeDecl).is_in_private_part),
            bd.cast(T.BaseTypeDecl).previous_part(True).defining_name,

            bd.then(lambda bd: bd.is_a(BasicDecl)),
            bd.body_part_entity.then(lambda bpe: bpe.defining_name)
            ._or(bd.defining_name),

            Entity.referenced_id(Entity.referenced_decl)
        )

    @langkit_property(return_type=AdaNode.entity.array,
                      kind=AbstractKind.abstract_runtime_check,
                      dynamic_vars=[env, origin])
    def all_env_els_impl(seq=(BoolType, True),
                         seq_from=(AdaNode, No(T.AdaNode))):
        pass

    @langkit_property()
    def all_env_elements(seq=(BoolType, True),
                         seq_from=(AdaNode, No(T.AdaNode))):
        """
        Return all elements in self's scope corresponding to self.
        """
        return origin.bind(
            Self, env.bind(Entity.node_env,
                           Entity.all_env_els_impl(seq=seq, seq_from=seq_from))
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def bottom_up_name_equation():
        return Self.innermost_name.as_entity.match(
            lambda ce=T.CallExpr: ce.general_xref_equation(Self),
            lambda ed=T.ExplicitDeref: ed.general_xref_equation(Self),
            lambda _: LogicFalse(),
        )

    @langkit_property(return_type=T.Name, ignore_warn_on_node=True)
    def innermost_name():
        """
        Helper property. Return the innermost name following the name chain.
        For example, given::

            A (B) (C) (D)
            ^-----------^ Self
            ^-------^     Self.name
            ^---^         Self.name.name

        `Self.innermost_name` will return the node corresponding to
        `Self.name.name`.
        """
        name = Var(Self.match(
            lambda ce=T.CallExpr: ce.name,
            lambda ed=T.ExplicitDeref: ed.prefix,
            lambda _: No(T.Name)

        ))

        return If(name.is_a(T.CallExpr, T.ExplicitDeref),
                  name.innermost_name,
                  Self)

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def parent_name_equation(typ=T.BaseTypeDecl.entity, root=T.Name):
        """
        Construct the xref equation for the chain of parent nested names.
        """
        return If(
            typ.is_null,
            LogicFalse(),
            Self.match(
                lambda ce=T.CallExpr:
                ce.as_entity.subscriptable_type_equation(typ),

                lambda ed=T.ExplicitDeref: ed.as_entity.eq_for_type(typ),

                lambda _: Bind(Self.type_var, No(T.AdaNode.entity).el),
            ) & Self.parent_name(root).as_entity.then(
                lambda pn: pn.parent_name_equation(
                    typ.comp_type(
                        is_subscript=Not(Self.is_a(T.ExplicitDeref))
                    ),
                    root
                ),
                default_val=LogicTrue()
            )
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def subtype_indication_equation():
        return Entity.xref_no_overloading

    @langkit_property(return_type=T.Name, ignore_warn_on_node=True)
    def parent_name(stop_at=T.Name):
        """
        Will return the parent name until the stop point.
        """
        return If(stop_at.is_null | (Self == stop_at),
                  No(T.Name),
                  Self.parent.cast(T.Name))

    @langkit_property(return_type=BoolType)
    def is_range_attribute():
        """
        Predicate that returns True if self is a range attribute ref.
        """
        return Self.cast(T.AttributeRef).then(
            lambda attr_ref:
            attr_ref.as_bare_entity.attribute.name_symbol == 'Range'
        )

    scope = Property(
        EmptyEnv, dynamic_vars=[env],
        doc="""
        Lexical environment this identifier represents. This is similar to
        designated_env although it handles only cases for child units and it is
        used only during the environment population pass so it does not return
        orphan environments.
        """
    )

    @langkit_property(return_type=T.BoolType)
    def is_simple_name():
        """
        Returns whether Self is a BaseId or a DottedName composed only of
        BaseIds.
        """
        return Self.match(
            lambda _=T.BaseId: True,
            lambda dt=T.DottedName: dt.prefix.is_simple_name,
            lambda _: False
        )

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      return_type=LogicVarType)
    def ref_var():
        """
        This property proxies the logic variable that points to the entity that
        this name refers to. For example, for a simple dotted name::

            A.B

        The dotted name's ref var is the one of the SingleTokNode B.
        """
        pass

    @langkit_property()
    def referenced_decl_internal(try_immediate=BoolType):
        return Self.logic_val(
            Entity, Self.ref_var,
            try_immediate
        ).cast_or_raise(T.BasicDecl)

    designated_type_impl = Property(
        No(BaseTypeDecl.entity),
        dynamic_vars=[env, origin],
        doc="""
        Assuming this name designates a type, return this type.

        Since in Ada this can be resolved locally without any non-local
        analysis, this doesn't use logic equations.
        """
    )

    name_designated_type = Property(
        env.bind(Entity.node_env,
                 origin.bind(Self, Entity.designated_type_impl)),
        doc="""
        Like SubtypeIndication.designated_type, but on names, since because of
        Ada's ambiguous grammar, some subtype indications will be parsed as
        names.
        """,
        public=True
    )

    @langkit_property(memoized=True)
    def name_designated_type_env():
        return Entity.name_designated_type.cast(T.TypeDecl)._.primitives_env

    @langkit_property(
        return_type=AnalysisUnitType, external=True, uses_entity_info=False,
        uses_envs=False,
        call_non_memoizable_because='Getting an analysis unit cannot appear'
                                    ' in a memoized context'
    )
    def internal_referenced_unit(kind=AnalysisUnitKind,
                                 load_if_needed=BoolType):
        """
        Return the analysis unit for the given "kind" corresponding to this
        Name. Return null if this is an illegal unit name. If "load_if_needed"
        is false and the target analysis unit is not loaded yet, don't load it
        and return a null unit.
        """
        pass

    @langkit_property()
    def referenced_unit(kind=AnalysisUnitKind):
        """
        Shortcut for: `.internal_referenced_unit(kind, True)`.
        """
        return Self.internal_referenced_unit(kind, True)

    @langkit_property(call_memoizable=True)
    def referenced_unit_or_null(kind=AnalysisUnitKind):
        """
        Shortcut for: `.internal_referenced_unit(kind, False)`.
        """
        return Self.internal_referenced_unit(kind, False)

    @langkit_property(return_type=BoolType)
    def matches(n=T.Name):
        """
        Return whether two names match each other.

        This compares the symbol for Identifier and StringLiteral nodes. We
        consider that there is no match for all other node kinds.
        """
        return Self.match(
            lambda id=Identifier:
                n.cast(Identifier).then(
                    lambda other_id: id.sym.equals(other_id.sym)
                ),
            lambda sl=StringLiteral:
                n.cast(StringLiteral).then(
                    lambda other_sl: sl.sym.equals(other_sl.sym)
                ),
            lambda di=DefiningName: n.matches(di.name),
            lambda _: False
        )

    @langkit_property(public=True)
    def name_matches(n=T.Name.entity):
        """
        Return whether two names match each other.

        This compares the symbol for Identifier and StringLiteral nodes. We
        consider that there is no match for all other node kinds.
        """
        return Self.matches(n.el)

    @langkit_property()
    def use_package_name_designated_env():
        """
        Assuming Self is a name that is the direct child of a
        UsePackageClause's package name list, return the memoized designated
        environment for it.
        """
        return (Self.parent.parent.cast_or_raise(T.UsePackageClause)
                .designated_envs.at(Self.child_index))

    relative_name = AbstractProperty(
        type=T.SingleTokNode.entity, runtime_check=True, public=True,
        doc="""
        Returns the relative name of this instance. For example,
        for a prefix A.B.C, this will return C.
        """
    )

    name_symbol = Property(Self.as_bare_entity.relative_name.symbol)

    base_name = Property(
        No(T.Name.entity),
        doc="""
        Returns the base name of this instance. For example,
        for a prefix A.B.C, this will return A.B.
        """
    )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def xref_no_overloading(sequential=(BoolType, True),
                            all_els=(BoolType, False)):
        """
        Simple xref equation for names. Doesn't try to resolve overloads. If
        ``all_els`` is True, then the name will be bound to the domain of all
        elements that corresponds. Else, it will be bound to the first one.

        ``sequential`` determines whether the lookup will be sequential or not.
        """
        return Entity.match(
            lambda dn=T.DottedName:
            dn.prefix.xref_no_overloading(sequential, all_els)
            & env.bind(
                dn.prefix.designated_env,
                dn.suffix.xref_no_overloading(sequential, all_els)
            ),
            lambda i=T.BaseId: If(
                all_els and Self.is_suffix,
                i.ref_var.domain(
                    env.get(
                        i.name_symbol,
                        from_node=If(sequential, Entity.el, No(T.Name)),
                    ),
                ),
                Bind(
                    i.ref_var,
                    env.get_first(
                        i.name_symbol,
                        from_node=If(sequential, Entity.el, No(T.Name)),
                        recursive=Self.is_prefix,
                    ),
                )
            ),

            # xref_no_overloading can be used to resolve type references in
            # generic instantiations. In that case, we might encounter a 'Class
            # attribute. We just want to resolve the prefix.
            lambda at=T.AttributeRef:
            at.prefix.xref_no_overloading(sequential, all_els),

            lambda _: LogicFalse()
        )

    @langkit_property(return_type=T.BoolType, memoized=True)
    def is_prefix():
        """
        Returns whether Self is the prefix in name. Is used to determine
        whether lookups on this name should be recursive or not, without having
        to pass down the information as a function parameter.
        """
        return Or(
            Self.parent.is_a(T.DottedName)
            & (Self.parent.cast(T.DottedName).prefix == Self)
            & Self.parent.cast(T.Name).is_prefix,
            Not(Self.parent.is_a(T.DottedName))
        )

    @langkit_property(return_type=T.BoolType, memoized=True)
    def is_suffix():
        """
        Returns whether Self is the suffix in name.
        """
        return Or(
            Self.parent.is_a(T.DottedName)
            & (Self.parent.cast(T.DottedName).suffix == Self)
            & Self.parent.cast(T.Name).is_suffix,

            Not(Self.parent.is_a(T.DottedName))
        )

    is_operator_name = Property(
        Entity.name_symbol.any_of(
            '"="',  '"="', '"/="', '"<"', '"<="', '">"', '">="', '"and"',
            '"or"', '"xor"', '"abs"', '"*"', '"/"', '"mod"', '"rem"', '"+"',
            '"-"', '"&"' '"+"', '"-"', '"not"', '"abs"'
        )
    )

    @langkit_property(public=True, return_type=SymbolType.array)
    def as_symbol_array():
        """
        Turn this name into an array of symbols.

        For instance, "A.B.C" is turned into ['A', 'B', 'C'].
        """
        return Self.match(
            lambda dname=T.DefiningName: dname.name.as_symbol_array,
            lambda tok=T.SingleTokNode: tok.symbol.singleton,
            lambda dot=T.DottedName: dot.prefix.as_symbol_array.concat(
                dot.suffix.as_symbol_array
            ),
            lambda _: PropertyError(SymbolType.array),
        )


class DiscreteSubtypeName(Name):
    subtype = Field(type=T.DiscreteSubtypeIndication)


class TargetName(Name):
    pass


class CallExpr(Name):
    """
    Represent a syntactic call expression.

    At the semantic level, this can be either a subprogram call, an array
    subcomponent access expression, an array slice or a type conversion.
    """
    name = Field(type=T.Name)
    suffix = Field(type=T.AdaNode)

    ref_var = Property(Self.name.ref_var)

    relative_name = Property(Entity.name.relative_name)

    @langkit_property()
    def designated_env():
        return Entity.env_elements.map(lambda e: e.match(
            lambda bd=BasicDecl.entity:       bd.defining_env,
            lambda _:                         EmptyEnv,
        )).env_group()

    @langkit_property()
    def env_elements_impl():
        return Entity.name.env_elements_impl

    # CallExpr can appear in type expressions: they are used to create implicit
    # subtypes for discriminated records or arrays.
    designated_type_impl = Property(Entity.name.designated_type_impl)

    params = Property(Self.suffix.cast(T.AssocList), ignore_warn_on_node=True)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return If(
            Not(Entity.name.name_designated_type.is_null),

            # Type conversion case
            Entity.type_conv_xref_equation,

            # General case. We'll call general_xref_equation on the innermost
            # call expression, to handle nested call expression cases.
            Entity.bottom_up_name_equation
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def type_conv_xref_equation():
        """
        Helper for xref_equation, handles construction of the equation in type
        conversion cases.
        """
        return And(
            Self.params.at(0).expr.as_entity.sub_equation,
            Entity.name.sub_equation,
            TypeBind(Self.type_var, Self.name.type_var),
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def general_xref_equation(root=T.Name):
        """
        Helper for xref_equation, handles construction of the equation in
        subprogram call cases.
        """
        # List of every applicable subprogram
        subps = Var(Entity.env_elements)

        def entity_equation(s):
            # The called entity is the matched entity
            return Bind(Self.name.ref_var, s) & Cond(

                # If s does not have any parameters, then we construct the
                # chain of name equations starting from self, with the parent
                # component.
                s.is_paramless, Entity.parent_name_equation(
                    s.expr_type, root
                ),

                # If S can be called in a paramless fashion, but can also be
                # called with parameters, we are forced to make a disjunction.
                s.can_be_paramless, Or(
                    Entity.parent_name_equation(
                        s.expr_type, root
                    ),

                    And(
                        Entity.subprogram_equation(
                            s.subp_spec_or_null,
                            s.info.md.dottable_subp,
                            s.info.md.primitive
                        ),
                        Entity.parent_name(root).as_entity.then(
                            lambda pn:
                            pn.parent_name_equation(s.expr_type, root),
                            default_val=LogicTrue()
                        )
                    )
                ),

                And(
                    Entity.subprogram_equation(
                        s.subp_spec_or_null,
                        s.info.md.dottable_subp,
                        s.info.md.primitive
                    ),
                    Entity.parent_name(root).as_entity.then(
                        lambda pn:
                        pn.parent_name_equation(s.expr_type, root),
                        default_val=LogicTrue()
                    )
                )
            )

        return And(
            Self.params.logic_all(lambda pa: pa.expr.as_entity.sub_equation),

            # For each potential entity match, we want to express the
            # following constraints:
            And(
                subps.logic_any(lambda e: Let(
                    lambda s=e.cast_or_raise(BasicDecl.entity):
                    entity_equation(s)
                )),
                Bind(Self.ref_var, Self.name.ref_var),
                Entity.name.sub_equation
            )
            | If(Entity.name.is_simple_name,
                 Entity.operator_equation,
                 LogicFalse())
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def operator_equation():
        rel_name = Var(Entity.name.name_symbol)

        def base_name_eq():
            return Entity.name.base_name._.sub_equation._or(LogicFalse())

        return Self.params._.unpacked_params.then(
            lambda params:
            Cond(
                (params.length == 2)
                & rel_name.any_of('"="',  '"="', '"/="', '"<"', '"<="', '">"',
                                  '">="'),
                TypeBind(params.at(0).assoc.expr.type_var,
                         params.at(1).assoc.expr.type_var)
                & bool_bind(Self.type_var)
                & base_name_eq(),


                (params.length == 2)
                & rel_name.any_of(
                    '"and"', '"or"', '"xor"', '"abs"', '"*"',
                    '"/"', '"mod"', '"rem"', '"+"', '"-"', '"&"'
                ),
                TypeBind(params.at(0).assoc.expr.type_var,
                         params.at(1).assoc.expr.type_var)
                & TypeBind(params.at(0).assoc.expr.type_var,
                           Self.type_var)
                & base_name_eq(),

                (params.length == 2) & (rel_name == '"**"'),
                TypeBind(params.at(1).assoc.expr.type_var,
                         Self.universal_int_type)
                & TypeBind(params.at(0).assoc.expr.type_var, Self.type_var)
                & base_name_eq(),

                (params.length == 1)
                & rel_name.any_of('"+"', '"-"', '"not"', '"abs"'),
                TypeBind(params.at(0).assoc.expr.type_var, Self.type_var)
                & base_name_eq(),

                LogicFalse()
            )
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def subscriptable_type_equation(typ=T.BaseTypeDecl.entity,
                                    constrain_params=(BoolType, True)):
        """
        Construct an equation verifying if Self is conformant to the type
        designator passed in parameter.
        """
        atd = Var(typ.then(lambda t: t.array_def_with_deref))
        real_typ = Var(typ.then(
            lambda t: If(t.is_implicit_deref, t.accessed_type, t))
        )

        return Cond(
            Not(atd._.indices.is_null), Self.suffix.match(
                # Regular array access
                lambda _=T.AssocList: Self.params._.logic_all(
                    lambda i, pa: If(
                        constrain_params,
                        pa.expr.as_entity.sub_equation,
                        LogicTrue()
                    )
                    & atd.indices.constrain_index_expr(pa.expr, i)
                )
                & TypeBind(Self.type_var, atd.comp_type),

                # Slice access
                lambda bo=T.BinOp:
                atd.indices.constrain_index_expr(bo.left, 0)
                & atd.indices.constrain_index_expr(bo.right, 0)
                & TypeBind(bo.type_var, bo.right.type_var)
                & TypeBind(Self.type_var, real_typ)
                & bo.as_entity.left.sub_equation
                & bo.as_entity.right.sub_equation,

                # Range attribute
                lambda ar=T.AttributeRef:
                ar.as_entity.sub_equation
                & atd.indices.constrain_index_expr(ar, 0)
                & TypeBind(Self.type_var, real_typ),

                # Subtype indication
                lambda st=T.SubtypeIndication:
                st.as_entity.sub_equation
                & TypeBind(Self.type_var, real_typ),

                lambda _: LogicFalse(),
            ),

            # Type has user defined indexing
            Not(typ.is_null) & typ.has_ud_indexing,
            typ.constant_indexing_fns.concat(typ.variable_indexing_fns)
            .logic_any(lambda fn: Let(
                lambda
                formal=fn.subp_spec_or_null.unpacked_formal_params.at(1),
                ret_type=fn.subp_spec_or_null.return_type,
                param=Self.params.at(0).expr:

                TypeBind(Self.type_var, ret_type)
                & If(constrain_params,
                     param.as_entity.sub_equation, LogicTrue())
                & TypeBind(param.type_var, formal.spec.type)
            )),

            typ.access_def.cast(AccessToSubpDef).then(
                lambda asd:
                Entity.subprogram_equation(asd.subp_spec, False, No(AdaNode))
                & Entity.params.logic_all(
                    lambda pa: pa.expr.as_entity.sub_equation
                ),
                default_val=LogicFalse(),
            ),
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def subprogram_equation(subp_spec=T.BaseFormalParamHolder.entity,
                            dottable_subp=BoolType,
                            primitive=AdaNode):

        prim_type = Var(primitive.cast_or_raise(T.BaseTypeDecl))

        return subp_spec.then(
            lambda subp_spec:
            # The type of the expression is the expr_type of the
            # subprogram.
            TypeBind(Self.type_var, subp_spec.return_type)

            # For each parameter, the type of the expression matches
            # the expected type for this subprogram.
            & subp_spec.match_param_list(
                Self.params, dottable_subp
            ).logic_all(
                lambda pm: Let(
                    lambda ft=pm.formal.spec.type_expression.designated_type:

                    # The type of each actual matches the type of the
                    # formal.
                    If(ft.el.as_bare_entity
                       .matching_type(prim_type.as_bare_entity),

                       Bind(pm.actual.assoc.expr.type_var, ft,
                            eq_prop=BaseTypeDecl.matching_formal_prim_type),
                       Bind(pm.actual.assoc.expr.type_var, ft,
                            eq_prop=BaseTypeDecl.matching_formal_type))
                ) & If(
                    # Bind actuals designators to parameters if there
                    # are designators.
                    pm.actual.name.is_null,
                    LogicTrue(),
                    Bind(pm.actual.name.ref_var, pm.formal.spec)
                )
            )
        )._or(LogicFalse())

    @langkit_property(return_type=BoolType, dynamic_vars=[env, origin])
    def check_for_type(typ=T.BaseTypeDecl.entity):
        """
        Check that self is an appropriate CallExpr for given type, which must
        be a subscriptable type (eg; a type for which it makes senses to do a
        call expr on an instance of the type, like an array type, or an access
        to subprogram type.
        """
        # Algorithm: We're Recursing down call expression and component types
        # up to self, checking for each level that the call expression
        # corresponds.

        atd = Var(typ.then(lambda t: t.array_def_with_deref))

        return origin.bind(Self, typ.then(lambda typ: And(
            Or(
                # Arrays
                atd.then(lambda _: Self.suffix.match(
                    # Array indexing case
                    lambda al=AssocList: atd.array_ndims == al.length,

                    # Array slice case
                    lambda _=BinOp: atd.array_ndims == 1,
                    lambda _: False
                ), default_val=False),

                # Accesses to subprograms
                typ.access_def.cast(T.AccessToSubpDef).then(
                    lambda sa:
                    sa.subp_spec.is_matching_param_list(Self.params, False)
                ),

                # Types with user defined indexing
                typ.has_ud_indexing
                & Self.suffix.cast(T.AssocList).then(lambda al: al.length == 1)
            ),

            Self.parent.cast(T.CallExpr).then(
                lambda ce: ce.check_for_type(
                    origin.bind(Self, typ.expr_type)
                ), default_val=True
            )
        )))


class ParamAssoc(BasicAssoc):
    """
    Assocation (X => Y) used for aggregates and parameter associations.
    """
    designator = Field(type=T.AdaNode)
    r_expr = Field(type=T.Expr)

    expr = Property(Self.r_expr)
    names = Property(If(Self.designator.is_null,
                        No(T.AdaNode.array), Self.designator.singleton))


class AggregateAssoc(BasicAssoc):
    """
    Assocation (X => Y) used for aggregates and parameter associations.
    """
    designators = Field(type=T.AlternativesList)
    r_expr = Field(type=T.Expr)

    expr = Property(Self.r_expr)
    names = Property(Self.designators.map(lambda d: d))


class MultiDimArrayAssoc(AggregateAssoc):
    pass


class AssocList(BasicAssoc.list):

    @langkit_property()
    def unpacked_params():
        """
        Given the list of ParamAssoc, that can in certain case designate
        several actual parameters at once, create an unpacked list of
        SingleActual instances.
        """
        return Self.mapcat(lambda pa: Let(lambda names=pa.names: If(
            names.length == 0,
            SingleActual.new(name=No(Identifier), assoc=pa).singleton,
            names.filtermap(
                lambda i: SingleActual.new(name=i.cast(T.BaseId), assoc=pa),
                lambda n: n.is_a(T.BaseId),
            )
        )))


class DeclList(AdaNode.list):
    pass


class StmtList(AdaNode.list):
    pass


class ExplicitDeref(Name):
    prefix = Field(type=T.Name)
    ref_var = Property(Self.prefix.ref_var)

    relative_name = Property(Entity.prefix.relative_name)

    @langkit_property()
    def designated_env():
        # Since we have implicit dereference in Ada, everything is directly
        # accessible through the prefix, so we just use the prefix's env.
        return Entity.prefix.designated_env

    @langkit_property()
    def env_elements_impl():
        return origin.bind(
            Self,
            Entity.prefix.env_elements_impl.filter(
                # Env elements for access derefs need to be of an access type
                lambda e:
                e.cast(BasicDecl)._.expr_type.then(lambda t: t.is_access_type)
            )
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def eq_for_type(typ=T.BaseTypeDecl.entity):
        return And(
            TypeBind(Self.prefix.type_var, typ),
            TypeBind(Self.type_var, typ.accessed_type)
        )

    @langkit_property()
    def xref_equation():
        return Entity.bottom_up_name_equation

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def general_xref_equation(root=(T.Name, No(T.Name))):
        env_els = Var(Entity.env_elements)

        return Entity.prefix.sub_equation & Or(
            env_els.logic_any(
                lambda el: el.cast(T.BasicDecl).expr_type.then(
                    lambda typ:

                    # Bind Self's ref var to the entity, with the access_entity
                    # field set to False, since self designated the non-access
                    # entity.
                    Bind(Self.ref_var, el.trigger_access_entity(False))

                    & Bind(Self.ref_var, Self.prefix.ref_var)
                    & Entity.eq_for_type(typ)
                    & typ.access_def.cast(AccessToSubpDef).then(
                        lambda _: Self.parent_name(root).as_entity.then(
                            lambda pn: pn.parent_name_equation(typ, root),
                            default_val=LogicTrue()
                        ),
                        default_val=Self.parent_name(root).as_entity.then(
                            lambda pn: pn.parent_name_equation(
                                typ.accessed_type, root
                            ), default_val=LogicTrue()
                        )
                    )
                )
            ),

        )


class BoxExpr(Expr):
    xref_equation = Property(LogicTrue())


class OthersDesignator(AdaNode):
    xref_equation = Property(LogicTrue())


class IfExpr(Expr):
    cond_expr = Field(type=T.Expr)
    then_expr = Field(type=T.Expr)
    alternatives = Field(type=T.ElsifExprPart.list)
    else_expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return (
            # Construct sub equations for common sub exprs
            Entity.cond_expr.sub_equation
            & Entity.then_expr.sub_equation

            & If(
                Not(Self.else_expr.is_null),

                # If there is an else, then construct sub equation
                Entity.else_expr.sub_equation
                # And bind the then expr's and the else expr's types
                & TypeBind(Self.then_expr.type_var, Self.else_expr.type_var),

                # If no else, then the then_expression has type bool
                bool_bind(Self.then_expr.type_var)
            ) & Entity.alternatives.logic_all(lambda elsif: (
                # Build the sub equations for cond and then exprs
                elsif.cond_expr.sub_equation
                & elsif.then_expr.sub_equation

                # The condition is boolean
                & bool_bind(elsif.cond_expr.type_var)

                # The elsif branch then expr has the same type as Self's
                # then_expr.
                & TypeBind(Self.then_expr.type_var, elsif.then_expr.type_var)
            )) & bool_bind(Self.cond_expr.type_var)
            & TypeBind(Self.then_expr.type_var, Self.type_var)
        )


class ElsifExprPart(AdaNode):
    cond_expr = Field(type=T.Expr)
    then_expr = Field(type=T.Expr)


class CaseExpr(Expr):
    expr = Field(type=T.Expr)
    cases = Field(type=T.CaseExprAlternative.list)

    @langkit_property()
    def xref_equation():
        # We solve Self.expr separately because it is not dependent on the rest
        # of the semres.
        ignore(Var(Entity.expr.resolve_names_internal(
            True, Predicate(BaseTypeDecl.is_discrete_type, Self.expr.type_var)
        )))

        return Entity.cases.logic_all(lambda alt: (
            alt.choices.logic_all(lambda c: c.match(
                # Expression case
                lambda e=T.Expr:
                TypeBind(e.type_var, Self.expr.type_val)
                & e.sub_equation,

                # TODO: Bind other cases: SubtypeIndication and Range
                lambda _: LogicTrue()
            ))

            # Equations for the dependent expressions
            & alt.expr.sub_equation

            # The type of self is the type of each expr. Also, the type of
            # every expr is bound together by the conjunction of this bind for
            # every branch.
            & TypeBind(Self.type_var, alt.expr.type_var)
        ))


class CaseExprAlternative(Expr):
    choices = Field(type=T.AlternativesList)
    expr = Field(type=T.Expr)


@abstract
class SingleTokNode(Name):
    token_node = True

    relative_name = Property(Entity)

    r_ref_var = UserField(LogicVarType, public=False)
    """
    This field is the logic variable for this node. It is not used directly,
    instead being retrieved via the ref_var property
    """

    ref_var = Property(Self.r_ref_var)

    sym = Property(
        Self.symbol, doc="Shortcut to get the symbol of this node"
    )

    @langkit_property(dynamic_vars=[env])
    def env_get_first(recursive=BoolType, from_node=T.AdaNode):
        """
        Like env.get_first, but returning the first visible element in the Ada
        sense.
        """
        return env.get(
            Self,
            recursive=recursive,
            from_node=from_node,
        ).find(
            lambda el:
            Or(Not(el.is_library_item),
               Self.has_with_visibility(el.el.unit))
        )


class DefiningName(Name):
    name = Field(type=T.Name)

    parent_scope = Property(Self.name.parent_scope)
    scope = Property(Self.name.scope)
    relative_name = Property(Entity.name.relative_name)
    ref_var = Property(Self.name.ref_var)
    env_elements_impl = Property(Entity.name.env_elements_impl)

    basic_decl = Property(
        Self.parents.find(lambda p: p.is_a(T.BasicDecl))
        .cast_or_raise(T.BasicDecl).as_entity,
        public=True, memoized=True,
        doc="Returns this DefiningName's basic declaration"
    )


class EndName(Name):
    name = Field(type=T.Name)

    parent_scope = Property(Self.name.parent_scope)
    scope = Property(Self.name.scope)
    relative_name = Property(Entity.name.relative_name)
    ref_var = Property(Self.name.ref_var)
    env_elements_impl = Property(Entity.name.env_elements_impl)

    basic_decl = Property(
        Self.parents.find(lambda p: p.is_a(T.NamedStmt)).then(
            lambda ns:
            ns.cast_or_raise(T.NamedStmt).decl.cast(T.BasicDecl).as_entity
        )._or(
            Self.parents.find(lambda p: p.is_a(T.BasicDecl))
            .cast_or_raise(T.BasicDecl).as_entity,
        ),
        public=True, memoized=True,
        doc="Returns this EndName's basic declaration"
    )

    xref_equation = Property(If(
        Entity.basic_decl.is_a(T.SubpBody),
        Bind(Self.ref_var, Entity.basic_decl),
        Entity.name.xref_no_overloading,
    ))

    xref_entry_point = Property(True)


@abstract
class BaseId(SingleTokNode):

    annotations = Annotations(custom_short_image=True)

    @langkit_property(memoized=True)
    def scope():
        elt = Var(env.get_first(Self))
        ret = Var(If(
            Not(elt.is_null) & elt.el.is_a(
                T.BasicDecl
            ),
            elt.children_env,
            EmptyEnv
        ))

        # If this the corresponding decl is a generic, go grab the internal
        # package decl.
        return ret.env_node.cast(T.GenericPackageDecl).then(
            lambda gen_pkg_decl: gen_pkg_decl.package_decl.children_env,
            default_val=ret
        )

    designated_env = Property(Entity.designated_env_impl)

    @langkit_property(dynamic_vars=[env])
    def designated_env_impl():
        """
        Decoupled implementation for designated_env, specifically used by
        DottedName when the parent is a library level package.
        """
        bd = Var(Self.parents.find(
            lambda p: p.is_a(T.GenericPackageInstantiation)
        ))
        env_els = Var(Entity.env_elements_baseid)
        pkg = Var(env_els.filter(lambda e: Not(e.el == bd)).at(0))

        return origin.bind(Self, If(
            pkg._.is_package,
            # If current item is a library item, we want to check that it
            # is visible from the current unit.
            If(
                Or(Not(pkg.is_library_item),
                   Self.has_with_visibility(pkg.unit)),
                Entity.pkg_env(pkg.cast(BasicDecl)),
                No(T.LexicalEnvType)
            ),

            env_els.map(
                lambda e: e.cast(BasicDecl).defining_env
            ).env_group()
        ))

    @langkit_property(dynamic_vars=[env, origin])
    def pkg_env(bd=T.BasicDecl.entity):
        """
        Return the lexical environment for this identifier, should it be a
        package. This method handles resolving to the most visible part of a
        package - private or body - if necessary.
        """
        env = Var(bd.defining_env)

        tl_item = Var(Self.top_level_item(Self.unit).as_entity)
        tl_item_env = Var(tl_item.children_env)

        return Cond(

            tl_item.cast(PackageBody).then(lambda ob: bd == ob.previous_part),
            tl_item_env,

            # TODO: Probably some special handling for separates here, because
            # they'll have full visibility on the package body in which they're
            # defined.

            Self.is_children_env(env, tl_item_env),
            env.get('__privatepart', recursive=False).at(0).then(
                lambda pp: pp.children_env, default_val=env
            ),

            env
        )

    parent_scope = Property(env)

    @langkit_property(memoized=True)
    def designated_type_impl():

        def get_real_type(basic_decl):
            return basic_decl.match(
                lambda t=T.BaseTypeDecl.entity: t,
                lambda tb=T.TaskBody.entity: tb.task_type,
                lambda _: No(BaseTypeDecl.entity)
            )

        # This is the view of the type where it is referenced
        des_type_1 = Var(Self.env_get_first(
            from_node=Self,
            recursive=Self.is_prefix
        ).then(
            lambda env_el: get_real_type(env_el)
        ))

        # This is the view of the type where it is used
        des_type_2 = Var(Self.env_get_first(
            from_node=origin,
            recursive=Self.is_prefix
        ).then(
            lambda env_el: get_real_type(env_el)
        ))

        des_type = Var(Cond(
            # In some cases des_type_1 can be null TODO: investigate
            des_type_1.is_null, des_type_2,

            # If same type, then it doesn't matter (return early from the view
            # checking below).
            des_type_1 == des_type_2, des_type_1,

            # If des_type_1 is a less complete version of des_type_2, then pick
            # des_type_2.
            des_type_1.then(lambda d: d.is_view_of_type(des_type_2)),
            des_type_2,

            # In any other case use des_type_1
            des_type_1
        ))

        # We might have a more complete view of the type at the origin point
        completer_view = Var(origin.then(
            lambda o: o.children_env.get_first(Self, from_node=origin))
            .cast(T.BaseTypeDecl)
        )

        # If completer_view is a more complete view of the type we're looking
        # up, then return completer_view. Else return des_type.
        return If(
            Not(completer_view.is_null)
            & des_type.then(lambda d: d.is_view_of_type(completer_view)),
            completer_view,
            des_type
        )

    @langkit_property(return_type=CallExpr, ignore_warn_on_node=True)
    def parent_callexpr():
        """
        If this BaseId is the main symbol qualifying the prefix in a call
        expression, this returns the corresponding CallExpr node. Return null
        otherwise. For example::

            C (12, 15);
            ^ parent_callexpr = <CallExpr>

            A.B.C (12, 15);
                ^ parent_callexpr = <CallExpr>

            A.B.C (12, 15);
              ^ parent_callexpr = null

            C (12, 15);
               ^ parent_callexpr = null
        """
        return Self.parents.take_while(lambda p: Or(
            p.is_a(CallExpr),
            p.is_a(DottedName, BaseId) & p.parent.match(
                lambda pfx=DottedName: pfx.suffix == p,
                lambda ce=CallExpr: ce.name == p,
                lambda _: False
            )
        )).find(lambda p: p.is_a(CallExpr)).cast(CallExpr)

    @langkit_property(dynamic_vars=[env])
    def env_elements_impl():
        return Entity.env_elements_baseid

    @langkit_property()
    def all_env_els_impl(seq=(BoolType, True),
                         seq_from=(AdaNode, No(T.AdaNode))):
        return env.get(
            Self, recursive=Self.is_prefix,
            from_node=If(seq, If(Not(seq_from.is_null), seq_from, Self),
                         No(T.AdaNode))
        )

    @langkit_property(dynamic_vars=[env], memoized=True)
    def env_elements_baseid():
        """
        Decoupled implementation for env_elements_impl, specifically used by
        designated_env when the parent is a library level package.
        """
        items = Var(env.get(
            Self, recursive=Self.is_prefix,
            # If we are in an aspect, then lookup is not sequential.
            # TODO: The fact that this is here is ugly, and also the logic is
            # probably wrong.
            from_node=If(
                Not(Self.parents.find(
                    lambda p: p.cast(T.AspectAssoc).then(
                        lambda a: a.id.as_entity
                        .name_symbol.any_of('Pre', 'Post')
                    )
                ).is_null),
                No(T.AdaNode),
                Self
            )
        ))

        # TODO: there is a big smell here: We're doing the filtering for parent
        # expressions in the baseid env_elements. We should solve that.

        pc = Var(Self.parent_callexpr)

        return origin.bind(Self, Cond(
            pc.is_null,

            # If it is not the main id in a CallExpr: either the name
            # designates something else than a subprogram, either it designates
            # a subprogram that accepts no explicit argument. So filter out
            # other subprograms.
            items.filter(lambda e: (

                # If there is a subp_spec, check that it corresponds to
                # a parameterless subprogram.
                Or(
                    e.cast_or_raise(BasicDecl).can_be_paramless,
                    e.cast(T.BaseSubpBody)._.in_scope
                )
            )),

            # This identifier is the name for a called subprogram or an array.
            # So only keep:
            # * subprograms for which the actuals match
            # * arrays for which the number of dimensions match
            # * any type that has a user defined indexing aspect.

            pc.suffix.cast(AssocList).then(lambda params: (
                items.filter(lambda e: e.match(
                    # Type conversion case
                    lambda _=BaseTypeDecl: params.length == 1,

                    lambda b=BasicDecl:
                    b.subp_spec_or_null.then(
                        lambda spec:

                        # Either the subprogram is matching the CallExpr's
                        # parameters.
                        And(
                            spec.is_matching_param_list(
                                params, b.info.md.dottable_subp
                            ),
                            pc.parent.cast(T.CallExpr).then(
                                lambda ce: ce.check_for_type(b.expr_type),
                                default_val=True
                            )
                        )

                        # Or the entity is parameterless, and the returned
                        # component (s) matches the callexpr (s).
                        | And(pc.check_for_type(b.expr_type),
                              spec.paramless(b.info.md)),

                        # In the case of ObjectDecls/CompDecls in general,
                        # verify that the callexpr is valid for the given type
                        # designator.
                        default_val=pc.check_for_type(b.expr_type)
                    ),

                    lambda _: False
                ))
            ), default_val=items)
        ))

    @langkit_property()
    def xref_equation():
        return Entity.base_id_xref_equation()

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def base_id_xref_equation():
        return Let(lambda dt=Entity.designated_type_impl: If(
            Not(dt.is_null),

            # Type conversion case
            Bind(Self.ref_var, dt) & TypeBind(Self.type_var, dt),

            Entity.general_xref_equation
        ))

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def general_xref_equation():
        env_els = Var(Entity.env_elements)

        return (
            Self.ref_var.domain(env_els)
            & Bind(Self.ref_var, Self.type_var, BasicDecl.expr_type,
                   eq_prop=BaseTypeDecl.matching_type)
        )


@has_abstract_list
class Identifier(BaseId):
    annotations = Annotations(repr_name="Id")
    is_not_class_id = Property(Not(Self.symbol == 'Class'))


class StringLiteral(BaseId):
    annotations = Annotations(repr_name="Str")

    @langkit_property()
    def xref_equation():
        return If(
            # StringLiteral can be in a name, if it is an operator, in which
            # case we don't want to constrain its type.
            Self.parent.is_a(Name),
            Entity.base_id_xref_equation,
            Or(
                TypeBind(Self.type_var, Self.std_entity('String')),
                Predicate(BaseTypeDecl.is_str_type_or_null, Self.type_var)
            )
        )


class EnumLiteralDecl(BasicDecl):
    name = Field(type=T.DefiningName)

    @langkit_property(public=True)
    def enum_type():
        """
        Return the enum type corresponding to this enum literal.
        """
        return Self.parents.find(
            lambda p: p.is_a(TypeDecl)
        ).cast(TypeDecl).as_entity

    @langkit_property()
    def expr_type():
        return If(
            # If this enum literal decl has been found through a primitive env
            # (type of md.primitive matches enum type), then we want  to return
            # the derived type from which we found this enum literal.
            Entity.info.md.primitive == Entity.enum_type.el,

            entity_no_md(
                BaseTypeDecl,
                Entity.info.md.primitive_real_type.cast(BaseTypeDecl),
                Entity.info.rebindings
            ),

            Entity.enum_type
        )

    defining_names = Property(Entity.name.singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Self.name_symbol, Self,
                      dest_env=Entity.enum_type.node_env),

        add_to_env_kv(
            Self.name_symbol, Self,
            dest_env=Entity.enum_type.primitives,
            metadata=Metadata.new(
                dottable_subp=False,
                primitive=Entity.enum_type.el,
                primitive_real_type=No(T.AdaNode),
                access_entity=False
            )
        )
    )


class CharLiteral(BaseId):
    annotations = Annotations(repr_name="Chr")

    @langkit_property()
    def xref_equation():
        return Predicate(BaseTypeDecl.is_not_null_char_type, Self.type_var)


@abstract
class NumLiteral(SingleTokNode):
    annotations = Annotations(repr_name="Num")


class RealLiteral(NumLiteral):
    annotations = Annotations(repr_name="Real")

    @langkit_property()
    def xref_equation():
        return universal_real_bind(Self.type_var)


class IntLiteral(NumLiteral):
    annotations = Annotations(repr_name="Int")

    @langkit_property()
    def xref_equation():
        return universal_int_bind(Self.type_var)

    @langkit_property(return_type=T.BigIntegerType, external=True, public=True,
                      uses_entity_info=False, uses_envs=False)
    def denoted_value():
        """
        Return the value that this literal denotes.
        """
        pass


class NullLiteral(SingleTokNode):
    annotations = Annotations(repr_name="Null")

    @langkit_property()
    def xref_equation():
        return Predicate(BaseTypeDecl.is_access_type, Self.type_var)


class SingleFormal(Struct):
    name = UserField(type=DefiningName)
    spec = UserField(type=BaseFormalParamDecl.entity)


class SingleActual(Struct):
    name = UserField(type=BaseId)
    assoc = UserField(type=T.BasicAssoc)


class ParamMatch(Struct):
    """
    Helper data structure to implement SubpSpec/ParamAssocList matching.

    Each value relates to one ParamAssoc.
    """
    has_matched = UserField(type=BoolType, doc="""
        Whether the matched ParamAssoc a ParamSpec.
    """)
    actual = UserField(type=SingleActual)
    formal = UserField(type=SingleFormal)


@abstract
class BaseSubpSpec(BaseFormalParamHolder):
    name = AbstractProperty(type=T.DefiningName, ignore_warn_on_node=True)
    returns = AbstractProperty(type=T.TypeExpr.entity)

    params = AbstractProperty(
        type=T.ParamSpec.entity.array, public=True, doc="""
        Returns the array of parameters specification for this subprogram spec.
        """
    )

    abstract_formal_params = Property(
        Entity.params.map(lambda p: p.cast(BaseFormalParamDecl))
    )

    @langkit_property(return_type=BoolType)
    def match_return_type(other=T.BaseSubpSpec.entity):
        # Check that the return type is the same. Caveat: it's not because
        # we could not find the canonical type that it is null!
        #
        # TODO: simplify this code when SubpSpec provides a kind to
        # distinguish functions and procedures.
        self_ret = Var(Entity.returns)
        other_ret = Var(other.returns)
        return Or(
            And(other_ret.is_null, self_ret.is_null),
            And(
                Not(other_ret.is_null), Not(self_ret.is_null),
                origin.bind(
                    Self,
                    origin.bind(other.el, other_ret.canonical_type)
                    .matching_type(self_ret.canonical_type)
                )
            )
        )

    @langkit_property(return_type=BoolType)
    def match_signature(other=T.BaseSubpSpec.entity, match_name=BoolType):
        """
        Return whether SubpSpec's signature matches Self's.

        Note that the comparison for types isn't just a name comparison: it
        compares the canonical types.

        If match_name is False, then the name of subprogram will not be
        checked.
        """
        return And(
            # Check that the names are the same
            Not(match_name) | Self.name.matches(other.name),
            Entity.match_return_type(other),
            Entity.match_formal_params(other, match_name),
        )

    @langkit_property(return_type=LexicalEnvType,
                      dynamic_vars=[origin])
    def defining_env():
        """
        Helper for BasicDecl.defining_env.
        """
        return If(Entity.returns.is_null,
                  EmptyEnv, Entity.returns.defining_env)

    @langkit_property(return_type=BaseTypeDecl.entity, dynamic_vars=[origin])
    def potential_dottable_type():
        """
        If self meets the criterias for being a subprogram callable via the dot
        notation, return the type of dottable elements.
        """
        return Entity.params._.at(0)._.type_expr._.element_type

    @langkit_property(return_type=BaseTypeDecl.entity, public=True)
    def primitive_subp_of():
        """
        Return the type of which this subprogram is a primitive of.
        """

        # TODO: This might be improved by checking for spelling before looking
        # up every type.

        bd = Var(Entity.parent.cast_or_raise(BasicDecl))
        params = Var(Entity.unpacked_formal_params)
        types = Var(origin.bind(
            Self,
            params.map(lambda p: p.spec.el_type._.canonical_type)
            .concat(Entity.returns._.designated_type
                    ._.canonical_type._.singleton)
        ))

        return types.find(lambda typ: typ.then(
            lambda typ: typ.declarative_scope.then(lambda ds: ds.any_of(
                bd.declarative_scope,
                bd.declarative_scope._.parent.cast(BasePackageDecl)
                ._.public_part
            ))
        ))

    @langkit_property(return_type=BaseTypeDecl.entity)
    def dottable_subp_of():
        """
        Returns wether the subprogram containing this spec is a subprogram
        callable via the dot notation.
        """
        bd = Var(Entity.parent.cast_or_raise(BasicDecl))
        return origin.bind(Entity.name, If(
            Entity.nb_max_params > 0,
            Entity.potential_dottable_type.then(lambda t: If(
                # Dot notation only works on tagged types, needs to be declared
                # in the same scope as the type.

                # NOTE: We are not actually implementing the correct Ada
                # semantics here, because you can call primitives via the dot
                # notation on private types with a tagged completion.
                # However, since private types don't have components, this
                # should not ever be a problem with legal Ada.
                (t.is_tagged_type | t.private_completion._.is_tagged_type)
                & (t.declarative_scope == bd.declarative_scope),

                t,

                No(T.BaseTypeDecl.entity)
            )),
            No(T.BaseTypeDecl.entity)
        ))

    return_type = Property(Entity.returns._.designated_type)

    xref_entry_point = Property(True)
    xref_equation = Property(Entity.returns._.sub_equation._or(LogicTrue()))


class SubpSpec(BaseSubpSpec):
    subp_kind = Field(type=T.SubpKind)
    subp_name = Field(type=T.DefiningName)
    subp_params = Field(type=T.Params)
    subp_returns = Field(type=T.TypeExpr)

    name = Property(Self.subp_name)

    params = Property(
        Entity.subp_params.then(
            lambda p: p.params.map(lambda p: p),
            default_val=No(T.ParamSpec.entity.array)
        )
    )
    returns = Property(Entity.subp_returns)


class EntryDecl(BasicDecl):
    overriding = Field(type=Overriding)
    spec = Field(type=T.EntrySpec)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.spec.name.as_entity.singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env()
    )


class EntrySpec(BaseSubpSpec):
    entry_name = Field(type=T.DefiningName)
    family_type = Field(type=T.AdaNode)
    entry_params = Field(type=T.Params)

    name = Property(Self.entry_name)
    params = Property(
        Entity.entry_params.then(
            lambda p: p.params.map(lambda p: p),
            default_val=No(T.ParamSpec.entity.array)
        )
    )
    returns = Property(No(T.TypeExpr.entity))


class Quantifier(EnumNode):
    alternatives = ["all", "some"]


class IterType(EnumNode):
    alternatives = ["in", "of"]


@abstract
class LoopSpec(AdaNode):
    pass


class ForLoopVarDecl(BasicDecl):
    id = Field(type=T.DefiningName)
    id_type = Field(type=T.SubtypeIndication)

    defining_names = Property(Entity.id.singleton)

    defining_env = Property(Entity.expr_type.defining_env)

    @langkit_property(memoized=True, call_memoizable=True)
    def expr_type():
        return If(
            Self.id_type.is_null,

            # The type of a for loop variable does not need to be annotated, it
            # can eventually be infered, which necessitates name resolution on
            # the loop specification. Run resolution if necessary.
            Entity.id.expression_type,

            # If there is a type annotation, just return it
            Entity.id_type.designated_type
        )

    env_spec = EnvSpec(add_to_env_kv(Self.name_symbol, Self))


class ForLoopSpec(LoopSpec):
    var_decl = Field(type=T.ForLoopVarDecl)
    loop_type = Field(type=IterType)
    has_reverse = Field(type=Reverse)
    iter_expr = Field(type=T.AdaNode)

    @langkit_property(memoized=True, call_memoizable=True)
    def iter_type():
        p = Var(Entity.iter_expr.resolve_names)
        typ = Var(If(p,
                     Entity.iter_expr.cast_or_raise(T.Expr)
                     .type_var.get_value.cast(T.BaseTypeDecl),
                     No(BaseTypeDecl.entity)))

        return If(
            typ.is_implicit_deref,
            origin.bind(Self, typ.accessed_type),
            typ
        )

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return Self.loop_type.match(

            # This is a for .. in
            lambda _=IterType.alt_in:

            # Let's handle the different possibilities
            Entity.iter_expr.match(
                # Anonymous range case: for I in 1 .. 100
                # In that case, the type of everything is Standard.Integer.
                lambda binop=T.BinOp:
                binop.sub_equation
                & TypeBind(Self.var_decl.id.type_var, binop.type_var),

                # Subtype indication case: the induction variable is of the
                # type.
                lambda t=T.SubtypeIndication:
                TypeBind(Self.var_decl.id.type_var,
                         t.designated_type.canonical_type),

                lambda r=T.AttributeRef:
                r.sub_equation
                & TypeBind(Self.var_decl.id.type_var, r.type_var),

                # Name case: Either the name is a subtype indication, or an
                # attribute on a subtype indication, in which case the logic is
                # the same as above, either it's an expression that yields an
                # iterator.
                lambda t=T.Name: t.name_designated_type.then(
                    lambda typ:
                    TypeBind(Self.var_decl.id.type_var, typ.canonical_type),

                    default_val=Entity.iterator_xref_equation
                ),

                lambda _: LogicTrue()  # should never happen
            ),

            # This is a for .. of
            lambda _=IterType.alt_of: Let(lambda it_typ=Entity.iter_type: If(

                it_typ.is_iterable_type,

                # Equation for the expression
                Entity.iter_expr.sub_equation

                # Then we want the type of the induction variable to be the
                # component type of the type of the expression.
                & TypeBind(Self.var_decl.id.type_var,
                           it_typ.iterable_comp_type)

                # If there is a type annotation, then the type of var should be
                # conformant.
                & If(Self.var_decl.id_type.is_null,
                     LogicTrue(),
                     TypeBind(Self.var_decl.id.type_var,
                              Entity.var_decl.id_type.designated_type)),

                LogicFalse()
            ))
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def iterator_xref_equation():
        iter_expr = Var(Entity.iter_expr.cast_or_raise(T.Expr))

        p = Var(iter_expr.resolve_names_internal(
            True,
            Predicate(BaseTypeDecl.is_iterator_type,
                      iter_expr.type_var)
        ))

        return If(
            p,
            TypeBind(
                Self.var_decl.id.type_var,
                iter_expr.type_var.get_value
                .children_env.get_first('Cursor').cast_or_raise(T.BaseTypeDecl)
            ),
            LogicFalse()
        )

    xref_entry_point = Property(True)


class QuantifiedExpr(Expr):
    quantifier = Field(type=Quantifier)
    loop_spec = Field(type=T.ForLoopSpec)
    expr = Field(type=T.Expr)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        spec_success = Var(Entity.loop_spec.resolve_names)

        return If(
            spec_success,
            Entity.expr.sub_equation
            & bool_bind(Entity.expr.type_var),
            LogicFalse()
        )


class Allocator(Expr):
    subpool = Field(type=T.Name)
    type_or_expr = Field(type=T.AdaNode)

    @langkit_property()
    def get_allocated_type():
        return origin.bind(Self, Entity.type_or_expr.match(
            lambda t=SubtypeIndication.entity: t.designated_type,
            lambda q=QualExpr.entity: q.designated_type,
            lambda _: No(BaseTypeDecl.entity)
        ))

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return (
            Entity.type_or_expr.sub_equation
            & Predicate(BaseTypeDecl.matching_allocator_type,
                        Self.type_var, Entity.get_allocated_type)
        )


class QualExpr(Name):
    prefix = Field(type=T.Name)
    suffix = Field(type=T.Expr)

    ref_var = Property(Self.prefix.ref_var)

    relative_name = Property(Entity.prefix.relative_name)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        typ = Entity.prefix.designated_type_impl

        return (
            Entity.suffix.sub_equation
            & Bind(Self.prefix.ref_var, typ)
            & TypeBind(Self.prefix.type_var, typ)
            & TypeBind(Self.suffix.type_var, typ)
            & TypeBind(Self.type_var, typ)
        )

    # TODO: once we manage to turn prefix into a subtype indication, remove
    # this property and update Allocator.get_allocated type to do:
    # q.prefix.designated_type.
    designated_type = Property(
        env.bind(Entity.node_env,
                 origin.bind(Self, Entity.designated_type_impl)),
    )
    designated_type_impl = Property(Entity.prefix.designated_type_impl)


class AttributeRef(Name):
    prefix = Field(type=T.Name)
    attribute = Field(type=T.Identifier)
    args = Field(type=T.AdaNode)

    ref_var = Property(Self.prefix.ref_var)
    relative_name = Property(Entity.prefix.relative_name)

    designated_type_impl = Property(
        If(Self.attribute.sym == 'Class',
           Entity.prefix.designated_type_impl._.classwide_type,
           Entity.prefix.designated_type_impl)
    )

    @langkit_property()
    def env_elements_impl():
        return If(
            Self.attribute.sym == 'Unrestricted_Access',
            Entity.prefix.env_elements_impl.map(
                lambda e:
                # Using unrestricted accesses, the entities are actually
                # anonymous access to entities, so mark the entities as such.
                e.cast_or_raise(T.BasicDecl).trigger_access_entity(True)
            ),
            No(T.AdaNode.entity.array),
        )

    is_access_attr = Property(
        Entity.attribute.name_symbol.any_of(
            'Access', 'Unchecked_Access', 'Unrestricted_Access'
        )
    )

    @langkit_property()
    def designated_env():
        return Cond(
            Entity.attribute.name_symbol == 'Model',
            Entity.designated_env_model_attr,

            Entity.is_access_attr,
            Entity.prefix.designated_env,

            Entity.attribute.name_symbol == 'Result',
            Self.parents.find(lambda p: p.is_a(BasicSubpDecl, SubpBody))
            .as_entity.cast(T.BasicDecl).subp_spec_or_null
            .return_type.defining_env,

            EmptyEnv
        )

    @langkit_property(return_type=LexicalEnvType, dynamic_vars=[env, origin])
    def designated_env_model_attr():
        model_types = Var(
            Entity.prefix.env_elements
            .map(lambda e: e.cast_or_raise(T.BasicDecl).expr_type)
            .map(lambda t: t.modeled_type(Self.unit))
            .filter(lambda t: Not(t.is_null))
        )

        return model_types.map(lambda mt: mt.defining_env).env_group()

    @langkit_property()
    def xref_equation():
        rel_name = Var(Entity.attribute.name_symbol)
        return Cond(
            rel_name.any_of('Succ', 'Pred'), Entity.succpred_xref_equation,
            rel_name.any_of('Min', 'Max'), Entity.minmax_equation,

            rel_name.any_of('First', 'Last', 'Range', 'Length'),
            Entity.array_attr_equation,

            rel_name == 'Size', Entity.size_equation,
            rel_name == 'Pos', Entity.pos_equation,
            rel_name == 'Val', Entity.val_equation,

            rel_name.any_of('Access',
                            'Unchecked_Access', 'Unrestricted_Access'),
            Entity.access_equation,

            rel_name == 'Image',
            Entity.image_equation(Self.std_entity('String')),

            rel_name == 'Wide_Image',
            Entity.image_equation(Self.std_entity('Wide_String')),

            rel_name == 'Wide_Wide_Image',
            Entity.image_equation(Self.std_entity('Wide_Wide_String')),

            rel_name == 'Value',
            Entity.value_equation(Self.std_entity('String')),

            rel_name == 'Wide_Value',
            Entity.value_equation(Self.std_entity('Wide_String')),

            rel_name == 'Wide_Wide_Value',
            Entity.value_equation(Self.std_entity('Wide_Wide_String')),

            rel_name == 'Aft', Entity.aft_equation,
            rel_name == 'Identity', Entity.identity_equation,
            rel_name == 'Address', Entity.address_equation,

            rel_name.any_of('Maximum_Alignment', 'Word_Size'),
            Entity.standard_attr_equation,

            rel_name.any_of('Small', 'Large'),
            Entity.universal_real_equation,

            rel_name == 'Img',
            Entity.img_equation(Self.std_entity('String')),

            rel_name.any_of('Write', 'Read', 'Output'),
            Entity.stream_attrs_equation(False),

            rel_name == 'Input', Entity.stream_attrs_equation(True),

            rel_name == 'Tag', Entity.tag_attr_equation,

            rel_name == 'Result', Entity.result_attr_equation,
            rel_name == 'Old',    Entity.old_attr_equation,

            rel_name == 'Class',    Entity.prefix.sub_equation,

            rel_name == 'Valid',
            Entity.prefix.sub_equation
            & bool_bind(Self.type_var),

            # Lal checkers specific
            rel_name == 'Model', Entity.model_attr_equation,

            LogicTrue()
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def model_attr_equation():
        return (
            Entity.prefix.sub_equation
            & Self.type_var.domain(
                Self.top_level_item(Self.unit)
                .cast_or_raise(T.PackageDecl).public_part
                .types_with_models.map(lambda t: t.cast(T.AdaNode))
            )
            & TypeBind(Self.type_var, Self.prefix.type_var,
                       conv_prop=BaseTypeDecl.model_of_type)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def old_attr_equation():
        return And(
            Entity.prefix.sub_equation,
            TypeBind(Self.type_var, Self.prefix.type_var),
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def result_attr_equation():
        containing_subp = Var(Self.parents.find(
            lambda p: p.is_a(BasicSubpDecl, BaseSubpBody)
        ).as_entity.cast(T.BasicDecl))

        returns = Var(containing_subp.subp_spec_or_null.then(
            lambda ss: ss.return_type
        ))

        return And(
            TypeBind(Self.type_var, returns),
            Bind(Entity.prefix.ref_var, containing_subp)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def tag_attr_equation():
        tag_type = Var(
            Entity
            .get_compilation_unit(['Ada', 'Tags'], UnitSpecification)
            ._.children_env.get_first('Tag', recursive=False)
            .cast(T.BaseTypeDecl)
        )

        return (
            # Prefix is an expression, bind prefix's ref var to it
            Entity.prefix.xref_equation

            # Type of self is String
            & TypeBind(Self.type_var, tag_type)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def stream_attrs_equation(return_obj=(BoolType, False)):
        typ = Var(Entity.prefix.name_designated_type)

        root_stream_type = Var(
            Entity
            .get_compilation_unit(['Ada', 'Streams'], UnitSpecification)
            ._.children_env.get_first('Root_Stream_Type', recursive=False)
            .cast(T.BaseTypeDecl).classwide_type.cast(T.BaseTypeDecl)
        )

        stream_arg = Var(Entity.args.cast_or_raise(T.AssocList).at(0).expr)
        obj_arg = Var(Entity.args.cast_or_raise(T.AssocList).at(1)._.expr)

        return (
            Entity.prefix.sub_equation
            & stream_arg.as_entity.sub_equation
            & TypeBind(stream_arg.type_var,
                       root_stream_type.anonymous_access_type)
            & If(
                return_obj,
                TypeBind(Self.type_var, typ),
                TypeBind(obj_arg.type_var, typ)
                & obj_arg.as_entity.sub_equation
            )
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def address_equation():
        address_type = Var(
            Entity
            .get_compilation_unit(['System'], UnitSpecification)
            ._.children_env.get_first('Address', recursive=False)
            .cast(T.BaseTypeDecl)
        )
        return (Entity.prefix.sub_equation
                & TypeBind(Self.type_var, address_type))

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def identity_equation():
        # NOTE: We don't verify that the prefix designates an exception
        # declaration, because that's legality, not name resolution.
        return (Entity.prefix.sub_equation
                & TypeBind(Self.prefix.ref_var, Self.type_var,
                           conv_prop=BasicDecl.identity_type))

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def universal_real_equation():
        return (
            universal_real_bind(Self.type_var)
            & Entity.prefix.sub_equation
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def standard_attr_equation():
        return (
            # TODO: run the equation of the prefix (std package), does not
            # work for the moment because the architecture is wrong.
            universal_int_bind(Self.type_var)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def succpred_xref_equation():
        typ = Var(Entity.prefix.name_designated_type)
        arg = Var(Self.args.cast_or_raise(T.AssocList).at(0).expr)

        return (
            TypeBind(Self.prefix.ref_var, typ)
            & TypeBind(arg.type_var, typ)
            & TypeBind(Self.type_var, typ)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def minmax_equation():
        typ = Var(Entity.prefix.name_designated_type)
        left = Var(Entity.args.cast_or_raise(T.AssocList).at(0).expr)
        right = Var(Entity.args.cast_or_raise(T.AssocList).at(1).expr)

        return (
            left.as_entity.sub_equation
            & right.as_entity.sub_equation
            # Prefix is a type, bind prefix's ref var to it
            & TypeBind(Self.prefix.ref_var, typ)
            & TypeBind(left.type_var, right.type_var)
            & TypeBind(Self.type_var, left.type_var)
            & TypeBind(Self.type_var, typ)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def aft_equation():
        typ = Var(Entity.prefix.name_designated_type)

        return (
            # Prefix is a type, bind prefix's ref var to it
            Bind(Self.prefix.ref_var, typ)
            & universal_int_bind(Self.type_var)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def value_equation(str_type=T.AdaNode.entity):
        typ = Var(Entity.prefix.name_designated_type)
        expr = Var(Self.args.cast_or_raise(T.AssocList).at(0).expr)

        return (
            expr.as_entity.sub_equation

            # Prefix is a type, bind prefix's ref var to it
            & Bind(Self.prefix.ref_var, typ)

            # Type of expression is str_type
            & TypeBind(expr.type_var, str_type)

            # Type of self is designated type
            & TypeBind(Self.type_var, typ)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def image_equation(str_type=T.AdaNode.entity):
        typ = Var(Entity.prefix.name_designated_type)
        expr = Var(Self.args.cast(T.AssocList).then(lambda al: al.at(0).expr))

        return If(
            typ.is_null,

            # If prefix is not a type, then it is an expression
            Entity.prefix.sub_equation
            & TypeBind(Self.type_var, str_type),

            expr.as_entity.sub_equation
            # Prefix is a type, bind prefix's ref var to it
            & Bind(Self.prefix.ref_var, typ)
            # Type of expression is designated type
            & TypeBind(expr.type_var, typ)
            # Type of self is String
            & TypeBind(Self.type_var, str_type)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def img_equation(str_type=T.AdaNode.entity):
        return (
            # Prefix is an expression, bind prefix's ref var to it
            Entity.prefix.xref_equation

            # Type of self is String
            & TypeBind(Self.type_var, str_type)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def pos_equation():
        typ = Var(Entity.prefix.name_designated_type)
        expr = Var(Self.args.cast_or_raise(T.AssocList).at(0).expr)

        return (
            # Prefix is a type, bind prefix's ref var to it
            Bind(Self.prefix.ref_var, typ)
            & universal_int_bind(Self.type_var)
            & Bind(expr.type_var, typ)
            & expr.as_entity.sub_equation
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def val_equation():
        typ = Var(Entity.prefix.name_designated_type)
        expr = Var(Self.args.cast_or_raise(T.AssocList).at(0).expr)
        return (
            # Prefix is a type, bind prefix's ref var to it
            Bind(Self.prefix.ref_var, typ)
            & TypeBind(Self.type_var, typ)
            & universal_int_bind(expr.type_var)
            & expr.as_entity.sub_equation
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def access_equation():
        return Or(
            # Access to
            Entity.prefix.xref_no_overloading(all_els=True)
            & Predicate(BaseTypeDecl.is_subp_access_of,
                        Self.type_var,
                        Self.prefix.ref_var),

            Entity.prefix.xref_equation
            & If(
                Entity.attribute.name_symbol == 'Unrestricted_Access',
                Bind(Self.prefix.type_var,
                     Self.type_var,
                     conv_prop=BaseTypeDecl.anonymous_access_type,
                     eq_prop=BaseTypeDecl.matching_prefix_type),

                Bind(Self.type_var,
                     Self.prefix.type_var,
                     conv_prop=BaseTypeDecl.accessed_type,
                     eq_prop=BaseTypeDecl.matching_formal_type_inverted),
            )
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def size_equation():
        typ = Var(Entity.prefix.name_designated_type)
        return If(
            Not(typ.is_null),

            Bind(Self.prefix.ref_var, typ)
            & universal_int_bind(Self.type_var),

            Entity.prefix.sub_equation
            & universal_int_bind(Self.type_var)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def array_attr_equation():
        is_length = Var(Entity.attribute.name_symbol == 'Length')
        typ = Var(Entity.prefix.name_designated_type)

        # If the range attribute has an argument, then it's a static expression
        # representing an int that we will use as a dimension.
        dim = Var(Self.args.then(
            lambda args:
            args.cast_or_raise(T.AssocList).at(0).expr.eval_as_int.as_int,
            default_val=1
        ) - 1)

        return If(
            Not(typ.is_null),

            # Prefix is a type
            Bind(Self.prefix.ref_var, typ) & Cond(
                typ.is_array & is_length, universal_int_bind(Self.type_var),

                # If it's an array, take the appropriate index type
                typ.is_array, TypeBind(Self.type_var, typ.index_type(dim)),

                # If it's a discrete type, then bind to the discrete type
                typ.is_discrete_type | typ.is_real_type & Not(is_length),

                TypeBind(Self.type_var, typ),

                LogicFalse()
            ),

            # Prefix is not a type: In that case we have permission to resolve
            # prefix separately.
            Let(lambda
                res=Entity.prefix.resolve_names_internal(True, LogicTrue()),
                pfx_typ=Entity.prefix.type_val.cast(T.BaseTypeDecl):

                If(res & Not(pfx_typ.array_def_with_deref.is_null),
                   If(is_length,
                      universal_int_bind(Self.type_var),
                      TypeBind(Self.type_var, pfx_typ.index_type(dim))),
                   LogicFalse()))
        )


class UpdateAttributeRef(AttributeRef):
    pass


class RaiseExpr(Expr):
    exception_name = Field(type=T.Name)
    error_message = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return Entity.exception_name.sub_equation


class DottedName(Name):
    prefix = Field(type=T.Name)
    suffix = Field(type=T.BaseId)
    ref_var = Property(Self.suffix.ref_var)

    @langkit_property()
    def designated_env():
        pfx_env = Var(Entity.prefix.designated_env)
        return env.bind(pfx_env,
                        Entity.suffix.designated_env_impl)

    @langkit_property()
    def all_env_els_impl(seq=(BoolType, True),
                         seq_from=(AdaNode, No(T.AdaNode))):
        pfx_env = Var(Entity.prefix.designated_env)
        return env.bind(pfx_env, Entity.suffix.all_env_els_impl(seq, seq_from))

    scope = Property(Self.suffix.then(
        lambda sfx: env.bind(Self.parent_scope, sfx.scope),
        default_val=EmptyEnv
    ))

    parent_scope = Property(Self.prefix.scope)

    relative_name = Property(Entity.suffix.relative_name)
    base_name = Property(Entity.prefix)

    @langkit_property()
    def env_elements_impl():
        pfx_env = Var(origin.bind(Self, Entity.prefix.designated_env))
        return env.bind(pfx_env, Entity.suffix.env_elements_baseid)

    @langkit_property()
    def designated_type_impl():
        return env.bind(Entity.prefix.designated_env,
                        Entity.suffix.designated_type_impl)

    @langkit_property()
    def xref_equation():
        base = Var(Entity.prefix.sub_equation
                   & env.bind(Entity.prefix.designated_env,
                              Entity.suffix.sub_equation))

        return If(
            Not(Entity.designated_type_impl.is_null),
            base,
            base & Entity.env_elements.logic_any(lambda e: (
                Bind(Self.suffix.ref_var, e)
                & e.cast(BasicDecl.entity).constrain_prefix(Self.prefix)
                & TypeBind(Self.type_var, Self.suffix.type_var)
            ))
        )


class CompilationUnit(AdaNode):
    """Root node for all Ada analysis units."""
    prelude = Field(doc="``with``, ``use`` or ``pragma`` statements.")
    body = Field(type=T.AdaNode)
    pragmas = Field(type=T.Pragma.list)
    no_env = UserField(type=T.LexicalEnvType, public=False)

    @langkit_property(external=True, uses_entity_info=False, uses_envs=True,
                      return_type=LexicalEnvType)
    def get_empty_env():
        """
        Returns an empty env to use in env specs. This is meant as an
        optimization: Langkit referenced envs that return empty env can never
        be cached, so we used a CompilationUnit specific empty env, that will
        live for the same duration as its analysis unit, and then be
        invalidated.
        """
        pass

    @langkit_property(public=True)
    def fully_qualified_name():
        """
        Return the fully qualified name corresponding to this declaration.
        """
        return Self.body.match(
            lambda li=T.LibraryItem: li.item.fully_qualified_name,
            lambda su=T.Subunit: su.fully_qualified_name,
            lambda _: PropertyError(
                SymbolType.array, 'Unexpected CompilationUnit.f_body attribute'
            ),
        )

    @langkit_property(public=True)
    def unit_kind():
        """
        Return the kind corresponding to this analysis unit.
        """
        return Self.body.match(
            lambda li=T.LibraryItem: li.item.match(
                lambda _=T.Body: UnitBody,
                lambda _: UnitSpecification
            ),
            lambda _=T.Subunit: UnitBody,
            lambda _: PropertyError(
                AnalysisUnitKind, 'Unexpected CompilationUnit.f_body attribute'
            ),
        )

    env_spec = EnvSpec(
        set_initial_env(Let(
            lambda n=Self.body.cast(T.LibraryItem).then(
                lambda i: i.item.as_bare_entity.defining_name
            ):

            Cond(
                Self.body.is_a(T.Subunit), Self.std_env,

                n.is_null, Self.initial_env,

                # If self is Standard package, then register self in the root
                # env.
                n.name.is_a(T.BaseId) & (n.name_symbol == 'Standard'),
                Self.initial_env,

                Self.std_env
            )
        ))
    )


@abstract
class BaseSubpBody(Body):
    env_spec = EnvSpec(
        call_env_hook(Self),

        set_initial_env(
            env.bind(Self.initial_env, Entity.body_scope(False)),
        ),

        # Add the body to its own parent env
        add_to_env_kv(Entity.name_symbol, Self,
                      dest_env=env.bind(Self.initial_env,
                                        Entity.body_scope(False))),

        add_env(transitive_parent=True),
        ref_used_packages(),

        # If Self, which is assumed to be a SubpBody, is a library-level
        # subprogram, it must "inherit" the use clauses of its declaration, if
        # there is one.
        reference(
            Self.cast(T.AdaNode).to_array,
            through=T.AdaNode.use_packages_in_spec_of_subp_body,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        ref_generic_formals(),

        handle_children(),

        # Add the __body link to the spec, if there is one
        add_to_env_kv(
            '__body', Self,
            dest_env=Entity.decl_part_entity.then(
                lambda d: d.el.children_env
            ),
        ),

        # Adding subp to the type's environment if the type is tagged and self
        # is a primitive of it.
        add_to_env(
            T.env_assoc.new(key=Entity.name_symbol, val=Self),
            dest_env=Self.as_bare_entity.subp_spec
            .dottable_subp_of._.children_env,
            # We pass custom metadata, marking the entity as a dottable
            # subprogram.
            metadata=Metadata.new(dottable_subp=True,
                                  primitive=No(T.AdaNode),
                                  primitive_real_type=No(T.AdaNode),
                                  access_entity=False)
        ),

        # Adding subp to the primitives env if the subp is a primitive. TODO:
        # Ada allows a subprogram to be a primitive of several types. This is
        # not handled for the moment, due to the limitations of the current env
        # spec format. We could modify dest_env to take an array optionally,
        # but that's one more kludge to the pile.

        add_to_env(
            T.env_assoc.new(key=Entity.name_symbol, val=Self),
            dest_env=(Self.as_bare_entity.subp_spec
                      .primitive_subp_of.cast(T.TypeDecl)._.primitives),
            metadata=Metadata.new(
                dottable_subp=False,
                primitive=(Self.as_bare_entity.subp_spec
                           .primitive_subp_of.cast(T.AdaNode).el),
                primitive_real_type=No(T.AdaNode),
                access_entity=False
            )
        )
    )

    overriding = Field(type=Overriding)
    subp_spec = Field(type=T.SubpSpec)

    defining_names = Property(Self.subp_spec.name.as_entity.singleton)

    @langkit_property(return_type=BoolType,
                      dynamic_vars=[origin])
    def in_scope():
        """
        Return True if ``origin`` is directly in the scope of this Subprogram
        body.
        """
        return And(
            origin.unit == Self.unit,
            Not(origin.parents.find(lambda p: p == Self).is_null)
        )

    @langkit_property(return_type=LexicalEnvType,
                      dynamic_vars=[origin])
    def defining_env():
        return If(
            Entity.in_scope,

            If(
                Entity.subp_spec_or_null
                ._.paramless(Entity.info.md, can_be=True),
                Array([
                    Entity.children_env, Entity.subp_spec.defining_env
                ]).env_group(),
                Entity.children_env
            ),

            Entity.subp_spec.defining_env
        )

    type_expression = Property(Entity.subp_spec.returns)


class ExprFunction(BaseSubpBody):
    expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    xref_equation = Property(
        Entity.expr.sub_equation
        & TypeBind(Entity.expr.type_var, Entity.subp_spec.return_type)
    )

    xref_entry_point = Property(True)


class SubpBody(BaseSubpBody):

    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)


class HandledStmts(AdaNode):
    annotations = Annotations(snaps=True)

    stmts = Field(type=T.StmtList)
    exceptions = Field(type=T.AdaNode.list)


class ExceptionHandler(BasicDecl):
    exception_name = Field(type=T.DefiningName)
    handled_exceptions = Field(type=T.AlternativesList)
    stmts = Field(type=T.StmtList)

    env_spec = EnvSpec(
        add_env(),
        add_to_env(
            env_mappings(Entity.exception_name.then(lambda n: n.singleton),
                         Self),
            dest_env=Self.children_env
        )
    )

    defining_names = Property(Entity.exception_name.singleton)

    @langkit_property()
    def expr_type():
        return (
            Entity
            .get_compilation_unit(['Ada', 'Exceptions'], UnitSpecification)
            ._.children_env.get_first('Exception_Occurrence', recursive=False)
            .cast(T.BaseTypeDecl)
        )

    xref_equation = Property(
        Self.handled_exceptions.logic_all(lambda he: he.as_entity.sub_equation)
    )

    xref_entry_point = Property(True)


@abstract
class Stmt(AdaNode):
    xref_entry_point = Property(True)


class ErrorStmt(Stmt):
    pass


@abstract
class SimpleStmt(Stmt):
    pass


@abstract
class CompositeStmt(Stmt):
    pass


class CallStmt(SimpleStmt):
    """
    Statement for entry or procedure calls.
    """
    call = Field(type=T.Name)

    @langkit_property()
    def xref_equation():
        return (
            Entity.call.sub_equation

            # Call statements can have no return value
            & Bind(Self.call.type_var, No(AdaNode.entity))
        )


class NullStmt(SimpleStmt):
    @langkit_property()
    def xref_equation():
        return LogicTrue()


class AssignStmt(SimpleStmt):
    dest = Field(type=T.Name)
    expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return (
            Entity.dest.sub_equation
            & Entity.expr.sub_equation
            & Bind(Self.expr.type_var, Self.dest.type_var,
                   eq_prop=BaseTypeDecl.matching_assign_type)
        )


class GotoStmt(SimpleStmt):
    label_name = Field(type=T.Name)

    @langkit_property()
    def xref_equation():
        return Entity.label_name.xref_no_overloading(sequential=False)


class ExitStmt(SimpleStmt):
    loop_name = Field(type=T.Identifier)
    cond_expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.cond_expr.then(lambda cond: (
                cond.sub_equation
                & bool_bind(cond.type_var)
            ), default_val=LogicTrue()),

            Entity.loop_name.then(
                lambda ln: ln.xref_no_overloading,
                default_val=LogicTrue()
            )
        )


class ReturnStmt(SimpleStmt):
    return_expr = Field(type=T.Expr)

    subp = Property(
        Self.parents.find(lambda p: p.is_a(SubpBody)).cast(SubpBody).as_entity,
        doc="Returns the subprogram this return statement belongs to"
    )

    @langkit_property()
    def xref_equation():
        return Entity.return_expr.then(
            lambda rexpr:
            rexpr.sub_equation
            & Bind(
                rexpr.type_var,
                Entity.subp.subp_spec.returns.designated_type,
                eq_prop=BaseTypeDecl.matching_assign_type
            )
        )._or(LogicTrue())


class RequeueStmt(SimpleStmt):
    call_name = Field(type=T.Expr)
    has_abort = Field(type=Abort)


class AbortStmt(SimpleStmt):
    names = Field(type=T.Name.list)

    @langkit_property()
    def xref_equation():
        return Entity.names.logic_all(
            lambda name:
            name.sub_equation & Predicate(BaseTypeDecl.is_task_type,
                                          name.type_var)
        )


class DelayStmt(SimpleStmt):
    has_until = Field(type=Until)
    expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return Entity.expr.sub_equation & TypeBind(
            Self.expr.type_var, Self.std_entity('Duration')
        )


class RaiseStmt(SimpleStmt):
    exception_name = Field(type=T.Name)
    error_message = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.exception_name.then(
                lambda en: en.sub_equation,
                default_val=LogicTrue()
            ),
            Entity.error_message.then(
                lambda er: And(
                    Predicate(BaseTypeDecl.is_str_type_or_null, er.type_var),
                    er.sub_equation
                ),
                default_val=LogicTrue()
            )
        )


class IfStmt(CompositeStmt):
    cond_expr = Field(type=T.Expr)
    then_stmts = Field(type=T.StmtList)
    alternatives = Field(type=T.ElsifStmtPart.list)
    else_stmts = Field(type=T.StmtList)

    @langkit_property()
    def xref_equation():
        return (
            Entity.cond_expr.sub_equation
            & bool_bind(Self.cond_expr.type_var)
            & Entity.alternatives.logic_all(
                lambda elsif: elsif.cond_expr.sub_equation
                & bool_bind(elsif.cond_expr.type_var)
            )
        )


class ElsifStmtPart(AdaNode):
    cond_expr = Field(type=T.Expr)
    stmts = Field(type=T.StmtList)


class LabelDecl(BasicDecl):
    name = Field(type=T.DefiningName)
    defining_names = Property(Entity.name.singleton)

    env_spec = EnvSpec(add_to_env_kv(Self.name_symbol, Self))


class Label(SimpleStmt):
    decl = Field(type=T.LabelDecl)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return LogicTrue()


class WhileLoopSpec(LoopSpec):
    expr = Field(type=T.Expr)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return Entity.expr.sub_equation & (
            bool_bind(Self.expr.type_var)
        )


class NamedStmtDecl(BasicDecl):
    """
    BasicDecl that is always the declaration inside a named statement.
    """
    name = Field(type=T.DefiningName)
    defining_names = Property(Entity.name.singleton)
    defining_env = Property(Self.parent.cast(T.NamedStmt).stmt.children_env)


class NamedStmt(CompositeStmt):
    """
    Wrapper class, used for composite statements that can be named (declare
    blocks, loops). This allows to both have a BasicDecl for the named entity
    declared, and a CompositeStmt for the statement hierarchy.
    """
    decl = Field(type=T.NamedStmtDecl)
    stmt = Field(type=T.CompositeStmt)

    env_spec = EnvSpec(
        add_to_env_kv(Self.decl.name_symbol, Self.decl),
        add_env()
    )

    xref_equation = Property(LogicTrue())


@abstract
class BaseLoopStmt(CompositeStmt):
    spec = Field(type=T.LoopSpec)
    stmts = Field(type=T.StmtList)
    end_name = Field(type=T.EndName)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return Entity.spec._.xref_equation._or(LogicTrue())


class LoopStmt(BaseLoopStmt):
    pass


class ForLoopStmt(BaseLoopStmt):
    env_spec = EnvSpec(add_env())


class WhileLoopStmt(BaseLoopStmt):
    pass


@abstract
class BlockStmt(CompositeStmt):
    env_spec = EnvSpec(add_env())

    xref_equation = Property(LogicTrue())


class DeclBlock(BlockStmt):
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)


class BeginBlock(BlockStmt):
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)


class ExtendedReturnStmt(CompositeStmt):
    decl = Field(type=T.ExtendedReturnStmtObjectDecl)
    stmts = Field(type=T.HandledStmts)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return LogicTrue()

    env_spec = EnvSpec(add_env())


class CaseStmt(CompositeStmt):
    expr = Field(type=T.Expr)
    alternatives = Field(type=T.CaseStmtAlternative.list)

    @langkit_property()
    def xref_equation():
        ignore(Var(Entity.expr.resolve_names_internal(
            True, Predicate(BaseTypeDecl.is_discrete_type,
                            Self.expr.type_var)
        )))

        return Entity.alternatives.logic_all(lambda alt: (
            alt.choices.logic_all(lambda c: c.match(
                # Expression case
                lambda e=T.Expr:
                TypeBind(e.type_var, Self.expr.type_val) & e.sub_equation,

                # TODO: Bind other cases: SubtypeIndication and Range
                lambda _: LogicTrue()
            ))
        ))


class CaseStmtAlternative(AdaNode):
    choices = Field(type=T.AlternativesList)
    stmts = Field(type=T.StmtList)


class AcceptStmt(CompositeStmt):
    name = Field(type=T.Identifier)
    entry_index_expr = Field(type=T.Expr)
    params = Field(type=T.Params)

    env_spec = EnvSpec(add_env())

    xref_equation = Property(LogicTrue())


class AcceptStmtWithStmts(AcceptStmt):
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    xref_equation = Property(LogicTrue())


class SelectStmt(CompositeStmt):
    guards = Field(type=T.SelectWhenPart.list)
    else_stmts = Field(type=T.StmtList)
    abort_stmts = Field(type=T.StmtList)

    @langkit_property()
    def xref_equation():
        return Entity.guards.logic_all(lambda wp: wp.sub_equation)


class SelectWhenPart(AdaNode):
    cond_expr = Field(type=T.Expr)
    stmts = Field(type=T.StmtList)

    @langkit_property()
    def xref_equation():
        return Entity.cond_expr.then(
            lambda c:
            c.sub_equation & bool_bind(Self.cond_expr.type_var),
            default_val=LogicTrue()
        )


class TerminateAlternative(SimpleStmt):
    xref_equation = Property(LogicTrue())


class PackageBody(Body):
    env_spec = child_unit(
        '__body',
        Entity.body_scope(True),

        # Add the __body link to the package decl
        dest_env=env.bind(
            Self.initial_env,
            # If this is a sub package, sub_package
            Entity.body_decl_scope

            ._or(Entity.body_scope(False))
        ),

        transitive_parent=True,

        more_rules=[
            reference(Self.cast(AdaNode).singleton,
                      through=T.PackageBody.subunit_pkg_decl,
                      cond=Not(Self.subunit_root.is_null)),

            reference(Self.cast(AdaNode).singleton,
                      through=T.Body.body_decl_scope,
                      transitive=True)
        ]
    )

    package_name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.package_name.singleton)
    defining_env = Property(Entity.children_env)

    @langkit_property()
    def subunit_pkg_decl():
        return env.bind(
            Self.subunit_root._.children_env,
            Entity.defining_name.scope
        )

    @langkit_property()
    def decl_part_entity():
        """
        Return the BasePackageDecl corresponding to this node.

        If the case of generic package declarations, this returns the
        "package_decl" field instead of the GenericPackageDecl itself.
        """
        return env.bind(
            Entity.node_env,
            Entity.package_name.env_elements.at(0)._.match(
                lambda pkg_decl=T.PackageDecl: pkg_decl,
                lambda gen_pkg_decl=T.GenericPackageDecl:
                    gen_pkg_decl.package_decl,
                lambda _: No(T.BasicDecl.entity)
            )
        )


class TaskBody(Body):
    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.name.singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env(),
    )

    @langkit_property()
    def task_type():
        return Entity.parent.node_env.get(Entity.name_symbol).find(
            lambda sp: sp.is_a(T.TaskTypeDecl)
        ).cast(T.TaskTypeDecl)


class ProtectedBody(Body):
    env_spec = child_unit(
        '__body',
        Entity.body_scope(True),

        # Add the __body link to the package decl
        dest_env=env.bind(
            Self.initial_env,
            # If this is a sub package, sub_package
            Entity.body_decl_scope

            ._or(Entity.body_scope(False))
        ),

        more_rules=[
            reference(Self.cast(AdaNode).singleton,
                      through=T.Body.body_decl_scope,
                      transitive=True)
        ]
    )

    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.name.singleton)


class EntryBody(Body):
    entry_name = Field(type=T.DefiningName)
    index_spec = Field(type=T.EntryIndexSpec)
    params = Field(type=T.Params)
    barrier = Field(type=T.Expr)

    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.entry_name.singleton)

    env_spec = EnvSpec(add_env())


class EntryIndexSpec(AdaNode):
    id = Field(type=T.Identifier)
    subtype = Field(type=T.AdaNode)


class Subunit(AdaNode):
    name = Field(type=T.Name)
    body = Field(type=T.Body)

    @langkit_property(public=True)
    def fully_qualified_name():
        """
        Return the fully qualified name corresponding to this subunit.
        """
        return Self.name.as_symbol_array.concat(
            Self.body.fully_qualified_name
        )


class ProtectedBodyStub(BodyStub):
    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)


class SubpBodyStub(BodyStub):
    overriding = Field(type=Overriding)
    subp_spec = Field(type=T.SubpSpec)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.subp_spec.name.as_entity.singleton)
    # Note that we don't have to override the defining_env property here since
    # what we put in lexical environment is their SubpSpec child.

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        # TODO: If subp body stubs can be separates, we need to handle that
        # here.
        add_env(),
    )

    type_expression = Property(Entity.subp_spec.returns)


class PackageBodyStub(BodyStub):
    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)


class TaskBodyStub(BodyStub):
    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)


class LibraryItem(AdaNode):
    has_private = Field(type=Private)
    item = Field(type=T.BasicDecl)


class RangeSpec(AdaNode):
    range = Field(type=Expr)

    xref_equation = Property(Entity.range.xref_equation)


class IncompleteTypeDecl(BaseTypeDecl):
    discriminants = Field(type=T.DiscriminantPart)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env()
    )

    discriminants_list = Property(Entity.discriminants.abstract_formal_params)


class IncompleteTaggedTypeDecl(IncompleteTypeDecl):
    has_abstract = Field(type=Abstract)


class Params(AdaNode):
    params = Field(type=ParamSpec.list)


class ParentList(Name.list):
    pass


class DiscriminantChoiceList(Identifier.list):
    pass


class AlternativesList(AdaNode.list):
    pass


class ExprAlternativesList(Expr.list):
    pass


class ConstraintList(AdaNode.list):
    pass


class UnconstrainedArrayIndex(AdaNode):
    subtype_indication = Field(type=SubtypeIndication)

    @langkit_property(dynamic_vars=[origin])
    def designated_type():
        return Entity.subtype_indication.designated_type
