from __future__ import absolute_import, division, print_function

from langkit.dsl import (
    AnalysisUnitKind, AnalysisUnitType, ASTNode, BoolType, EnumNode,
    EquationType, Field, LexicalEnvType, LogicVarType, LongType, Struct,
    Symbol, T, UserField, abstract, synthetic, env_metadata,
    has_abstract_list, Annotations
)
from langkit.envs import (
    EnvSpec, reference, add_to_env, add_env, handle_children, set_initial_env,
    call_env_hook, do
)
from langkit.expressions import (
    AbstractKind, AbstractProperty, And, Bind, DynamicVariable, EmptyArray,
    EmptyEnv, EnvGroup, If, Let, Literal, No, Not, Or, Property, Self, Entity,
    Var, ignore, langkit_property
)
from langkit.expressions.analysis_units import UnitBody, UnitSpecification
from langkit.expressions.logic import Predicate, LogicTrue, LogicFalse


env = DynamicVariable('env', LexicalEnvType)
origin = DynamicVariable('origin', T.AdaNode)


def ref_used_packages():
    """
    If Self is a library item or a subunit, reference the environments for
    packages that are used at the top-level here. See
    UsePackageClause's ref_env_nodes for the rationale.
    """
    return reference(Self.top_level_use_package_clauses,
                     T.Name.use_package_name_designated_env)


def ref_used_packages_in_spec():
    """
    If Self, which is assumed to be a SubpBody, is a library-level subprogram,
    it must "inherit" the use clauses of its declaration, if there is one.
    """
    return reference(Self.self_toplevel_item_or_none,
                     through=T.AdaNode.use_packages_in_spec_of_subp_body)


def ref_std():
    """
    Make the Standard package automatically used.
    """
    return reference(Self.self_toplevel_item_or_none,
                     through=AdaNode.std_env)


def ref_generic_formals():
    """
    If Self is a generic package/subprogram and not a library item,
    then the generic formals are not available in parent
    environments. Make them available with ref_envs.
    """
    return reference(Self.cast(T.AdaNode).to_array,
                     through=T.AdaNode.generic_formal_env_of_not_library_item)


def add_to_env_kv(key, val, *args, **kwargs):
    """
    Wrapper around envs.add_to_env, that takes a key and a val expression, and
    creates the intermediate env_assoc Struct.
    """
    return add_to_env(
        T.env_assoc.new(key=key, val=val), *args, **kwargs
    )


def env_mappings(base_id_list, entity):
    """
    Creates an env mapping array from a list of BaseId to be used as keys, and
    an entity to be used as value in the mappings.
    """
    return base_id_list.map(
        lambda base_id: T.env_assoc.new(key=base_id.sym, val=entity)
    )


@env_metadata
class Metadata(Struct):
    dottable_subp = UserField(
        BoolType, doc="Whether the stored element is a subprogram accessed "
                      "through the dot notation"
    )
    implicit_deref = UserField(
        BoolType, doc="Whether the stored element is accessed through an "
                      "implicit dereference"
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

    type_val = Property(
        No(T.AdaNode.entity),
        public=True,
        doc="""
        This will return the value of the type of this node after symbol
        resolution. NOTE: For this to be bound, resolve_names needs to be
        called on the appropriate parent node first.
        """
    )
    ref_val = Property(
        No(T.AdaNode.entity),
        public=True,
        doc="""
        This will return the node this nodes references after symbol
        resolution. NOTE: For this to be bound, resolve_names needs to be
        called on the appropriate parent node first.
        """
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
                  LogicTrue(),
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

    @langkit_property(return_type=BoolType, public=True)
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
                decl.defining_name.referenced_unit(UnitBody),
        ),

        public=True, doc="""
        If this unit has a body, fetch and return it.
        """
    )

    spec_unit = Property(
        Self.top_level_item(Self.unit)
        .cast(T.Body)._.defining_name.referenced_unit(UnitSpecification),

        public=True, doc="""
        If this unit has a spec, fetch and return it. Return the null analysis
        unit otherwise. Note that this returns null for specs, as they don't
        have another spec themselves.
        """
    )

    parent_unit_spec = Property(
        Self.top_level_item(Self.unit)._.defining_name.cast(T.DottedName)
        ._.referenced_unit(UnitSpecification),

        public=True, doc="""
        If this unit is a spec and is a child unit, return the spec of the
        parent unit. Return a null analysis unit for all other cases.
        """
    )

    std = Property(
        # This property is used during referenced envs resolution. As a
        # consequence, a recursive env lookup here would yield infinite
        # recursion, as all recursive env lookups will eventually evaluate
        # this. We know that Standard is available without any use clause
        # anyway, so non-recursive lookup is fine.
        Self.unit.root.node_env.get('Standard', recursive=False).at(0),
        doc="""
        Retrieves the standard unit. Used to access standard types.
        """
    )

    std_env = Property(
        Self.std.children_env,
        doc="""
        Get the children env of the Standard package.
        """
    )

    std_entity = Property(
        lambda sym=Symbol: Self.std_env.get(sym).at(0),
        doc="Return an entity from the standard package with name `sym`"
    )

    bool_type = Property(Self.std_entity('Boolean'))
    int_type = Property(Self.std_entity('Integer'))

    @langkit_property()
    def has_with_visibility(refd_unit=AnalysisUnitType):
        """
        Return whether Self's unit has "with visibility" on "refd_unit".

        In other words, whether Self's unit has a WITH clause on "refd_unit",
        or if its spec, or one of its parent specs has one.
        """
        return (
            refd_unit.is_referenced_from(Self.unit)
            | Self.unit.root.spec_unit.then(
                lambda u: refd_unit.is_referenced_from(u)
            ) | Self.unit.root.parent_unit_spec.then(
                lambda u: refd_unit.is_referenced_from(u)
            )
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
            lambda n=T.Name.entity: n.name_designated_type,

            lambda _: No(T.entity),
        )

    @langkit_property()
    def top_level_use_package_clauses():
        """
        If Self is a library item or a subunit, return a flat list of all names
        for top-level UsePackageClause nodes. See
        UsePackageClause.env_spec.ref_envs for more details.
        """
        return If(
            Self.parent.is_a(T.LibraryItem, T.Subunit),

            Self.parent.parent.cast_or_raise(T.CompilationUnit)
            .prelude
            .filter(lambda p: p.is_a(UsePackageClause))
            .mapcat(
                lambda p: p.cast_or_raise(UsePackageClause).packages.map(
                    lambda n: n.cast(AdaNode)
                )
            ),

            EmptyArray(AdaNode)
        )

    @langkit_property()
    def self_toplevel_item_or_none():
        """
        Helper for Standard package automatic "use". If Self is a toplevel item
        (library item or subunit), return a singleton array for Self.
        Otherwise, return an empty array.
        """
        return If(Self.parent.is_a(T.LibraryItem, T.Subunit),
                  Self.to_array,
                  EmptyArray(AdaNode))

    @langkit_property()
    def use_packages_in_spec_of_subp_body():
        """
        If Self is a library-level SubpBody, fetch the environments USE'd in
        its declaration. See "ref_used_packages_in_spec".
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
                ).env_group,
                default_val=EmptyEnv
            ),

            EmptyEnv
        ))

    @langkit_property(memoized=True)
    def generic_formal_env_of_not_library_item():
        """
        Assuming Self is a generic package (or subprogram) body that is not a
        library item, return the lexical environment for the corresponding
        GenericPackageDecl (or GenericSubpDecl) node. Return an empty
        environment in all other cases.

        This is a helper for generic formals visibility in generic bodies. See
        the use in the child_unit macro.

        The following property is evaluated each time we make a recursive
        lexical environment lookup on a child unit. As it does itself a lot of
        lookups, memoizing it is very important.
        """
        # TODO: refine this property to preserve entities when it makes sense
        gen_decl = Var(If(
            Self.is_library_item,

            No(T.AdaNode),

            Self.as_bare_entity.match(
                lambda pkg_body=T.PackageBody:
                    pkg_body.decl_part_entity.
                    _.parent.cast(T.GenericPackageDecl),
                lambda subp_body=T.SubpBody:
                    # If subp_body is the body of a generic subprogram, then
                    # the environment lookup below should return its
                    # specification as the second node (the first being
                    # subp_body). If it's not a generic declaration, then we
                    # know subp_body is not a generic subprogram, and thus we
                    # must return a null node here.
                    subp_body.parent.node_env.get(
                        subp_body.relative_name
                    ).at(1).el.cast(T.GenericSubpDecl).cast(T.AdaNode),
                lambda _: No(T.AdaNode)
            )
        ))
        return gen_decl._.children_env

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
        return unit.root.then(
            lambda root:
                root.cast_or_raise(T.CompilationUnit).body.match(
                    lambda li=T.LibraryItem: li.item,
                    lambda su=T.Subunit: su.body,
                    lambda _: No(T.BasicDecl),
                )
        )


def child_unit(name_expr, scope_expr):
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

    return EnvSpec(
        call_env_hook(Self),
        set_initial_env(
            env.bind(Self.initial_env, Let(
                lambda scope=scope_expr: If(scope == EmptyEnv, env, scope)
            ))
        ),
        add_to_env_kv(name_expr, Self),
        add_env(),
        ref_used_packages(),
        ref_generic_formals(),
        ref_std()
    )


@abstract
class BasicDecl(AdaNode):

    defining_names = AbstractProperty(
        type=T.Name.array, public=True, uses_entity_info=False,
        doc="""
        Get all the names of this basic declaration.
        """
    )

    defining_name = Property(
        Self.defining_names.at(0), public=True, ignore_warn_on_node=True,
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

    array_ndims = Property(
        Literal(0),
        doc="""
        If this designates an entity with an array-like interface, return its
        number of dimensions. Return 0 otherwise.
        """
    )

    is_array = Property(Entity.array_ndims > 0)

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
        return Entity.type_expression._.designated_type

    type_expression = Property(
        No(T.TypeExpr).as_entity,
        type=T.TypeExpr.entity,
        doc="""
        Return the type expression for this BasicDecl if applicable, a null
        otherwise.
        """
    )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[origin])
    def canonical_expr_type():
        """
        Same as expr_type, but will instead return the canonical type
        declaration.
        """
        return Entity.expr_type._.canonical_type

    @langkit_property(return_type=T.BaseFormalParamHolder.entity)
    def subp_spec_or_null():
        """
        If node is a Subp, returns the specification of this subprogram.
        """
        return Entity.match(
            lambda subp=BasicSubpDecl: subp.subp_decl_spec,
            lambda subp=SubpBody:      subp.subp_spec,
            lambda subp=SubpBodyStub:  subp.subp_spec,
            lambda entry=EntryDecl:    entry.spec,
            lambda _:                  No(SubpSpec.entity),
        )

    @langkit_property(return_type=BoolType)
    def paramless_subp():
        """
        Return true if entity denotes a paramless subprogram entity, when used
        in an expression context.
        """
        return Entity.subp_spec_or_null.then(
            lambda ss: ss.paramless(Entity.info.md),
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

    relative_name = Property(Self.defining_name.relative_name)

    @langkit_property()
    def body_part_entity():
        """
        Return the body corresponding to this node if applicable.
        """
        ignore(Var(Self.body_unit))
        return Self.children_env.get('__body', recursive=False).at(0)


@abstract
class Body(BasicDecl):

    decl_part_entity = Property(If(
        Self.is_library_item,

        # If library item, we just return the spec. We don't check if it's
        # a valid and matching subprogram because that's an error case.
        Self.top_level_item(Self.spec_unit).as_entity,

        # If not a library item, find the matching subprogram spec in the
        # env.
        Entity.parent.node_env.get(Self.relative_name)
        .find(lambda sp: sp.match(
            # If this body completes a generic subprogram, then we just return
            # it (no need to match the signature).
            lambda _=T.GenericSubpDecl: True,

            lambda subp_decl=T.BasicSubpDecl:
            subp_decl.subp_decl_spec.match_signature(
                Entity.subp_spec_or_null.cast(T.SubpSpec)
            ),

            lambda _: False
        )).cast_or_raise(T.BasicDecl.entity)
    ),
        doc="""
        Return the decl corresponding to this body. For convenience, the
        default implemention is for subprograms, overloaded in other kinds of
        bodies.
        """,
    )

    decl_part = Property(
        Self.as_bare_entity.decl_part_entity.el,
        public=True, ignore_warn_on_node=True,
        doc="""
        Return the decl corresponding to this node if applicable.
        """
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
    identifiers = AbstractProperty(type=T.BaseId.array)
    is_mandatory = Property(False)

    type = Property(
        origin.bind(
            Self, Entity.type_expression.designated_type.canonical_type
        )
    )


class DiscriminantSpec(BaseFormalParamDecl):
    ids = Field(type=T.Identifier.list)
    type_expr = Field(type=T.TypeExpr)
    default_expr = Field(type=T.Expr)

    env_spec = EnvSpec(
        add_to_env(env_mappings(Self.ids, Self))
    )

    defining_names = Property(Self.ids.map(lambda id: id.cast(T.Name)))

    identifiers = Property(Self.ids.map(lambda i: i.cast(BaseId)))
    type_expression = Property(Entity.type_expr)


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
        Entity.abstract_formal_params.mapcat(
            lambda spec: spec.identifiers.map(lambda id: SingleFormal.new(
                name=id, spec=spec
            ))
        ),
        doc='Couples (identifier, param spec) for all parameters'
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
    def paramless(md=Metadata):
        """
        Utility function. Given a subprogram spec and its associated metadata,
        determine if it can be called without parameters (and hence without a
        callexpr).
        """
        return Or(
            md.dottable_subp & (Self.nb_min_params == 1),
            Self.nb_min_params == 0
        )

    @langkit_property(return_type=T.ParamMatch.array,
                      dynamic_vars=[env])
    def match_param_list(params=T.AssocList, is_dottable_subp=BoolType):
        """
        For each ParamAssoc in a AssocList, return whether we could find a
        matching formal in Self, and whether this formal is optional (i.e. has
        a default value).
        """
        def matches(formal, actual):
            return ParamMatch.new(has_matched=True,
                                  formal=formal, actual=actual)

        unpacked_formals = Var(Entity.unpacked_formal_params)

        return params.unpacked_params.map(lambda i, a: If(
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
        ))

    @langkit_property(return_type=BoolType)
    def match_formal_params(other=T.BaseFormalParamHolder.entity):
        # Check that there is the same number of formals and that each
        # formal matches.
        return Let(
            lambda
            self_params=Entity.unpacked_formal_params,
            other_params=other.unpacked_formal_params:

            And(self_params.length == other_params.length,
                self_params.all(
                    lambda i, p:
                    And(
                        p.name.matches(other_params.at(i).name),
                        origin.bind(
                            Self,
                            p.spec.type_expression._.canonical_type
                        ) == origin.bind(
                            other.el,
                            other_params.at(i).spec
                            .type_expression._.canonical_type
                        )
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


@abstract
class DiscriminantPart(BaseFormalParamHolder):

    @langkit_property()
    def abstract_formal_params():
        return EmptyArray(T.BaseFormalParamDecl.entity)


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
    array_ndims = Property(
        Literal(0),
        doc="""
        If this designates an array type, return its number of dimensions.
        Return 0 otherwise.
        """
    )

    is_real_type = Property(False, doc="Whether type is a real type or not.")

    @langkit_property(dynamic_vars=[origin])
    def is_int_type():
        """Whether type is an integer type or not."""
        return False

    is_access_type = Property(False, uses_entity_info=False,
                              doc="Whether type is an access type or not.")
    is_char_type = Property(False)
    is_enum_type = Property(False)

    @langkit_property(dynamic_vars=[origin])
    def accessed_type():
        return No(BaseTypeDecl.entity)

    is_tagged_type = Property(False, doc="Whether type is tagged or not")
    base_type = Property(
        No(T.BaseTypeDecl.entity), doc="""
        Return the base type entity for this derived type definition.
        """
    )

    @langkit_property(dynamic_vars=[origin])
    def defining_env():
        return EmptyEnv

    containing_type = Property(
        Entity.parent.cast_or_raise(T.TypeDecl).as_entity, doc="""
        Return the TypeDecl containing this TypeDef
        """
    )


class Variant(AdaNode):
    choice_list = Field(type=T.AdaNode.list)
    components = Field(type=T.ComponentList)


class VariantPart(AdaNode):
    discr_name = Field(type=T.Identifier)
    variant = Field(type=T.Variant.list)


class ComponentDecl(BaseFormalParamDecl):
    ids = Field(type=T.Identifier.list)
    component_def = Field(type=T.ComponentDef)
    default_expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    env_spec = EnvSpec(
        add_to_env(env_mappings(Self.ids, Self)),
    )

    identifiers = Property(Self.ids.map(lambda e: e.cast(BaseId)))
    defining_env = Property(
        Entity.component_def.type_expr.defining_env,
        doc="See BasicDecl.defining_env"
    )

    defining_names = Property(Self.ids.map(lambda id: id.cast(T.Name)))
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
        typ = Var(Entity.canonical_expr_type)
        return (
            Entity.component_def.type_expr.sub_equation
            & Entity.default_expr.then(
                lambda de:
                de.sub_equation
                & Bind(Self.default_expr.type_var,
                       typ,
                       eq_prop=BaseTypeDecl.matching_assign_type),
                default_val=LogicTrue()
            )
        )


class ComponentList(BaseFormalParamHolder):
    components = Field(type=T.AdaNode.list)
    variant_part = Field(type=T.VariantPart)

    type_def = Property(Self.parent.parent.cast(T.TypeDef).as_entity)
    type_decl = Property(Entity.type_def.parent.cast(T.TypeDecl).as_entity)

    parent_component_list = Property(
        Entity.type_def.cast(T.DerivedTypeDef)._.base_type.record_def.comps
    )

    @langkit_property()
    def abstract_formal_params():
        # TODO: Incomplete definition. We need to handle variant parts.
        self_comps = Var(Self.components.keep(BaseFormalParamDecl).map(
            lambda e: e.as_entity
        ))

        with_discrs = Var(self_comps.concat(
            Entity.type_decl._.discriminants._.abstract_formal_params()
        ))

        return Entity.parent_component_list.then(
            lambda pcl: pcl.abstract_formal_params.concat(with_discrs),
            default_val=with_discrs
        )


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
        # We don't want to be able to access env elements in parents,
        # so we orphan the env.
        Entity.children_env,
        type=LexicalEnvType
    )

    is_tagged_type = Property(Self.has_tagged.as_bool)

    xref_equation = Property(LogicTrue())


@abstract
class RealTypeDef(TypeDef):
    is_real_type = Property(True)
    xref_equation = Property(LogicTrue())


@abstract
class BaseTypeDecl(BasicDecl):
    type_id = Field(type=T.Identifier)

    env_spec = EnvSpec(
        add_to_env_kv(Self.relative_name, Self)
    )

    defining_names = Property(Self.type_id.cast(T.Name).singleton)

    is_task_type = Property(False, doc="Whether type is a task type")
    is_real_type = Property(False, doc="Whether type is a real type or not.")
    is_enum_type = Property(False)

    @langkit_property(dynamic_vars=[origin])
    def is_discrete_type():
        return Entity.is_int_type | Entity.is_enum_type

    @langkit_property(dynamic_vars=[origin])
    def is_int_type():
        """Whether type is an integer type or not."""
        return False

    is_access_type = Property(False,
                              doc="Whether type is an access type or not")

    access_def = Property(No(T.AccessDef.entity))

    is_char_type = Property(False,
                            doc="Whether type is a character type or not")

    @langkit_property(dynamic_vars=[origin])
    def is_str_type():
        return Entity.is_array & Entity.comp_type._.is_char_type

    @langkit_property(dynamic_vars=[origin])
    def accessed_type():
        return No(T.BaseTypeDecl.entity)

    @langkit_property(dynamic_vars=[origin])
    def is_access_of(other=T.BaseTypeDecl.entity):
        """
        Returns whether self is an access type whose accessed type matches
        other.
        """
        access_type = Var(Entity)
        return access_type.accessed_type.matching_type(other)

    is_tagged_type = Property(False, doc="Whether type is tagged or not")
    base_type = Property(
        No(T.BaseTypeDecl.entity), doc="""
        Return the base type entity for this derived type declaration.
        """
    )
    array_def = Property(No(T.ArrayTypeDef.entity))
    record_def = Property(No(T.BaseRecordDef.entity))

    @langkit_property(dynamic_vars=[origin])
    def comp_type():
        """
        Return the component type of the type, if applicable. The component
        type is the type you'll get if you call an instance of the Self type.
        So it can either be:

            1. The component type for an array.
            2. The return type for an access to function.
        """
        return Entity.array_def.then(lambda ad: ad.comp_type)._or(
            Entity.access_def.cast(T.AccessToSubpDef).then(
                lambda sa: sa.subp_spec.returns.designated_type
            )
        )

    # A BaseTypeDecl in an expression context corresponds to a type conversion,
    # so its type is itself.
    expr_type = Property(Entity)

    @langkit_property(return_type=BoolType)
    def is_derived_type(other_type=T.BaseTypeDecl.entity):
        """
        Whether Self is derived from other_type.
        """
        return Or(
            Entity == other_type,
            (Not(Entity.classwide_type.is_null)
             & (Entity.classwide_type == other_type.classwide_type)),
            Entity.base_type._.is_derived_type(other_type)
        )

    is_iterable_type = Property(
        # TODO: Only works with array types at the moment, need to implement
        # on:
        #
        #   * Spark iterable types (Iterable aspect).
        #   * Ada 2012 iterable types.
        Entity.is_array,
        doc="""
        Whether Self is a type that is iterable in a for .. of loop
        """
    )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_prefix_type(container_type=T.BaseTypeDecl.entity):
        """
        Given a dotted expression A.B, where container_type is the container
        type for B, and Self is a potential type for A, returns whether Self is
        a valid type for A in the dotted expression.
        """
        cont_type = Var(container_type.canonical_type)
        return Or(
            # Derived type case
            Entity.canonical_type.is_derived_type(cont_type),

            # Access to derived type case
            Entity.canonical_type.accessed_type._.is_derived_type(cont_type),
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
            lambda _: False
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_formal_type(formal_type=T.BaseTypeDecl.entity):
        actual_type = Var(Entity)
        return Or(
            And(
                formal_type.is_classwide,
                actual_type.is_derived_type(formal_type)
            ),
            And(
                actual_type.is_classwide,
                actual_type.is_derived_type(formal_type)
            ),
            actual_type.canonical_type == formal_type.canonical_type,
            actual_type.matching_access_type(formal_type)
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_assign_type(expected_type=T.BaseTypeDecl.entity):
        actual_type = Var(Entity)
        return Or(
            Entity.matching_type(expected_type),
            And(
                expected_type.is_classwide,
                actual_type.is_derived_type(expected_type)
            )
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_type(expected_type=T.BaseTypeDecl.entity):
        actual_type = Var(Entity)
        return Or(
            actual_type == expected_type,
            actual_type.matching_access_type(expected_type)
        )

    @langkit_property(return_type=BoolType, dynamic_vars=[origin])
    def matching_allocator_type(allocated_type=T.BaseTypeDecl.entity):
        return And(
            Entity.is_access_type,
            allocated_type.matching_type(Entity.accessed_type)
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[origin])
    def canonical_type():
        """
        Return the canonical type declaration for this type declaration. For
        subtypes, it will return the base type declaration.
        """
        return Entity

    @langkit_property(memoized=True, ignore_warn_on_node=True)
    def classwide_type_node():
        return If(Self.is_tagged_type,
                  T.ClasswideTypeDecl.new(type_id=Self.type_id),
                  No(T.ClasswideTypeDecl))

    classwide_type = Property(Self.classwide_type_node.as_entity)

    is_classwide = Property(False)


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
    record_def = Property(Entity.typedecl.record_def)
    classwide_type = Property(Entity)
    is_iterable_type = Property(Entity.typedecl.is_iterable_type)
    defining_env = Property(Entity.typedecl.defining_env)


class TypeDecl(BaseTypeDecl):
    discriminants = Field(type=T.DiscriminantPart)
    type_def = Field(type=T.TypeDef)
    aspects = Field(type=T.AspectSpec)

    array_ndims = Property(Entity.type_def.array_ndims)

    is_real_type = Property(Entity.type_def.is_real_type)
    is_int_type = Property(Entity.type_def.is_int_type)
    is_access_type = Property(Self.as_bare_entity.type_def.is_access_type)
    accessed_type = Property(Entity.type_def.accessed_type)
    access_def = Property(Entity.type_def.match(
        lambda ad=T.AccessDef: ad,
        lambda dtd=T.DerivedTypeDef: dtd.base_type.access_def,
        lambda _: No(T.AccessDef.entity)
    ))

    is_tagged_type = Property(Self.type_def.is_tagged_type)
    base_type = Property(Entity.type_def.base_type)
    is_char_type = Property(Entity.type_def.is_char_type)
    is_enum_type = Property(Entity.type_def.is_enum_type)

    array_def = Property(Entity.type_def.match(
        lambda atd=T.ArrayTypeDef: atd,
        lambda dtd=T.DerivedTypeDef: dtd.base_type.array_def,
        lambda _: No(T.ArrayTypeDef.entity)
    ))

    defining_env = Property(
        # Evaluating in type env, because the defining environment of a type
        # is always its own.
        env.bind(Entity.children_env, Entity.type_def.defining_env)
    )

    env_spec = EnvSpec(
        add_to_env_kv(Self.relative_name, Self),
        add_env()
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
        return (Entity.type_def.cast(AccessDef)._.accessed_type
                .matching_type(other.accessed_type))

    # We don't want to add anonymous type declarations to the lexical
    # environments, so we reset the env spec.
    env_spec = EnvSpec()


class EnumTypeDecl(BaseTypeDecl):
    enum_literals = Field(type=T.EnumLiteralDecl.list)
    aspects = Field(type=T.AspectSpec)

    is_char_type = Property(Self.enum_literals.any(
        lambda lit: lit.enum_identifier.is_a(T.CharLiteral)
    ))

    is_enum_type = Property(True)


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
    assoc_expr = AbstractProperty(
        type=T.Expr, public=True, ignore_warn_on_node=True
    )


@abstract
class Constraint(AdaNode):
    pass


class RangeConstraint(Constraint):
    range = Field(type=T.RangeSpec)


class DigitsConstraint(Constraint):
    digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)


class DeltaConstraint(Constraint):
    digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)


class IndexConstraint(Constraint):
    constraints = Field(type=T.AdaNode.list)


class DiscriminantConstraint(Constraint):
    constraints = Field(type=T.DiscriminantAssoc.list)


class DiscriminantAssoc(Constraint):
    ids = Field(type=T.Identifier.list)
    expr = Field(type=T.Expr)


class DerivedTypeDef(TypeDef):
    has_abstract = Field(type=Abstract)
    has_limited = Field(type=Limited)
    has_synchronized = Field(type=Synchronized)
    subtype_indication = Field(type=T.SubtypeIndication)
    interfaces = Field(type=T.Name.list)
    record_extension = Field(type=T.BaseRecordDef)
    has_with_private = Field(type=WithPrivate)

    array_ndims = Property(Entity.base_type.array_ndims)

    base_type = Property(
        origin.bind(Self, Entity.subtype_indication.designated_type)
    )

    is_real_type = Property(Entity.base_type.is_real_type)
    is_int_type = Property(Entity.base_type.is_int_type)
    is_access_type = Property(Self.as_bare_entity.base_type.is_access_type)
    is_char_type = Property(Entity.base_type.is_char_type)
    accessed_type = Property(Entity.base_type.accessed_type)
    is_tagged_type = Property(True)
    is_enum_type = Property(Entity.base_type.is_enum_type)

    defining_env = Property(EnvGroup(
        Entity.children_env,

        # Add environments from parent type defs
        Entity.base_type.canonical_type.defining_env
    ))

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return Entity.subtype_indication.xref_equation


class PrivateTypeDef(TypeDef):
    has_abstract = Field(type=Abstract)
    has_tagged = Field(type=Tagged)
    has_limited = Field(type=Limited)

    is_tagged_type = Property(Self.has_tagged.as_bool)

    xref_equation = Property(LogicTrue())


class SignedIntTypeDef(TypeDef):
    range = Field(type=T.RangeSpec)
    is_int_type = Property(True)

    xref_equation = Property(
        # We consider that the range expression is of the type we're defining.
        # Not sure how good of an idea this is but works in most cases.
        Bind(Entity.range.range.type_var, Entity.containing_type)
        & Entity.range.xref_equation
    )


class ModIntTypeDef(TypeDef):
    expr = Field(type=T.Expr)
    is_int_type = Property(True)

    xref_equation = Property(Entity.expr.sub_equation)


@abstract
class ArrayIndices(AdaNode):
    ndims = AbstractProperty(
        type=LongType,
        doc="""Number of dimensions described in this node."""
    )

    @langkit_property(return_type=EquationType, dynamic_vars=[origin])
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
        ignore(index_expr, dim)
        return LogicTrue()


class UnconstrainedArrayIndices(ArrayIndices):
    types = Field(type=T.UnconstrainedArrayIndex.list)
    ndims = Property(Self.types.length)

    @langkit_property(return_type=EquationType)
    def constrain_index_expr(index_expr=T.Expr, dim=LongType):
        return Bind(
            index_expr.type_var,
            Entity.types.at(dim).designated_type.canonical_type
        )


class ConstrainedArrayIndices(ArrayIndices):
    list = Field(type=T.AdaNode.list)

    ndims = Property(Self.list.length)

    @langkit_property(return_type=EquationType)
    def constrain_index_expr(index_expr=T.Expr, dim=LongType):
        return Self.list.at(dim).match(
            lambda n=T.SubtypeIndication:
            Bind(index_expr.type_var,
                 n.as_entity.designated_type.canonical_type),

            lambda _=T.Expr: Bind(index_expr.type_var, Self.int_type),

            lambda _: LogicTrue()
        )

    @langkit_property()
    def xref_equation():
        return Entity.list.logic_all(
            lambda index: index.sub_equation
            & index.match(
                lambda expr=T.Expr: Bind(expr.type_var, Self.int_type),
                lambda _: LogicTrue()
            )
        )


class ComponentDef(AdaNode):
    has_aliased = Field(type=Aliased)
    type_expr = Field(type=T.TypeExpr)


class ArrayTypeDef(TypeDef):
    indices = Field(type=T.ArrayIndices)
    component_type = Field(type=T.ComponentDef)

    @langkit_property(dynamic_vars=[origin])
    def comp_type():
        """Returns the type stored as a component in the array."""
        return (Entity.component_type.type_expr
                .designated_type.canonical_type)

    array_ndims = Property(Self.indices.ndims)

    @langkit_property()
    def xref_equation():
        return Entity.indices.sub_equation

    defining_env = Property(Entity.comp_type.defining_env)


class InterfaceKind(EnumNode):
    alternatives = ["limited", "task", "protected", "synchronized"]


class InterfaceTypeDef(TypeDef):
    interface_kind = Field(type=InterfaceKind)
    interfaces = Field(type=T.Name.list)

    is_tagged_type = Property(True)


class SubtypeDecl(BaseTypeDecl):
    subtype = Field(type=T.SubtypeIndication)
    aspects = Field(type=T.AspectSpec)

    array_ndims = Property(Entity.subtype.array_ndims)
    defining_env = Property(Entity.subtype.defining_env)

    canonical_type = Property(Entity.subtype.designated_type
                              .canonical_type)

    accessed_type = Property(Entity.canonical_type.accessed_type)

    is_int_type = Property(Entity.canonical_type.is_int_type)


class TaskDef(AdaNode):
    interfaces = Field(type=T.Name.list)
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)
    end_id = Field(type=T.Identifier)


class ProtectedDef(AdaNode):
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)
    end_id = Field(type=T.Identifier)


class TaskTypeDecl(BaseTypeDecl):
    discrs = Field(type=T.DiscriminantPart)
    aspects = Field(type=T.AspectSpec)
    definition = Field(type=T.TaskDef)
    is_task_type = Property(True)

    defining_names = Property(Self.type_id.cast(T.Name).singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Self.type_id.sym, Self),
        add_env()
    )

    defining_env = Property(Entity.children_env)


class SingleTaskTypeDecl(TaskTypeDecl):
    env_spec = EnvSpec(
        # In this case, we don't want to add this type to the env, because it's
        # the single task that contains this type decl that will be added to
        # the env. So we don't call the inherited env spec.
        add_env()
    )


class ProtectedTypeDecl(BasicDecl):
    protected_type_name = Field(type=T.Identifier)
    discrs = Field(type=T.DiscriminantPart)
    aspects = Field(type=T.AspectSpec)
    interfaces = Field(type=T.Name.list)
    definition = Field(type=T.ProtectedDef)

    defining_names = Property(Self.protected_type_name.cast(T.Name).singleton)


@abstract
class AccessDef(TypeDef):
    has_not_null = Field(type=NotNull)

    is_access_type = Property(True)
    defining_env = Property(Entity.accessed_type.defining_env)


class AccessToSubpDef(AccessDef):
    has_protected = Field(type=Protected, repr=False)
    subp_spec = Field(type=T.SubpSpec)

    xref_equation = Property(LogicTrue())


class TypeAccessDef(AccessDef):
    has_all = Field(type=All)
    has_constant = Field(type=Constant)
    subtype_indication = Field(type=T.SubtypeIndication)
    constraint = Field(type=T.Constraint)

    accessed_type = Property(Entity.subtype_indication.designated_type)
    xref_equation = Property(Entity.subtype_indication.xref_equation)


class FormalDiscreteTypeDef(TypeDef):
    xref_equation = Property(LogicTrue())


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


@abstract
class UseClause(AdaNode):
    pass


class UsePackageClause(UseClause):
    packages = Field(type=T.Name.list)

    env_spec = EnvSpec(
        reference(
            # We don't want to process use clauses that appear in the top-level
            # scope here, as they apply to the library item's environment,
            # which is not processed at this point yet. See CompilationUnit's
            # ref_env_nodes.
            If(Self.parent.parent.is_a(T.CompilationUnit),
               EmptyArray(AdaNode),
               Self.packages.map(lambda n: n.cast(AdaNode))),

            T.Name.use_package_name_designated_env
        )
    )

    @langkit_property(memoized=True, return_type=LexicalEnvType.array)
    def designated_envs():
        """
        Return the array of designated envs corresponding to each package name.

        It is very important for this property to be memoized, as it is used a
        lot during lexical environment lookups.
        """
        return Self.packages.map(
            lambda n:
            env.bind(n.node_env,
                     origin.bind(n, n.as_bare_entity.designated_env))
        )


class UseTypeClause(UseClause):
    has_all = Field(type=All)
    types = Field(type=T.Name.list)


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

    @langkit_property(dynamic_vars=[origin])
    def accessed_type():
        return Entity.designated_type._.accessed_type

    @langkit_property(dynamic_vars=[origin])
    def defining_env():
        return Entity.designated_type.defining_env

    designated_type = AbstractProperty(
        type=BaseTypeDecl.entity, runtime_check=True,
        dynamic_vars=[origin],
        doc="""
        Return the type designated by this type expression.
        """
    )

    @langkit_property(return_type=BaseTypeDecl.entity, dynamic_vars=[origin])
    def element_type():
        """
        If self is an anonymous access, return the accessed type. Otherwise,
        return the designated type.
        """
        d = Entity.designated_type
        return If(d.is_null, Entity.accessed_type, d)

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
        return Bind(Self.name.ref_var, Entity.designated_type)


class ConstrainedSubtypeIndication(SubtypeIndication):
    pass


class DiscreteSubtypeIndication(SubtypeIndication):
    pass


class Mode(EnumNode):
    alternatives = ["in", "out", "in_out", "default"]


class ParamSpec(BaseFormalParamDecl):
    ids = Field(type=T.Identifier.list)
    has_aliased = Field(type=Aliased)
    mode = Field(type=Mode)
    type_expr = Field(type=T.TypeExpr)
    default = Field(type=T.Expr)

    identifiers = Property(Self.ids.map(lambda e: e.cast(BaseId)))
    is_mandatory = Property(Self.default.is_null)
    defining_names = Property(Self.ids.map(lambda id: id.cast(T.Name)))

    env_spec = EnvSpec(
        add_to_env(env_mappings(Self.ids, Self))
    )

    type_expression = Property(Entity.type_expr)

    @langkit_property()
    def defining_env():
        return Entity.type_expr.defining_env


class AspectSpec(AdaNode):
    aspect_assocs = Field(type=T.AspectAssoc.list)


class Overriding(EnumNode):
    alternatives = ["overriding", "not_overriding", "unspecified"]


@abstract
class BasicSubpDecl(BasicDecl):
    defining_names = Property(
        Self.as_bare_entity.subp_decl_spec.name.singleton)
    defining_env = Property(Entity.subp_decl_spec.defining_env)

    type_expression = Property(
        Entity.subp_decl_spec.returns, doc="""
        The expr type of a subprogram declaration is the return type of the
        subprogram if the subprogram is a function.
        """
    )

    subp_decl_spec = AbstractProperty(type=T.SubpSpec.entity)

    @langkit_property(public=True, ignore_warn_on_node=True)
    def body_part():
        """
        Return the SubpBody corresponding to this node.
        """
        return Self.body_part_entity.cast(SubpBody).el

    env_spec = EnvSpec(
        # Call the env hook to parse eventual parent unit
        call_env_hook(Self),

        set_initial_env(
            env.bind(Self.initial_env,
                     Self.as_bare_entity.subp_decl_spec.name.parent_scope)
        ),

        add_to_env_kv(Self.relative_name, Self),
        add_env(),
        ref_used_packages(),
        ref_std(),

        handle_children(),

        # Adding subp to the type's environment if the type is tagged and self
        # is a primitive of it.
        add_to_env(
            # TODO: We can refactor this to not use an array, thanks to
            # mappings.
            Self.as_bare_entity.subp_decl_spec.dottable_subp.map(
                lambda dp: T.env_assoc.new(key=Self.relative_name, val=dp)
            ),
            dest_env=Let(
                lambda spec=Self.as_bare_entity.subp_decl_spec:
                origin.bind(spec.el.subp_name,
                            spec.potential_dottable_type._.children_env)
            ),
            # We pass custom metadata, marking the entity as a dottable
            # subprogram.
            metadata=Metadata.new(dottable_subp=True, implicit_deref=False)

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


class ExprFunction(ClassicSubpDecl):
    expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)


class SubpRenamingDecl(ClassicSubpDecl):
    renames = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)


class Pragma(AdaNode):
    id = Field(type=T.Identifier)
    args = Field(type=T.BaseAssoc.list)


class PragmaArgumentAssoc(BaseAssoc):
    id = Field(type=T.Identifier)
    expr = Field(type=T.Expr)
    assoc_expr = Property(Self.expr)


@abstract
class AspectClause(AdaNode):
    pass


class EnumRepClause(AspectClause):
    type_name = Field(type=T.Name)
    aggregate = Field(type=T.BaseAggregate)


class AttributeDefClause(AspectClause):
    attribute_expr = Field(type=T.Expr)
    expr = Field(type=T.Expr)


class ComponentClause(AdaNode):
    id = Field(type=T.Identifier)
    position = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)


class RecordRepClause(AspectClause):
    component_name = Field(type=T.Name)
    at_expr = Field(type=T.Expr)
    components = Field(type=T.ComponentClause.list)


class AtClause(AspectClause):
    name = Field(type=T.BaseId)
    expr = Field(type=T.Expr)


class SingleTaskDecl(BasicDecl):
    task_type = Field(type=T.SingleTaskTypeDecl)
    defining_names = Property(Self.task_type.type_id.cast(T.Name).singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Self.task_type.type_id.sym, Self)
    )

    expr_type = Property(Entity.task_type)


class SingleProtectedDecl(BasicDecl):
    protected_name = Field(type=T.Identifier)
    aspects = Field(type=T.AspectSpec)
    interfaces = Field(type=T.Name.list)
    definition = Field(type=T.ProtectedDef)

    defining_names = Property(Self.protected_name.cast(T.Name).singleton)


class AspectAssoc(AdaNode):
    id = Field(type=T.Expr)
    expr = Field(type=T.Expr)


class NumberDecl(BasicDecl):
    ids = Field(type=T.Identifier.list)
    expr = Field(type=T.Expr)

    defining_names = Property(Self.ids.map(lambda id: id.cast(T.Name)))


class ObjectDecl(BasicDecl):
    ids = Field(type=T.Identifier.list)
    has_aliased = Field(type=Aliased)
    has_constant = Field(type=Constant)
    inout = Field(type=Mode)
    type_expr = Field(type=T.TypeExpr)
    default_expr = Field(type=T.Expr)
    renaming_clause = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)

    env_spec = EnvSpec(add_to_env(env_mappings(Self.ids, Self)))

    array_ndims = Property(Entity.type_expr.array_ndims)
    defining_names = Property(Self.ids.map(lambda id: id.cast(T.Name)))
    defining_env = Property(Entity.type_expr.defining_env)
    type_expression = Property(Entity.type_expr)

    @langkit_property()
    def xref_equation():
        typ = Var(Entity.canonical_expr_type)
        return Entity.type_expr.sub_equation & Entity.default_expr.then(
            lambda de:
            de.sub_equation
            & Bind(Self.default_expr.type_var,
                   typ,
                   eq_prop=BaseTypeDecl.matching_assign_type),
            default_val=LogicTrue()
        )

    xref_entry_point = Property(True)


class ExtendedReturnStmtObjectDecl(ObjectDecl):
    pass


class DeclarativePart(AdaNode):
    decls = Field(type=T.AdaNode.list)


class PrivatePart(DeclarativePart):
    env_spec = EnvSpec(
        add_to_env_kv('__privatepart', Self),
        add_env()
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
    package_name = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)
    end_id = Field(type=T.Name)

    defining_names = Property(Self.package_name.singleton)
    defining_env = Property(Entity.children_env)

    @langkit_property(return_type=T.PackageBody, public=True,
                      ignore_warn_on_node=True)
    def body_part():
        """
        Return the PackageBody corresponding to this node.
        """
        return Self.body_part_entity.cast(T.PackageBody).el


class PackageDecl(BasePackageDecl):
    """
    Non-generic package declarations.
    """
    env_spec = child_unit(Self.relative_name, Self.package_name.parent_scope)


class ExceptionDecl(BasicDecl):
    """
    Exception declarations.
    """
    ids = Field(type=T.Identifier.list)
    renames = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)
    defining_names = Property(Self.ids.map(lambda id: id.cast(T.Name)))

    env_spec = EnvSpec(add_to_env(env_mappings(Self.ids, Self)))


@abstract
class GenericInstantiation(BasicDecl):
    """
    Instantiations of generics.
    """

    instantiation_env_holder = Field(type=T.EnvHolder)

    generic_entity_name = AbstractProperty(
        type=T.Name.entity, doc="""
        Return the name of the generic entity designated by this generic
        instantiation
        """
    )

    designated_generic_decl = Property(
        env.bind(
            Self.node_env,
            Self.as_bare_entity.generic_entity_name.env_elements.at(0)
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


class EnvHolder(AdaNode):
    """
    This type does not correspond to anything in the source. It is just here
    to hold a lexical environment.

    TODO: This should be do-able in a simpler fashion, by exposing a
    LexicalEnvType field that is automatically initialized.
    """
    env_spec = EnvSpec(add_env())


class GenericSubpInstantiation(GenericInstantiation):
    overriding = Field(type=Overriding)
    kind = Field(type=T.SubpKind)
    subp_name = Field(type=T.Name)
    generic_subp_name = Field(type=T.Name)
    subp_params = Field(type=T.AssocList)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.subp_name.singleton)

    generic_entity_name = Property(Entity.generic_subp_name)

    @langkit_property()
    def designated_subp():
        """
        Return the subprogram decl designated by this instantiation.
        """
        p = Var(Self.designated_generic_decl)

        return BasicSubpDecl.entity.new(
            el=p.el.cast(GenericSubpDecl).subp_decl,
            info=T.entity_info.new(
                md=p.info.md,
                rebindings=p.info.rebindings.append_rebinding(
                    p.el.children_env,
                    Self.instantiation_env_holder.children_env
                )
            )
        ).cast(T.entity)

    env_spec = EnvSpec(
        add_env(),
        ref_used_packages(),
        ref_std(),

        handle_children(),
        add_to_env(
            env.bind(
                Self.initial_env,
                Self.designated_generic_decl._.formal_part.match_param_list(
                    Self.subp_params, False
                ).map(lambda pm: T.env_assoc.new(
                    key=pm.formal.name.sym, val=pm.actual.assoc.expr
                ))
            ),
            dest_env=Self.instantiation_env_holder.children_env,
            resolver=AdaNode.resolve_generic_actual,
        ),
        add_to_env_kv(Self.relative_name, Self,
                      resolver=T.GenericSubpInstantiation.designated_subp),
    )


class GenericPackageInstantiation(GenericInstantiation):
    name = Field(type=T.Name)
    generic_pkg_name = Field(type=T.Name)
    params = Field(type=T.AssocList)
    aspects = Field(type=T.AspectSpec)

    generic_entity_name = Property(Entity.generic_pkg_name)

    @langkit_property(return_type=LexicalEnvType)
    def defining_env():
        p = Var(Self.designated_generic_decl)

        # We use the formal env to create rebindings. There, we purposefully
        # want the children env of the P node, with no rebindings associated,
        # since the rebinding indication concerns the *naked* generic. Hence
        # we use p.el.children_env.
        formal_env = Var(p.then(lambda p: p.el.children_env))

        return p._.decl.children_env.rebind_env(
            # If this generic instantiation is inside a generic instantiation,
            # then it inherits the rebindings of the enclosing instantiation.
            Entity.info.rebindings.append_rebinding(
                formal_env, Self.instantiation_env_holder.children_env
            )
        )

    defining_names = Property(Self.name.singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Self.relative_name, Self),
        add_env(),
        ref_used_packages(),
        ref_std(),

        handle_children(),
        add_to_env(
            env.bind(
                Self.initial_env,
                Self.designated_generic_decl._.formal_part.match_param_list(
                    Self.params, False
                ).map(lambda pm: T.env_assoc.new(
                    key=pm.formal.name.sym, val=pm.actual.assoc.expr
                ))
            ),
            dest_env=Self.instantiation_env_holder.children_env,
            resolver=AdaNode.resolve_generic_actual,
        )
    )


class RenamingClause(AdaNode):
    """
    Renaming clause, used everywhere renamings are valid.
    """
    renamed_object = Field(type=T.Expr)


class PackageRenamingDecl(BasicDecl):
    name = Field(type=T.Name)
    renames = Field(type=RenamingClause)
    aspects = Field(type=T.AspectSpec)

    env_spec = child_unit(Self.relative_name, Self.name.parent_scope)

    defining_names = Property(Self.name.singleton)
    defining_env = Property(env.bind(
        Entity.node_env,
        Entity.renames.renamed_object.env_elements.at(0)
        ._.cast(BasicDecl).defining_env
    ))


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
    env_spec = child_unit(Self.relative_name, Self.name.parent_scope)

    name = Field(type=T.Name)
    renames = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.name.singleton)
    defining_env = Property(Entity.resolve.defining_env)
    renaming_name = Property(Entity.renames)


class SubpKind(EnumNode):
    alternatives = ["procedure", "function"]


class GenericSubpRenamingDecl(GenericRenamingDecl):
    env_spec = child_unit(Self.relative_name, Self.name.parent_scope)

    kind = Field(type=T.SubpKind)
    name = Field(type=T.Name)
    renames = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.name.singleton)
    renaming_name = Property(Entity.renames)


@abstract
class FormalSubpDecl(ClassicSubpDecl):
    """
    Formal subprogram declarations, in generic declarations formal parts.
    """
    default_value = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.subp_spec.name.singleton)


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
    identifiers = Property(
        Self.decl.defining_names.map(lambda p: p.cast_or_raise(T.BaseId))
    )
    defining_names = Property(Self.decl.defining_names)


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
    env_spec = child_unit(Self.relative_name,
                          Self.subp_decl.subp_spec.name.parent_scope)

    subp_decl = Field(type=T.GenericSubpInternal)

    defining_names = Property(Self.subp_decl.subp_spec.name.singleton)

    @langkit_property(public=True, ignore_warn_on_node=True)
    def body_part():
        """
        Return the SubpBody corresponding to this node.
        """
        return Self.body_part_entity.cast(SubpBody).el

    env_spec = EnvSpec(
        # Process eventual parent unit
        call_env_hook(Self),

        set_initial_env(
            env.bind(Self.initial_env, Self.defining_name.parent_scope)
        ),
        add_to_env_kv(Self.relative_name, Self),
        add_env(),
        ref_used_packages(),
        ref_std()
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
    env_spec = child_unit(Self.relative_name,
                          Self.package_decl.package_name.parent_scope)

    package_decl = Field(type=GenericPackageInternal)

    defining_names = Property(Self.package_decl.package_name.singleton)

    @langkit_property(public=True, ignore_warn_on_node=True)
    def body_part():
        """
        Return the PackageBody corresponding to this node, or null if there is
        none.
        """
        return Entity.package_decl.body_part

    decl = Property(Entity.package_decl)


@abstract
class Expr(AdaNode):

    type_var = UserField(LogicVarType, public=False)
    type_val = Property(Self.type_var.get_value)

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      return_type=LexicalEnvType, dynamic_vars=[env, origin])
    def designated_env():
        """
        Returns the lexical environment designated by this name.

        If this name involves overloading, this will return a combination of
        the various candidate lexical environments.
        """
        pass

    parent_scope = AbstractProperty(
        type=LexicalEnvType, runtime_check=True,
        dynamic_vars=[env],
        doc="""
        Returns the lexical environment that is the scope in which the
        entity designated by this name is defined/used.
        """
    )

    env_elements = Property(
        Entity.env_elements_impl().filter(lambda e: (
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

    @langkit_property(dynamic_vars=[env])
    def matching_nodes_impl():
        return Self.as_bare_entity.env_elements.map(lambda e: e.el)

    @langkit_property(return_type=AdaNode.array, public=True)
    def matching_nodes():
        """
        Return the list of AST nodes that can be a match for this expression
        before overloading analysis.
        """
        return env.bind(Self.node_env, Self.matching_nodes_impl)


class ContractCaseAssoc(BaseAssoc):
    guard = Field(type=T.AdaNode)
    consequence = Field(type=T.Expr)

    assoc_expr = Property(Self.consequence)


class ContractCases(Expr):
    contract_cases = Field(ContractCaseAssoc.list)


class ParenExpr(Expr):
    expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return Entity.expr.sub_equation & Bind(Self.expr.type_var,
                                               Self.type_var)


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
            lambda _=Op.alt_neq: '"/="',
            lambda _=Op.alt_lt: '"<"',
            lambda _=Op.alt_lte: '"<="',
            lambda _=Op.alt_gt: '">"',
            lambda _=Op.alt_gte: '">="',
            lambda _: '<<>>',
        )).keep(T.BasicSubpDecl.entity),
        doc="""
        Return the subprograms corresponding to this operator accessible in the
        lexical environment.
        """
    )

    ref_var = UserField(type=LogicVarType, public=False)


class UnOp(Expr):
    op = Field(type=Op)
    expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        subps = Var(Entity.op.subprograms.filter(
            lambda s: s.subp_decl_spec.nb_max_params == 1
        ))
        return Entity.expr.sub_equation & (subps.logic_any(lambda subp: Let(
            lambda ps=subp.subp_decl_spec.unpacked_formal_params:

            # The subprogram's first argument must match Self's left
            # operand.
            Bind(Self.expr.type_var, ps.at(0).spec.type)

            # The subprogram's return type is the type of Self
            & Bind(Self.type_var, subp.subp_decl_spec.returns.designated_type)

            # The operator references the subprogram
            & Bind(Self.op.ref_var, subp)
        )) | Bind(Self.type_var, Self.expr.type_var))


class BinOp(Expr):
    left = Field(type=T.Expr)
    op = Field(type=Op)
    right = Field(type=T.Expr)

    ref_val = Property(Self.op.ref_var.get_value)

    @langkit_property()
    def xref_equation():
        subps = Var(Entity.op.subprograms.filter(
            lambda s: s.subp_decl_spec.nb_max_params == 2
        ))
        return (
            Entity.left.sub_equation
            & Entity.right.sub_equation
        ) & (subps.logic_any(lambda subp: Let(
            lambda ps=subp.subp_decl_spec.unpacked_formal_params:

            # The subprogram's first argument must match Self's left
            # operand.
            Bind(Self.left.type_var, ps.at(0).spec.type)

            # The subprogram's second argument must match Self's right
            # operand.
            & Bind(Self.right.type_var, ps.at(1).spec.type)

            # The subprogram's return type is the type of Self
            & Bind(Self.type_var, subp.subp_decl_spec.returns.designated_type)

            # The operator references the subprogram
            & Bind(Self.op.ref_var, subp)
        )) | Self.no_overload_equation())

    no_overload_equation = Property(
        Bind(Self.type_var, Self.left.type_var)
        & Bind(Self.type_var, Self.right.type_var),
        doc="""
        When no subprogram is found for this node's operator, use this property
        to construct the xref equation for this node.
        """
    )


class RelationOp(BinOp):
    no_overload_equation = Property(
        Bind(Self.left.type_var, Self.right.type_var)
        & Bind(Self.type_var, Self.bool_type)
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
        Entity.expr.sub_equation
        & Entity.membership_exprs.logic_all(
            lambda m:
            m.sub_equation & Bind(Entity.expr.type_var, m.type_var)
        )
    )


class DiscreteSubtypeExpr(Expr):
    subtype = Field(type=T.DiscreteSubtypeIndication)


@abstract
class BaseAggregate(Expr):
    ancestor_expr = Field(type=T.Expr)
    assocs = Field(type=T.AssocList)


class Aggregate(BaseAggregate):

    xref_stop_resolution = Property(True)

    @langkit_property()
    def xref_equation():
        td = Var(Self.type_val.cast(BaseTypeDecl.entity))

        atd = Var(td.array_def)
        return If(
            atd.is_null,

            # First case, aggregate for a record
            td.record_def.comps.match_param_list(
                Self.assocs, False
            ).logic_all(
                lambda pm:
                Bind(pm.actual.assoc.expr.type_var,
                     pm.formal.spec.type_expression.designated_type)
                & pm.actual.assoc.expr.as_entity.sub_equation
                & If(pm.actual.name.is_null,
                     LogicTrue(),
                     Bind(pm.actual.name.ref_var, pm.formal.spec))
            ),

            # Second case, aggregate for an array
            Entity.assocs.logic_all(
                lambda assoc:
                assoc.expr.as_entity.sub_equation
                & Bind(assoc.expr.type_var, atd.comp_type)
            )
        )


class NullRecordAggregate(BaseAggregate):
    pass


@abstract
class Name(Expr):

    scope = Property(
        EmptyEnv, dynamic_vars=[env],
        doc="""
        Lexical environment this identifier represents. This is similar to
        designated_env although it handles only cases for child units and it is
        used only during the environment population pass so it does not return
        orphan environments.
        """
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

    ref_val = Property(Self.ref_var.get_value)

    designated_type_impl = AbstractProperty(
        type=BaseTypeDecl.entity, runtime_check=True,
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

    @langkit_property(return_type=AnalysisUnitType, external=True,
                      uses_entity_info=False)
    def referenced_unit(kind=AnalysisUnitKind):
        """
        Return the analysis unit for the given "kind" corresponding to this
        Name. Return null if this is an illegal unit name.
        """
        pass

    @langkit_property(public=True)
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
            lambda _: False
        )

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
        type=Symbol, runtime_check=True,
        doc="""
        Returns the relative name of this instance. For example,
        for a prefix A.B.C, this will return C.
        """
    )

    base_name = Property(
        No(T.Name.entity),
        doc="""
        Returns the base name of this instance. For example,
        for a prefix A.B.C, this will return A.B.
        """
    )


class CallExpr(Name):
    """
    Represent a syntactic call expression.

    At the semantic level, this can be either a subprogram call, an array
    subcomponent access expression, an array slice or a type conversion.
    """
    name = Field(type=T.Name)
    suffix = Field(type=T.AdaNode)

    ref_var = Property(Self.name.ref_var)

    @langkit_property()
    def designated_env():
        return Entity.env_elements().map(lambda e: e.match(
            lambda bd=BasicDecl.entity:       bd.defining_env,
            lambda _:                         EmptyEnv,
        )).env_group

    @langkit_property()
    def env_elements_impl():
        return Entity.name.env_elements_impl()

    # CallExpr can appear in type expressions: they are used to create implicit
    # subtypes for discriminated records or arrays.
    designated_type_impl = Property(Entity.name.designated_type_impl)

    params = Property(Self.suffix.cast(T.AssocList), ignore_warn_on_node=True)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return If(
            Not(Entity.name.designated_type_impl.is_null),

            # Type conversion case
            Entity.type_conv_xref_equation,

            # General case. We'll call general_xref_equation on the innermost
            # call expression, to handle nested call expression cases.
            Self.innermost_callexpr.as_entity.general_xref_equation
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
            Bind(Self.type_var, Self.name.type_var),
            Bind(Self.ref_var, Self.name.ref_var)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def general_xref_equation():
        """
        Helper for xref_equation, handles construction of the equation in
        subprogram call cases.
        """
        # List of every applicable subprogram
        subps = Var(Entity.env_elements)

        return And(
            Self.params.logic_all(lambda pa: pa.expr.as_entity.sub_equation),

            # For each potential entity match, we want to express the
            # following constraints:
            And(
                subps.logic_any(lambda e: Let(
                    lambda s=e.cast_or_raise(BasicDecl.entity):

                    # The called entity is the matched entity
                    Bind(Self.name.ref_var, e)

                    & If(
                        # Test if the entity is a parameterless subprogram
                        # call, or something else (a component/local
                        # variable/etc), that would make this callexpr an array
                        # access.
                        s.paramless_subp,
                        Entity.array_type_equation(s.expr_type),
                        Entity.subprogram_equation(s)
                    )
                    & Self.parent_nested_callexpr.as_entity.then(
                        lambda pce: pce.parent_callexprs_equation(
                            # If s is paramless, then Self was a subscript to
                            # the s object, and the parent callexpr is a
                            # subscript to its component. However, if s is not
                            # paramless, then Self was a call to s, and the
                            # parent callexpr is a subscript to an instance of
                            # s's return type.
                            If(s.paramless_subp,
                               s.expr_type.comp_type, s.expr_type),
                        ), default_val=LogicTrue()
                    ),
                )),
                Bind(Self.ref_var, Self.name.ref_var),
                Entity.name.sub_equation
            )
            | Entity.operator_equation
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def operator_equation():
        return Self.params._.unpacked_params.then(
            lambda params:
            Entity.name.base_name._.sub_equation._or(LogicFalse()) & If(
                params.length == 2,
                Entity.binop_equation(),
                If(params.length == 1, Entity.unop_equation(), LogicFalse())
            )
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def binop_equation():
        rel_name = Var(Self.name.relative_name)
        params = Var(Self.params.unpacked_params)
        return If(
            rel_name.any_of('"="',  '"="', '"/="', '"<"', '"<="', '">"',
                            '">="'),

            Bind(params.at(0).assoc.expr.type_var,
                 params.at(1).assoc.expr.type_var)
            & Bind(Self.type_var, Self.bool_type),

            If(rel_name.any_of('"and"', '"or"', '"xor"', '"abs"', '"not"',
                               '"**"', '"*"', '"/"', '"mod"', '"rem"', '"+"',
                               '"-"', '"&"'),

                Bind(params.at(0).assoc.expr.type_var,
                     params.at(1).assoc.expr.type_var)

                & Bind(params.at(0).assoc.expr.type_var,
                       Self.type_var),

                LogicFalse())
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def unop_equation():
        return Bind(
            Self.params.unpacked_params.at(0).assoc.expr.type_var,
            Self.type_var
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def array_type_equation(typ=T.BaseTypeDecl.entity):
        """
        Construct an equation verifying if Self is conformant to the type
        designator passed in parameter.
        """
        atd = Var(typ.array_def)

        return atd._.indices.then(
            lambda indices:
            Self.suffix.match(
                # Regular array access
                lambda _=T.AssocList: Self.params._.logic_all(
                    lambda i, pa:
                    pa.expr.as_entity.sub_equation()
                    & indices.constrain_index_expr(pa.expr, i)
                )
                & Bind(Self.type_var, atd.comp_type),

                # Slice access
                lambda bo=T.BinOp:
                indices.constrain_index_expr(bo.left, 0)
                & indices.constrain_index_expr(bo.right, 0)
                & Bind(bo.type_var, bo.right.type_var)
                & Bind(Self.type_var, typ),

                # TODO: Handle remaining cases (SubtypeIndication?)
                lambda _: LogicFalse()
            )
        )._or(LogicFalse())

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def subprogram_equation(subp=T.BasicDecl.entity):
        return (
            # The type of the expression is the expr_type of the
            # subprogram.
            Bind(Self.type_var, subp.expr_type)

            # For each parameter, the type of the expression matches
            # the expected type for this subprogram.
            & subp.subp_spec_or_null.match_param_list(
                Self.params, subp.info.md.dottable_subp
            ).logic_all(
                lambda pm: (
                    # The type of each actual matches the type of the
                    # formal.
                    Bind(
                        pm.actual.assoc.expr.type_var,
                        pm.formal.spec.type_expression.designated_type,
                        eq_prop=BaseTypeDecl.matching_formal_type
                    )
                ) & If(
                    # Bind actuals designators to parameters if there
                    # are designators.
                    pm.actual.name.is_null,
                    LogicTrue(),
                    Bind(pm.actual.name.ref_var, pm.formal.spec)
                )
            )
        )

    @langkit_property(return_type=BoolType)
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
        return typ.then(lambda typ: And(
            # TODO: For the moment this is specialized for arrays, but we need
            # to handle the case when the return value is an access to
            # subprogram.
            typ.array_ndims == Self.suffix.cast_or_raise(AssocList).length,


            Self.parent.cast(T.CallExpr).then(
                lambda ce: ce.check_for_type(
                    origin.bind(Self, typ.expr_type)
                ), default_val=True
            )
        ))

    @langkit_property(return_type=T.CallExpr, ignore_warn_on_node=True)
    def innermost_callexpr():
        """
        Helper property. Will return the innermost call expression following
        the name chain. For, example, given::

            A (B) (C) (D)
            ^-----------^ Self
            ^-------^     Self.name
            ^---^         Self.name.name

        Self.innermost_callexpr will return the node corresponding to
        Self.name.name.
        """
        return Self.name.cast(T.CallExpr).then(
            lambda ce: ce.innermost_callexpr(), default_val=Self
        )

    @langkit_property(return_type=T.CallExpr, ignore_warn_on_node=True)
    def parent_nested_callexpr():
        """
        Will return the parent callexpr iff Self is the name of the parent
        callexpr.
        """
        return Self.parent.cast(T.CallExpr).then(
            lambda ce: If(ce.name == Self, ce, No(CallExpr))
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def parent_callexprs_equation(typ=T.BaseTypeDecl.entity):
        """
        Construct the xref equation for the chain of parent nested callexprs.
        """
        return (
            Entity.array_type_equation(typ)
            & Self.parent_nested_callexpr.as_entity.then(
                lambda pce:
                pce.parent_callexprs_equation(typ.comp_type),
                default_val=LogicTrue()
            )
        )


@abstract
@has_abstract_list
class BasicAssoc(AdaNode):
    expr = AbstractProperty(type=T.Expr, ignore_warn_on_node=True)
    names = AbstractProperty(type=T.AdaNode.array)


class ParamAssoc(BasicAssoc):
    """
    Assocation (X => Y) used for aggregates and parameter associations.
    """
    designator = Field(type=T.AdaNode)
    r_expr = Field(type=T.Expr)

    expr = Property(Self.r_expr)
    names = Property(If(Self.designator.is_null,
                        EmptyArray(AdaNode), Self.designator.singleton))


class AggregateAssoc(BasicAssoc):
    """
    Assocation (X => Y) used for aggregates and parameter associations.
    """
    designators = Field(type=T.AdaNode.list)
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
                filter_expr=lambda n: n.is_a(T.BaseId),
                expr=lambda i:
                SingleActual.new(name=i.cast(T.BaseId), assoc=pa)
            )
        )))


class DeclList(AdaNode.list):
    pass


class StmtList(AdaNode.list):
    pass


class ExplicitDeref(Name):
    prefix = Field(type=T.Name)
    ref_var = Property(Self.prefix.ref_var)

    @langkit_property()
    def designated_env():
        # Since we have implicit dereference in Ada, everything is directly
        # accessible through the prefix, so we just use the prefix's env.
        return Entity.prefix.designated_env()

    @langkit_property()
    def env_elements_impl():
        return origin.bind(
            Self,
            Entity.prefix.env_elements_impl.filter(
                # Env elements for access derefs need to be of an access type
                lambda e:
                e.cast(BasicDecl)._.canonical_expr_type.is_access_type
            )
        )

    @langkit_property()
    def xref_equation():
        return (
            Entity.prefix.sub_equation
            # Evaluate the prefix equation

            & Self.ref_var.domain(Entity.env_elements)
            # Restrict the domain of the reference to entities that are of an
            # access type.

            & Bind(Self.ref_var, Self.prefix.ref_var)
            # Propagate this constraint upward to the prefix expression

            & Bind(Self.prefix.type_var,
                   Self.type_var,
                   BaseTypeDecl.accessed_type)
            # We don't need to check if the type is an access type, since we
            # already constrained the domain above.
        )


class BoxExpr(Expr):
    xref_equation = Property(LogicTrue())


class OthersDesignator(AdaNode):
    pass


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
                & Bind(Self.then_expr.type_var, Self.else_expr.type_var),

                # If no else, then the then_expression has type bool
                Bind(Self.then_expr.type_var, Self.bool_type)
            ) & Entity.alternatives.logic_all(lambda elsif: (
                # Build the sub equations for cond and then exprs
                elsif.cond_expr.sub_equation
                & elsif.then_expr.sub_equation

                # The condition is boolean
                & Bind(elsif.cond_expr.type_var, Self.bool_type)

                # The elsif branch then expr has the same type as Self's
                # then_expr.
                & Bind(Self.then_expr.type_var, elsif.then_expr.type_var)
            )) & Bind(Self.cond_expr.type_var, Self.bool_type)
            & Bind(Self.then_expr.type_var, Self.type_var)
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
                Bind(e.type_var, Self.expr.type_val)
                & e.sub_equation,

                # TODO: Bind other cases: SubtypeIndication and Range
                lambda _: LogicTrue()
            ))

            # Equations for the dependent expressions
            & alt.expr.sub_equation

            # The type of self is the type of each expr. Also, the type of
            # every expr is bound together by the conjunction of this bind for
            # every branch.
            & Bind(Self.type_var, alt.expr.type_var)
        ))


class CaseExprAlternative(Expr):
    choices = Field(type=T.AdaNode.list)
    expr = Field(type=T.Expr)


@abstract
class SingleTokNode(Name):
    tok = Field(type=T.Token)
    relative_name = Property(Self.tok.symbol)

    r_ref_var = UserField(LogicVarType, public=False)
    """
    This field is the logic variable for this node. It is not used directly,
    instead being retrieved via the ref_var property
    """

    ref_var = Property(Self.r_ref_var)

    sym = Property(
        Self.tok.symbol, doc="Shortcut to get the symbol of this node"
    )


@abstract
class BaseId(SingleTokNode):

    @langkit_property()
    def scope():
        elt = Var(env.get(Self.tok).at(0))
        return If(
            Not(elt.is_null) & elt.el.is_a(
                T.PackageDecl, T.PackageBody, T.GenericPackageDecl,
                T.GenericSubpDecl
            ),
            elt.children_env,
            EmptyEnv
        )

    designated_env = Property(Entity.designated_env_impl(False))

    @langkit_property(dynamic_vars=[env])
    def designated_env_impl(is_parent_pkg=BoolType):
        """
        Decoupled implementation for designated_env, specifically used by
        DottedName when the parent is a library level package.
        """
        ents = Var(Entity.env_elements_baseid(is_parent_pkg))

        return origin.bind(Self, Let(lambda el=ents.at(0): If(
            el._.is_package,
            el.cast(BasicDecl).defining_env,
            ents.map(lambda e: e.cast(BasicDecl).defining_env).env_group
        )))

    parent_scope = Property(env)
    relative_name = Property(Self.tok.symbol)

    designated_type_impl = Property(
        env.get_sequential(Self.tok, sequential_from=origin)
        .at(0)._.match(
            lambda t=T.BaseTypeDecl.entity: t,
            lambda tb=T.TaskBody.entity: tb.task_type,
            lambda _: No(BaseTypeDecl.entity)
        )
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
        return Entity.env_elements_baseid(False)

    @langkit_property(dynamic_vars=[env])
    def env_elements_baseid(is_parent_pkg=BoolType):
        """
        Decoupled implementation for env_elements_impl, specifically used by
        designated_env when the parent is a library level package.

        :param is_parent_pkg: Whether the origin of the env request is a
            package or not.
        """

        # Some constructs, like use clauses, add a referenced env to their
        # parent scope, which uses a resolver that will itself do a lookup. So
        # that this does not generate an infinitely recursive lookup, the
        # lookup must be passed a sequential start point that will exclude the
        # referenced env.
        #
        # TODO: This might be solvable at the langkit level. In principle this
        # should never happen because we could pass down to the nested lookup
        # operation the information that we're resolving a reference at point X
        # in code, and that this reference should subsequently not be followed.
        parent_clause = Var(
            Self.parents.filter(
                lambda p: p.is_a(UsePackageClause, GenericSubpInstantiation)
            ).at(0)
        )

        items = Var(env.get_sequential(
            Self.tok,
            recursive=Not(is_parent_pkg),
            sequential_from=parent_clause.then(
                lambda p: p, default_val=Self
            )
        ))

        pc = Var(Self.parent_callexpr.then(lambda pc: pc.innermost_callexpr))

        return origin.bind(Self, If(
            pc.is_null,

            # If it is not the main id in a CallExpr: either the name
            # designates something else than a subprogram, either it designates
            # a subprogram that accepts no explicit argument. So filter out
            # other subprograms.
            items.filter(lambda e: (

                # If current item is a library item, we want to check that it
                # is visible from the current unit.
                (Not(e.is_library_item) | Self.has_with_visibility(e.unit))
                # If there is a subp_spec, check that it corresponds to
                # a parameterless subprogram.
                & e.cast_or_raise(BasicDecl).paramless_subp
            )),

            # This identifier is the name for a called subprogram or an array.
            # So only keep:
            # * subprograms for which the actuals match;
            # * arrays for which the number of dimensions match.
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
        return Let(lambda dt=Entity.designated_type_impl: If(
            Not(dt.is_null),

            # Type conversion case
            Bind(Self.ref_var, dt) & Bind(Self.type_var, dt),

            # Other cases
            Self.ref_var.domain(Entity.env_elements)
            & Bind(Self.ref_var, Self.type_var,
                   BasicDecl.canonical_expr_type)
        ))


class Identifier(BaseId):
    annotations = Annotations(repr_name="Id")


class StringLiteral(BaseId):
    annotations = Annotations(repr_name="Str")

    @langkit_property()
    def xref_equation():
        return Predicate(BaseTypeDecl.is_str_type, Self.type_var)


class EnumLiteralDecl(BasicDecl):
    enum_identifier = Field(type=T.BaseId)

    @langkit_property()
    def canonical_expr_type():
        return Self.parents.find(
            lambda p: p.is_a(BaseTypeDecl)
        ).cast(BaseTypeDecl).as_entity

    defining_names = Property(Self.enum_identifier.cast(T.Name).singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Self.enum_identifier.sym, Self)
    )


class CharLiteral(BaseId):
    annotations = Annotations(repr_name="Chr")

    @langkit_property()
    def xref_equation():
        return Predicate(BaseTypeDecl.is_char_type, Self.type_var)


@abstract
class NumLiteral(SingleTokNode):
    annotations = Annotations(repr_name="Num")


class RealLiteral(NumLiteral):
    annotations = Annotations(repr_name="Real")

    @langkit_property()
    def xref_equation():
        return Predicate(BaseTypeDecl.is_real_type, Self.type_var)


class IntLiteral(NumLiteral):
    annotations = Annotations(repr_name="Int")

    @langkit_property()
    def xref_equation():
        return Predicate(BaseTypeDecl.is_int_type, Self.type_var)


class NullLiteral(SingleTokNode):
    annotations = Annotations(repr_name="Null")

    @langkit_property()
    def xref_equation():
        return Predicate(BaseTypeDecl.is_access_type, Self.type_var)


class SingleFormal(Struct):
    name = UserField(type=BaseId)
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
    name = AbstractProperty(type=T.Name, ignore_warn_on_node=True)
    returns = AbstractProperty(type=T.TypeExpr.entity)

    node_params = AbstractProperty(type=T.ParamSpec.array, public=True)
    params = Property(Self.node_params.map(lambda p: p.as_entity))

    abstract_formal_params = Property(
        Entity.params.map(lambda p: p.cast(BaseFormalParamDecl))
    )

    @langkit_property(return_type=BoolType)
    def match_return_type(other=T.SubpSpec.entity):
        # Check that the return type is the same. Caveat: it's not because
        # we could not find the canonical type that it is null!
        #
        # TODO: simplify this code when SubpSpec provides a kind to
        # distinguish functions and procedures.
        return Or(
            And(other.returns.is_null, Entity.returns.is_null),
            And(
                Not(other.returns.is_null), Not(Entity.returns.is_null),
                origin.bind(other.el, other.returns._.canonical_type)
                == origin.bind(Self, Entity.returns._.canonical_type)
            )
        )

    @langkit_property(return_type=BoolType)
    def match_signature(other=T.SubpSpec.entity):
        """
        Return whether SubpSpec's signature matches Self's.

        Note that the comparison for types isn't just a name comparison: it
        compares the canonical subtype.
        """
        return And(
            # Check that the names are the same
            Self.name.matches(other.name),
            Entity.match_return_type(other),
            Entity.match_formal_params(other),
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

    @langkit_property(return_type=T.BasicDecl.array)
    def dottable_subp():
        """
        Used for environments. Returns either an empty array, or an array
        containg the subprogram declaration for this spec, if self meets the
        criterias for being a dottable subprogram.
        """
        bd = Var(Entity.parent.cast_or_raise(BasicDecl))
        return origin.bind(Entity.name, If(
            And(
                Entity.nb_max_params > 0,
                Entity.potential_dottable_type.then(lambda t: And(
                    # Dot notation only works on tagged types
                    t.is_tagged_type,

                    Or(
                        # Needs to be declared in the same scope as the type
                        t.declarative_scope == bd.declarative_scope,

                        # Or in the private part corresponding to the type's
                        # public part. TODO: This is invalid because it will
                        # make private subprograms visible from the outside.
                        # Fix:
                        #
                        # 1. Add a property that synthetizes a full view node
                        # for a tagged type when there isn't one in the source.
                        #
                        # 2. Add this synthetized full view to the private
                        # part of the package where the tagged type is defined,
                        # if there is one, as part of the tagged type
                        # definition's env spec.
                        #
                        # 3. When computing the private part, if there is a
                        # real in-source full view for the tagged type,
                        # replace the synthetic one.
                        #
                        # 4. Then we can just add the private dottable
                        # subprograms to the private full view.

                        t.declarative_scope == (
                            bd.declarative_scope._.parent.cast(PackageDecl)
                            .then(lambda pd: pd.public_part)
                        )
                    )
                ))
            ),
            bd.singleton,
            EmptyArray(T.BasicDecl)
        ))


class SubpSpec(BaseSubpSpec):
    subp_kind = Field(type=T.SubpKind)
    subp_name = Field(type=T.Name)
    subp_params = Field(type=T.Params)
    subp_returns = Field(type=T.TypeExpr)

    name = Property(Self.subp_name)

    node_params = Property(
        Self.subp_params.then(
            lambda p: p.params.map(lambda p: p),
            default_val=EmptyArray(ParamSpec)
        )
    )
    returns = Property(Entity.subp_returns)


class EntryDecl(BasicDecl):
    overriding = Field(type=Overriding)
    spec = Field(type=T.EntrySpec)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.spec.name.cast(T.Name).singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Self.relative_name, Self),
        add_env()
    )


class EntrySpec(BaseFormalParamHolder):
    name = Field(type=T.Identifier)
    family_type = Field(type=T.AdaNode)
    params = Field(type=T.Params)

    abstract_formal_params = Property(
        Entity.params.params.map(lambda p: p.cast(BaseFormalParamDecl))
    )


class Quantifier(EnumNode):
    alternatives = ["all", "some"]


class IterType(EnumNode):
    alternatives = ["in", "of"]


@abstract
class LoopSpec(AdaNode):
    pass

    @langkit_property(return_type=EquationType,
                      kind=AbstractKind.abstract_runtime_check)
    def xref_equation():
        pass


class ForLoopVarDecl(BasicDecl):
    id = Field(type=T.Identifier)
    id_type = Field(type=T.SubtypeIndication)

    defining_names = Property(Self.id.cast(T.Name).singleton)

    expr_type = Property(If(
        Self.id_type.is_null,

        # The type of a for loop variable does not need to be annotated, it can
        # eventually be infered, which necessitates name resolution on the loop
        # specification. Run resolution if necessary.
        Let(lambda p=If(
            Self.id.type_val.is_null,
            Self.parent.parent
            .cast(T.ForLoopStmt).spec.as_entity.resolve_names,
            True
        ): If(p, Self.id.type_val.cast_or_raise(BaseTypeDecl.entity),
              No(BaseTypeDecl.entity))),

        # If there is a type annotation, just return it
        Entity.id_type.designated_type.canonical_type
    ))

    env_spec = EnvSpec(add_to_env_kv(Self.id.sym, Self))


class ForLoopSpec(LoopSpec):
    var_decl = Field(type=T.ForLoopVarDecl)
    loop_type = Field(type=IterType)
    has_reverse = Field(type=Reverse)
    iter_expr = Field(type=T.AdaNode)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        int = Var(Self.std_entity('Integer'))

        return Self.loop_type.match(

            # This is a for .. in
            lambda _=IterType.alt_in:

            # Let's handle the different possibilities
            Entity.iter_expr.match(
                # Anonymous range case: for I in 1 .. 100
                # In that case, the type of everything is Standard.Integer.
                lambda binop=T.BinOp:
                Bind(binop.type_var, int) &
                Bind(binop.left.type_var, int) &
                Bind(binop.right.type_var, int) &
                Bind(Self.var_decl.id.type_var, int),

                # Subtype indication case: the induction variable is of the
                # type.
                lambda t=T.SubtypeIndication:
                Bind(Self.var_decl.id.type_var,
                     t.designated_type.canonical_type),

                # Name case: Either the name is a subtype indication, or an
                # attribute on a subtype indication, in which case the logic is
                # the same as above, either it's an expression that yields an
                # iterator.
                lambda t=T.Name: t.name_designated_type.then(
                    lambda typ:
                    Bind(Self.var_decl.id.type_var, typ.canonical_type),
                    # TODO: Handle the iterator case
                    default_val=LogicTrue()
                ),

                lambda _: LogicTrue()  # should never happen
            ),

            # This is a for .. of
            lambda _=IterType.alt_of:
            # Equation for the expression
            Entity.iter_expr.sub_equation

            # Then we want the type of the induction variable to be the
            # component type of the type of the expression.
            & Bind(Self.iter_expr.cast(T.Expr).type_var,
                   Self.var_decl.id.type_var,
                   BaseTypeDecl.comp_type)

            # If there is a type annotation, then the type of var should be
            # conformant.
            & If(Self.var_decl.id_type.is_null,
                 LogicTrue(),
                 Bind(Self.var_decl.id.type_var,
                      Entity.var_decl.id_type.designated_type
                      .canonical_type))

            # Finally, we want the type of the expression to be an iterable
            # type.
            & Predicate(BaseTypeDecl.is_iterable_type,
                        Self.iter_expr.cast(T.Expr).type_var)
        )


class QuantifiedExpr(Expr):
    quantifier = Field(type=Quantifier)
    loop_spec = Field(type=T.ForLoopSpec)
    expr = Field(type=T.Expr)


class Allocator(Expr):
    subpool = Field(type=T.Expr)
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

    @langkit_property(return_type=EquationType)
    def xref_equation():
        typ = Entity.prefix.designated_type_impl.canonical_type

        return (
            Entity.suffix.sub_equation
            & Bind(Self.prefix.ref_var, typ)
            & Bind(Self.prefix.type_var, typ)
            & Bind(Self.suffix.type_var, typ)
            & Bind(Self.type_var, typ)
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

    designated_type_impl = Property(
        If(Self.attribute.sym == 'Class',
           Entity.prefix.designated_type_impl._.classwide_type,
           Entity.prefix.designated_type_impl)
    )

    @langkit_property()
    def xref_equation():
        rel_name = Var(Self.attribute.relative_name)
        return If(
            rel_name.any_of('First', 'Last'),
            Entity.firstlast_xref_equation,
            If(
                rel_name == 'Access',
                Entity.access_equation,
                LogicTrue()
            )
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def access_equation():
        return (
            Entity.prefix.xref_equation
            & Predicate(BaseTypeDecl.is_access_of,
                        Self.type_var,
                        Self.prefix.type_var)
        )

    @langkit_property(return_type=EquationType, dynamic_vars=[env, origin])
    def firstlast_xref_equation():
        typ = Entity.prefix.name_designated_type
        return If(
            Not(typ.is_null),

            # Prefix is a type, bind prefix's ref var to it
            Bind(Self.prefix.ref_var, typ)
            # Self is of this type
            & Bind(Self.type_var, Self.prefix.ref_var),

            # Prefix is not a type, it's an instance
            Entity.prefix.sub_equation
            # It must be an array
            & Predicate(BasicDecl.is_array, Self.prefix.ref_var)
            # Its component type is the type of Self
            & Bind(Self.type_var, Self.prefix.type_var,
                   conv_prop=BaseTypeDecl.comp_type)
        )


class UpdateAttributeRef(AttributeRef):
    pass


class RaiseExpr(Expr):
    exception_name = Field(type=T.Expr)
    error_message = Field(type=T.Expr)


class DottedName(Name):
    prefix = Field(type=T.Name)
    suffix = Field(type=T.BaseId)
    ref_var = Property(Self.suffix.ref_var)

    @langkit_property()
    def designated_env():
        pfx_env = Var(Entity.prefix.designated_env)
        return env.bind(
            pfx_env,
            Entity.suffix.designated_env_impl(True)
        )

    scope = Property(Self.suffix.then(
        lambda sfx: env.bind(Self.parent_scope, sfx.scope),
        default_val=EmptyEnv
    ))

    parent_scope = Property(Self.prefix.scope)

    relative_name = Property(Self.suffix.relative_name)
    base_name = Property(Entity.prefix)

    @langkit_property()
    def env_elements_impl():
        pfx_env = Var(origin.bind(Self, Entity.prefix.designated_env))
        return env.bind(pfx_env, Entity.suffix.env_elements_baseid(True))

    designated_type_impl = Property(lambda: (
        env.bind(Entity.prefix.designated_env,
                 Entity.suffix.designated_type_impl)
    ))

    @langkit_property()
    def xref_equation():
        dt = Entity.designated_type_impl
        base = Var(
            Entity.prefix.sub_equation
            & env.bind(Entity.prefix.designated_env,
                       Entity.suffix.sub_equation)
        )
        return If(
            Not(dt.is_null),
            base,
            base & Entity.env_elements.logic_any(lambda e: (
                Bind(Self.suffix.ref_var, e)
                & e.cast(BasicDecl.entity).constrain_prefix(Self.prefix)
                & Bind(Self.type_var, Self.suffix.type_var)
            ))
        )


class CompilationUnit(AdaNode):
    """Root node for all Ada analysis units."""
    prelude = Field(doc="``with``, ``use`` or ``pragma`` statements.")
    body = Field(type=T.AdaNode)
    pragmas = Field(type=T.Pragma.list)

    env_spec = EnvSpec(call_env_hook(Self))


class SubpBody(Body):
    env_spec = EnvSpec(
        call_env_hook(Self),

        set_initial_env(
            env.bind(Self.initial_env, If(
                Self.is_library_item,
                # In case the subp spec for this library level subprogram is
                # missing, we'll put it in the parent's scope. This way, the
                # xref to it should still resolve.
                Self.subp_spec.name.scope._or(
                    Self.subp_spec.name.parent_scope
                ),
                Self.parent.children_env
            )),
        ),

        # Add the body to its own parent env
        add_to_env_kv(Self.relative_name, Self),

        add_env(),
        ref_used_packages(),
        ref_used_packages_in_spec(),
        ref_generic_formals(),
        ref_std(),

        handle_children(),

        # Add the __body link to the spec, if there is one
        add_to_env_kv(
            '__body', Self,
            dest_env=Self.as_bare_entity.decl_part_entity.then(
                lambda d: d.children_env
            ),
        )
    )

    overriding = Field(type=Overriding)
    subp_spec = Field(type=T.SubpSpec)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_id = Field(type=T.Name)

    defining_names = Property(Self.subp_spec.name.singleton)
    defining_env = Property(Entity.subp_spec.defining_env)

    type_expression = Property(Entity.subp_spec.returns)


class HandledStmts(AdaNode):
    stmts = Field(type=T.AdaNode.list)
    exceptions = Field(type=T.AdaNode.list)


class ExceptionHandler(AdaNode):
    exc_name = Field(type=T.Identifier)
    handled_exceptions = Field(type=T.AdaNode.list)
    stmts = Field(type=T.AdaNode.list)


@abstract
class Stmt(AdaNode):
    xref_entry_point = Property(True)


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
    call = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return (
            Entity.call.sub_equation

            # Call statements can have no return value
            & Bind(Self.call.type_var, No(AdaNode.entity))
        )


class NullStmt(SimpleStmt):
    null_lit = Field(repr=False)

    @langkit_property()
    def xref_equation():
        return LogicTrue()


class AssignStmt(SimpleStmt):
    dest = Field(type=T.Expr)
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
        return Entity.label_name.sub_equation


class ExitStmt(SimpleStmt):
    loop_name = Field(type=T.Identifier)
    condition = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return (
            Entity.condition.sub_equation
            & Bind(Self.condition.type_var, Self.bool_type)
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
                Entity.subp.subp_spec.returns.designated_type.canonical_type,
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
        return Entity.expr.sub_equation & Bind(
            Self.expr.type_var, Self.std_entity('Duration')
        )


class RaiseStmt(SimpleStmt):
    exception_name = Field(type=T.Expr)
    error_message = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return Entity.exception_name.sub_equation


class IfStmt(CompositeStmt):
    cond_expr = Field(type=T.Expr)
    then_stmts = Field(type=T.AdaNode.list)
    alternatives = Field(type=T.ElsifStmtPart.list)
    else_stmts = Field(type=T.AdaNode.list)

    @langkit_property()
    def xref_equation():
        return (
            Entity.cond_expr.sub_equation
            & Bind(Self.cond_expr.type_var, Self.bool_type)
            & Entity.alternatives.logic_all(
                lambda elsif: elsif.cond_expr.sub_equation
                & Bind(elsif.cond_expr.type_var, Self.bool_type)
            )
        )


class ElsifStmtPart(AdaNode):
    cond_expr = Field(type=T.Expr)
    stmts = Field(type=T.AdaNode.list)


class LabelDecl(BasicDecl):
    name = Field(type=T.Identifier)
    env_spec = EnvSpec(add_to_env_kv(Self.name.sym, Self))
    defining_names = Property(Self.name.cast(T.Name).singleton)


class Label(SimpleStmt):
    decl = Field(type=T.LabelDecl)


class WhileLoopSpec(LoopSpec):
    expr = Field(type=T.Expr)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return Entity.expr.sub_equation & (
            Bind(Self.expr.type_var, Self.bool_type)
        )


class NamedStmtDecl(BasicDecl):
    """
    BasicDecl that is always the declaration inside a named statement.
    """
    name = Field(type=T.Identifier)
    defining_names = Property(Self.name.cast(T.Name).singleton)
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
        add_to_env_kv(Self.decl.name.sym, Self.decl),
        add_env()
    )

    xref_equation = Property(LogicTrue())


@abstract
class BaseLoopStmt(CompositeStmt):
    spec = Field(type=T.LoopSpec)
    stmts = Field(type=T.AdaNode.list)
    end_id = Field(type=T.Identifier)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return Entity.spec._.xref_equation._or(LogicTrue())


class LoopStmt(BaseLoopStmt):
    pass


class ForLoopStmt(BaseLoopStmt):
    pass


class WhileLoopStmt(BaseLoopStmt):
    pass


@abstract
class BlockStmt(CompositeStmt):
    env_spec = EnvSpec(add_env())

    xref_equation = Property(LogicTrue())


class DeclBlock(BlockStmt):
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_id = Field(type=T.Identifier)


class BeginBlock(BlockStmt):
    stmts = Field(type=T.HandledStmts)
    end_id = Field(type=T.Identifier)


class ExtendedReturnStmt(CompositeStmt):
    object_decl = Field(type=T.ObjectDecl)
    stmts = Field(type=T.HandledStmts)

    @langkit_property(return_type=EquationType)
    def xref_equation():
        return LogicTrue()

    env_spec = EnvSpec(add_env())


class CaseStmt(CompositeStmt):
    case_expr = Field(type=T.Expr)
    case_alts = Field(type=T.CaseStmtAlternative.list)

    @langkit_property()
    def xref_equation():
        ignore(Var(Entity.case_expr.resolve_names_internal(
            True, Predicate(BaseTypeDecl.is_discrete_type,
                            Self.case_expr.type_var)
        )))

        return Entity.case_alts.logic_all(lambda alt: (
            alt.choices.logic_all(lambda c: c.match(
                # Expression case
                lambda e=T.Expr:
                Bind(e.type_var, Self.case_expr.type_val) & e.sub_equation,

                # TODO: Bind other cases: SubtypeIndication and Range
                lambda _: LogicTrue()
            ))
        ))


class CaseStmtAlternative(AdaNode):
    choices = Field(type=T.AdaNode.list)
    stmts = Field(type=T.AdaNode.list)


class AcceptStmt(CompositeStmt):
    name = Field(type=T.Identifier)
    entry_index_expr = Field(type=T.Expr)
    params = Field(type=T.Params)

    env_spec = EnvSpec(add_env())

    xref_equation = Property(LogicTrue())


class AcceptStmtWithStmts(AcceptStmt):
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.Name)

    xref_equation = Property(LogicTrue())


class SelectStmt(CompositeStmt):
    guards = Field(type=T.SelectWhenPart.list)
    else_stmts = Field(type=T.StmtList)
    abort_stmts = Field(type=T.StmtList)

    @langkit_property()
    def xref_equation():
        return Entity.guards.logic_all(lambda wp: wp.sub_equation)


class SelectWhenPart(AdaNode):
    condition = Field(type=T.Expr)
    stmts = Field(type=T.AdaNode.list)

    @langkit_property()
    def xref_equation():
        return (
            Entity.condition.sub_equation
            & Bind(Self.condition.type_var, Self.bool_type)
        )


class TerminateAlternative(SimpleStmt):
    pass


class PackageBody(Body):
    env_spec = child_unit('__body', Self.body_scope)

    package_name = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_id = Field(type=T.Name)

    defining_names = Property(Self.package_name.singleton)
    defining_env = Property(Entity.children_env)

    @langkit_property(dynamic_vars=[env])
    def body_scope():
        scope = Var(env.bind(
            Self.parent.node_env,

            # Subunits always appear at the top-level in package bodies. So if
            # this is a subunit, the scope is the same as the scope of the
            # corresponding "is separate" decl, hence: the defining env of this
            # top-level package body.
            Self.parent.cast(T.Subunit).then(
                lambda su: Self.top_level_item(
                    su.name.referenced_unit(UnitBody)
                ).children_env,
                default_val=Self.package_name.scope
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
        return public_scope.get('__privatepart', recursive=False).at(0).then(
            lambda pp: pp.children_env, default_val=public_scope
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
    name = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.Name)

    defining_names = Property(Self.name.singleton)

    env_spec = EnvSpec(
        add_to_env_kv(Self.relative_name, Self),
        add_env(),
    )

    @langkit_property()
    def task_type():
        return Entity.parent.node_env.get(Self.relative_name).find(
            lambda sp: sp.is_a(T.TaskTypeDecl)
        ).cast(T.TaskTypeDecl)


class ProtectedBody(Body):
    name = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    end_name = Field(type=T.Name)

    defining_names = Property(Self.name.singleton)


class EntryBody(Body):
    entry_name = Field(type=T.Identifier)
    index_spec = Field(type=T.EntryIndexSpec)
    params = Field(type=T.Params)
    barrier = Field(type=T.Expr)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.Name)

    defining_names = Property(Self.entry_name.cast(Name).singleton)


class EntryIndexSpec(AdaNode):
    id = Field(type=T.Identifier)
    subtype = Field(type=T.AdaNode)


class Subunit(AdaNode):
    name = Field(type=T.Name)
    body = Field(type=T.Body)


class ProtectedBodyStub(BodyStub):
    name = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.name.singleton)


class SubpBodyStub(BodyStub):
    overriding = Field(type=Overriding)
    subp_spec = Field(type=T.SubpSpec)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.subp_spec.name.singleton)
    # Note that we don't have to override the defining_env property here since
    # what we put in lexical environment is their SubpSpec child.

    env_spec = EnvSpec(
        add_to_env_kv(Self.relative_name, Self),
        # TODO: If subp body stubs can be separates, we need to handle that
        # here.
        add_env(),
    )

    type_expression = Property(Entity.subp_spec.returns)


class PackageBodyStub(BodyStub):
    name = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.name.singleton)


class TaskBodyStub(BodyStub):
    name = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Self.name.singleton)


class LibraryItem(AdaNode):
    has_private = Field(type=Private)
    item = Field(type=T.BasicDecl)


class RangeSpec(AdaNode):
    range = Field(type=Expr)

    xref_equation = Property(Entity.range.xref_equation)


class IncompleteTypeDecl(BaseTypeDecl):
    discriminants = Field(type=T.DiscriminantPart)


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
