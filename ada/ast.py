from langkit.dsl import (
    ASTNode, AbstractField, AnalysisUnit, AnalysisUnitKind, Annotations, Bool,
    Enum, EnumValue, Equation, Field, Int, LexicalEnv, LogicVar,
    LookupKind as LK, NullField, Struct, Symbol, T, UserField, abstract,
    env_metadata, has_abstract_list, synthetic
)
from langkit.envs import (
    EnvSpec, RefKind, add_env, add_to_env, add_to_env_kv, do, handle_children,
    reference, set_initial_env
)
from langkit.expressions import (
    AbstractKind, AbstractProperty, And, ArrayLiteral as Array, BigIntLiteral,
    Bind, Cond, DynamicLexicalEnv, DynamicVariable, EmptyEnv, Entity, If, Let,
    Literal, NPropagate, No, Not, Or, Property, PropertyError, RefCategories,
    Self, String, Try, Var, current_env, direct_env, ignore, langkit_property,
    lazy_field, named_env, new_env_assoc, no_env
)
from langkit.expressions.logic import LogicFalse, LogicTrue, Predicate


env = DynamicVariable('env', LexicalEnv)
origin = DynamicVariable('origin', T.AdaNode)
no_visibility = DynamicVariable('no_visibility', T.Bool)
include_ud_indexing = DynamicVariable('include_ud_indexing', T.Bool)
"""
The ``include_ud_indexing`` var is used to propagate the information of
whether we should look for user-defined indexing functions when building the
defining env of a type. Typically, this is set to True when the defining env
is needed by a CallExpr, but is False in other contexts.
"""

imprecise_fallback = DynamicVariable('imprecise_fallback', Bool)

UnitSpecification = AnalysisUnitKind.unit_specification
UnitBody = AnalysisUnitKind.unit_body

all_categories = RefCategories(default=True)
no_prims = RefCategories(inherited_primitives=False, default=True)


def default_origin():
    """
    Helper to return an origin dynamic param spec wich defaults to
    No(AdaNode).
    """
    return (origin, No(T.AdaNode))


def default_no_visibility():
    """
    Helper to return a no_visibility dynamic param spec which defaults to
    False.
    """
    return (no_visibility, False)


def default_imprecise_fallback():
    """
    Helper to return an imprecise fallback dynamic param spec which defaults to
    False.
    """
    return (imprecise_fallback, False)


def default_include_ud_indexing():
    """
    Helper to return an include_ud_indexing dynamic param spec which defaults
    to False.
    """
    return (include_ud_indexing, False)


@env_metadata
class Metadata(Struct):
    dottable_subp = UserField(
        Bool, doc="Whether the stored element is a subprogram accessed through"
                  " the dot notation",
        default_value=False
    )
    primitive = UserField(
        T.AdaNode,
        doc="The type for which this subprogram is a primitive, if any",
        default_value=No(T.AdaNode)
    )
    primitive_real_type = UserField(
        T.AdaNode,
        doc="The type for which this subprogram is a primitive, if any",
        default_value=No(T.AdaNode)
    )


class CompletionItem(Struct):
    decl = UserField(T.BasicDecl.entity)
    is_dot_call = UserField(T.Bool, default_value=False)
    is_visible = UserField(T.Bool, default_value=True)
    weight = UserField(T.Int, default_value=0)


@abstract
class AdaNode(ASTNode):
    """
    Root node class for the Ada syntax tree.
    """

    annotations = Annotations(
        generic_list_type='AdaList',
        warn_on_node=True
    )

    declarative_scope = Property(
        Self.parents.find(
            lambda p: p.is_a(T.DeclarativePart)
        ).cast(T.DeclarativePart),
        doc="Return the scope of definition of this basic declaration.",
        ignore_warn_on_node=True,
        public=True
    )

    @langkit_property(ignore_warn_on_node=True)
    def owning_unit_kind():
        """
        Return the kind of the compilation unit owning this node.
        """
        return Self.unit.root.cast_or_raise(T.CompilationUnit).unit_kind

    @langkit_property(ignore_warn_on_node=True)
    def withed_unit_helper(unit_name=T.Name):
        """
        Static method helper. Fetch the unit designated by unit_name. Return
        the compilation unit node.

        This is designed in a way that will emit a
        ``unit_requested(not_found_is_error=True, ...)`` event when not
        finding the unit is supposed to be an error within Ada semantics.
        """
        # Try to fetch the spec and the body for ``unit_name``, but do not emit
        # a unit_requested event yet.
        unit_name_array = Var(unit_name.as_symbol_array)
        spec = Var(Self.designated_compilation_unit(
            unit_name_array,
            kind=UnitSpecification,
            not_found_is_error=False
        ))
        body = Var(If(
            spec.is_null,
            Self.designated_compilation_unit(
                unit_name_array,
                kind=UnitBody,
                not_found_is_error=False
            ),

            No(T.CompilationUnit)
        ))

        # Emit an event if one missing unit is actually required by Ada's
        # semantics: either when we have a package body but got no spec, or
        # when we have no body and no spec.
        ignore(Var(If(
            body._.decl.is_a(T.PackageBody) & spec.is_null
            | spec.is_null & body.is_null,

            Self.designated_compilation_unit(
                unit_name_array, kind=UnitSpecification,
                not_found_is_error=True
            ),

            No(T.CompilationUnit)
        )))

        # Return the requested unit (the spec takes precedence)
        return spec._or(body)

    @langkit_property(return_type=T.String)
    def custom_id_text():
        """
        Custom Unique identifying text used to recognize this node. Not
        applicable to all nodes, but on AdaNode because it spans more than one
        hierarchy of node types.
        """
        return String("")

    @langkit_property(return_type=Bool)
    def is_contract_aspect(name=Symbol):
        return name.any_of(
            "Pre", "Pre'Class", "Post", "Post'Class", "Refined_Post",
            "Precondition", "Postcondition", "Precondition'Class",
            "Postcondition'Class", "Invariant", "Invariant'Class",
            "Type_Invariant", "Type_Invariant'Class",
            "Predicate", "Static_Predicate", "Dynamic_Predicate",
            "Default_Initial_Condition",
            "Contract_Cases", "Test_Case",
            "Global", "Refined_Global", "Refined_State",
            "Stable_Properties",
            "Depends", "Refined_Depends",
            "Predicate_Failure"
        )

    in_contract = Property(Not(Self.parents.find(
        lambda p: p.cast(T.AspectAssoc).then(
            lambda a: Self.is_contract_aspect(a.id.as_bare_entity.name_symbol)
        )._or(p.cast(Pragma).then(
            lambda p: Self.is_contract_aspect(p.id.as_bare_entity.name_symbol)
        ))
    ).is_null))

    @langkit_property()
    def in_aspect(name=T.Symbol):
        """
        Return whether Self is contained by an aspect whose name is ``name``.
        """
        return Self.parents.any(
            lambda p: p.cast(AspectAssoc).then(lambda a: a.id.name_is(name))
        )

    empty_env = Property(
        Self.parents.find(lambda p: p.is_a(T.CompilationUnit))
        .cast(T.CompilationUnit).get_empty_env,
    )

    @langkit_property(return_type=Bool)
    def is_not_null():
        """
        Return True iff this node is not null.
        """
        # TODO: Remove this once we have better logic predicates: it is
        # currently not possible to pass an arbitrary DSL expression to a
        # predicate, so we must have a property for every expression that we
        # might want to pass to a predicate.
        return Not(Entity.is_null)

    @langkit_property(return_type=T.EvalDiscreteRange)
    def eval_discrete_range(dr=T.DiscreteRange):
        """
        Static method. Evaluate the bounds of ``dr``.
        """
        return If(
            dr == No(T.DiscreteRange),
            PropertyError(
                EvalDiscreteRange,
                "Attempting to evaluate a null discrete range"
            ),
            T.EvalDiscreteRange.new(
                low_bound=dr.low_bound.then(
                    lambda lb: lb.eval_as_int,
                    default_val=BigIntLiteral(0)
                ),
                high_bound=dr.high_bound.eval_as_int
            )
        )

    @langkit_property(return_type=T.String)
    def sym_join(syms=Symbol.array, sep=T.String):
        """
        Static method. Return the array of symbols joined by separator ``sep``.
        """
        return sep.join(syms.map(lambda s: s.image))

    @langkit_property(return_type=T.CompilationUnit, ignore_warn_on_node=True,
                      public=True)
    def enclosing_compilation_unit():
        """
        Return the compilation unit containing this node.

        .. note:: This returns the :typeref:`CompilationUnit` node, which is
           different from the ``AnalysisUnit``. In particular, an analysis unit
           can contain multiple compilation units.
        """
        return Self.parents.find(
            lambda n: n.is_a(CompilationUnit)
        ).cast_or_raise(CompilationUnit)

    @langkit_property(return_type=Bool)
    def is_children_env(parent=LexicalEnv, current_env=LexicalEnv):
        """
        Static property. Will return True if current_env is a children of
        parent.
        """
        return Cond(
            parent == EmptyEnv, False,
            current_env == parent, True,
            current_env.is_null, False,
            Self.is_children_env(parent, current_env.env_parent)
        )

    @langkit_property(return_type=T.AdaNode.entity)
    def without_md():
        """
        Return Entity with an empty metadata field.
        """
        return AdaNode.entity.new(
            node=Entity.node, info=T.entity_info.new(
                rebindings=Entity.info.rebindings,
                md=No(Metadata),
                from_rebound=Entity.info.from_rebound
            )
        )

    @langkit_property(return_type=T.AdaNode.entity, public=True)
    def get_uninstantiated_node():
        """
        Assuming this node comes from an instantiated generic declaration,
        return its non-instantiated counterpart lying in the generic
        declaration.
        """
        return Self.as_bare_entity

    @langkit_property(public=True, return_type=T.CompletionItem.iterator,
                      final=True)
    def complete():
        """
        Return possible completions at this point in the file.
        """
        return origin.bind(
            Self.origin_node,
            Entity.complete_items.filter(
                # This property filters out `SyntheticSubpDecl` items
                # because they are of no use for completion. Additional
                # filtering can be done in `complete_items`.
                lambda n: n.decl.cast(T.SyntheticSubpDecl).is_null
            ).to_iterator
        )

    @langkit_property(return_type=T.CompletionItem.array,
                      dynamic_vars=[origin])
    def complete_items():
        """
        Internal method used by ``complete`` to get the array of possible
        completions for the current node. This method has to be overridden in
        order to specialize the completion.
        """
        return Self.env_get_public(
            Self.children_env,
            No(Symbol)
        ).map(
            lambda n: CompletionItem.new(
                decl=n.cast(T.BasicDecl),
                is_dot_call=n.info.md.dottable_subp,
                is_visible=Self.has_with_visibility(n.unit),
                weight=Self.complete_item_weight(n.cast(T.BasicDecl))
            )
        )

    @langkit_property(return_type=Int, dynamic_vars=[origin])
    def complete_item_weight(item=T.BasicDecl.entity):
        """
        Internal method used by ``complete_items`` that can be used to
        specialize the completion weight field only.
        """
        ignore(item)
        return 0

    @langkit_property(public=True, return_type=T.Symbol.array)
    def valid_keywords():
        """
        Return the list of keywords that are valid at this point in the file.

        .. note::
            This is work in progress. It will return all keywords for now,
            without looking at the context.
        """
        return Array([
            "abort", "abs", "abstract", "accept", "access", "aliased", "all",
            "and", "array", "at", "begin", "body", "case", "constant",
            "declare", "delay", "delta", "digits", "do", "else", "elsif",
            "end", "entry", "exception", "exit", "for", "function", "generic",
            "goto", "if", "in", "interface", "is", "limited", "loop", "mod",
            "new", "not", "null", "others", "out", "of", "or", "overriding",
            "package", "pragma", "private", "procedure", "protected", "raise",
            "range", "record", "rem", "renames", "requeue", "return",
            "reverse", "select", "separate", "some", "subtype", "synchronized",
            "tagged", "task", "terminate", "then", "type", "until", "use",
            "when", "while", "with", "xor"
        ])

    @langkit_property(public=True)
    def generic_instantiations():
        """
        Return the potentially empty list of generic package/subprogram
        instantiations that led to the creation of this entity. Outer-most
        instantiations appear last.
        """
        return Self.generic_instantiations_internal(Entity.info.rebindings)

    @langkit_property(return_type=T.GenericInstantiation.entity.array)
    def generic_instantiations_internal(r=T.EnvRebindings):
        return If(
            r == No(T.EnvRebindings),
            No(T.GenericInstantiation.entity.array),

            Let(lambda
                head=(r.new_env.env_node
                      .cast_or_raise(T.GenericInstantiation).as_bare_entity),
                tail=Self.generic_instantiations_internal(r.get_parent):
                head.singleton.concat(tail))
        )

    @langkit_property(return_type=T.EnvRebindings)
    def remove_rebindings(base=T.EnvRebindings, suffix=T.EnvRebindings):
        """
        If the rebindings in ``base`` end with ``suffix``, ``base`` is
        returned without it. Otherwise ``base`` is returned as-is.
        """
        return Cond(
            base.is_null | suffix.is_null,
            base,

            And(base.old_env == suffix.old_env,
                base.new_env == suffix.new_env),
            Self.remove_rebindings(base.get_parent, suffix.get_parent),

            base
        )

    # We mark this property as memoizable because for the moment, we only ever
    # get the first result of logic resolution, so we only ever want the result
    # of the first evaluation of this property. When we change that, we'll
    # probably change the solving API anyway.
    @langkit_property(call_memoizable=True, return_type=T.LogicValResult)
    def logic_val(from_node=T.AdaNode.entity, lvar=LogicVar):
        success = Var(from_node.resolve_names_from_closest_entry_point)

        return LogicValResult.new(success=success, value=If(
            success, lvar.get_value, No(T.AdaNode.entity)
        ))

    @langkit_property(return_type=T.AdaNode.entity)
    def semantic_parent_helper(env=LexicalEnv):
        return env.then(lambda env: env.env_node.as_entity._or(
            Entity.semantic_parent_helper(env.env_parent)
        ))

    @langkit_property(public=True)
    def semantic_parent():
        """
        Return the semantic parent for this node, if applicable, null
        otherwise.

        .. note:: A node lying outside of a library item's declaration or
            subunit's body does not have a parent environment, meaning that
            this property will return null.
        """
        return Entity.semantic_parent_helper(Entity.node_env)

    @langkit_property(return_type=T.Bool)
    def is_in_top_level_public_part():
        """
        Return whether this node ultimately lies in the public part of a
        top-level package.
        """
        node = Var(Entity.semantic_parent)
        return Cond(
            node.is_null,
            False,

            node.is_a(PrivatePart)
            & node.parent.cast(BasePackageDecl)._.is_compilation_unit_root,
            False,

            node.is_a(PackageBody)
            & node.cast(PackageBody).is_compilation_unit_root,
            False,

            node.is_a(BasePackageDecl)
            & node.cast(BasePackageDecl).is_compilation_unit_root,
            True,

            node.is_in_top_level_public_part
        )

    @langkit_property(public=True, return_type=T.BasicDecl.entity)
    def parent_basic_decl():
        """
        Return the parent basic decl for this node, if applicable, null
        otherwise.

        .. note:: If the parent BasicDecl of the given node is a generic
            declaration, this call will return the instantiation from which
            the node was retrieved instead, if any.

        .. note:: When called on a subunit's body, this property will return
            the its corresponding body stub.

        .. note:: When called on a node lying outside of a library item's
            declaration or subunit's body this property will return null.
        """
        return If(
            # On synthetic types that are rooted in their parents, we want to
            # call parent_basic_decl on the parent type, to avoid getting the
            # type itself as a parent_basic_decl (since some types introduce a
            # scope).
            Entity.is_a(ClasswideTypeDecl, DiscreteBaseSubtypeDecl,
                        SynthAnonymousTypeDecl),
            Entity.semantic_parent.parent_basic_decl,
            Entity.semantic_parent.then(
                lambda sp: If(
                    sp.is_a(GenericSubpInternal, GenericPackageInternal),
                    sp.cast(BasicDecl).get_instantiation,
                    sp.cast(BasicDecl)
                )._or(sp.parent_basic_decl)
            )
        )

    @langkit_property(return_type=T.LexicalEnv)
    def immediate_declarative_region():
        """
        Return the immediate declarative region (:rmlink:`8.1`)
        corresponding to this node, that is, the concatenation of the
        declarative parts of itself and all its completion. This does not
        include the declarative regions of the enclosed declarations.

        This is mainly used to restrict the scope in which to search for the
        previous part of a declaration.
        """
        return No(T.LexicalEnv)

    @langkit_property(
        return_type=AnalysisUnit, external=True, uses_entity_info=False,
        uses_envs=False,
        # TODO (S917-027): re-enable this protection or remove it once we
        # moved forward on memoization soundness issues in Langkit.
        call_non_memoizable_because=(
            None and
            'Getting an analysis unit cannot appear in a memoized context'
        )
    )
    def get_unit(name=Symbol.array, kind=AnalysisUnitKind,
                 load_if_needed=Bool, not_found_is_error=Bool,
                 process_parents=(Bool, True)):
        """
        Return the analysis unit for the given ``kind`` corresponding to this
        Name. Return null if ``load_if_needed`` is false and the unit is not
        loaded yet.

        For nested library units, this will trigger the processing of parent
        library units, so for example, if you ``get_unit('A.B.C')``, this will
        load units ``A.B.C``, ``A.B`` and ``A``, except if ``process_parents``
        is False.

        ``not_found_is_error`` will condition the parameter of the same name in
        the ``Unit_Requested`` callback. The client of ``get_unit`` is supposed
        to pass ``True`` if the unit not being found is an error in the Ada
        sense.
        """
        pass

    @langkit_property(return_type=T.CompilationUnit, ignore_warn_on_node=True)
    def designated_compilation_unit(name=T.Symbol.array,
                                    kind=AnalysisUnitKind,
                                    load_if_needed=(Bool, True),
                                    not_found_is_error=(Bool, True),
                                    process_parents=(Bool, True)):
        """
        Fetch the compilation unit designated by the given name defined in an
        analysis unit of the given kind.
        """
        designated_analysis_unit = Var(
            Self.get_unit(name, kind, load_if_needed,
                          not_found_is_error, process_parents)
        )

        return Self.compilation_unit_with_name(designated_analysis_unit, name)

    @langkit_property(return_type=T.CompilationUnit, ignore_warn_on_node=True)
    def compilation_unit_with_name(unit=T.AnalysisUnit, name=T.Symbol.array):
        """
        Helper for ``designated_compilation_unit``. From a given analysis unit,
        that might contain several compilation units, and a name, return the
        corresponding compilation unit.
        """
        return unit.root._.match(
            # If the root of the analysis unit is a single compilation unit,
            # it is necessarily the one we look for.
            lambda single=CompilationUnit: single,

            # If the root of the analysis unit comprises multiple compilation
            # units, look for the one with a matching fully qualified name.
            lambda multi=CompilationUnit.list: multi.find(
                lambda c: c.syntactic_fully_qualified_name == name
            ),

            lambda _: PropertyError(CompilationUnit,
                                    "Unexpected analysis unit root")
        )

    @langkit_property(return_type=T.BasicDecl, uses_entity_info=False,
                      ignore_warn_on_node=True)
    def get_unit_root_decl(name=Symbol.array, kind=AnalysisUnitKind,
                           load_if_needed=(Bool, True),
                           not_found_is_error=(Bool, True),
                           process_parents=(Bool, True)):
        """
        If the corresponding analysis unit is loaded, return the root decl
        node for the given analysis unit ``kind`` and correpsonding to the
        name ``name``. If it's not loaded, return none.
        """
        cu = Var(Self.designated_compilation_unit(
            name, kind, load_if_needed, not_found_is_error, process_parents
        ))
        return cu._.decl

    @langkit_property(public=True, return_type=AnalysisUnit.array,
                      external=True, uses_entity_info=False, uses_envs=False)
    def filter_is_imported_by(units=AnalysisUnit.array, transitive=Bool):
        """
        Filters out among the list of given units those that cannot refer to
        the unit in which this node lies. If transitive is True, the whole
        transitive closure of imports will be used to find a reference to the
        unit of this node.
        """
        pass

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      return_type=Equation, dynamic_vars=[env, origin],
                      # xref_equation is only called from the external property
                      # resolve_own_names, so we need to ignore the warning.
                      warn_on_unused=False)
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
    stop_resolution_equation = Property(
        LogicTrue(),
        dynamic_vars=[env, origin]
    )

    @langkit_property()
    def xref_initial_env():
        """
        Return the environment to bind initially during the construction of the
        xref equation for this node. Note that this only makes sense if this
        node is an xref entry point.
        """
        return Entity.children_env

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
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

    @langkit_property(return_type=Bool, external=True, call_memoizable=True,
                      dynamic_vars=[env, origin], uses_entity_info=True,
                      uses_envs=True)
    def resolve_own_names():
        """
        Internal helper for resolve_names. Resolve names for this node up to
        xref_entry_point and xref_stop_resolution boundaries.
        """
        pass

    @langkit_property(return_type=Bool, dynamic_vars=[env, origin])
    def resolve_children_names():
        """
        Internal helper for resolve_names, implementing the recursive logic
        needed to resolve names across xref_stop_resolution boundaries.
        """
        return Self.children.all(lambda c: c.then(
            # Only resolve nodes that have xref_stop_resolution set, and do not
            # recursively explore nodes that are xref entry points.
            lambda c: If(
                c.xref_entry_point,
                True,
                If(
                    c.as_entity.xref_stop_resolution,
                    c.as_entity.resolve_own_names,
                    True
                ) & c.as_entity.resolve_children_names,
            ),
            default_val=True
        ))

    @langkit_property(return_type=Bool, dynamic_vars=[env, origin])
    def resolve_names_internal():
        """
        Resolves names for this node up to xref_entry_point boundaries.
        """
        return Entity.resolve_own_names & Entity.resolve_children_names

    @langkit_property(return_type=Bool, dynamic_vars=[env, origin])
    def resolve_names_internal_with_eq(additional_equation=Equation):
        """
        Resolves names in this node with an additional constraint given by
        ``additional_equation``, up to xref_entry_point boundaries.
        """
        return ((Entity.xref_equation & additional_equation).solve
                & Entity.resolve_children_names)

    xref_entry_point = Property(
        False,
        public=True,
        doc="""
        Designates entities that are entry point for the xref solving
        infrastructure. If this returns true, then resolve_names can be called
        on it.

        .. note::
            For convenience, and unlike what is defined in the ARM wrt.
            complete contexts for name resolution, ``xref_entry_points`` can be
            nested.
        """
    )

    @langkit_property(return_type=Bool, public=True,
                      memoized=True, call_memoizable=True)
    def resolve_names():
        """
        This will resolve names for this node. If the operation is successful,
        then type_var and ref_var will be bound on appropriate subnodes of the
        statement.
        """
        return env.bind(
            Entity.xref_initial_env,
            origin.bind(Self.origin_node, Entity.resolve_names_internal)
        )

    @langkit_property(return_type=T.LexicalEnv)
    def resolve_names_from_closest_entry_point_impl():
        """
        Implementation helper for ``resolve_names_from_closest_entry_point``.
        Instead of returning a Boolean, it returns a LexicalEnv, which is
        either None (indicating that resolution failed), or contains the
        lexical environment which the children of that node should bind when
        resolving their own names. This allows propagating the initial env
        we got from ``Entity.xref_initial_env`` on the closest xref entry
        point.
        """
        return If(
            # This is the closest entry point: resolve its names and return
            # its `xref_initial_env` if resolution succeeded, so that children
            # will be able to use it to resolve their own names.
            Entity.xref_entry_point,
            env.bind(
                Entity.xref_initial_env,
                origin.bind(
                    Self.origin_node,
                    If(Entity.resolve_own_names, env, No(LexicalEnv))
                )
            ),

            Let(
                # Recurse in order to resolve names from the closest entry
                # point: `res` will contain the environment to use if we need
                # to resolve names inside Self, or None if resolution failed.
                lambda
                res=Entity.parent
                ._.resolve_names_from_closest_entry_point_impl:

                Cond(
                    # Resolution failed for the parent, so return None as well
                    res == No(T.LexicalEnv),
                    res,

                    # Resolution succeeded for the parent and this is a stop
                    # resolution, so re-use the parent environment to resolve
                    # Self's names.
                    Entity.xref_stop_resolution,
                    env.bind(
                        res,
                        origin.bind(
                            Self.origin_node,
                            If(Entity.resolve_own_names, res, No(LexicalEnv))
                        )
                    ),

                    # Resolution succeeded but there is nothing to do on that
                    # particular node: return the parent environment, so that
                    # deeper children can use it.
                    res
                )
            )
        )

    @langkit_property(return_type=Bool)
    def resolve_names_from_closest_entry_point():
        """
        Resolve names from the closest entry point up to this node. Note that
        unlike ``resolve_names``, this will *not* trigger resolution of every
        node with stop_resolution that lie in the sub-tree formed by the
        closest entry point. It will only resolve those that are in the path to
        resolving Self. Consider for example the following entry point:

        .. code::

            R := (A, B);

        Since aggregate association nodes have ``stop_resolution`` set to True,
        calling ``resolve_names_from_closest_entry_point`` on ``B`` will
        resolve nodes ``R`` and ``B`` but not ``A``, because ``A`` does not lie
        on the path to ``B``.

        This can be useful for resolving aggregates of variant records, because
        resolution of a component association can safely call the resolution
        of a discriminant association without triggering an infinite recursion,
        as both are on different "paths".
        """
        result = Var(Entity.resolve_names_from_closest_entry_point_impl)
        return result != No(LexicalEnv)

    @langkit_property(return_type=LexicalEnv)
    def parent_unit_env_helper(unit=AnalysisUnit, env=LexicalEnv):
        return env.env_parent.then(lambda parent_env: parent_env.env_node.then(
            lambda parent_node: If(
                parent_node.unit == unit,
                Self.parent_unit_env_helper(unit, parent_env),
                parent_env
            )
        ))

    @langkit_property()
    def parent_unit_env(env=LexicalEnv):
        """
        Given env's AnalysisUnit, return the first env that has a different
        analysis unit in the env parent chain.
        """
        return env.then(
            lambda env: Self.parent_unit_env_helper(env.env_node.unit, env)
        )

    @langkit_property(return_type=T.AnalysisUnit, public=True,
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
        lambda sym=Symbol: Self.unit.root.std_entity_implem(sym),
        public=True,
        doc="Static property. Return an entity from the standard package"
            " with name ``sym``."
    )

    std_entity_implem = Property(
        lambda sym=Symbol: Self.std_env.get_first(sym, categories=no_prims),
        memoized=True
    )

    bool_type = Property(
        Self.std_entity('Boolean').cast(T.BaseTypeDecl), public=True, doc="""
        Static method. Return the standard Boolean type.
        """
    )
    int_type = Property(
        Self.std_entity('Integer').cast(T.BaseTypeDecl), public=True, doc="""
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
    std_char_type = Property(
        Self.std_entity('Character').cast(T.BaseTypeDecl), public=True, doc="""
        Static method. Return the standard Character type.
        """
    )
    std_wide_char_type = Property(
        Self.std_entity('Wide_Character').cast(T.BaseTypeDecl), public=True,
        doc="""
        Static method. Return the standard Wide_Character type.
        """
    )
    std_wide_wide_char_type = Property(
        Self.std_entity('Wide_Wide_Character').cast(T.BaseTypeDecl),
        public=True, doc="""
        Static method. Return the standard Wide_Wide_Character type.
        """
    )

    std_root_types = Property(
        Self.std_entity('root_types_').cast(T.PackageDecl)._.children_env,
        doc="""
        Static method. Return the package containing the definitions of the
        root types.
        """
    )
    root_int_type = Property(
        Self.std_root_types.get_first(
            'root_integer', categories=no_prims, lookup=LK.minimal
        ).cast(T.BaseTypeDecl),
        doc="Static method. Return the root_integer type."
    )
    root_real_type = Property(
        Self.std_root_types.get_first(
            'root_real', categories=no_prims, lookup=LK.minimal
        ).cast(T.BaseTypeDecl),
        doc="Static method. Return the root_real type."
    )

    @langkit_property(return_type=T.BasicDecl.entity.array, memoized=True,
                      memoize_in_populate=True)
    def root_type_ops_impl(sym=T.Symbol):
        """
        See ``root_type_ops``.
        """
        return Self.std_root_types.get(
            sym, lookup=LK.minimal, categories=no_prims
        ).filtermap(
            lambda n: n.cast(BasicDecl),
            lambda n: n.cast_or_raise(BasicDecl).is_subprogram
        )

    @langkit_property(return_type=T.BasicDecl.entity.array)
    def root_type_ops(sym=T.Symbol):
        """
        Lookup the given symbol in the builtin ``root_types`` package. This is
        used for fast-access to predefined operator on root types.
        """
        # Typical strategy for memoizing "static" functions
        return Self.unit.root.root_type_ops_impl(sym)

    exc_id_type = Property(
        Self
        .get_unit_root_decl(['Ada', 'Exceptions'], UnitSpecification)
        ._.children_env.get_first('Exception_Id', lookup=LK.flat)
        .cast(T.BaseTypeDecl), doc="""
        Return the type Ada.Exceptions.Exception_Id.
        """

    )

    task_id_type = Property(
        Self.get_unit_root_decl(['Ada', 'Task_Identification'],
                                UnitSpecification)
        ._.children_env.get_first('Task_Id', lookup=LK.flat)
        .cast(T.BaseTypeDecl), doc="""
        Return the type Ada.Task_Identification.Task_Id.
        """
    )

    root_buffer_type = Property(
        Self.get_unit_root_decl(['Ada', 'Strings', 'Text_Buffers'],
                                UnitSpecification)
        ._.children_env.get_first('Root_Buffer_Type', lookup=LK.flat)
        .cast(T.BaseTypeDecl), doc="""
        Return the type Ada.Strings.Text_Buffers.Root_Buffer_Type
        """
    )

    root_stream_type = Property(
        Self.get_unit_root_decl(['Ada', 'Streams'], UnitSpecification)
        ._.children_env.get_first('Root_Stream_Type', lookup=LK.flat)
        .cast(T.BaseTypeDecl).classwide_type.cast(T.BaseTypeDecl), doc="""
        Return the type Ada.Streams.Root_Stream_Type
        """
    )

    @langkit_property(return_type=Bool)
    def has_with_visibility(refd_unit=AnalysisUnit):
        """
        Return whether Self's unit has ``with visibility`` on ``refd_unit``.

        In other words, whether Self's unit has a WITH clause on ``refd_unit``,
        or if its spec, or one of its parent specs has one.
        """
        return Or(
            refd_unit.is_referenced_from(Self.unit),
            Self.parent_unit_env(
                # Here we go and explicitly grab the top level item, rather
                # than use Self's children env, because of use clauses, that
                # can be at the top level but semantically belong to the env of
                # the top level item.
                Self.top_level_decl(Self.unit).children_env
            )
            .env_node._.has_with_visibility(refd_unit),

            # With clauses from a library level subprogram declaration are
            # visible by its corresponding body. Since the decl is not the
            # parent of the body, we must specifically take this case into
            # account.
            Self.top_level_decl(
                Self.unit
            ).as_bare_entity.cast(BaseSubpBody).then(
                lambda b: If(
                    b.is_library_item,
                    b.defining_name.referenced_unit(
                        UnitSpecification, not_found_is_error=False
                    ).then(
                        lambda u: u.root._.has_with_visibility(refd_unit)
                    ),
                    False
                )
            ),

            # because of the GNAT kludge around the child packages of
            # Ada.Text_IO, always consider those to be visible. Otherwise it
            # will break any access to P.Integer_IO & co. for any package P
            # that is a renaming of Ada.Text_IO. Indeed, since Integer_IO & co.
            # must behave as nested packages even though they are implemented
            # as child packages, we must consider them visible as soon as P
            # is visible.
            refd_unit.root.cast(CompilationUnit)._.is_text_io_child,
        )

    @langkit_property(return_type=Bool)
    def has_visibility(other_entity=T.AdaNode.entity):
        return Or(
            # The node is a generic package instantiation coming from a formal
            # package.
            other_entity.cast(GenericPackageInstantiation)._.info.from_rebound,

            other_entity.cast(PackageRenamingDecl)._.info.from_rebound,

            # The node is not an unit root
            Not(other_entity.cast(T.BasicDecl).is_compilation_unit_root),

            # Else, check with visibility
            Self.has_with_visibility(other_entity.node.unit)
        )

    @langkit_property()
    def resolve_generic_actual():
        """
        Helper property to resolve the actuals of generic instantiations.
        """
        return Entity.match(
            lambda aod=T.AnonymousExprDecl.entity: aod,

            # Depending on the formal that matches this actual, this name
            # can be either an object, a type or a subprogram.
            # TODO: the code below should execute a specific logic
            # depending on the corresponding kind of the formal (type, object,
            # subprogram, etc.), so we should find a way to make it available.
            lambda n=T.Name.entity:
            # We first try to find a type
            n.name_designated_type.cast(T.entity)
            # If it's an attribute, it might be a reference to a function
            ._or(n.cast(AttributeRef)._.attribute_subprogram)
            # If all that didn't work, find something else
            ._or(n.all_env_elements.at(0)),

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
    def top_level_use_type_clauses():
        """
        If Self is a library item or a subunit, return a flat list of all names
        for top-level UseTypeClause nodes. See UseTypeClause.env_spec
        for more details.
        """
        return (
            Self.parent.parent.cast_or_raise(T.CompilationUnit)
            .prelude
            .filter(lambda p: p.is_a(UseTypeClause))
            .mapcat(
                lambda p: p.cast_or_raise(UseTypeClause).types.map(
                    lambda n: n.cast(AdaNode)
                )
            )
        )

    @langkit_property(return_type=T.UseClause.array)
    def top_level_use_clauses():
        """
        If Self is a library item or a subunit, return a flat list of all names
        for top-level UseClause nodes.
        """
        cu = Var(Self.parent.parent.cast_or_raise(T.CompilationUnit))
        return cu.prelude.filtermap(
            lambda p: p.cast(UseClause),
            lambda p: p.is_a(UseClause)
        )

    @langkit_property(return_type=T.Name.array)
    def top_level_with_package_clauses():
        """
        Return a flat list of all package names that are with'ed by top-level
        WithClause nodes of the compilation unit this node lies in.
        """
        return (
            Self.enclosing_compilation_unit
            .prelude
            .mapcat(lambda p: p.cast(WithClause)._.packages.as_array)
        )

    @langkit_property()
    def use_clauses_in_spec_of_subp_body():
        """
        If Self is a library-level SubpBody, fetch the environments USE'd in
        its declaration.
        """
        fqn = Var(
            Self.enclosing_compilation_unit.syntactic_fully_qualified_name
        )
        spec = Var(Self.designated_compilation_unit(
            name=fqn,
            kind=UnitSpecification,
            not_found_is_error=False
        ))
        return spec._.decl._.top_level_use_clauses.map(
            lambda clause: clause.as_bare_entity.used_envs
        ).env_group()

    @langkit_property()
    def use_clauses_in_generic_formal_part():
        """
        Assuming Self is a generic entity's body that is nested (not a library
        item), return the grouped lexical environment containing all the
        environments that are referred by use clauses inside formal part of
        its generic declaration. Return an empty environment if this is not
        the body of a generic decl.
        """
        gen_decl = Var(Self.as_bare_entity.cast(Body)._.safe_generic_decl_part)
        return gen_decl.then(
            lambda gd: gd.formal_part.use_clauses_envs,
            default_val=Self.empty_env
        )

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
        gen_decl = Var(Self.as_bare_entity.cast(Body)._.safe_generic_decl_part)
        return gen_decl.then(
            lambda gd: gd.node.children_env,
            default_val=Self.empty_env
        )

    @langkit_property()
    def is_package():
        """
        Property helper to determine if an entity is a package or not.
        """
        return Self.is_a(PackageDecl, PackageBody, GenericPackageInstantiation,
                         PackageRenamingDecl, GenericPackageDecl)

    @langkit_property()
    def default_initial_env():
        """
        Provide the default lexical environment to use in EnvSpec's
        initial_env.
        """
        return Self.parent.then(lambda p: p.children_env,
                                default_val=Self.children_env)

    @langkit_property(ignore_warn_on_node=True, public=True)
    def top_level_decl(unit=AnalysisUnit):
        """
        Static method. Get the top-level decl in ``unit``.  This is the body of
        a Subunit, or the item of a ``LibraryItem``.
        """
        return unit._.root._.cast_or_raise(T.CompilationUnit).decl

    @langkit_property()
    def unpack_formals(formal_params=T.BaseFormalParamDecl.entity.array):
        """
        Static method. DefiningName for all parameters.
        """
        return Self.unit.root.unpack_formals_impl(formal_params)

    @langkit_property()
    def unpack_formals_impl(formal_params=T.BaseFormalParamDecl.entity.array):
        return formal_params.mapcat(lambda spec: spec.defining_names)

    @langkit_property(return_type=T.ParamMatch.array)
    def match_formals(formal_params=T.BaseFormalParamDecl.entity.array,
                      params=T.AssocList.entity,
                      is_dottable_subp=Bool):
        """
        Static method. For each ParamAssoc in a AssocList, return whether we
        could find a matching formal in Self, and whether this formal is
        optional (i.e. has a default value).
        """
        unpacked_formals = Var(Self.unpack_formals(formal_params))

        return params.then(lambda p: p.unpacked_params.map(lambda i, a: If(
            a.name.is_null,

            Let(
                lambda idx=If(is_dottable_subp, i + 1, i):
                # Positional parameter case: if this parameter has no
                # name association, make sure we have enough formals.
                unpacked_formals.at(idx).then(
                    lambda sp: ParamMatch.new(
                        has_matched=True,
                        formal=sp, actual=a
                    )
                )
            ),

            # Named parameter case: make sure the designator is
            # actually a name and that there is a corresponding
            # formal.
            a.name.then(lambda id: (
                unpacked_formals.find(lambda p: p.name.matches(id)).then(
                    lambda sp: ParamMatch.new(
                        has_matched=True,
                        formal=sp, actual=a
                    )
                )
            ))
        )))

    @langkit_property(return_type=Bool, public=True)
    def choice_match(value=T.BigInt):
        """
        Assuming that self is a choice expression (such as what can appear in
        an alternative of a case statement or in the RHS of a membership
        expression, this property returns whether the given value satisfies it.

        .. ATTENTION::
            This is an experimental feature, so even if it is exposed to allow
            experiments, it is totally unsupported and the API and behavior are
            very likely to change in the future.
        """
        return Entity.match(

            # If choice is a binop, it is either a range, or a static
            # arithmetic expression.
            lambda bo=T.BinOp: If(
                # If choice is a range, then check that val is in the range
                bo.op.is_a(Op.alt_double_dot),

                And(value >= bo.left.eval_as_int,
                    value <= bo.right.eval_as_int),

                value == bo.eval_as_int,
            ),

            # If choice is a name, it is either a subtype name, either a
            # constant number name.
            lambda n=T.Name: n.name_designated_type.then(
                lambda dt: dt.discrete_range.then(
                    lambda dr: Let(
                        lambda edr=Self.eval_discrete_range(dr): And(
                            value >= edr.low_bound, value <= edr.high_bound
                        )
                    ),
                    default_val=True
                ) & origin.bind(
                    Self, imprecise_fallback.bind(
                        False, dt.satisfies_type_predicates(value)
                    )
                ),
                default_val=(value == n.eval_as_int)
            ),

            # If choice is a subtype indication, then get the range
            lambda st=T.SubtypeIndication: st.discrete_range.then(
                lambda dr: Let(
                    lambda edr=Self.eval_discrete_range(dr): And(
                        value >= edr.low_bound, value <= edr.high_bound
                    )
                ),
                default_val=True
            ) & origin.bind(
                Self, imprecise_fallback.bind(
                    False, st.designated_type.satisfies_type_predicates(value)
                )
            ),

            # If it is an expr, then just check for equality
            lambda e=T.Expr: value == e.eval_as_int,

            # If 'others', always return true
            lambda _=T.OthersDesignator: True,

            lambda _: False,
        )

    @langkit_property(public=True, dynamic_vars=[default_imprecise_fallback()])
    def gnat_xref():
        """
        Return a cross reference from this name to a defining identifier,
        trying to mimic GNAT's xrefs as much as possible.
        """

        bd = Var(Entity.cast(T.Name).enclosing_defining_name
                 .then(lambda dn: dn.basic_decl))

        return origin.bind(Self, Cond(
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
            .primitive_subp_first_type.defining_name,

            bd.then(lambda bd: bd.is_a(T.BasicSubpDecl)),
            bd.cast(T.BasicSubpDecl).subp_decl_spec
            .primitive_subp_first_type.then(
                lambda prim_typ:
                prim_typ.is_tagged_type.then(
                    lambda _: prim_typ.private_completion.then(
                        lambda pc: pc.defining_name
                    )._or(prim_typ.defining_name)
                )
            ),

            Entity.cast(T.Name)._.gnat_xref_decl.then(
                lambda ret:
                Let(lambda dbd=ret.basic_decl: Cond(
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
                    .decl_part._or(dbd).defining_name,

                    ret
                ))
            )
        ))

    @langkit_property(return_type=T.AdaNode, ignore_warn_on_node=True)
    def env_get_real_from_node(from_node=T.AdaNode):
        """
        Static property. Finds the closest BasicSubpDecl / BaseSubpBody /
        GenericInstantiation. Is used by env_get and env_get_first wrappers to
        implement correct visibility rules for those. See documentation on
        those properties.
        """
        return If(from_node.is_null, from_node, Let(
            lambda c=from_node.parents.find(
                lambda n: n.is_a(T.GenericInstantiation, T.BaseSubpSpec)
            ): Cond(
                c.is_null,
                from_node,

                c.is_a(BaseSubpSpec),
                c.cast(BaseSubpSpec).as_bare_entity.name.node,

                c
            )
        ))

    @langkit_property()
    def entity_no_md(n=T.AdaNode, rebindings=T.EnvRebindings,
                     from_rebound=T.Bool):
        """
        Static property. Create an entity from the arguments with a null
        metadata.
        """
        return T.Entity.new(
            node=n,
            info=If(n.is_null, No(T.entity_info), T.entity_info.new(
                rebindings=rebindings,
                md=No(T.env_md),
                from_rebound=from_rebound
            ))
        )

    @langkit_property()
    def env_mappings(defining_names=T.DefiningName.list, value=T.AdaNode):
        """
        Static method. Create an env mapping array from a list of BaseId to be
        used as keys, and a node to be used as value in the mappings.
        """
        return defining_names.map(
            lambda n: new_env_assoc(key=n.name_symbol, value=value)
        )

    @langkit_property(dynamic_vars=[origin])
    def comp_bind(left=T.LogicVar, right=T.LogicVar):
        return Bind(left, right, conv_prop=BaseTypeDecl.comp_type)

    @langkit_property(dynamic_vars=[origin])
    def universal_int_bind(type_var=T.LogicVar):
        """
        Static method. Return an equation that will bind type_var to any
        integer value, corresponding to the notion of universal_integer in the
        Ada RM (see :rmlink:`3.4.1`).
        """
        return Bind(type_var, Self.universal_int_type)

    @langkit_property(dynamic_vars=[origin])
    def universal_real_bind(type_var=T.LogicVar):
        """
        Static method. Return an equation that will bind type_var to any real
        value, corresponding to the notion of universal_real in the Ada RM (see
        :rmlink:`3.4.1`).

        """
        return Bind(type_var, Self.universal_real_type)

    @langkit_property(ignore_warn_on_node=True)
    def origin_node():
        """
        Return a null node iff we are in the definition of an aspect clause
        where sequential lookup needs to be deactivated. Return Self otherwise.
        """
        return If(Self.in_contract, No(T.AdaNode), Self)

    @langkit_property()
    def env_hook():
        """
        Hook for the EnvSpec of units.

        Return value is not significant: the only purpose of this property lies
        in its side effects.
        """
        return Self.parent.match(
            lambda _=T.LibraryItem: Self.match(
                lambda b=T.Body: b.env_hook_body,
                lambda bd=T.BasicDecl: bd.env_hook_basic_decl,
                lambda _: False,
            ),
            lambda su=T.Subunit: su.env_hook_subunit,
            lambda _: False,
        )

    @langkit_property()
    def env_get(
        env=T.LexicalEnv,
        symbol=T.Symbol,
        lookup=(T.LookupKind, LK.recursive),
        from_node=(T.AdaNode, No(T.AdaNode)),
        categories=(T.RefCategories, all_categories)
    ):
        """
        Wrapper for ``env.get``. Refines the results so that Ada visibility
        rules for subprogram specifications and generic instantiations are
        correctly handled: names inside the two aforementioned constructs do
        not have visibility on their enclosing declaration, such that the
        following is legal:

        .. code:: ada

            type T is null record;
            procedure T (X : T) is null;

        Here, calling ``env_get("T")`` in the subp spec of subprogram ``T``
        must not return the subprogram ``T`` itself, because according to Ada
        the subprogram is not yet visible.
        """
        real_from_node = Var(Self.env_get_real_from_node(from_node))
        results = Var(env.get(symbol, lookup, real_from_node, categories))

        # Fetch the BasicDecl corresponding to ``real_from_node``, so that
        # we can filter it out from ``results``.
        enclosing_bd = Var(real_from_node.cast(BasicDecl)._or(
            real_from_node.cast(DefiningName).then(
                lambda name: name.parent.parent.cast(BasicDecl)
            )
        ))

        return If(
            # If ``symbol`` corresponds to the name of the enclosing
            # subprogram or instantiation, then filter it out, as we
            # should not have visibility on it yet.
            enclosing_bd.as_bare_entity._.defining_name._.name_is(symbol),
            results.filter(lambda r: r.node != enclosing_bd),
            results
        )

    @langkit_property()
    def env_get_public(
        env=T.LexicalEnv,
        symbol=T.Symbol,
        lookup=(T.LookupKind, LK.recursive),
        from_node=(T.AdaNode, No(T.AdaNode)),
        categories=(T.RefCategories, all_categories)
    ):
        """
        Like ``env_get`` but should be used when the results are to be returned
        to users: this wrapper takes care of removing internal structures
        which are of no use for users.
        """
        return Self.env_get(env, symbol, lookup, from_node, categories).filter(
            lambda x: x.cast(PackageDecl).then(
                lambda pkg: pkg.name_symbol != "root_types_",
                default_val=True
            )
        )

    @langkit_property(return_type=T.DefiningName, memoized=True,
                      ignore_warn_on_node=True)
    def synthesize_defining_name(sym=T.Symbol):
        """
        Synthesizes a defining name and its inner identifier using the given
        symbol.
        """
        return SyntheticDefiningName.new(
            name=SyntheticIdentifier.new(
                sym=sym,
                logic_vars=No(T.Address)
            ),
            logic_vars=No(T.Address)
        )

    @langkit_property(return_type=T.env_assoc, memoized=True)
    def create_unop_assoc(op=T.Symbol, rhs=T.BaseTypeDecl, ret=T.BaseTypeDecl):
        """
        Synthesizes a subprogram declaration named after the given symbol,
        with a "Right" parameter having the ``rhs`` type, and the given
        return type.
        """
        return new_env_assoc(
            key=op,
            value=SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
                subp_symbol=op,
                right_param=SyntheticFormalParamDecl.new(
                    param_name='right',
                    param_type=SyntheticTypeExpr.new(target_type=rhs)
                ),
                return_type_expr=SyntheticTypeExpr.new(target_type=ret)
            ))
        )

    @langkit_property(return_type=T.env_assoc, memoized=True)
    def create_binop_assoc_impl(op=T.Symbol,
                                lhs=T.TypeExpr, rhs=T.TypeExpr,
                                ret=T.TypeExpr):
        """
        Implementation for the various ``create_binop_assoc*`` variants. The
        shorthands take care of synthesizing type expressions when necessary.
        """
        return new_env_assoc(
            key=op,
            value=SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
                subp_symbol=op,
                left_param=SyntheticFormalParamDecl.new(
                    param_name='left',
                    param_type=lhs
                ),
                right_param=SyntheticFormalParamDecl.new(
                    param_name='right',
                    param_type=rhs
                ),
                return_type_expr=ret
            ))
        )

    @langkit_property(return_type=T.env_assoc, memoized=True)
    def create_binop_assoc(op=T.Symbol,
                           lhs=T.BaseTypeDecl, rhs=T.BaseTypeDecl,
                           ret=T.BaseTypeDecl):
        """
        Synthesizes a subprogram declaration named after the given symbol,
        with a "Left" parameter having the ``lhs`` type, a "Right" parameter
        having the ``rhs`` type, and the given return type.
        """
        return Self.create_binop_assoc_impl(
            op,
            SyntheticTypeExpr.new(target_type=lhs),
            SyntheticTypeExpr.new(target_type=rhs),
            SyntheticTypeExpr.new(target_type=ret)
        )

    @langkit_property(return_type=T.env_assoc, memoized=True)
    def create_binop_assoc_l_expr(op=T.Symbol,
                                  lhs=T.TypeExpr, rhs=T.BaseTypeDecl,
                                  ret=T.BaseTypeDecl):
        """
        Like ``create_binop_assoc`` but the left parameter's type is given as a
        type expression.
        """
        return Self.create_binop_assoc_impl(
            op,
            lhs,
            SyntheticTypeExpr.new(target_type=rhs),
            SyntheticTypeExpr.new(target_type=ret)
        )

    @langkit_property(return_type=T.env_assoc, memoized=True)
    def create_binop_assoc_r_expr(op=T.Symbol,
                                  lhs=T.BaseTypeDecl, rhs=T.TypeExpr,
                                  ret=T.BaseTypeDecl):
        """
        Like ``create_binop_assoc`` but the right parameter's type is given as
        a type expression.
        """
        return Self.create_binop_assoc_impl(
            op,
            SyntheticTypeExpr.new(target_type=lhs),
            rhs,
            SyntheticTypeExpr.new(target_type=ret)
        )

    @langkit_property(return_type=T.env_assoc, memoized=True)
    def create_binop_assoc_l_r_expr(op=T.Symbol,
                                    lhs=T.TypeExpr, rhs=T.TypeExpr,
                                    ret=T.BaseTypeDecl):
        """
        Like ``create_binop_assoc`` but the left and right parameters' types
        are given as type expressions.
        """
        return Self.create_binop_assoc_impl(
            op, lhs, rhs, SyntheticTypeExpr.new(target_type=ret)
        )


class DocAnnotation(Struct):
    """
    Documentation annotation.
    """
    key = UserField(T.String, doc="Annotation key")
    value = UserField(T.String, doc="Annotation value")


class Aspect(Struct):
    """
    Composite field representing the aspect of an entity (:rmlink:`13`).
    """
    exists = UserField(Bool, doc="Whether the aspect is defined or not")
    node = UserField(T.AdaNode.entity,
                     doc="Syntactic node that defines the aspect")
    value = UserField(T.Expr.entity,
                      doc="Expr node defining the value of the aspect")


@abstract
class BasicDecl(AdaNode):
    """
    Root class for an Ada declaration (:rmlink:`3.1`). A declaration
    associates a name with a language entity, for example a type or a variable.
    """

    @langkit_property()
    def env_hook_basic_decl():
        """
        Helper for AdaNode.env_hook. Handle library-level unit decl nodes.
        """
        return If(
            # For library-level subprogram/package declarations, process the
            # parent spec.
            Self.is_a(T.PackageDecl, T.BasicSubpDecl, T.PackageRenamingDecl,
                      T.GenericPackageDecl, T.GenericPackageInstantiation,
                      T.GenericSubpInstantiation, T.GenericSubpDecl,
                      T.SubpBody),

            Self.as_bare_entity.defining_name.name.cast(T.DottedName).then(
                lambda dn:
                Self.get_unit(dn.prefix.as_symbol_array,
                              UnitSpecification,
                              load_if_needed=True,
                              not_found_is_error=Not(Self.is_a(T.SubpBody)))
                .then(lambda _: False)
            ),
            False
        )

    @langkit_property()
    def populate_body_unit():
        """
        For library-level subprogram declarations, we always want to populate
        the unit containing the body, so that the lexical envs always contain
        the spec and the body, no matter which was initially requested.
        """
        return If(
            Self.is_library_item,
            Self.get_unit(
                Self.as_bare_entity.defining_name.name.as_symbol_array,
                UnitBody,
                load_if_needed=True,
                not_found_is_error=False
            ).then(lambda _: False),
            False
        )

    @langkit_property(return_type=T.Bool)
    def has_top_level_env_name_impl(allow_bodies=Bool):
        """
        Helper for ``has_top_level_env_name``. See its docstring for more
        information.
        """
        is_decl = Var(Self.is_a(
            BasePackageDecl, BasicSubpDecl, GenericDecl,
            TaskTypeDecl, ProtectedTypeDecl,
            SingleTaskDecl, SingleProtectedDecl
        ))

        is_body = Var(Self.is_a(Body))

        return Self.is_compilation_unit_root | And(
            is_decl | (allow_bodies & is_body),
            Self.node_env.env_node.then(
                lambda node: node.cast(BasicDecl).then(
                    lambda p: If(
                        Self == p,
                        True,
                        p.has_top_level_env_name_impl(
                            allow_bodies=And(
                                allow_bodies,
                                Not(Self.is_a(BaseSubpBody)),
                                Not(p.is_a(BaseSubpBody))
                            )
                        )
                    ),
                    default_val=And(
                        node.is_a(PrivatePart),
                        node.parent.cast(BasicDecl).then(
                            lambda bd:
                            bd.has_top_level_env_name_impl(allow_bodies)
                        )
                    )
                ),
                default_val=True
            )
        )

    @langkit_property(return_type=T.Bool)
    def has_top_level_env_name():
        """
        Return True if this declaration is exposed to other compilation units.
        This is equivalent to asking if this declaration's env should be named.

        Find a few examples below.

        .. code::

            package A is                     -- True
                package B is                 -- True
                    procedure Foo;           -- True
                end B;
            end A;

            package body A is                -- True
                package B is                 -- True
                    procedure Foo;           -- True
                end B;
            end A;

            package body A is                -- True
                package body B is            -- True
                    procedure Foo;           -- False
                end B;
            end A;

            package body A is                -- True
                package body B is            -- True
                    procedure Foo is null;   -- True
                end B;
            end A;

            package body A is                -- True
                procedure B is               -- True
                    procedure Foo is null;   -- False
                begin
                    ...
                end B;
            end A;

            procedure A is                   -- True
                procedure Foo;               -- True
            begin
                ...
            end A;

            procedure A is                   -- True
                package body B is            -- False
                    procedure Foo is null;   -- False
                end B;
            begin
                ...
            end A;

            procedure A is                   -- True
                package B is                 -- True
                end B;

                package body B is            -- True
                end B;
            begin
            end A;
        """
        # Gotcha: at this point, Self.children_env actual refers to its parent
        # env. That's because Self does not have yet have a children env (this
        # property is typically called in env specs before add_env() in order
        # to understand where we should create this children_env).
        return Self.children_env.env_node.then(
            lambda node: node.cast(BasicDecl).then(
                lambda bd: bd.has_top_level_env_name_impl(
                    allow_bodies=True
                ),
                default_val=And(
                    node.is_a(PrivatePart),
                    node.parent.cast(BasicDecl).then(
                        lambda bd: bd.has_top_level_env_name_impl(
                            allow_bodies=True
                        )
                    )
                )
            ),
            default_val=True
        )

    @langkit_property(return_type=T.String)
    def env_spec_fully_qualified_name_impl(self_env=T.LexicalEnv):
        """
        Helper to implement ``env_spec_fully_qualified_name``.
        """
        return Cond(
            # For a compilation unit root, simply use the existing syntactic
            # fully qualified name property, which does not rely on envs.
            Self.is_compilation_unit_root,
            Self.sym_join(
                Self.enclosing_compilation_unit.syntactic_fully_qualified_name,
                String(".")
            ),

            # For internal nodes, ignore them and recurse on their parent,
            # which are the real declarations.
            Self.is_a(GenericPackageInternal, GenericSubpInternal),
            Self.parent.cast(BasicDecl).env_spec_fully_qualified_name_impl(
                self_env=Self.parent.node_env
            ),

            # Find the enclosing BasicDecl
            self_env.env_node.cast(BasicDecl)._or(
                self_env.env_node.cast(PrivatePart)._.parent.cast(BasicDecl)
            ).then(
                # Recurse and append the basic decl's name
                lambda bd:
                bd.env_spec_fully_qualified_name_impl(self_env=bd.node_env)
                    .concat(String("."))
                    .concat(Self.name_symbol.image),
            )
        )

    @langkit_property(return_type=T.String)
    def env_spec_fully_qualified_name():
        """
        Return a the fully qualified name of this declaration to be used by
        env specs. This should not be used elsewhere, as it does some
        assumption about envs that are not True anymore after envs are
        populated.
        """
        # Gotcha: at this point, Self.children_env actual refers to its parent
        # env. See similar notice in BasicDecl.has_top_level_env_name.
        return Self.env_spec_fully_qualified_name_impl(Self.children_env)

    @langkit_property(return_type=T.String)
    def top_level_env_name():
        """
        Return the name that this BasicDecl should use to create its lexical
        environment. An empty name is returned if it shouldn't use a named
        env.
        """
        return If(
            Self.has_top_level_env_name,
            Self.env_spec_fully_qualified_name,
            No(T.String)
        )

    @langkit_property(return_type=T.Symbol)
    def child_decl_initial_env_name(private_part=(T.Bool, False)):
        """
        If this is a child declaration, return the lexical environment name of
        its parent declaration. Otherwise return an empty string.

        If ``private_part`` is True, return the env name of the private part
        of its parent.
        """
        defining_name = Var(Self.as_bare_entity.defining_name)
        child_name = Var(defining_name.name.cast(DottedName))
        return Cond(
            # The standard package is the only library item that does not have
            # a named parent.
            defining_name.text.to_symbol == 'standard',
            No(T.Symbol),

            Self.is_library_item,
            child_name.then(
                # If this declaration's name is a dotted name, use the prefix
                # to retrieve the name of its parent.
                lambda n: If(
                    private_part,
                    n.prefix.text.concat(String(".__privatepart")).to_symbol,
                    n.prefix.text.to_symbol
                ),
                # If it's not a dotted name, its parent is the standard package
                default_val='standard'
            ),

            # This declaration
            No(T.Symbol)
        )

    @langkit_property(return_type=T.DesignatedEnv)
    def child_decl_initial_env(private_part=(T.Bool, False)):
        """
        Return the initial env for this basic declaration. This is used
        to set the parent environment of a child declaration to its actual
        parent in terms of Ada semantics.

        If ``private_part`` is True, return the env of the private part of its
        parent.
        """
        return Self.child_decl_initial_env_name(private_part).then(
            lambda name: named_env(name),
            default_val=direct_env(Self.default_initial_env)
        )

    @langkit_property(return_type=T.env_assoc.array)
    def basic_decl_env_assocs(dest_env=T.DesignatedEnv):
        """
        Return an array of env assocs that should be added in the environment
        designated by ``dest_env``. In the general case, it simply adds an
        entry for Self using this declaration's name as key. However, if Self
        corresponds to the declaration of a ``"="`` operator, we also generate
        an order to add an entry for the ``"/="`` operator, as described in
        :rmlink:`4.5.2` 25.a.
        """
        name = Var(Entity.name_symbol)

        base_assoc = Var(
            new_env_assoc(
                key=Entity.name_symbol,
                value=Self,
                dest_env=dest_env
            ).singleton
        )

        implicit_neq_assoc = Var(If(
            name == '"="',
            new_env_assoc(
                key='"/="',
                value=Self,
                dest_env=dest_env
            ).singleton,
            No(T.env_assoc.array)
        ))

        return base_assoc.concat(implicit_neq_assoc)

    @langkit_property(return_type=T.env_assoc.array)
    def child_decl_env_assocs():
        """
        Return the env association that describes where to register this
        basic declaration. For a child declaration in particular, this orders
        adding itself inside its parent declaration's environment.

        .. note::
            This intercepts user-defined "=" operators so as to introduce an
            implicit "/=" operator, as per :rmlink:`4.5.2` 25.a.
        """
        dest_env = Var(named_env(
            Self.child_decl_initial_env_name(False),
            or_current=True
        ))
        return Entity.basic_decl_env_assocs(dest_env)

    is_formal = Property(
        Self.parent.is_a(T.GenericFormal),
        public=True,
        doc="""
        Whether this decl is the nested decl of a generic formal declaration.
        """
    )

    @langkit_property(public=True, external=True,
                      return_type=DocAnnotation.array,
                      uses_entity_info=False, uses_envs=False)
    def doc_annotations():
        """
        Return the documentation annotations associated with this decl.
        Annotations are any comment line of the form::

            --% [annotation_name]: [annotation]

        Raises a property error if the doc is incorrectly formatted.

        .. ATTENTION:: This is an experimental feature, so even if it is
           exposed to allow experiments, it is totally unsupported and the API
           and behavior are very likely to change in the future.
        """
        pass

    @langkit_property(public=True, external=True, return_type=T.String,
                      uses_entity_info=False, uses_envs=False)
    def doc():
        """
        Return the documentation associated with this decl. Raises a property
        error if the doc is incorrectly formatted.

        .. ATTENTION:: This is an experimental feature, so even if it is
           exposed to allow experiments, it is totally unsupported and the API
           and behavior are very likely to change in the future.
        """
        pass

    @langkit_property(public=True, dynamic_vars=[default_imprecise_fallback()])
    def previous_part_for_decl():
        """
        Return the previous part for this decl, if applicable.

        .. note:: It is not named previous_part, because BaseTypeDecl has a
            more precise version of previous_part that returns a BaseTypeDecl.
            Probably, we want to rename the specific versions, and have the
            root property be named previous_part. (TODO R925-008)
        """
        return Entity.match(
            lambda btd=T.BaseTypeDecl:
            btd.previous_part(True).cast(T.BasicDecl),
            lambda bd=T.Body: bd.previous_part_internal,
            lambda _: No(T.BasicDecl.entity)
        )

    @langkit_property(public=True, return_type=T.BasicDecl.entity,
                      memoized=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def canonical_part():
        """
        Return the canonical part for this decl. In the case of decls composed
        of several parts, the canonical part will be the first part.
        """
        return Entity.previous_part_for_decl.then(
            lambda pp: pp.canonical_part, default_val=Entity
        )

    @langkit_property(return_type=T.BasicDecl.entity.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def all_previous_parts():
        """
        Return all previous parts of this entity, where the first part
        is at the beginning of the array.
        """
        return Entity.previous_part_for_decl.then(
            lambda pp: If(
                Entity == pp,
                No(BasicDecl.entity.array),
                pp.all_previous_parts.concat(pp.singleton)
            )
        )

    @langkit_property(return_type=T.BasicDecl.entity.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def all_next_parts():
        """
        Return all next parts of this entity, where the last part is at the
        end of the array.
        """
        return Entity.next_part_for_decl.then(
            lambda np: If(
                Entity == np,
                No(BasicDecl.entity.array),
                np.singleton.concat(np.all_next_parts)
            )
        )

    @langkit_property(return_type=T.BasicDecl.entity.array, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def all_parts():
        """
        Return all parts that define this entity, sorted from first part to
        last part.
        """
        prevs = Var(Entity.all_previous_parts)
        nexts = Var(Entity.all_next_parts)
        return prevs.concat(Entity.singleton).concat(nexts)

    @langkit_property(public=True, dynamic_vars=[default_imprecise_fallback()])
    def is_static_decl():
        """
        Return whether this declaration is static.
        """
        return False

    @langkit_property(return_type=T.BasicDecl.entity)
    def unshed_rebindings(rebindings=T.EnvRebindings):
        """
        Put ``rebindings`` back on ``Entity`` if ``Entity`` is rebound
        somewhere in the chain of rebindings. Ensure coherency, e.g. that if
        Entity already has some rebindings, the one that we add are a superset
        of the one it already has.
        """
        return Cond(
            rebindings == No(T.EnvRebindings),
            Entity,

            rebindings.old_env.env_node == Self,

            If(
                Or(
                    Entity.info.rebindings == No(T.EnvRebindings),
                    rebindings.get_parent == Entity.info.rebindings,
                ),

                BasicDecl.entity.new(
                    node=Self,
                    info=T.entity_info.new(
                        rebindings=rebindings,
                        md=Entity.info.md,
                        from_rebound=Entity.info.from_rebound
                    )
                ),
                PropertyError(BasicDecl.entity, "Incorrect rebindings")
            ),

            Entity.unshed_rebindings(rebindings.get_parent)
        )

    @langkit_property(return_type=T.Bool)
    def is_library_item():
        """
        Return whether this is a top-level element.
        """
        return Self.parent.is_a(LibraryItem)

    decl_private_part = Property(Entity.match(
        lambda bpd=T.BasePackageDecl: bpd.private_part,
        lambda ttd=T.TaskTypeDecl: ttd.definition.private_part,
        lambda td=T.SingleTaskDecl:     td.task_type.definition.private_part,
        lambda ptd=T.ProtectedTypeDecl: ptd.definition.private_part,
        lambda spd=T.SingleProtectedDecl: spd.definition.private_part,
        lambda _: No(T.PrivatePart.entity),
    ))

    @langkit_property(return_type=T.DeclarativePart.entity.array)
    def declarative_parts():
        """
        Return the declarative parts directly associated to this BasicDecl, if
        any.
        """
        return No(T.DeclarativePart.entity.array)

    @langkit_property(memoized=True)
    def immediate_declarative_region():
        return Entity.all_previous_parts.concat(Entity.singleton).mapcat(
            lambda part: part.declarative_parts.map(
                lambda p: p.children_env
            )
        ).env_group()

    aspects = AbstractField(type=T.AspectSpec, doc="""
        Return the list of aspects that are attached to this node.
    """)

    @langkit_property(return_type=T.AspectAssoc.entity, public=True)
    def get_aspect_assoc(name=Symbol):
        """
        Return the aspect with name ``name`` for this entity.
        """
        return Entity.aspects._.aspect_assocs.find(
            lambda asp: asp.aspect_name(asp.id) == name.image
        )

    @langkit_property(return_type=T.Expr.entity, public=True)
    def get_aspect_spec_expr(name=Symbol):
        """
        Return the expression associated to the aspect with name ``name`` for
        this entity.
        """
        return Entity.get_aspect_assoc(name)._.expr

    @langkit_property()
    def library_item_pragmas():
        """
        If this entity is a library item, return the compilation unit pragmas.
        """
        return Cond(
            Entity.parent.is_a(T.LibraryItem),
            Entity.parent.parent.cast(T.CompilationUnit).pragmas,

            Entity.parent.parent.is_a(T.LibraryItem),
            Entity.parent.parent.parent.cast(T.CompilationUnit).pragmas,

            No(T.Pragma.list.entity)
        )

    @langkit_property(return_type=Aspect, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_aspect(name=Symbol):
        """
        Return the aspect with name ``name`` associated to this entity.

        Aspects are properties of entities that can be specified by the Ada
        program, either via aspect specifications, pragmas, or attributes.

        This will return the syntactic node corresponding to attribute
        directly.

        Note: for some aspects (e.g. Inline), Libadalang will check if they are
        defined on any part of the entity.
        """
        return Entity.defining_name_or_raise._.get_aspect(name)

    @langkit_property(return_type=Bool, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def has_aspect(name=Symbol):
        """
        Returns whether the boolean aspect named ``name`` is set on the entity
        represented by this node.

        "Aspect" is used as in RM terminology (see :rmlink:`13`).
        """
        return Entity.defining_name_or_raise._.has_aspect(name)

    @langkit_property(return_type=T.Pragma.entity, public=True)
    def get_pragma(name=Symbol):
        """
        Return the pragma with name ``name`` associated to this entity.

        Please use the ``p_get_aspects`` property instead if you are interested
        in aspects, i.e. information that can be represented by either aspect
        specification nodes, pragma nodes or attribute definition nodes.
        """
        return Entity.defining_name_or_raise._.get_pragma(name)

    @langkit_property(return_type=T.AttributeDefClause.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_representation_clause(name=Symbol):
        """
        Return the representation clause associated to this type decl that
        defines the given attribute name.
        """
        return Entity.defining_name_or_raise._.get_representation_clause(name)

    @langkit_property(return_type=T.AtClause.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_at_clause():
        """
        Return the at clause associated to this declaration.
        """
        return Entity.defining_name_or_raise._.get_at_clause()

    @langkit_property(public=True)
    def is_imported():
        """
        Whether this declaration is imported from another language.
        """
        return Entity.defining_name_or_raise._.is_imported

    @langkit_property(public=True, return_type=T.Bool)
    def is_ghost_code():
        """
        Return whether this declaration is ghost code or not. See SPARK RM 6.9.
        """
        return Entity.defining_name_or_raise._.is_ghost_code

    @langkit_property(return_type=T.GenericInstantiation.entity)
    def get_instantiation():
        """
        Assuming Self is a Generic*Internal node (BasicDecl is their greatest
        common parent), return the GenericInstantiation node from which this
        Generic*Internal node is derived.

        .. ATTENTION:: If this Generic*Internal is not part of an
            instantiation, but has been fetched through the formal generic
            subprogram, this will return None. None is also returned if the
            rebindings do not correspond to the instantiation of this generic
            declaration.
        """
        inst_node = Var(Entity.info.rebindings.then(
            lambda r: r.new_env.env_node.cast_or_raise(GenericInstantiation)
        ))
        designated_decl = Var(
            inst_node.as_bare_entity._.designated_generic_decl
        )
        return If(
            designated_decl.node == Self.parent,
            T.GenericInstantiation.entity.new(
                node=inst_node,
                info=T.entity_info.new(
                    # Since we return the instantiation itself, remove
                    # it from its rebindings.
                    rebindings=Self.remove_rebindings(
                        Entity.info.rebindings,
                        designated_decl.info.rebindings
                    ),
                    from_rebound=Entity.info.from_rebound,
                    md=T.Metadata.new()
                )
            ),
            No(GenericInstantiation.entity)
        )

    @langkit_property(public=True)
    def is_compilation_unit_root():
        """
        Whether a BasicDecl is the root decl for its unit.
        """
        return Self.parent.then(lambda p: p.match(
            lambda _=T.LibraryItem: True,
            lambda gen_pkg_decl=T.GenericPackageDecl:
                gen_pkg_decl.parent.then(lambda p: p.is_a(LibraryItem)),
            lambda _=T.Subunit: True,
            lambda _: False,
        ))

    @langkit_property()
    def populate_dependent_units():
        return If(
            Self.is_compilation_unit_root,
            Self.top_level_with_package_clauses.map(
                lambda package_name:
                Self.withed_unit_helper(package_name)
            ),
            No(CompilationUnit.array)
        )

    @langkit_property(return_type=Bool)
    def should_ref_generic_formals():
        """
        Helper property used to determine whether we should add a
        referenced_env to the generic formal part of a given entity.
        """
        # We want to reference the generic formal env if:
        return Or(
            # 1. This potential generic body is not a compilation unit root. In
            # that case, the parent is the lexical parent of the body (eg the
            # containing entity), and we need to reference the formals.
            Not(Self.is_compilation_unit_root),

            # 2. This potential generic body is a subunit. In that case,
            # similarly, the parent is the lexical parent of the stub part, and
            # we need to reference the generic formals.
            Self.parent.is_a(Subunit),

            # 3. This is the declaration of a subprogram body. In that case,
            # we should add a reference to the generic formals even for
            # library-level subprograms, because their parent is not their
            # declaration.
            Self.is_a(BaseSubpBody)
        )

    is_in_public_part = Property(Self.parent.parent.is_a(T.PublicPart))
    is_in_private_part = Property(Self.parent.parent.is_a(T.PrivatePart))

    @langkit_property(return_type=Bool, public=True)
    def is_visible(from_node=T.AdaNode.entity):
        """
        Return whether this declaration is visible from the point of view of
        the given ``origin`` node.

        .. ATTENTION::
            Only package-level (public or private) declarations are supported
            for now.
        """
        return Cond(
            # If Self is declared in a private part, check that we can find it
            # from origin's env.
            Entity.is_in_private_part,
            from_node.node_env.get(Entity.name_symbol).contains(Entity)
            # Even if the above expression is True, we may not have visibility
            # according to Ada rules: in LAL, library-level child packages'
            # parent environments are defined to be their parent packages'
            # private part. The expression below filters out cases where Entity
            # is declared in the private part of a library-level package and
            # from_node lies in the public part of a child package.
            & Not(
                Entity.parent.parent.parent.cast(PackageDecl)
                .is_compilation_unit_root
                & from_node.is_in_top_level_public_part
            ),

            # If Self is declared in a public part, origin has visibility on it
            # iff it has visibility on the parent of Self: do a recursive call
            # on the parent scope.
            Entity.is_in_public_part,
            Entity.parent_basic_decl.is_visible(from_node),

            # If Self is declared at the top-level (but is not a subunit), we
            # necessarily have visibility on it.
            And(
                Entity.is_compilation_unit_root,
                Not(Entity.cast(Body)._.is_subunit)
            ),
            True,

            # Unhandled case: raise PropertyError
            PropertyError(Bool, "Only package-level declaration support"
                                " visibility checks for now.")
        )

    @langkit_property(return_type=Bool)
    def subp_decl_match_signature(other=T.BasicDecl.entity):
        return (
            Entity.subp_spec_or_null.match_signature(
                other.subp_spec_or_null.cast_or_raise(T.SubpSpec),
                False
            )
        )

    @langkit_property(return_type=T.BasicDecl.entity.array,
                      dynamic_vars=[imprecise_fallback])
    def base_subp_declarations_impl():
        """
        Actual implementation of ``base_subp_declarations`` that already
        assumes Entity is a subprogram.
        """
        parent = Var(Entity.canonical_part.parent_basic_decl)
        task_or_protected = Var(parent.is_a(ProtectedTypeDecl, TaskTypeDecl))

        # We use `without_md` below because we don't want to take into
        # account Entity's metadata, as the result of this property
        # shouldn't depend upon how this node was retrieved.
        spec = Var(Entity.without_md.cast(BasicDecl).subp_spec_or_null)

        # Retrieve an environment that contains all the candidate subprograms
        # that can be base declarations of this one.
        prims_env = Var(If(
            # If we are in a task or protected type, accumulate the primitives
            # of the parent interfaces, and add the subprogram defined in the
            # scope of the protected or task type.
            task_or_protected,
            parent.cast(BaseTypeDecl).base_types.map(
                lambda bt: bt.full_view.primitives_env
            ).concat(parent.children_env.singleton).env_group(),

            # For classical types, we can simply fetch the primitives_env
            spec.candidate_primitive_subp_tagged_type(canonicalize=False).then(
                lambda t: t.full_view.primitives_env
            )
        ))

        # We don't want the canonicalized primitive type, but the most
        # visible: the most visible might be a private type that has a
        # more specific derivation than the canonical (public) type:
        #
        # type A is tagged private;
        # type B is new A with private;
        # type P is new A with private;
        # private
        # type P is new B with record ...

        # We can call the `candidate_` version because the over-
        # approximation will get cancelled by the following logic.
        return prims_env.get(Entity.name_symbol, lookup=LK.minimal).filtermap(
            lambda bd: bd.cast(BasicDecl).canonical_part,
            lambda bd: bd.cast(BasicDecl)._.subp_spec_or_null.then(
                lambda s:
                # Since `s` is retrieved from `t`'s primitives env,
                # its metadata fields `primitive` and
                # `primitive_real_type` are set and therefore
                # the following `match_signature` call will return
                # true if `s` is overridable by `spec`.
                s.match_signature(
                    spec,
                    match_name=False,
                    use_entity_info=True,
                    ignore_first_param=task_or_protected
                )
            )
        ).unique

    @langkit_property(return_type=T.BasicDecl.entity.array, memoized=True,
                      public=True, dynamic_vars=[default_imprecise_fallback()])
    def base_subp_declarations():
        """
        If Self declares a primitive subprogram of some tagged type T, return
        the set of all subprogram declarations that it overrides (including
        itself).

        .. note:: for the moment this only works for tagged types. Remains to
            be seen if we need to extend it.
        """
        return If(
            Entity.is_subprogram,
            Entity.base_subp_declarations_impl,
            No(BasicDecl.entity.array)
        )

    @langkit_property(return_type=T.BasicDecl.entity.array,
                      dynamic_vars=[origin, imprecise_fallback])
    def root_subp_declarations_impl():
        """
        Actual implementation of ``root_subp_declarations`` that already
        assumes Entity is a subprogram.
        """
        # Get all the parent overrides defined for this subprogram. That is,
        # if this subprogram is a primitive of some type T and overrides some
        # subprogram P, get all the other overrides of P which are primitives
        # of parent types of T.
        raw_base_decls = Var(Entity.base_subp_declarations)

        # If this subprogram is defined in a protected type or task type,
        # we must do things a bit different since the `primitive` metadata
        # field is not set on those subprograms. Fortunately, the reasoning is
        # quite trivial for those cases: since one cannot override a subprogram
        # defined in a protected type or task type, the root subprogram cannot
        # be Self unless there is no other base subprogram. So, simply filter
        # Self out of the base subp declarations in that case.
        parent = Var(Entity.canonical_part.parent_basic_decl)
        task_or_protected = Var(parent.is_a(ProtectedTypeDecl, TaskTypeDecl))
        base_decls = Var(If(
            task_or_protected & (raw_base_decls.length > 1),
            raw_base_decls.filter(lambda bd: bd.node != Self),
            raw_base_decls
        ))

        # Compute the set of all such types for which an override is declared
        base_types = Var(base_decls.map(
            lambda d: d.info.md.primitive.cast(BaseTypeDecl).as_bare_entity
        ).unique)

        # Among this set of type, find the ones which are not derived from any
        # of the others, i.e. the base-most types on which the original
        # subprogram is declared.
        return base_types.filter(
            lambda t: Not(base_types.any(
                lambda u: And(
                    t != u,
                    t.is_derived_type(u)
                )
            ))
        ).map(
            # Get back the subprograms declared on the base types
            lambda root_type: base_decls.find(
                lambda d: d.info.md.primitive == root_type.node
            )
        )

    @langkit_property(return_type=T.BasicDecl.entity.array, public=True,
                      dynamic_vars=[default_origin(),
                                    default_imprecise_fallback()])
    def root_subp_declarations():
        """
        If Self declares a primitive subprogram of some tagged type T, return
        the root subprogram declarations that it overrides. There can be
        several, as in the following scenario:

        - package Root defines the root tagged type T and subprogram Foo.
        - package Itf defines interface I and abstract subprogram Foo.
        - package D defines "type U is new Root.T and Itf.I" and an overriding
          subprogram Foo.

        Here, root_subp_declarations of Foo defined in package D will return
        both Foo from package Root and Foo from package Itf.
        """
        return If(
            Entity.is_subprogram,
            Entity.root_subp_declarations_impl,
            No(BasicDecl.entity.array)
        )

    @langkit_property(public=True, return_type=T.BasicDecl.entity.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def find_all_overrides(units=T.AnalysisUnit.array):
        """
        If Self is the declaration of a primitive of some type T, return
        the list of all subprogram that override this subprogram among the
        given units.
        """
        spec = Var(Entity.subp_spec_or_null)

        # We can call the `candidate_` version because the over-approximation
        # will get cancelled by the following logic.
        prim_type = Var(spec._.candidate_primitive_subp_tagged_type)

        derivations = Var(prim_type._.find_all_derived_types(units))

        return derivations.mapcat(
            lambda t: Let(
                # Get all primitives that are named just like Self
                lambda prims=t.primitives_env.get(Entity.name_symbol).map(
                    lambda p: p.cast_or_raise(BasicDecl)
                ): Let(
                    # Retrieve Self among the primitives, so that it carries
                    # the adequate real_primitive_type metadata field.
                    lambda base_p=prims.find(lambda p: p.node == Self):

                    prims.filter(
                        # Among all the primitives ``p`` available on type
                        # ``t``, keep ``p`` if it both:
                        lambda p: And(
                            # is a primitive "owned" by ``t`` (i.e. not an
                            # inherited one).
                            p.info.md.primitive == t.node,

                            # overrides Self
                            base_p.subp_spec_or_null.match_signature(
                                p.subp_spec_or_null,
                                match_name=False, use_entity_info=True
                            )
                        )
                    ).map(
                        lambda p:
                        p.canonical_part.without_md.cast_or_raise(BasicDecl)
                    )
                )
            )
        ).unique

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
        dynamic_vars=[origin, default_include_ud_indexing()],
        doc="""
        Return a lexical environment that contains entities that are accessible
        as suffixes when Self is a prefix.
        """
    )

    @langkit_property(return_type=T.DefiningName.entity)
    def defining_name_or_raise():
        """
        Return the defining name of this ``BasicDecl``, if and only if there
        is a unique defining name for it. Otherwise, raise a property error.
        """
        dns = Var(Entity.defining_names)
        return If(
            dns.length > 1,
            PropertyError(DefiningName.entity,
                          "BasicDecl with multiple defining names"),
            dns.at(0)
        )

    @langkit_property(dynamic_vars=[origin], return_type=T.BaseTypeDecl.entity)
    def identity_type():
        return Entity.match(
            lambda _=T.ExceptionDecl: Self.exc_id_type,
            lambda _=T.SingleTaskDecl: Self.task_id_type,

            # An object decl on which you can call 'Identity implies that its
            # type is a task type.
            lambda _=T.ObjectDecl: Self.task_id_type,
            lambda _: No(T.BaseTypeDecl.entity)
        )

    @langkit_property(dynamic_vars=[origin], return_type=Int)
    def array_ndims():
        return Entity.expr_type.array_ndims

    is_array = Property(Entity.array_ndims > 0, dynamic_vars=[origin])

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[origin],
                      memoized=True)
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
        return Entity.type_expression.then(lambda te: te.designated_type)

    type_expression = Property(
        No(T.TypeExpr).as_entity,
        type=T.TypeExpr.entity,
        public=True,
        doc="""
        Return the type expression for this BasicDecl if applicable, a null
        otherwise.
        """
    )

    @langkit_property(return_type=T.BaseSubpSpec.entity, public=True)
    def subp_spec_or_null(follow_generic=(Bool, True)):
        """
        If Self is a Subp, returns the specification of this subprogram.

        If ``follow_generic`` is True, will also work for instances of
        ``GenericSubpDecl``.
        """
        return Entity.match(
            lambda subp=BasicSubpDecl:  subp.subp_decl_spec,
            lambda subp=BaseSubpBody:   subp.subp_spec,
            lambda subp=SubpBodyStub:   subp.subp_spec,
            lambda gsp=GenericSubpDecl:
            If(follow_generic, gsp.subp_decl.subp_spec, No(SubpSpec.entity)),
            lambda gsi=GenericSubpInstantiation:
            If(follow_generic,
               gsi.designated_subp._.subp_spec_or_null,
               No(SubpSpec.entity)),
            lambda gsr=GenericSubpRenamingDecl:
            If(follow_generic,
               gsr.resolve._.subp_spec_or_null,
               No(SubpSpec.entity)),
            lambda _:                   No(SubpSpec.entity),
        )

    @langkit_property(return_type=T.BaseFormalParamHolder.entity)
    def formal_param_holder_or_null():
        return Entity.match(
            lambda t=T.TypeDecl: t.discriminants,
            lambda e=T.EntryBody: e.params,
            lambda _: Entity.subp_spec_or_null
        )

    @langkit_property(return_type=Bool, public=True)
    def is_subprogram():
        """
        Return True if self is a subprogram node in the general sense (which
        is, an entity that can be called). This includes separates and entries.

        .. attention: This is a purely syntactic query and will return True for
            everything that is a syntactic entity that can be called like a
            subprogram in some contexts, even generic formal subprograms for
            example.
        """
        return Self.is_a(BasicSubpDecl, BaseSubpBody, SubpBodyStub, EntryDecl,
                         GenericSubpDecl, GenericSubpInstantiation,
                         GenericSubpRenamingDecl)

    @langkit_property(return_type=T.Bool)
    def is_valid_reducer_candidate():
        """
        Return True if self is a subprogram node that is a valid reducer
        candidate as per RM 4.5.10 definition of the reducer program used by
        the ``'Reduce`` attribute (Ada 2022).
        """
        # A reducer candidate can be a function or a procedure
        return Entity.is_subprogram.then(
            lambda _: Entity.subp_spec_or_null.then(
                lambda spec: Let(
                    lambda param_types=spec.param_types,
                    return_type=spec.return_type:

                    And(
                        # It should have two params
                        param_types.length == 2,

                        If(
                            return_type.is_null,
                            # If it is a procedure, the first param mode should
                            # be `in out` while the second one should be `in`.
                            Let(
                                lambda param_modes=spec.param_modes:
                                param_modes.at(0).is_a(Mode.alt_in_out)
                                & param_modes.at(1).is_a(Mode.alt_in,
                                                         Mode.alt_default)
                            ),
                            # Else, it is a function, and its return type
                            # should be identical to its first param type.
                            return_type == param_types.at(0)
                        )
                    )
                )
            )
        )

    @langkit_property(return_type=Bool)
    def is_stream_subprogram_for_type(typ=T.BaseTypeDecl.entity,
                                      return_obj=Bool):
        root_stream_type = Var(
            Entity
            .get_unit_root_decl(['Ada', 'Streams'], UnitSpecification)
            ._.children_env
            .get_first('Root_Stream_Type', lookup=LK.flat)
            .cast(T.BaseTypeDecl).classwide_type.cast(T.BaseTypeDecl)
        )
        params = Var(Entity.subp_spec_or_null._.unpacked_formal_params)

        return origin.bind(
            Self.origin_node,
            Self.is_subprogram
            & params.at(0).formal_decl.formal_type
            .is_access_to(root_stream_type)
            & If(
                return_obj,
                Entity.subp_spec_or_null.return_type.matching_formal_type(typ),
                params.at(1).formal_decl.formal_type.matching_formal_type(typ),
            )
        )

    @langkit_property(return_type=Bool)
    def is_put_image_subprogram_for_type(typ=T.BaseTypeDecl.entity):
        """
        Return whether this subprogram has the correct profile to be given
        as argument to the ``Put_Image`` aspect.
        """
        root_buffer_type = Var(
            Self.root_buffer_type.classwide_type.cast(T.BaseTypeDecl)
        )
        params = Var(Entity.subp_spec_or_null._.unpacked_formal_params)

        return origin.bind(
            Self.origin_node,
            Self.is_subprogram

            & params.at(0).formal_decl.formal_type
            .matching_formal_type(root_buffer_type)

            & params.at(1).formal_decl.formal_type
            .matching_formal_type(typ)
        )

    @langkit_property(return_type=Bool)
    def can_be_paramless():
        """
        Return true if entity can be a paramless subprogram entity, when used
        in an expression context.
        """
        return Entity.subp_spec_or_null.then(
            lambda ss: ss.paramless(
                Entity.info.md.dottable_subp, can_be=True
            ),
            default_val=True
        )

    @langkit_property(return_type=Bool)
    def is_paramless():
        """
        Return true if entity is a paramless subprogram entity, when used
        in an expression context.
        """
        return Entity.subp_spec_or_null.then(
            lambda ss: ss.paramless(
                Entity.info.md.dottable_subp, can_be=False
            ),
            default_val=True
        )

    @langkit_property(return_type=Equation, dynamic_vars=[origin])
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

    relative_name = Property(
        Entity.defining_name._.relative_name, public=True, doc="""
        Return the relative name for Self. If Self's defining name is
        ``A.B.C``, return ``C`` as a node.
        """
    )

    relative_name_text = Property(
        Entity.relative_name._.symbol, doc="""
        Return the relative name for Self, as text.
        """, public=True
    )

    name_symbol = Property(Self.as_bare_entity.relative_name.symbol)

    @langkit_property(dynamic_vars=[default_imprecise_fallback()])
    def basic_decl_next_part_for_decl():
        """
        Implementation of next_part_for_decl for basic decls, that can be
        reused by subclasses when they override next_part_for_decl.
        """
        # Fetch the library level body unit that might contain the next part
        # for this declaration.
        ignore(Var(
            Self.enclosing_compilation_unit.decl.match(
                lambda _=Body: No(AnalysisUnit),
                lambda b=BasicDecl:
                b.as_bare_entity._.defining_name._.referenced_unit(
                    UnitBody,
                    not_found_is_error=Not(Or(
                        # Body not mandatory if the library level declaration
                        # is a package (regular, generic, or instantiated). We
                        # don't try to be more precise than that.
                        b.is_a(
                            T.BasePackageDecl,
                            T.GenericPackageDecl,
                            T.GenericPackageInstantiation,
                            T.PackageRenamingDecl,
                            T.SubpRenamingDecl,
                            T.GenericRenamingDecl
                        ),

                        # A body is not expected if the library level
                        # declaration is an imported subprogram.
                        And(b.is_a(T.BasicSubpDecl, T.GenericSubpDecl),
                            b.as_entity.has_aspect("Import"))
                    ))
                )
            )
        ))

        return Entity.children_env.get_first(
            '__nextpart',
            lookup=LK.minimal,
            categories=no_prims
        ).cast(T.BasicDecl)

    @langkit_property(public=True, dynamic_vars=[default_imprecise_fallback()])
    def next_part_for_decl():
        """
        Return the next part of this declaration, if applicable.

        .. note:: It is not named next_part, because BaseTypeDecl has a
            more precise version of next_part that returns a BaseTypeDecl.
            Probably, we want to rename the specific versions, and have the
            root property be named next_part. (TODO R925-008)
        """
        return Entity.basic_decl_next_part_for_decl()

    @langkit_property(public=True, dynamic_vars=[default_imprecise_fallback()])
    def body_part_for_decl():
        """
        Return the body corresponding to this declaration, if applicable.

        .. note:: It is not named body_part, subclasses have more precise
            versions named body_part and returning a more precise result.
            Probably, we want to rename the specific versions, and have the
            root property be named body_part. (TODO R925-008)
        """
        return Entity.next_part_for_decl().then(
            lambda np: np.match(
                lambda stub=BodyStub: stub.next_part_for_decl(),
                lambda other: other
            )
        ).cast(T.Body)

    @langkit_property(return_type=T.BasicDecl.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def next_part_for_name(sym=T.Symbol):
        """
        Internal method for computing the next part of a basic decl according
        to one of its defining names. By default, this property behaves just
        like ``next_part_for_decl``. However it can be overriden for node types
        for which the next part depends on the defining name to consider. One
        example of that are constant declarations:

        .. code:: ada

            package Pkg is
                X, Y : constant Integer;
            private
                X : constant Integer := 1;
                Y : constant Integer := 2;
            end Pkg;

        So, ``next_part_for_name`` is overriden in ``ObjectDecl``.
        """
        ignore(sym)
        return Entity.next_part_for_decl

    @langkit_property(return_type=T.BasicDecl.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def previous_part_for_name(sym=T.Symbol):
        """
        Internal method for computing the previous part of a basic decl
        according to one of its defining names. By default, this property
        behaves just like ``next_part_for_decl``. However it can be overriden
        for node types for which the previous part depends on the defining name
        to consider. One example of that are subprogram parameters:

        .. code:: ada

            package Pkg is
                X : constant Integer;
                Y : constant Integer;
            private
                X, Y : constant Integer := 1;
            end Pkg;

        So, ``previous_part_for_name`` is overriden in ``ObjectDecl``.
        """
        ignore(sym)
        return Entity.previous_part_for_decl

    @langkit_property(return_type=T.BasicDecl.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def canonical_part_for_name(sym=T.Symbol):
        """
        Return the canonical part for this decl. In the case of decls composed
        of several parts, the canonical part will be the first part.
        """
        return Entity.previous_part_for_name(sym).then(
            lambda pp: pp.canonical_part_for_name(sym), default_val=Entity
        )

    @langkit_property(return_type=T.BasicDecl.entity, public=True,
                      dynamic_vars=[origin, default_imprecise_fallback()])
    def most_visible_part():
        """
        Given an origin node and the entity represented by Self, this property
        returns the most visible completion of Self that can be seen by origin,
        according to Ada's visibility rules.
        """
        return Entity.defining_name_or_raise.most_visible_part._.basic_decl

    @langkit_property(return_type=T.BasicDecl.entity,
                      dynamic_vars=[origin, imprecise_fallback])
    def most_visible_part_for_name(sym=T.Symbol):
        """
        Internal method for computing the most visible part of a basic decl
        according to one of its defining names.
        """
        np = Var(Entity.next_part_for_name(sym))
        return Cond(
            # This is already the most visible part
            np.is_null,
            Entity,

            # A null origin means any "find the most complete part"
            origin.is_null,
            np.most_visible_part_for_name(sym),

            # If the entity is not a package declaration, we only need to check
            # if its lexical env is one of the parents of origin's env.
            Not(np.is_in_private_part | np.is_in_public_part),
            If(
                origin.node_env.get(sym).contains(Entity),
                np.most_visible_part_for_name(sym),
                Entity
            ),

            # Otherwise this is a package declaration, so we can use the
            # is_visible property.
            np.is_visible(origin.as_bare_entity),
            np.most_visible_part_for_name(sym),

            # Otherwise this was the most visible part
            Entity
        )

    @langkit_property(return_type=T.String.array)
    def fully_qualified_name_impl(
        include_profile=(T.Bool, False),
        dn=(T.DefiningName.entity, No(T.DefiningName.entity))
    ):
        """
        Return the fully qualified name corresponding to this declaration, as
        an array of symbols.

        If ``dn`` is null, take the first defining name for the declaration.
        Else, assume that dn is one of the defining names for this declaration.
        """
        # If this basic decl has several names, and a defining name was not
        # passed, then raise an error. We assume that all internal callers will
        # pass this correctly, so the only cases in which this property error
        # should be raised is when the user calls a public property on a decl
        # with several names.
        ignore(Var(If(
            dn.is_null & (Entity.defining_names.length > 1),
            PropertyError(T.Bool,
                          "Can't call on a declaration with several names"),
            False
        )))

        def_name = Var(If(
            dn.is_null, Entity.defining_name, dn
        ).match(
            lambda scel=T.SyntheticDefiningName: [scel.name_symbol.image],
            lambda n: n.as_single_tok_node_array.map(lambda t: t.text)
        ))

        self_name = Var(def_name.map(
            lambda i, t: t.concat(
                If(include_profile, Entity.custom_id_text, String(""))
            ).concat(If(
                i == (def_name.length - 1),
                Cond(Entity.is_a(T.ClasswideTypeDecl), String("'Class"),
                     Entity.is_a(T.DiscreteBaseSubtypeDecl), String("'Base"),

                     # For the moment, SynthAnonymousTypeDecl is used solely to
                     # generate anonymous access types. We give those a name.
                     # NOTE: this is not an Ada type as per the RM, and is used
                     # for the GNAT specific 'Unrestricted_Access attribute, so
                     # we give this type a name that doesn't exist in the RM
                     # either.
                     Entity.is_a(T.SynthAnonymousTypeDecl),
                     String("'Anonymous_Access"),

                     String("")),
                String("")
            ))
        ))

        fqn = Var(If(
            Entity.is_compilation_unit_root,
            self_name,

            Entity.parent_basic_decl
            ._.fully_qualified_name_impl(include_profile=include_profile)
            .then(lambda fqn: If(
                Self.is_a(T.GenericPackageInternal, T.GenericSubpInternal),
                fqn,
                fqn.concat(self_name)
            ))
        ))

        return Self.parent.cast(
            T.Subunit)._.name.as_single_tok_node_array.map(
                lambda t: t.text).concat(fqn)._or(fqn)

    @langkit_property(return_type=T.String.array)
    def fully_qualified_name_string_array(include_profile=(T.Bool, False)):
        """
        Return the fully qualified name corresponding to this declaration, as
        an array of symbols.
        """
        return Entity.fully_qualified_name_impl(
            include_profile=include_profile
        )

    @langkit_property(public=True, return_type=T.Symbol.array)
    def fully_qualified_name_array(include_profile=(T.Bool, False)):
        """
        Return the fully qualified name corresponding to this declaration, as
        an array of symbols.
        """
        return Entity.fully_qualified_name_string_array(
            include_profile=include_profile
        ).map(lambda t: t.to_symbol)

    @langkit_property(public=True, return_type=T.String)
    def fully_qualified_name():
        """
        Return the fully qualified name corresponding to this declaration.
        """
        return String(".").join(Entity.fully_qualified_name_impl())

    @langkit_property(return_type=T.String)
    def canonical_fully_qualified_name_impl(
        include_profile=(T.Bool, False),
        dn=(T.DefiningName.entity, No(T.DefiningName.entity))
    ):
        """
        Implementation of canonical_fully_qualified_name.
        """
        return String(".").join(
            Entity.fully_qualified_name_impl(
                include_profile=include_profile, dn=dn
            )
            # Map to symbol & back to canonicalize
            .map(lambda t: t.to_symbol).map(lambda t: t.image)
        )

    @langkit_property(public=True, return_type=T.String)
    def canonical_fully_qualified_name():
        """
        Return a canonical representation of the fully qualified name
        corresponding to this declaration.
        """
        return Entity.canonical_fully_qualified_name_impl()

    @langkit_property(return_type=T.String)
    def unique_identifying_name_impl(
        dn=(T.DefiningName.entity, No(T.DefiningName.entity))
    ):
        """
        Implementation for unique_identifying_name.
        """
        return Entity.match(
            lambda _=T.AnonymousTypeDecl: Entity.custom_id_text,
            lambda _: Entity.canonical_fully_qualified_name_impl(
                include_profile=True, dn=dn
            )
        )

    @langkit_property(public=True, return_type=T.String)
    def unique_identifying_name():
        """
        Return a unique identifying name for this declaration, provided this
        declaration is a public declaration. In the case of subprograms, this
        will include the profile.

        .. attention::
            This will only return a unique name for public declarations.
            Notably, anything nested in an unnamed declare block won't be
            handled correctly.
        """
        return Entity.unique_identifying_name_impl()

    @langkit_property(return_type=T.String)
    def custom_id_text():
        return Entity.subp_spec_or_null.then(
            # For subprograms, we'll compute their profiles as the unique
            # identifying text.
            lambda ss: String("(").concat(
                ss.returns.then(lambda _: String("(")).concat(
                    ss.unpacked_formal_params.then(
                        lambda ufp: String(", ").join(
                            ufp.map(
                                lambda p:
                                p.formal_decl.type_expression.custom_id_text
                            )
                        )
                    ).concat(ss.returns.then(lambda _: String(")")))
                )
            ).concat(ss.returns.then(
                lambda r: String(" -> ").concat(r.custom_id_text)
            )).concat(String(")")),
            default_val=String("")
        )

    @langkit_property(return_type=Bool)
    def does_aspects_make_preelaborable(from_body=T.Bool):
        """
        Implementation helper for ``CompilationUnit.is_preelaborable``.

        Return whether ``Entity`` has aspects that make it preelaborable.

        If ``from_body``, consider that ``Entity`` is a spec and that we are
        computing whether its body is preelaborable.
        """
        return imprecise_fallback.bind(False, Or(
            # The following aspects apply to bodies...
            Entity.has_aspect('Pure'),
            Entity.has_aspect('Preelaborate'),
            Entity.has_aspect('Shared_Passive'),
            Not(from_body) & Or(
                # ... but the ones below apply only to specs
                Entity.has_aspect('Remote_Types'),
                Entity.has_aspect('Remote_Call_Interface')
            )
        ))

    @langkit_property(return_type=T.BasicDecl.entity)
    def wrap_public_reference():
        """
        Return a public-friendly view of this entity. For now this only needs
        to handle the case where Self is a ``GenericSubpInternal``, in which
        case we prefer to return its parent ``GenericSubpInstantiation`` node.

        .. attention:: Properties typically use ``wrap_public_reference`` to
            sanitize their return value for users. Sometimes however, those
            properties end up being used by internal properties for practical
            reasons, meaning those properties will work on biased values,
            which could become problematic. Moreover, as of yet this property
            only exists to handle the ``GenericSubpInternal`` case, which could
            actually be addressed cleanly in at least two different ways:

            - By adding interfaces to langkit, so that a
              ``GenericSubpInstantiation`` could be both a
              ``GenericInstantiation`` and a ``BasicSubpDecl``.

            - By also working with ``GenericSubpInstantiation`` nodes
              internally. This mostly means getting rid of
              ``GenericSubpInternal`` nodes in the envs.
        """
        return If(
            Entity.is_a(GenericSubpInternal, GenericPackageInternal),
            Entity.get_instantiation._or(Entity),
            Entity
        )

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      public=True, return_type=Bool)
    def is_constant_object():
        """
        Return whether this object is constant or not.
        """
        pass


class ErrorDecl(BasicDecl):
    """
    Placeholder node for syntax errors in lists of declarations.
    """
    error_node = True
    aspects = NullField()
    defining_names = Property(No(T.DefiningName.entity.array))

    @langkit_property()
    def child_decl_initial_env_name(private_part=(T.Bool, False)):
        """
        Override for error decls, which cannot be child decls.
        """
        # TODO: investigate if we can/should do better here
        ignore(private_part)
        return No(Symbol)


@abstract
class Body(BasicDecl):
    """
    Base class for an Ada body (:rmlink:`3.11`). A body is the completion
    of a declaration.
    """

    @langkit_property()
    def env_hook_body():
        """
        Helper for the AdaNode.env_hook. Handle library-level unit body nodes.
        """
        return If(
            # If this a library-level subprogram/package body, load the spec
            # corresponding to this body.
            Self.is_a(T.PackageBody, T.SubpBody),

            Let(
                lambda _=Self.get_unit(
                    Self.as_bare_entity.defining_name.as_symbol_array,
                    UnitSpecification,
                    load_if_needed=True,
                    not_found_is_error=Not(Self.is_a(T.SubpBody))
                ):

                # A library level subprogram body does not have to have a spec.
                # So we have to compute the parents directly from here.
                Self.cast(T.SubpBody).then(
                    lambda subp_body: subp_body.env_hook_basic_decl
                )
            ),

            False,
        )

    @langkit_property()
    def subunit_decl_env():
        return env.bind(
            Self.default_initial_env,
            Entity.body_scope(True).get(
                Entity.name_symbol,
                categories=no_prims
            ).at(0).match(
                lambda gpd=T.GenericPackageDecl:
                # For generic package decls, we regroup the formal part & the
                # package decl itself, since the reference will be
                # non-transitive.
                Array([gpd.children_env, gpd.package_decl.children_env])
                .env_group(),
                lambda pd=T.BasicDecl: pd.children_env,
                lambda _: PropertyError(LexicalEnv),
            ).then(lambda public_part: public_part.get(
                '__privatepart', LK.flat, categories=no_prims
            ).at(0).then(
                # If there is a private part, group it with the rest
                lambda pp: Array([pp.children_env, public_part]).env_group(),
                default_val=public_part
            ))
        )

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def in_scope():
        """
        Return True if ``origin`` is directly in the scope of this body.
        """
        return And(
            Not(origin.is_null),
            origin.unit == Self.unit,
            Not(origin.parents.find(lambda p: p == Self).is_null)
        )

    @langkit_property()
    def body_decl_scope():
        """
        Return the scope of this body's decl.
        """
        return env.bind(
            Self.default_initial_env,
            Entity.body_scope(True, True)
        )

    @langkit_property(return_type=T.Symbol)
    def body_initial_env_name():
        """
        A package or subprogram body has a named parent env only if it is a
        compilation unit root, in which case it will be the name of its
        corresponding declaration.
        """
        return Cond(
            Self.is_library_item,
            Self.top_level_env_name.concat(String(".__privatepart")).to_symbol,

            Self.is_subunit,
            Self.top_level_env_name.concat(String("__stub")).to_symbol,

            No(T.Symbol)
        )

    @langkit_property(return_type=T.DesignatedEnv)
    def body_initial_env():
        """
        Return the initial env for a body. It's always the current environment
        except for compilation unit roots for which we use the environment of
        their corresponding declaration.
        """
        return named_env(
            Self.body_initial_env_name,
            or_current=True
        )

    @langkit_property(return_type=T.Symbol)
    def previous_part_env_name():
        """
        Return the name of the lexical env of the previous part of this body.
        For a subunit, the previous part is its stub, otherwise it's the body's
        declaration. If that declaration has a named env, it will be registered
        with the same top_level_env_name as this body.
        """
        return If(
            Self.is_subunit,
            Self.top_level_env_name.concat(String("__stub")).to_symbol,
            Self.top_level_env_name.to_symbol
        )

    @langkit_property(return_type=T.env_assoc)
    def previous_part_link_env_assoc():
        """
        Return the env association that describes where to add a ``__nextpart``
        entry for this body, if it corresponds to a non-overloadable entity
        (i.e. not a subprogram).

        Note that entry navigation is handled a bit differently and in
        particular we don't need a ``__nextpart`` link for them. Hence this
        property is never called from EntryBody env specs.
        """
        return new_env_assoc(
            key='__nextpart',
            value=Self,
            dest_env=Self.previous_part_env_name.then(
                lambda name: named_env(name),
                default_val=direct_env(
                    env.bind(
                        Self.default_initial_env,
                        Entity.body_scope(
                            follow_private=False,
                            force_decl=True
                        )
                    ),
                    or_current=True
                )
            )
        )

    @langkit_property(return_type=T.BasicDecl.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def subp_previous_part():
        """
        Return the decl corresponding to this body. Specialized implementation
        for subprogram bodies.

        .. ATTENTION:: It is important to not perform any signature match in
            cases where we don't need to (top-level subprograms), as the
            robustness of some important properties is at stake (e.g.
            imported_units, and therefore find_all_references).
        """
        parent = Var(If(
            Entity.is_library_item,
            Entity.parent.parent,
            Entity.semantic_parent
        ))

        elements = Var(Cond(
            parent.is_null,
            No(AdaNode.entity.array),

            # If this is a library-level subprogram, the previous part can be
            # found by fetching the compilation unit spec.
            parent.is_a(CompilationUnit),
            parent.cast(CompilationUnit).other_part.then(
                # Make sure the previous part is at least a subprogram,
                # and not an arbitrary declaration.
                lambda cu: If(
                    cu.decl.is_a(BasicSubpDecl, GenericSubpDecl),
                    cu.decl.cast(AdaNode).as_entity.singleton,
                    No(AdaNode.entity.array)
                )
            ),

            # If this subprogam's parent is a BodyStub, this subprogram is
            # necessarily a Subunit and the stub is thus its previous part.
            parent.is_a(BodyStub),
            parent.singleton,

            # Otherwise, look in its immediate declarative region
            parent.immediate_declarative_region.get(
                Entity.name_symbol,
                lookup=LK.minimal
            ),
        ))

        # Since no overloading is possible for library-level subprograms and
        # separate subprograms, the element we found is already precise, and so
        # we don't need to perform the signature matching below.
        already_precise = Var(parent.is_a(CompilationUnit, BodyStub))

        precise = Var(If(
            already_precise,
            elements.at(0),
            elements.find(lambda sp: And(
                Not(sp.is_null),
                Not(sp.node == Self),
                sp.match(
                    # If this body completes a generic subprogram, then we
                    # just return it (no need to match the signature).
                    lambda _=T.GenericSubpDecl: True,

                    # A formal subprogram cannot be the previous part of any
                    # subprogram.
                    lambda _=T.FormalSubpDecl: False,

                    lambda subp_decl=T.BasicSubpDecl:
                    subp_decl.subp_decl_spec.match_signature(
                        Entity.subp_spec_or_null.cast(T.SubpSpec), True,
                        # We set use_entity_info to False so as to not match
                        # base subprograms.
                        use_entity_info=False
                    ),

                    lambda subp_stub=T.SubpBodyStub:
                    subp_stub.subp_spec.match_signature(
                        Entity.subp_spec_or_null.cast(T.SubpSpec), True,
                        # We set use_entity_info to False so as to not match
                        # base subprograms.
                        use_entity_info=False
                    ),

                    lambda _: False
                )
            ))
        ).cast_or_raise(T.BasicDecl.entity))

        return If(
            precise.is_null & imprecise_fallback,
            elements.find(
                lambda sp: And(
                    Not(sp.is_null),
                    Not(sp.node == Self),
                    sp.is_a(BasicSubpDecl, SubpBodyStub)
                )
            ).cast_or_raise(T.BasicDecl.entity),
            precise
        )

    @langkit_property(dynamic_vars=[env])
    def entry_previous_part():
        """
        Return the EntryDecl corresponding to this node.
        """
        spec = Var(Entity.cast_or_raise(EntryBody).params)
        return env.get(Entity.name_symbol, categories=no_prims).find(
            lambda sp: And(
                Not(sp.is_null),
                Not(sp.node == Self),
                sp.match(
                    lambda entry_decl=T.EntryDecl:
                    entry_decl.spec.match_formal_params(spec),

                    lambda _: False
                )
            )
        ).cast_or_raise(T.EntryDecl.entity)

    @langkit_property(dynamic_vars=[env])
    def package_previous_part():
        """
        Return the BasePackageDecl corresponding to this node.

        If the case of generic package declarations, this returns the
        ``package_decl`` field instead of the ``GenericPackageDecl`` itself.
        """
        return If(
            Self.is_library_item,
            Self.enclosing_compilation_unit.other_part.then(
                lambda part: part.decl.as_entity.cast(AdaNode)._.singleton
            ),
            origin.bind(
                Self.origin_node, Entity.defining_name.all_env_els_impl
            )
        ).map(
            lambda e: e.match(
                lambda pkg_decl=T.PackageDecl: pkg_decl,
                lambda gen_pkg_decl=T.GenericPackageDecl:
                gen_pkg_decl.package_decl,
                lambda _: No(T.BasicDecl.entity)
            )
        ).find(
            lambda e: Not(e.is_null)
        )

    @langkit_property(dynamic_vars=[env])
    def task_previous_part():
        """
        Return the task decl corresponding to this node.
        """
        return origin.bind(
            Self.origin_node, Entity.defining_name.all_env_els_impl
        ).at(0).cast(T.BasicDecl)

    @langkit_property(dynamic_vars=[env])
    def protected_previous_part():
        """
        Return the ProtectedDecl corresponding to this node.
        """
        return Entity.defining_name.env_elements.at(0)._.match(
            lambda prot_type=T.ProtectedTypeDecl: prot_type,
            lambda prot_decl=T.SingleProtectedDecl: prot_decl,
            lambda _: No(T.BasicDecl.entity)
        )

    @langkit_property(return_type=T.BasicDecl.entity,
                      dynamic_vars=[env, default_imprecise_fallback()])
    def unbound_previous_part():
        """
        Return the previous part for this body. Might be a declaration or a
        body stub.
        """
        pp = Var(Entity.match(
            lambda _=T.BaseSubpBody: Entity.subp_previous_part,
            lambda _=T.SubpBodyStub: Entity.subp_previous_part,
            lambda _=T.EntryBody: Entity.entry_previous_part,
            lambda _=T.PackageBody: Entity.package_previous_part,
            lambda _=T.PackageBodyStub: Entity.package_previous_part,
            lambda _=T.ProtectedBody: Entity.protected_previous_part,
            lambda _=T.ProtectedBodyStub: Entity.protected_previous_part,
            lambda _=T.TaskBody: Entity.task_previous_part,
            lambda _=T.TaskBodyStub: Entity.task_previous_part
        ))

        # TODO: It would be cleaner if the previous_part implems returned
        # the stubs, but for the moment they're not even added to the lexical
        # environments.

        return If(
            Entity.is_a(BodyStub),
            pp,
            Let(lambda pp_next_part=pp._.next_part_for_decl: If(
                pp_next_part.is_a(BodyStub),
                pp_next_part,
                pp
            ))
        )

    @langkit_property(return_type=T.BasicDecl.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def previous_part_internal():
        """
        Return the previous part for this body. Might be a declaration or a
        body stub.

        .. note::
            This internal property was introduced by T812-020 in order to break
            an infinite recursion.
        """
        # Use self.as_bare_entity and not Entity as a prefix for the following
        # call to unbound_previous_part. The reasoning is that the previous
        # part of a given Body is a static concept that does not depend on a
        # particular context, and thus should not be impacted by an entity's
        # metadata. In particular, this addresses T610-028.
        return env.bind(
            Self.node_env,
            Self.as_bare_entity.unbound_previous_part.node.as_entity
        )

    @langkit_property(public=True, return_type=T.BasicDecl.entity,
                      dynamic_vars=[default_imprecise_fallback()],
                      memoized=True)
    def previous_part():
        """
        Return the previous part for this body. Might be a declaration or a
        body stub.
        """
        return Entity.previous_part_internal

    @langkit_property(public=True, dynamic_vars=[default_imprecise_fallback()])
    def decl_part():
        """
        Return the decl corresponding to this node if applicable.
        """
        return Entity.previous_part_internal.then(
            lambda prev_part: prev_part.match(
                # Stubs have one more previous part. Go back one more level
                # to get the decl.
                lambda stub=T.BodyStub: stub.previous_part_internal,
                lambda other: other
            )
        )

    @langkit_property(return_type=T.GenericDecl.entity)
    def safe_generic_decl_part():
        """
        Return the generic declaration corresponding to this body, if relevant.
        This property is designed to be usable from within env specs.
        """
        return imprecise_fallback.bind(
            False,
            Cond(
                Self.is_a(PackageBody),
                Entity.decl_part.then(
                    lambda d: d.parent.cast(T.GenericPackageDecl)
                ),

                Self.is_a(BaseSubpBody, SubpBodyStub),
                # We're only searching for generics. We look at index 1 and
                # 2, because if self is a subunit, the first entity we find
                # will be the separate declaration. NOTE: We don't use
                # decl_part/previous_part on purpose: They can cause env
                # lookups, hence doing an infinite recursion.
                Entity.children_env.env_parent.get(
                    Entity.name_symbol, categories=no_prims
                ).then(
                    lambda results:
                    results.at(1).cast(T.GenericSubpDecl)._or(
                        results.at(2).cast(T.GenericSubpDecl)
                    )
                ),

                No(T.GenericDecl.entity)
            )
        )

    @langkit_property()
    def next_part_for_decl():
        """
        By default, bodies don't have a next part. This is not true for body
        stubs, hence this property is overriden there.
        """
        return No(BasicDecl.entity)

    @langkit_property()
    def is_subunit():
        return Self.parent.is_a(T.Subunit)

    @langkit_property(ignore_warn_on_node=True, public=True)
    def subunit_root():
        """
        If self is a subunit, return the body in which it is rooted.
        """
        return Self.parent.cast(T.Subunit).then(lambda su: su.body_root)

    @langkit_property(dynamic_vars=[env])
    def body_scope(follow_private=Bool, force_decl=(Bool, False)):
        """
        Return the scope for this body.
        If follow_private, then returns the private part if possible.

        If force_decl, then returns the corresponding declaration's scope,
        rather than the parent body's scope.
        """

        scope = Var(Cond(
            # Subunits always appear at the top-level in package bodies. So if
            # this is a subunit, the scope is the same as the scope of the
            # corresponding "is separate" decl, hence: the defining env of this
            # top-level package body.
            Not(Self.subunit_root.is_null), Self.subunit_root.children_env,

            # In case this is a library level subprogram that has no spec
            # (which is legal), we'll register this body in the parent
            # scope.
            Self.is_subprogram & Self.is_compilation_unit_root,
            Let(lambda dns=Entity.defining_name.scope:
                # If the scope is self's scope, return parent scope, or
                # else we'll have an infinite recursion.
                If(dns.is_null | (dns.env_node == Self),
                   Entity.defining_name.parent_scope,
                   dns)),

            # If this is a library level unit, or force_decl is True, return
            # the enclosing decl.
            Self.is_compilation_unit_root | force_decl,
            Entity.defining_name.scope,

            # The rest of cases are nested declarations: In that case we want
            # to take the parent's env.
            Self.parent.children_env,
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
            public_scope.get(
                '__privatepart', lookup=LK.flat, categories=no_prims
            ).at(0).then(
                lambda pp: pp.children_env, default_val=public_scope
            ),
            public_scope
        )


@abstract
class BodyStub(Body):
    """
    Base class for a body stub (:rmlink:`10.1.3`). A body stub is meant to
    be completed by .
    """

    @langkit_property(public=True)
    def next_part_for_decl():
        return Self.get_unit_root_decl(
            Entity.fully_qualified_name_array,
            UnitBody,
            process_parents=False

        ).as_entity

    @langkit_property(public=True)
    def syntactic_fully_qualified_name():
        """
        Return the syntactic fully qualified name to refer to this body.

        Note that this can raise a Property_Error when the stub is in an
        illegal place (too nested, in a declare block, etc.).
        """
        # Compute the "relative" name of the body for this stub
        rel_name = Var(Self.as_bare_entity.defining_name.as_symbol_array)

        # Fetch the compilation unit in which this stub is rooted.
        #
        # Body stubs can appear only in the top-level declarative part of a
        # library-level body or of a subunit. This means that:
        #
        # * ``Self.parent`` must be an ``AdaNode.list``,
        # * ``Self.parent.parent`` must be a ``DeclarativePart``,
        # * ``Self.parent.parent.parent`` must be a library item or subunit
        #   ``Body``.
        top_body = Var(Self.parent.parent.parent.match(
            lambda b=T.SubpBody: b,
            lambda b=T.ProtectedBody: b,
            lambda b=T.PackageBody: b,
            lambda b=T.TaskBody: b,
            lambda _: PropertyError(T.Body, "invalid body stub"),
        ))
        cu = Var(If(
            top_body.parent.is_a(T.LibraryItem)
            | top_body.parent.is_a(T.Subunit),

            top_body.parent.parent.cast(T.CompilationUnit),
            PropertyError(T.CompilationUnit, "invalid body stub"),
        ))

        return cu.syntactic_fully_qualified_name.concat(rel_name)

    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        """
        All body stubs allow for a named environment, which is registered with
        a ``__stub`` appended to the body's top_level_env_name.
        """
        return Array([
            Self.top_level_env_name.concat(String("__stub")).to_symbol
        ])

    @langkit_property()
    def previous_part_link_env_assoc():
        return new_env_assoc(
            key='__nextpart',
            value=Self,
            dest_env=named_env(Self.top_level_env_name.to_symbol)
        )


@abstract
class BaseFormalParamDecl(BasicDecl):
    """
    Base class for formal parameter declarations. This is used both for records
    components and for subprogram parameters.

    This is a Libadalang abstraction, that has no ARM existence.
    """
    is_mandatory = Property(False)

    @langkit_property(return_type=T.BaseTypeDecl.entity, public=True,
                      dynamic_vars=[default_origin()])
    def formal_type():
        """
        Return the type for this formal.
        """
        return Entity.type_expression._.designated_type

    @langkit_property()
    def parent_decl():
        return Entity.semantic_parent.cast(T.BasicDecl)

    @langkit_property(return_type=T.DefiningName.entity)
    def get_param(part=T.BasicDecl.entity, param=T.Symbol):
        return part.then(
            lambda d:
            d.formal_param_holder_or_null._.unpacked_formal_params
            .find(lambda sf: sf.name_is(param))
        )

    @langkit_property(return_type=T.DefiningName.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def decl_param(param=T.DefiningName.entity):
        """
        If self is a ParamSpec of a subprogram body, go fetch the equivalent
        spec in the subprogram decl.
        """
        return Entity.get_param(
            Entity.parent_decl.cast(T.BaseSubpBody)._.decl_part,
            param.name_symbol
        )._or(param)

    @langkit_property()
    def next_part_for_name(sym=T.Symbol):
        return Entity.get_param(
            Entity.parent_decl._.next_part_for_decl,
            sym
        )._.basic_decl

    @langkit_property()
    def previous_part_for_name(sym=T.Symbol):
        return Entity.get_param(
            Entity.parent_decl._.previous_part_for_decl,
            sym
        )._.basic_decl

    @langkit_property()
    def next_part_for_decl():
        return Entity.next_part_for_name(Entity.name_symbol)

    @langkit_property()
    def previous_part_for_decl():
        return Entity.previous_part_for_name(Entity.name_symbol)


class DiscriminantSpec(BaseFormalParamDecl):
    """
    Known list of discriminants in type declarations (:rmlink:`3.7`).
    """
    ids = Field(type=T.DefiningName.list)
    type_expr = Field(type=T.TypeExpr)
    default_expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    env_spec = EnvSpec(add_to_env(Self.env_mappings(Self.ids, Self)))

    defining_names = Property(Self.ids.map(lambda id: id.as_entity))
    defining_env = Property(Entity.type_expr.defining_env)

    type_expression = Property(Entity.type_expr)

    xref_entry_point = Property(True)

    is_constant_object = Property(True)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.type_expr.sub_equation,
            Entity.default_expr.then(
                lambda de:
                Bind(de.expected_type_var, Entity.expr_type)
                & de.sub_equation
                & de.matches_expected_assign_type,
                default_val=LogicTrue()
            )
        )


@abstract
class BaseFormalParamHolder(AdaNode):
    """
    Base class for lists of formal parameters. This is used in every case a
    list of "formals" can be called or instantiated, so in all the following
    cases:

    * Subprogram specifications (and subprogram calls).
    * Component lists (and aggregates).
    * Generic formals (and generic instantiations).

    This allows to share the parameter unpacking/matching logic.

    This is a Libadalang abstraction that has no existence in the Ada reference
    manual.
    """

    abstract_formal_params = AbstractProperty(
        type=BaseFormalParamDecl.entity.array,
        doc="Return the list of abstract formal parameters for this holder.",
        public=True
    )

    unpacked_formal_params = Property(
        Self.unpack_formals(Entity.abstract_formal_params),
        doc="""
        Return ``DefiningName`` for all parameters.
        """
    )

    @langkit_property(return_type=T.DefiningName.entity.array, public=True)
    def formal_params():
        """
        Return all parameters as a ``DefiningName`` array. This property
        doesn't return record discriminants nor variants when called on a
        record component list.
        """
        return Entity.match(
            lambda r=T.ComponentList:
            Self.unpack_formals(r.components.keep(BaseFormalParamDecl)),
            lambda _: Entity.unpacked_formal_params
        )

    @langkit_property(return_type=T.ParamMatch.array, dynamic_vars=[env])
    def match_param_list(params=T.AssocList.entity,
                         is_dottable_subp=Bool):
        return Self.match_formals(
            Entity.abstract_formal_params, params, is_dottable_subp
        )

    nb_min_params = Property(
        Self.as_bare_entity.unpacked_formal_params.filter(
            lambda p: p.formal_decl.is_mandatory
        ).length,
        type=Int, public=True, doc="""
        Return the minimum number of parameters this subprogram can be called
        while still being a legal call.
        """
    )

    nb_max_params = Property(
        Self.as_bare_entity.unpacked_formal_params.length, public=True,
        type=Int, doc="""
        Return the maximum number of parameters this subprogram can be called
        while still being a legal call.
        """
    )

    @langkit_property(return_type=Bool)
    def paramless(dottable_subp=Bool, can_be=(Bool, True)):
        """
        Utility function. Given a subprogram spec and whether the subprogram
        was referenced using the dot notation, determine if it can be called
        without parameters (and hence without a callexpr).
        """
        nb_params = Var(If(can_be, Self.nb_min_params, Self.nb_max_params))
        return Or(
            dottable_subp & (nb_params == 1),
            nb_params == 0
        )

    @langkit_property(return_type=Bool, dynamic_vars=[env])
    def is_matching_param_list(params=T.AssocList.entity,
                               is_dottable_subp=Bool):
        """
        Return whether a AssocList is a match for this SubpSpec, i.e.
        whether the argument count (and designators, if any) match.
        """
        bare = Var(Self.as_bare_entity)
        match_list = Var(bare.match_param_list(params, is_dottable_subp))
        nb_max_params = Var(
            If(is_dottable_subp, bare.nb_max_params - 1, bare.nb_max_params)
        )
        nb_min_params = Var(
            If(is_dottable_subp, bare.nb_min_params - 1, bare.nb_min_params)
        )

        return And(
            params.length <= nb_max_params,
            match_list.all(lambda m: m.has_matched),
            match_list.filter(
                lambda m: m.formal.formal_decl.is_mandatory
            ).length == nb_min_params,
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity.array,
                      dynamic_vars=[default_origin()], public=True)
    def param_types():
        """
        Returns the type of each parameter of Self.
        """
        return Entity.unpacked_formal_params.map(
            lambda fp:
            Entity.real_designated_type(fp.formal_decl.type_expression)
        )

    @langkit_property(return_type=T.Mode.entity.array)
    def param_modes():
        """
        Returns the mode of each parameter of Self.
        """
        return Entity.unpacked_formal_params.map(
            lambda fp: fp.formal_decl.cast(ParamSpec).mode
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()])
    def real_type(typ=T.BaseTypeDecl.entity):
        """
        Return the real type denoted by ``typ``, taking into account that
        ``typ`` might be the type of a derived primitive. In that case, return
        the derived primitive type.
        """
        # Compute the type entity of which self is a primitive
        prim_type = Var(Entity.entity_no_md(
            Entity.info.md.primitive,
            Entity.info.rebindings,
            Entity.info.from_rebound
        ).cast(BaseTypeDecl))

        return Cond(
            prim_type.node.is_null,
            typ,

            prim_type._.canonical_type.node == typ._.canonical_type.node,
            Entity.entity_no_md(
                Entity.info.md.primitive_real_type._or(typ.node),
                Entity.info.rebindings,
                Entity.info.from_rebound
            ).cast(BaseTypeDecl),

            # Handle the case where the primitive is defined on an anonymous
            # access type, by returning an anonymous access type over the
            # real_type of the accessed type.
            typ.cast(AnonymousTypeDecl).then(
                lambda td: Not(td.type_def.is_a(AccessToSubpDef))
            ),
            typ.accessed_type.then(
                lambda at: Entity.real_type(at).then(
                    lambda rat: If(
                        at == rat,
                        typ,
                        rat.anonymous_access_type
                    )
                )
            ),

            typ
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[origin])
    def real_designated_type(typ=T.TypeExpr.entity):
        """
        Given a type expression that is part of this subprogram specification
        (for example, appearing in a parameter specification), return the real
        type it designates, taking into account the fact that Self might be
        the specification of an inherited subprogram. Overall, we can
        distinguish the following cases:

        - Self is a primitive subprogram inherited from a base type and
          ``typ`` designates that base type, in which case we should return
          the inheriting type.

        - Self is a primitive subprogram inherited from a base type but
          ``typ`` does not designate that base type, in which case we must
          compute the actual designated type by taking into account the
          rebindings associated with the base type. This is done by
          traversing the inheritance hierarchy starting from the inheriting
          type up to the inherited type and extracting the rebindings that we
          got along the way.

        - Self is not an inherited primitive subprogram, in which case we
          simply return the designated type using the normal path.

        The first two points are illustrated with the following example.

        .. code::

            generic
               type G is private;
            package Pkg is
               type T is null record;

               function Foo (Self : T) return G;      --  A
            end Pkg;

            package My_Pkg is new Pkg (Integer);      --  B

            type My_T is new My_Pkg.T;

            X : My_T    := (null record);
            Y : Integer := Foo (X);                    -- C

        Resolving the reference to ``Foo`` at line C gets us the function
        declaration at line A with the appropriate metadata indicating it is a
        primitive subprogram of T inherited by My_T.

        Calling this property on the ``T`` node from the ``Self : T`` parameter
        specification is an instance of the first case. We should obviously
        return ``My_T`` in that case.

        Calling it on ``G`` from the return type specification is an instance
        of the second case. We traverse up the inheritance hierarchy starting
        from ``My_T`` and get to ``T [B]``, where ``[B]`` indicates the
        rebindings corresponding to the instantiation at line B. We can now use
        those rebindings to compute the actual designated type (the type
        designated by ``G [B]``) which correctly yields ``Integer``.

        This property is used during the construction of xref equations for
        call expressions in order to match the right parameter and return
        types.
        """
        base_rebindings = Var(
            Entity.info.md.primitive_real_type.cast(BaseTypeDecl)
            .as_bare_entity._.find_base_type_rebindings(
                Entity.info.md.primitive.cast(BaseTypeDecl)
            )
        )

        return typ.designated_type._.without_md.cast(BaseTypeDecl).then(
            lambda t: Let(
                lambda rt=Entity.real_type(t):
                If(
                    t == rt,
                    If(
                        base_rebindings.is_null,
                        t,
                        TypeExpr.entity.new(
                            node=typ.node,
                            info=T.entity_info.new(
                                md=typ.info.md,
                                rebindings=typ.info.rebindings
                                .concat_rebindings(base_rebindings),
                                from_rebound=typ.info.from_rebound
                            )
                        ).designated_type
                    ),
                    rt
                )
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def call_argument_equation(param=T.BaseFormalParamDecl.entity,
                               arg=T.Expr.entity):
        """
        Generate the equation that binds the type_var of this expression
        given its corresponding parameter in the context of a subprogram call.
        This takes into account the fact that the called subprogram might
        be an inherited primitive.
        """
        param_type = Var(Entity.real_designated_type(param.type_expression))
        return And(
            Bind(arg.expected_type_var, param_type),
            arg.matches_expected_formal_type
        )

    @langkit_property(return_type=Bool)
    def match_formal_params(other=T.BaseFormalParamHolder.entity,
                            match_names=(Bool, True),
                            ignore_first_param=(Bool, False)):
        """
        Check whether self's params match other's.
        """
        # Check that there is the same number of formals and that each
        # formal matches.
        self_params = Var(Entity.unpacked_formal_params.then(
            lambda params: If(
                ignore_first_param,
                params.filter(lambda i, e: i != 0),
                params
            )
        ))
        other_params = Var(other.unpacked_formal_params)

        self_types = Var(origin.bind(Self.origin_node, Entity.param_types))
        other_types = Var(origin.bind(other.node.origin_node,
                                      other.param_types))
        return And(
            self_params.length == other_params.length,
            origin.bind(Self.origin_node, self_params.all(
                lambda i, p:
                Or(Not(match_names),
                   p.name.matches(other_params.at(i).name.node))
                & self_types.at(i)._.matching_type(other_types.at(i))
            ))
        )

    @langkit_property(return_type=Bool)
    def match_other(other=T.BaseFormalParamHolder.entity,
                    match_names=(Bool, True)):
        """
        Base method of any BaseFormalParamHolder that checks whether the
        other given BaseFormalParamHolder matches. In practice, this will call
        match_formal_params, except for BaseSubpSpecs for which it will call
        match_signature.
        """
        return Entity.match_formal_params(other, match_names)


@abstract
class DiscriminantPart(BaseFormalParamHolder):
    """
    Specification for discriminants in type declarations.
    """

    @langkit_property()
    def abstract_formal_params():
        return No(T.BaseFormalParamDecl.entity.array)


class KnownDiscriminantPart(DiscriminantPart):
    """
    Known list of discriminants in type declarations (:rmlink:`3.7`).
    """

    discr_specs = Field(type=T.DiscriminantSpec.list)

    @langkit_property()
    def abstract_formal_params():
        return Self.discr_specs.map(
            lambda e: e.cast(BaseFormalParamDecl).as_entity
        )


class UnknownDiscriminantPart(DiscriminantPart):
    """
    Unknown list of discriminants in type declarations (:rmlink:`3.7`).
    """
    pass


@abstract
class TypeDef(AdaNode):
    """
    Base class for type definitions (:rmlink:`3.2.1`).
    """

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

    is_real_type = Property(
        Entity.is_float_type | Entity.is_fixed_point,
        dynamic_vars=[origin],
        doc="Whether type is a real type or not."
    )

    is_float_type = Property(
        False, doc="Whether type is a float type or not.",
        dynamic_vars=[origin]
    )
    is_fixed_point = Property(
        False, doc="Whether type is a fixed point type or not.",
        dynamic_vars=[origin]
    )

    @langkit_property(return_type=T.env_assoc.array)
    def predefined_operators():
        """
        Return the list of predefined operators for this type definition.
        See TypeDecl.predefined_operators.

        This property is overridden by the various TypeDef concrete classes to
        implement type-specific logic.
        """
        return No(T.env_assoc.array)

    @langkit_property(return_type=T.env_assoc.array)
    def predefined_equality_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)
        return [
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type)
        ]

    @langkit_property(dynamic_vars=[origin])
    def is_discrete_type():
        return Entity.base_type.then(
            lambda bt: bt.is_discrete_type,
            default_val=Or(
                Entity.is_int_type, Entity.is_enum_type, Entity.is_char_type
            )
        )

    @langkit_property(dynamic_vars=[origin])
    def is_int_type():
        """Whether type is an integer type or not."""
        return False

    is_access_type = Property(
        False, uses_entity_info=False,
        doc="Whether type is an access type or not.",
        dynamic_vars=[origin]
    )

    is_char_type = Property(False, dynamic_vars=[default_origin()])
    is_enum_type = Property(False, dynamic_vars=[default_origin()])

    @langkit_property(dynamic_vars=[origin])
    def accessed_type():
        return No(BaseTypeDecl.entity)

    @langkit_property(dynamic_vars=[default_origin()])
    def is_tagged_type():
        """
        Return whether this type is tagged.
        """
        return False

    base_type = Property(
        No(T.BaseTypeDecl.entity), doc="""
        Return the base type entity for this derived type definition.
        """,
        dynamic_vars=[origin]
    )

    base_types = Property(
        Entity.base_type.then(lambda bt: bt.singleton)
        .concat(Entity.base_interfaces),
        doc="""
        Return all the base types for this type (base type + base interfaces)
        """,
        dynamic_vars=[origin]
    )

    base_interfaces = Property(
        No(T.BaseTypeDecl.entity.array), doc="""
        Return the interfaces this type derives from
        """
    )

    @langkit_property(dynamic_vars=[origin, include_ud_indexing])
    def defining_env():
        # Regroup implementations for subclasses here instead of overriding to
        # avoid code duplication (multiple cases have the same implementation).
        return Cond(
            # A "record" or "private" type def may be the completion of a
            # previous type declaration, so we need to include the defining
            # env of its previous part as well.
            Self.is_a(T.RecordTypeDef, T.PrivateTypeDef),
            Array([
                Entity.children_env,
                Entity.dottable_subps_env,
                Entity.previous_part_env
            ]).env_group(),

            # Same for "derived" and "interface" type definitions, but we also
            # need to include the defining environments of their base types.
            Self.is_a(T.DerivedTypeDef, T.InterfaceTypeDef),
            # Make sure to put own defining env before base types' defining
            # envs in the result, so that most-overriden subprograms will be
            # considered first during name resolution.
            Array([
                Entity.children_env,
                Entity.dottable_subps_env,
                Entity.previous_part_env
            ]).concat(
                Entity.base_types.map(lambda bt: bt._.defining_env)
            ).env_group(),

            Entity.match(
                lambda ar=T.ArrayTypeDef: ar.comp_type.defining_env,

                # An access to procedure will have a null accessed_type, hence
                # the use of the underscore.
                lambda ac=T.AccessDef: ac.accessed_type._.defining_env,

                lambda _: EmptyEnv
            )
        )

    containing_type = Property(
        Entity.parent.cast_or_raise(T.TypeDecl), doc="""
        Return the TypeDecl containing this TypeDef
        """
    )

    previous_part = Property(Entity.containing_type.previous_part(True))

    previous_part_env = Property(
        Entity.previous_part._.defining_env,
        dynamic_vars=[origin]
    )

    dottable_subps_env = Property(
        # Return the environment containing all subprograms that can be called
        # with the dot-notation on values of the type which this is defined.
        # It is important to rebind the env with our current rebindings,
        # so that subsequent calls to env.get on this env return those
        # subprograms with the adequate rebindings.
        Entity.containing_type.dottable_subps_env.rebind_env(
            Entity.info.rebindings
        ),
        dynamic_vars=[origin]
    )

    is_static = Property(False, dynamic_vars=[default_imprecise_fallback()])


class Variant(AdaNode):
    """
    Single variant in a discriminated type record declaration.

    This corresponds to a ``when ... => ...`` section in a variant part.
    """
    choices = Field(type=T.AlternativesList)
    components = Field(type=T.ComponentList)

    @langkit_property(return_type=Bool)
    def matches(expr=T.Expr.entity):
        """
        Check if any choice in the choice list matches expr's value.
        """
        # Statically evaluate expr
        expr_val = Var(expr.eval_as_int)

        return Entity.choices.any(
            lambda c: c.choice_match(expr_val)
        )


class VariantPart(AdaNode):
    """
    Variant part in a discriminated type record declaration
    (:rmlink:`3.8.1`).

    This corresponds to the whole ``case ... is ... end case;`` block.
    """
    discr_name = Field(type=T.Identifier)
    variant = Field(type=T.Variant.list)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        ignore(Var(Entity.discr_name.resolve_names_internal))

        return Entity.variant.logic_all(
            lambda var: var.choices.logic_all(lambda c: c.match(
                # Expression case
                lambda e=T.Expr: If(
                    Not(e.cast(Name)._.name_designated_type.is_null),

                    e.cast(Name).xref_no_overloading,

                    Bind(e.expected_type_var, Self.discr_name.type_val)
                    & e.sub_equation
                    & e.matches_expected_type
                ),


                # SubtypeIndication case (``when Color range Red .. Blue``)
                lambda t=T.SubtypeIndication: t.xref_equation,

                lambda _=T.OthersDesignator: LogicTrue(),

                lambda _: PropertyError(T.Equation, "Should not happen")
            ))
        )

    @langkit_property(return_type=T.BaseFormalParamDecl.entity.array)
    def get_components(discriminants=T.ParamMatch.array):
        """
        Get components for this variant part, depending on the values of
        discriminants.
        """
        # Get the specific discriminant this variant part depends upon
        discr = Var(discriminants.find(
            lambda d: d.formal.name.name_is(Self.discr_name.symbol)
        ))

        # Get the variant branch with a choice that matches the discriminant's
        # value.
        variant = Var(Entity.variant.find(
            lambda v: v.matches(discr.actual.assoc.expr)
        ))

        # Get the components for this variant branch. We're passing down
        # discriminants, because there might be a nested variant part in this
        # variant branch.
        return variant.components.abstract_formal_params_impl(
            discriminants, False, False
        )


class ComponentDecl(BaseFormalParamDecl):
    """
    Declaration for a component (:rmlink:`3.8`).
    """
    ids = Field(type=T.DefiningName.list)
    component_def = Field(type=T.ComponentDef)
    default_expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    env_spec = EnvSpec(add_to_env(Self.env_mappings(Self.ids, Self)))

    defining_env = Property(
        Entity.component_def.type_expr.defining_env,
        doc="See BasicDecl.defining_env"
    )

    defining_names = Property(Self.ids.map(lambda i: i.as_entity))

    array_ndims = Property(Entity.component_def.type_expr.array_ndims)

    type_expression = Property(Self.component_def.type_expr.as_entity)

    is_constant_object = Property(False)

    @langkit_property(return_type=Equation)
    def constrain_prefix(prefix=T.Expr):
        return If(
            # If the prefix is `X'Unrestricted_Access`, we have an implicit
            # dereference. Do not constrain the equation further here and let
            # the AttributeRef's xref_equation handle this case.
            prefix.cast(AttributeRef)._.is_access_attr,

            LogicTrue(),

            # The expected type of the prefix is the record type of this
            # component.
            And(
                Bind(prefix.expected_type_var, Entity.container_type),
                prefix.matches_expected_prefix_type
            )
        )

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
                Bind(de.expected_type_var, typ)
                & de.sub_equation
                & de.matches_expected_assign_type,
                default_val=LogicTrue()
            )
        )

    xref_entry_point = Property(True)


class DiscriminantValues(Struct):
    """
    Represent a set of values (as a list of choices) on a discriminant.
    """
    discriminant = UserField(type=T.Identifier.entity)
    values = UserField(type=T.AlternativesList.entity)


class Shape(Struct):
    """
    Represent one of the shapes that a variant record can have, as a list of
    the available components.
    """
    components = UserField(type=T.BaseFormalParamDecl.entity.array)
    discriminants_values = UserField(type=T.DiscriminantValues.array)


class ComponentList(BaseFormalParamHolder):
    """
    List of component declarations (:rmlink:`3.8`).
    """
    components = Field(type=T.AdaNode.list)
    variant_part = Field(type=T.VariantPart)

    type_def = Property(Self.parent.parent.cast(T.TypeDef).as_entity)
    type_decl = Property(Entity.type_def.parent.cast(T.TypeDecl))

    parent_component_list = Property(origin.bind(
        Self,
        Entity.type_def.cast(T.DerivedTypeDef)._.base_type.record_def._.comps
    ))

    @langkit_property(return_type=BaseFormalParamDecl.entity.array,
                      dynamic_vars=[env, default_origin()])
    def abstract_formal_params_for_assocs(assocs=T.AssocList.entity):

        td = Var(Entity.type_decl)
        discriminants = Var(td.discriminants_list)

        # Get param matches for discriminants only
        discriminants_matches = Var(Self.match_formals(
            discriminants, assocs, False
        ).filter(
            lambda pm: And(
                Not(pm.formal.is_null),
                Not(discriminants.find(
                    lambda d: d == pm.formal.formal_decl).is_null)
                )
        ))

        # Get param matches for all aggregates' params. Here, we use and pass
        # down the discriminant matches, so that abstract_formal_params_impl is
        # able to calculate the list of components belonging to variant parts,
        # depending on the static value of discriminants.
        return td.record_def.comps.abstract_formal_params_impl(
            discriminants=discriminants_matches
        )

    @langkit_property(return_type=BaseFormalParamDecl.entity.array)
    def abstract_formal_params_for_delta_assocs():
        """
        Return all the components lists of this ComponentList, which includes
        the current and parent components, and all the components lists of the
        variant part, recursively, regardless of the discriminants values. This
        list is used for DeltaAggregate name resolution.
        """
        variant_part_components = Var(Entity.variant_part._.variant.mapcat(
            lambda v: v.components.abstract_formal_params_for_delta_assocs
        ))
        self_components = Var(Entity.components.keep(BaseFormalParamDecl))
        parent_components = Var(Entity.parent_component_list
                                ._.abstract_formal_params_for_delta_assocs)
        return (variant_part_components.concat(self_components)
                .concat(parent_components))

    @langkit_property(return_type=BaseFormalParamDecl.entity.array)
    def abstract_formal_params_impl(
        discriminants=T.ParamMatch.array,
        include_discriminants=(Bool, True),
        recurse=(Bool, True)
    ):

        # Get self's components. We pass along discriminants, to get variant
        # part's components too.
        self_comps = Var(Entity.components.keep(BaseFormalParamDecl).concat(
            Entity.variant_part._.get_components(discriminants)
        ))

        # Append parent's components: the parent could have a variant part too,
        # which discriminants can be constrained by the subtype indication from
        # our DerivedTypeDef. The code below retrieves the relevant components
        # from the parent record taking that into account.
        ret = Var(If(
            recurse,
            Entity.parent_component_list.then(
                lambda pcl: pcl.abstract_formal_params_impl(
                    pcl.match_formals(
                        pcl.type_decl.discriminants_list,
                        Entity.type_def.cast(DerivedTypeDef)
                        .subtype_indication.constraint
                        .cast(CompositeConstraint)._.constraints,
                        is_dottable_subp=False
                    ),
                    include_discriminants=False
                ).concat(self_comps),
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

    @langkit_property(return_type=Shape.array)
    def shapes():
        """
        Return all the possible shapes that this component list spans.
        """
        self_comps = Var(Entity.components.keep(BaseFormalParamDecl))
        return Entity.variant_part.then(
            lambda vpart: vpart.variant.mapcat(
                lambda v: v.components.shapes.map(
                    lambda s: Shape.new(
                        components=self_comps.concat(s.components),
                        discriminants_values=T.DiscriminantValues.new(
                            discriminant=vpart.discr_name,
                            values=v.choices
                        ).singleton.concat(s.discriminants_values)
                    )
                )
            ),
            default_val=Shape.new(
                components=self_comps,
                discriminants_values=No(T.DiscriminantValues.array)
            ).singleton
        )


@abstract
class BaseRecordDef(AdaNode):
    """
    Base class for record definitions (:rmlink:`3.8`).
    """
    components = Field(type=T.ComponentList)

    # TODO: Kludge, to remove when Q619-018 is implemented
    comps = Property(Entity.components)


class RecordDef(BaseRecordDef):
    """
    Record definition that contains components (``record ... end record``).
    """
    pass


class NullRecordDef(BaseRecordDef):
    """
    Record definition for ``null record``.
    """
    pass


class Tagged(AdaNode):
    """
    Qualifier for the ``tagged`` keyword.
    """
    enum_node = True
    qualifier = True


class Abstract(AdaNode):
    """
    Qualifier for the ``abstract`` keyword.
    """
    enum_node = True
    qualifier = True


class Limited(AdaNode):
    """
    Qualifier for the ``limited`` keyword.
    """
    enum_node = True
    qualifier = True


class Private(AdaNode):
    """
    Qualifier for the ``private`` keyword.
    """
    enum_node = True
    qualifier = True


class Aliased(AdaNode):
    """
    Qualifier for the ``aliased`` keyword.
    """
    enum_node = True
    qualifier = True


class NotNull(AdaNode):
    """
    Qualifier for the ``not null`` keywords.
    """
    enum_node = True
    qualifier = True


class Constant(AdaNode):
    """
    Qualifier for the ``constant`` keyword.
    """
    enum_node = True
    qualifier = True


class All(AdaNode):
    """
    Qualifier for the ``all`` keyword.
    """
    enum_node = True
    qualifier = True


class Abort(AdaNode):
    """
    Qualifier for the ``abort`` keyword.
    """
    enum_node = True
    qualifier = True


class Reverse(AdaNode):
    """
    Qualifier for the ``reverse`` keyword.
    """
    enum_node = True
    qualifier = True


class WithPrivate(AdaNode):
    """
    Qualifier for the ``private`` keyword in ``with private`` record clauses.
    """
    enum_node = True
    qualifier = True


class Until(AdaNode):
    """
    Qualifier for the ``until`` keyword.
    """
    enum_node = True
    qualifier = True


class Synchronized(AdaNode):
    """
    Qualifier for the ``synchronized`` keyword.
    """
    enum_node = True
    qualifier = True


class Protected(AdaNode):
    """
    Qualifier for the ``protected`` keyword.
    """
    enum_node = True
    qualifier = True


class RecordTypeDef(TypeDef):
    """
    Type definition for a record (:rmlink:`3.8`).
    """
    has_abstract = Field(type=Abstract)
    has_tagged = Field(type=Tagged)
    has_limited = Field(type=Limited)
    record_def = Field(type=T.BaseRecordDef)

    is_tagged_type = Property(Self.has_tagged.as_bool)

    xref_equation = Property(LogicTrue())

    @langkit_property(memoized=True)
    def predefined_operators():
        return If(
            Self.has_limited.as_bool,
            No(T.env_assoc.array),
            Self.predefined_equality_operators
        )


@abstract
class RealTypeDef(TypeDef):
    """
    Type definition for real numbers (:rmlink:`3.5.6`).
    """
    is_static = Property(True)

    @langkit_property(memoized=True)
    def predefined_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)
        int_type = Var(Self.int_type.node)
        root_int_type = Var(Self.root_int_type.node)

        defaults = Var([
            Self.create_binop_assoc('"+"', self_type, self_type, self_type),
            Self.create_binop_assoc('"-"', self_type, self_type, self_type),
            Self.create_binop_assoc('"*"', self_type, self_type, self_type),
            Self.create_binop_assoc('"/"', self_type, self_type, self_type),

            Self.create_binop_assoc('"**"', self_type, int_type, self_type),

            Self.create_binop_assoc('"<"', self_type, self_type, bool_type),
            Self.create_binop_assoc('"<="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type),
            Self.create_binop_assoc('">"', self_type, self_type, bool_type),
            Self.create_binop_assoc('">="', self_type, self_type, bool_type),

            Self.create_unop_assoc('"+"', self_type, self_type),
            Self.create_unop_assoc('"-"', self_type, self_type),
            Self.create_unop_assoc('"abs"', self_type, self_type),
        ])

        # The root_real type also defines the three following operators
        specials = Var(If(
            self_type == Self.root_real_type.node,
            [
                Self.create_binop_assoc(
                    '"*"', root_int_type, self_type, self_type
                ),
                Self.create_binop_assoc(
                    '"*"', self_type, root_int_type, self_type
                ),
                Self.create_binop_assoc(
                    '"/"', self_type, root_int_type, self_type
                ),
            ],
            No(T.env_assoc.array)
        ))

        return defaults.concat(specials)


class EvalDiscreteRange(Struct):
    """
    Represent the range of a discrete type or subtype. The bounds are already
    evaluated, so the type of the fields is BigInt.
    """
    low_bound = UserField(type=T.BigInt)
    high_bound = UserField(type=T.BigInt)


class DiscreteRange(Struct):
    """
    Represent the range of a discrete type or subtype. The bounds are not
    evaluated, you need to call ``eval_as_int`` on them, if they're static, to
    get their value.
    """
    low_bound = UserField(type=T.Expr.entity)
    high_bound = UserField(type=T.Expr.entity)


class LogicValResult(Struct):
    """
    Represent the result of a call to logic_val. ``success`` is True iff
    solving the logic equation was successful, and ``value`` holds the value of
    the logic variable.
    """
    success = UserField(type=Bool)
    value = UserField(type=T.AdaNode.entity)


@synthetic
class TypeAttributesRepository(AdaNode):
    """
    Synthetic node that contains the lazy fields for the attribute subprograms
    of a given type. The lazy fields are not directly on the BaseTypeDecl node
    itself to minimize its size in memory: with this indirection, a type for
    which no function attribute is ever synthesized will not waste any memory.
    """
    base_type = UserField(type=T.BaseTypeDecl, public=False)

    @lazy_field(return_type=T.SyntheticTypeExpr, ignore_warn_on_node=True)
    def base_type_expr():
        return SyntheticTypeExpr.new(target_type=Self.base_type)

    @lazy_field(return_type=T.SyntheticTypeExpr, ignore_warn_on_node=True)
    def universal_int_type_expr():
        return SyntheticTypeExpr.new(
            target_type=Self.universal_int_type.cast(BaseTypeDecl).node
        )

    @lazy_field(return_type=T.SyntheticFormalParamDecl,
                ignore_warn_on_node=True)
    def base_type_param():
        return SyntheticFormalParamDecl.new(
            param_name='Value',
            param_type=Self.base_type_expr
        )

    @lazy_field(return_type=T.SyntheticFormalParamDecl,
                ignore_warn_on_node=True)
    def universal_int_param():
        return SyntheticFormalParamDecl.new(
            param_name='Value',
            param_type=Self.universal_int_type_expr
        )

    @lazy_field(return_type=T.SyntheticFormalParamDecl,
                ignore_warn_on_node=True)
    def universal_real_param():
        return SyntheticFormalParamDecl.new(
            param_name='Value',
            param_type=SyntheticTypeExpr.new(
                target_type=Self.universal_real_type.cast(BaseTypeDecl).node
            )
        )

    @lazy_field(return_type=T.SyntheticFormalParamDecl,
                ignore_warn_on_node=True)
    def root_stream_param():
        return SyntheticFormalParamDecl.new(
            param_name='S',
            param_type=SyntheticTypeExpr.new(
                target_type=Self.root_stream_type.anonymous_access_type.node
            )
        )

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def succ():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Succ",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def pred():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Pred",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def min():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Min",
            left_param=Self.base_type_param,
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def max():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Max",
            left_param=Self.base_type_param,
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def rounding():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Rounding",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def ceiling():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Ceiling",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def floor():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Floor",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def truncation():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Truncation",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def machine():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Machine",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def machine_rounding():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Machine_Rounding",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def fraction():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Fraction",
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def exponent():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Exponent",
            right_param=Self.base_type_param,
            return_type_expr=Self.universal_int_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def copy_sign():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Copy_Sign",
            left_param=Self.base_type_param,
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def remainder():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Remainder",
            left_param=Self.base_type_param,
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def adjacent():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Adjacent",
            left_param=Self.base_type_param,
            right_param=Self.base_type_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def scaling():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Scaling",
            left_param=Self.base_type_param,
            right_param=Self.universal_int_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def compose():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Compose",
            left_param=Self.base_type_param,
            right_param=Self.universal_int_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def mod():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Mod",
            right_param=Self.universal_int_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def value():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Value",
            right_param=SyntheticFormalParamDecl.new(
                param_name="Val",
                param_type=SyntheticTypeExpr.new(
                    target_type=Self.std_entity("String")
                    .cast(BaseTypeDecl).node
                )
            ),
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def wide_value():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Wide_Value",
            right_param=SyntheticFormalParamDecl.new(
                param_name="Val",
                param_type=SyntheticTypeExpr.new(
                    target_type=Self.std_entity("Wide_String")
                    .cast(BaseTypeDecl).node
                )
            ),
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def wide_wide_value():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Wide_Wide_Value",
            right_param=SyntheticFormalParamDecl.new(
                param_name="Val",
                param_type=SyntheticTypeExpr.new(
                    target_type=Self.std_entity("Wide_Wide_String")
                    .cast(BaseTypeDecl).node
                )
            ),
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def fixed_value():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Fixed_Value",
            right_param=Self.universal_int_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def integer_value():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Integer_Value",
            right_param=Self.universal_real_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def pos():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Pos",
            right_param=Self.base_type_param,
            return_type_expr=Self.universal_int_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def val():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Val",
            right_param=Self.universal_int_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def enum_rep():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Enum_Rep",
            right_param=Self.base_type_param,
            return_type_expr=Self.universal_int_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def enum_val():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Enum_Val",
            right_param=Self.universal_int_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def read():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Read",
            left_param=Self.root_stream_param,
            right_param=Self.base_type_param,
            return_type_expr=No(TypeExpr)
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def write():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Write",
            left_param=Self.root_stream_param,
            right_param=Self.base_type_param,
            return_type_expr=No(TypeExpr)
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def input():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Input",
            right_param=Self.root_stream_param,
            return_type_expr=Self.base_type_expr
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def output():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Output",
            left_param=Self.root_stream_param,
            right_param=Self.base_type_param,
            return_type_expr=No(TypeExpr)
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def image():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Image",
            right_param=Self.base_type_param,
            return_type_expr=SyntheticTypeExpr.new(
                target_type=Self.std_entity("String").cast(BaseTypeDecl).node
            )
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def wide_image():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Wide_Image",
            right_param=Self.base_type_param,
            return_type_expr=SyntheticTypeExpr.new(
                target_type=Self.std_entity("Wide_String")
                .cast(BaseTypeDecl).node
            )
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def wide_wide_image():
        return SyntheticSubpDecl.new(spec=SyntheticUnarySpec.new(
            subp_symbol="Wide_Wide_Image",
            right_param=Self.base_type_param,
            return_type_expr=SyntheticTypeExpr.new(
                target_type=Self.std_entity("Wide_Wide_String")
                .cast(BaseTypeDecl).node
            )
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def put_image():
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Put_Image",
            left_param=SyntheticFormalParamDecl.new(
                param_name="Buffer",
                param_type=SyntheticTypeExpr.new(
                    target_type=Self.root_buffer_type.classwide_type.node
                )
            ),
            right_param=Self.base_type_param,
            return_type_expr=No(TypeExpr)
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def asm_input():
        input_type = Var(
            Self.get_unit_root_decl(['System', 'Machine_Code'],
                                    UnitSpecification)
            ._.children_env.get_first('Asm_Input_Operand', lookup=LK.flat)
            .node.cast(T.BaseTypeDecl)
        )
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Asm_Input",
            left_param=SyntheticFormalParamDecl.new(
                param_name="S",
                param_type=SyntheticTypeExpr.new(
                    target_type=Self.std_entity("String")
                    .cast(BaseTypeDecl).node
                )
            ),
            right_param=Self.base_type_param,
            return_type_expr=SyntheticTypeExpr.new(target_type=input_type)
        ))

    @lazy_field(return_type=T.BasicSubpDecl, ignore_warn_on_node=True)
    def asm_output():
        output_type = Var(
            Self.get_unit_root_decl(['System', 'Machine_Code'],
                                    UnitSpecification)
            ._.children_env.get_first('Asm_Output_Operand', lookup=LK.flat)
            .node.cast(T.BaseTypeDecl)
        )
        return SyntheticSubpDecl.new(spec=SyntheticBinarySpec.new(
            subp_symbol="Asm_Output",
            left_param=SyntheticFormalParamDecl.new(
                param_name="S",
                param_type=SyntheticTypeExpr.new(
                    target_type=Self.std_entity("String")
                    .cast(BaseTypeDecl).node
                )
            ),
            right_param=Self.base_type_param,
            return_type_expr=SyntheticTypeExpr.new(target_type=output_type)
        ))


@abstract
class BaseTypeDecl(BasicDecl):
    """
    Base class for type declarations. It unifies every kind of type that exists
    in Ada, including types that have no source existence like classwide types.
    """
    name = Field(type=T.DefiningName)

    env_spec = EnvSpec(add_to_env_kv(Entity.name_symbol, Self))

    defining_names = Property(Entity.name.singleton)

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()],
                      memoized=True, public=True)
    def base_subtype():
        """
        If this type decl is a subtype decl, return the base subtype. If not,
        return ``Self``.
        """
        return Entity.match(
            lambda db=T.DiscreteBaseSubtypeDecl: db,
            lambda st=T.BaseSubtypeDecl: st.get_type.base_subtype,
            lambda _: Entity
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity, memoized=True)
    def anonymous_access_type():
        return T.SynthAnonymousTypeDecl.new(
            name=Self.name,
            discriminants=No(T.DiscriminantPart),
            type_def=T.AnonymousTypeAccessDef.new(
                has_not_null=T.NotNullAbsent.new(),
                type_decl=Self
            ),
            aspects=No(T.AspectSpec)
        ).cast(T.BaseTypeDecl).as_entity

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def anonymous_access_type_or_null():
        return Entity._.anonymous_access_type

    @lazy_field(return_type=T.TypeAttributesRepository,
                ignore_warn_on_node=True)
    def attributes_repo():
        return TypeAttributesRepository.new(base_type=Self)

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

    @langkit_property(return_type=Bool)
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

    @langkit_property(dynamic_vars=[origin], return_type=Bool)
    def is_array_or_rec():
        return Entity.is_array | Entity.is_record_type

    @langkit_property(public=True, return_type=Bool)
    def is_inherited_primitive(p=T.BasicDecl.entity):
        """
        Assuming that P is a primitive of Self, return whether the given
        primitive P is inherited from one of Self's parents.
        """
        return Entity.node != p.info.md.primitive

    @langkit_property(return_type=T.RecordRepClause.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_record_representation_clause():
        """
        Return the record representation clause associated to this type decl,
        if applicable (i.e. this type decl defines a record type).
        """
        return Entity.declarative_scope._.decls.as_entity.find(
            lambda d: d.cast(T.RecordRepClause).then(
                lambda p: p.name.referenced_decl == Entity
            )
        ).cast(T.RecordRepClause.entity)

    @langkit_property(return_type=T.EnumRepClause.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_enum_representation_clause():
        """
        Return the enum representation clause associated to this type decl,
        if applicable (i.e. this type decl defines an enum type).
        """
        return Entity.declarative_scope._.decls.as_entity.find(
            lambda d: d.cast(T.EnumRepClause).then(
                lambda p: p.type_name.referenced_decl == Entity
            )
        ).cast(T.EnumRepClause.entity)

    @langkit_property(memoized=True)
    def primitives_env():
        return EmptyEnv

    @langkit_property(public=True, dynamic_vars=[default_origin()])
    def is_record_type():
        """
        Return whether this type is a record type.

        .. ATTENTION:: Private tagged types extending public tagged records are
            not considered as record types.
        """
        return Not(Entity.record_def.is_null)

    @langkit_property(public=True, dynamic_vars=[default_origin()])
    def is_array_type():
        """
        Return whether this type is an array type.
        """
        return Entity.is_array

    @langkit_property(public=True, return_type=T.TypeDecl.entity.array,
                      dynamic_vars=[origin, default_imprecise_fallback()])
    def find_derived_types(root=T.AdaNode.entity):
        """
        Find types derived from self in the given ``root`` and its children.
        """
        # TODO: Factor the traversal between this and `find_derived_types`
        return root.children.then(
            lambda c: c.filter(lambda n: Not(n.is_null | (n.node == origin)))
            .mapcat(lambda n: Entity.find_derived_types(n))
        ).concat(root.cast(TypeDecl).then(
            lambda type_decl: type_decl.is_derived_type(Entity).then(
                lambda _: type_decl.singleton
            )
        ))

    is_task_type = Property(False, doc="Whether type is a task type")

    is_real_type = Property(
        False, doc="Whether type is a real type or not.", public=True,
        dynamic_vars=[default_origin()]
    )
    is_float_type = Property(
        False, doc="Whether type is a float type or not.", public=True,
        dynamic_vars=[default_origin()]
    )
    is_fixed_point = Property(
        False, doc="Whether type is a fixed point type or not.", public=True,
        dynamic_vars=[default_origin()]
    )

    is_enum_type = Property(
        False, doc="Whether type is an enum type", public=True,
        dynamic_vars=[default_origin()]
    )

    is_classwide = Property(False)

    is_access_type = Property(
        False, public=True,
        doc="Whether Self is an access type or not",
        dynamic_vars=[(origin, No(T.AdaNode))]
    )

    is_implicit_deref = Property(
        Entity.is_access_type | Not(Entity.get_imp_deref.is_null),
        doc="Whether Self is an implicitly dereferenceable type or not",
        dynamic_vars=[origin]
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
        No(T.Expr.entity),
        doc="If self has an Implicit_Dereference aspect, return its expression"
    )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()])
    def derefed_type():
        """
        If this type defines an Implicit_Dereference aspect, return the
        accessed type, otherwise return Self.
        """
        return If(
            Entity.is_null | Entity.get_imp_deref.is_null,
            Entity,
            Entity.accessed_type,
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()])
    def derefed_base_subtype():
        """
        Return the base subtype of this subtype. If this type defines an
        Implicit_Dereference aspect, return the base subtype of the accessed
        type instead.
        """
        return If(
            Entity.is_null,
            Entity,
            If(
                Not(Entity.get_imp_deref.is_null),
                Entity.accessed_type,
                Entity
            ).base_subtype
        )

    @langkit_property(return_type=Bool)
    def one_non_universal(second=T.BaseTypeDecl.entity):
        return Or(
            Not(Entity.is_null) & Entity.is_not_universal_type,
            Not(second.is_null) & second.is_not_universal_type
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()])
    def non_universal_base_subtype(second=T.BaseTypeDecl.entity):
        first_ok = Var(Not(Entity.is_null) & Entity.is_not_universal_type)
        second_ok = Var(Not(second.is_null) & second.is_not_universal_type)
        return Cond(
            first_ok & second_ok,
            Let(
                lambda
                first_st=Entity.derefed_base_subtype,
                second_st=second.derefed_base_subtype:
                If(
                    second_st.matching_formal_type(first_st),
                    first_st,
                    second_st
                )
            ),

            first_ok,
            Entity.derefed_base_subtype,

            second_ok,
            second.derefed_base_subtype,

            No(BaseTypeDecl.entity)
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()])
    def array_concat_result_type(other=T.BaseTypeDecl.entity):
        """
        Considering that Self is the actual type of the left operand of an
        array concatenation and ``other`` the actual type of its right operand,
        return the type of the result of the array concatenation.
        """
        return Cond(
            Entity.is_null | other.is_null,
            No(BaseTypeDecl.entity),

            other.matching_formal_type(Entity)
            | other.matching_formal_type(Entity.comp_type),
            Entity.derefed_base_subtype,

            Entity.matching_formal_type(other)
            | Entity.matching_formal_type(other.comp_type),
            other.derefed_base_subtype,

            No(BaseTypeDecl.entity)
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()])
    def expected_array_concat_operand_type(operand_type=T.BaseTypeDecl.entity):
        """
        Considering that Self is the result type of an array concatenation and
        ``operand_type`` is the actual type of one of the operands, return the
        expected type for that operand. In other words: if the actual type of
        the operand is a subtype of the component-type of the resulting array,
        return the component type of the array. Otherwise return the array type
        itself.
        """
        return Cond(
            Entity.is_null | operand_type.is_null,
            No(BaseTypeDecl.entity),

            operand_type.matching_formal_type(Entity)
            | operand_type.matching_formal_type(Entity.comp_type),
            operand_type.derefed_base_subtype,

            No(BaseTypeDecl.entity)
        )

    access_def = Property(No(T.AccessDef.entity), dynamic_vars=[origin])

    is_char_type = Property(
        False,
        doc="Whether type is a character type or not",
        dynamic_vars=[default_origin()],
        public=True
    )

    is_non_null_char_type = Property(
        Not(Self.is_null) & Entity.is_char_type,
        dynamic_vars=[default_origin()],
    )

    # TODO: Not clear if the below origin.bind is correct, investigate later
    classwide_type = Property(origin.bind(Self, If(
        Entity.is_tagged_type,
        Self.classwide_type_node.as_entity,
        No(T.ClasswideTypeDecl.entity)
    )))

    scalar_base_type = Property(Self.scalar_base_subtype_node.as_entity)

    @langkit_property(return_type=Bool)
    def is_universal_type():
        """
        Return whether this type is one of the two universal types (universal
        integer or universal real).

        .. note::
            Returns False if Self is null.
        """
        return Not(Entity.is_null) & Or(
            Entity == Self.universal_int_type,
            Entity == Self.universal_real_type
        )

    @langkit_property(return_type=Bool)
    def is_not_universal_type():
        """
        Return whether this type is *not* one of the two universal types
        (universal integer or universal real).

        .. note::
            Returns False if Self is null.
        """
        return Not(Entity.is_null) & Not(Entity.is_universal_type)

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def is_access_type_predicate():
        """
        Predicate to use by logic equation. Return True iff this is an access
        type, but checks first that this type is not null, in which case it
        returns False.
        """
        return Entity._.is_access_type

    @langkit_property(dynamic_vars=[origin], return_type=Int)
    def array_ndims():
        return Literal(0)

    @langkit_property(return_type=DiscreteRange, public=True)
    def discrete_range():
        """
        Return the discrete range for this type decl, if applicable.
        """
        return No(DiscreteRange)

    @langkit_property(return_type=T.Expr.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def static_predicate():
        """
        Return the expression from the Static_Predicate or the Predicate aspect
        defined on this type.
        """
        return Entity.get_aspect('Static_Predicate').value._or(
            Entity.get_aspect('Predicate').value
        )

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_imprecise_fallback(),
                                    default_origin()])
    def satisfies_type_predicates(value=T.BigInt):
        """
        Return true if the given value satisfies all of this type's static
        predicates, including its parent predicates (in case this is a derived
        type) and its base type predicate (if this is a subtype declaration).
        Return true if no type predicates are defined for this type.
        """
        true_val = Var(BigIntLiteral(1))

        satisfies_own_predicate = Var(
            Entity.static_predicate.then(
                lambda pred: true_val == pred.eval_as_int_in_env(
                    Substitution.new(
                        from_decl=Entity,
                        value_type=Entity,
                        to_value=value
                    ).singleton
                ),
                default_val=True
            )
        )

        from_type = Var(Entity.cast(T.BaseSubtypeDecl)._.get_type)
        base_type = Var(Entity.base_type)

        return satisfies_own_predicate & Cond(
            Not(from_type.is_null),
            from_type.satisfies_type_predicates(value),

            Not(base_type.is_null),
            base_type.satisfies_type_predicates(value),

            True
        )

    @langkit_property(dynamic_vars=[origin], memoized=True)
    def is_iterator_type():
        iifcs = Var(Entity.get_unit_root_decl(
            ['Ada', 'Iterator_Interfaces'], UnitSpecification
        ))
        typ = Var(Entity.cast(T.ClasswideTypeDecl).then(
            lambda cw: cw.typedecl, default_val=Entity)
        )
        return Or(
            typ.semantic_parent.semantic_parent.node == iifcs,
            Entity.canonical_part.has_aspect("Iterable")
        )

    @langkit_property(dynamic_vars=[default_origin()], public=True)
    def is_discrete_type():
        """
        Whether type is a discrete type or not.
        """
        return Entity.is_int_type | Entity.is_enum_type | Entity.is_char_type

    @langkit_property(return_type=Bool)
    def is_not_root_int_type():
        return Not(Self.is_null) & (Entity != Self.root_int_type)

    @langkit_property(dynamic_vars=[default_origin()], public=True)
    def is_int_type():
        """Whether type is an integer type or not."""
        return False

    @langkit_property(dynamic_vars=[default_origin()])
    def allows_universal_int():
        """
        Whether a universal integer can be used to initialize a value of this
        type. This is true for integer types in general, but also for arbitrary
        types that define the Integer_Literal aspect.
        """
        return Entity.is_int_type | Entity.base_subtype.then(
            # We check on the base_subtype because the aspect can only be
            # specified on the type's first subtype.
            lambda bt: Not(bt.get_aspect_spec_expr("Integer_Literal").is_null),
            default_val=False
        )

    @langkit_property(dynamic_vars=[default_origin()])
    def allows_universal_real():
        """
        Whether a universal real can be used to initialize a value of this
        type. This is true for real types in general, but also for arbitrary
        types that define the Real_Literal aspect.
        """
        return Entity.is_real_type | Entity.base_subtype.then(
            # We check on the base_subtype because the aspect can only be
            # specified on the type's first subtype.
            lambda bt: Not(bt.get_aspect_spec_expr("Real_Literal").is_null),
            default_val=False
        )

    @langkit_property(dynamic_vars=[default_origin()])
    def allows_string_literal():
        """
        Whether a string literal can be used to initialize a value of this
        type. This is true for string types in general, but also for arbitrary
        types that define the String_Literal aspect.

        .. note:: We also check that the node is not null because this property
           is used directly as a logic predicate and may be invoked with null
           nodes (unlike the other ``allows_*`` properties).
        """
        return And(
            Not(Self.is_null),
            Entity.is_str_type | Entity.base_subtype.then(
                # We check on the base_subtype because the aspect can only be
                # specified on the type's first subtype.
                lambda bt:
                Not(bt.get_aspect_spec_expr("String_Literal").is_null),
                default_val=False
            )
        )

    @langkit_property(dynamic_vars=[origin])
    def is_str_type():
        """
        Whether this is a string type (a one dimensional array of characters).
        """
        return (Entity.array_ndims == 1) & Entity.comp_type._.is_char_type

    @langkit_property(dynamic_vars=[default_origin()], public=True)
    def accessed_type():
        """
        If this type is an access type, or a type with an Implicit_Dereference
        aspect, return the type of a dereference of an instance of this type.
        """
        return No(T.BaseTypeDecl.entity)

    @langkit_property(dynamic_vars=[origin])
    def accessed_type_no_call():
        """
        Like ``BaseTypeDecl.accessed_type``, but does not perform an implicit
        call if Self represents an access-to-subprogram.
        """
        return If(
            Entity.is_null | Entity.access_def.is_a(AccessToSubpDef),
            No(BaseTypeDecl.entity),
            Entity.accessed_type
        )

    @langkit_property(dynamic_vars=[origin], return_type=T.BaseTypeDecl.entity)
    def final_accessed_type(first_call=(Bool, True)):
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
            lambda sa: entity.subp_spec_or_null.then(
                lambda se: sa.subp_spec.match_signature(se, False)
            )
        )

    @langkit_property(return_type=Bool)
    def is_generic_formal():
        """
        Return whether this type declaration is a generic formal.
        """
        return Or(
            Self.parent.is_a(GenericFormalTypeDecl),
            Self.parent.cast(BaseTypeDecl)._.is_generic_formal
        )

    is_tagged_type = Property(
        False, doc="Whether type is tagged or not", public=True,
        dynamic_vars=[default_origin()]
    )

    @langkit_property(dynamic_vars=[default_origin()])
    def is_tagged_type_with_deref():
        """
        Return whether Self is a tagged type after being implicitly
        dereferenced.
        """
        return If(
            Not(Entity.get_imp_deref.is_null),
            Entity.accessed_type,
            Entity
        ).is_tagged_type

    base_type = Property(
        No(T.BaseTypeDecl.entity), doc="""
        Return the base type entity for this derived type declaration
        """, public=True, dynamic_vars=[default_origin()]
    )

    base_types = Property(
        Entity.base_type.then(lambda bt: bt.singleton)
        .concat(Entity.base_interfaces),
        public=True, dynamic_vars=[default_origin()],
        doc="Return the list of base types for Self."

    )

    base_interfaces = Property(
        No(T.BaseTypeDecl.entity.array), dynamic_vars=[origin]
    )

    @langkit_property(public=True, return_type=T.TypeDecl.entity.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def find_all_derived_types(units=T.AnalysisUnit.array):
        """
        Return the list of all types that inherit (directly or inderictly) from
        Self among the given units.
        """
        return origin.bind(
            Self,
            Entity.canonical_type.filter_is_imported_by(units, True)
            .mapcat(lambda u: u.root.then(
                lambda r: Entity.find_derived_types(r.as_bare_entity)
            )),
        )

    record_def = Property(No(T.BaseRecordDef.entity), dynamic_vars=[origin])
    array_def = Property(No(T.ArrayTypeDef.entity), dynamic_vars=[origin])

    @langkit_property(dynamic_vars=[origin])
    def array_def_with_deref():
        """
        Return the array definition corresponding to type ``Self`` in the
        context of array-indexing, e.g. implicitly dereferencing if ``Self`` is
        an access.
        """
        return Cond(
            Entity.is_array, Entity.array_def,

            Entity.is_implicit_deref,
            Entity.accessed_type.then(lambda c: c.array_def),

            No(T.ArrayTypeDef.entity)
        )

    is_array_def_with_deref = Property(
        Not(Self.is_null) & Not(Entity.array_def_with_deref.is_null),
        dynamic_vars=[origin]
    )

    is_array_def_with_deref_or_null = Property(
        Self.is_null | Not(Entity.array_def_with_deref.is_null),
        dynamic_vars=[origin]
    )

    @langkit_property(dynamic_vars=[default_origin()],
                      return_type=T.BaseTypeDecl.entity, public=True)
    def comp_type(is_subscript=(Bool, False)):
        """
        Return the component type of ``Self``, if applicable. The component
        type is the type you'll get if you call a value whose type is ``Self``.
        So it can either be:

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

    @langkit_property(dynamic_vars=[default_origin()], public=True)
    def index_type(dim=Int):
        """
        Return the index type for dimension ``dim`` for this type, if
        applicable.

        .. WARNING:: ``dim`` is 0-based, so the first ``index_type`` is at
            index 0.
        """
        return Entity.array_def_with_deref.then(lambda ad: ad.index_type(dim))

    # A BaseTypeDecl in an expression context corresponds to a type conversion,
    # so its type is itself.
    expr_type = Property(Entity)

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_origin()], public=True)
    def is_derived_type(other_type=T.BaseTypeDecl.entity):
        """
        Whether Self is derived from other_type.
        """
        entity_can = Var(Entity.canonical_type)
        other_can = Var(other_type.canonical_type)
        return Or(
            entity_can == other_can,
            And(Not(entity_can.classwide_type.is_null),
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

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_origin()], public=True)
    def is_interface_type():
        """
        Return True iff this type declaration is an interface definition.
        """
        return Entity.full_view._.match(
            lambda td=TypeDecl: td.type_def.is_a(InterfaceTypeDef),
            lambda sb=SubtypeDecl: sb.get_type.is_interface_type,
            lambda _: False
        )

    @langkit_property(dynamic_vars=[origin])
    def iterable_comp_type():
        return No(T.BaseTypeDecl.entity)

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def matching_prefix_type(container_type=T.BaseTypeDecl.entity):
        """
        Given a dotted expression A.B, where container_type is the container
        type for B, and Self is a potential type for A, returns whether Self is
        a valid type for A in the dotted expression.
        """
        cont_type = Var(container_type)
        return Not(Self.is_null) & Not(container_type.is_null) & Or(
            # Derived type case
            Entity.matching_formal_prim_type(cont_type),

            # Access to derived type case
            Entity.final_accessed_type._.matching_formal_prim_type(cont_type),

            # Dot notation: The prefix can be a value type and the formal an
            # access type to this value type.
            cont_type.accessed_type.then(
                lambda at: Entity.matching_formal_prim_type(at)
            ),
        )

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def matching_access_type(expected_type=T.BaseTypeDecl.entity,
                             for_assignment=Bool):
        """
        Whether self is a matching access type for expected_type.
        """
        actual_type = Var(Entity)
        return expected_type.match(
            lambda atd=T.AnonymousTypeDecl.entity:
            atd.access_def_matches(actual_type, for_assignment),
            lambda _: actual_type.match(
                lambda atd2=T.AnonymousTypeDecl.entity:
                atd2.access_def_matches(expected_type, for_assignment),
                lambda _: False
            )
        )

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def matching_formal_prim_type(formal_type=T.BaseTypeDecl.entity):
        return And(
            Not(formal_type.is_null),
            Not(Self.is_null),
            Entity.matching_formal_type_impl(formal_type, True)
        )

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def matching_formal_type(formal_type=T.BaseTypeDecl.entity):
        return And(
            Not(formal_type.is_null),
            Not(Self.is_null),
            Entity.matching_formal_type_impl(formal_type)
        )

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def matching_formal_type_impl(formal_type=T.BaseTypeDecl.entity,
                                  accept_derived=(Bool, False)):
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
                    lambda formal_access: Or(
                        And(formal_access.is_classwide | accept_derived,
                            actual_access.is_derived_type(formal_access)),
                        And(actual_access.is_classwide,
                            actual_access.is_derived_type(formal_access))
                    )
                )
            ),

            And(Not(actual_type.get_imp_deref.is_null),
                actual_type
                .accessed_type.matching_formal_type(formal_type)),

            actual_type.matching_type(formal_type)
        )

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def matching_assign_type(expected_type=T.BaseTypeDecl.entity):
        actual_type = Var(Entity)
        return Not(Self.is_null) & Not(expected_type.is_null) & Or(
            Entity.matching_type(expected_type),

            And(
                expected_type.is_classwide,
                actual_type.matching_formal_prim_type(expected_type)
            ),

            And(
                Not(actual_type.get_imp_deref.is_null),
                actual_type
                .accessed_type.matching_assign_type(expected_type)
            ),

            And(
                Not(expected_type.get_imp_deref.is_null),
                expected_type
                .accessed_type.matching_assign_type(actual_type)
            ),

            Entity.matching_access_type(expected_type, True)
        )

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_origin()], public=True)
    def matching_type(expected_type=T.BaseTypeDecl.entity):
        """
        Return whether ``self`` matches ``expected_type``.
        """
        actual_type = Var(Entity)
        return And(
            Not(expected_type.is_null),
            Not(actual_type.is_null),
            Or(
                And(actual_type == Self.universal_int_type,
                    expected_type.allows_universal_int),

                And(expected_type == Self.universal_int_type,
                    actual_type.is_int_type),

                And(actual_type == Self.universal_real_type,
                    expected_type.allows_universal_real),

                And(expected_type == Self.universal_real_type,
                    actual_type.is_real_type),

                actual_type.canonical_type == expected_type.canonical_type,

                actual_type.matching_access_type(expected_type, False)
            )
        )

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def matching_allocator_type(allocated_type=T.BaseTypeDecl.entity):
        return And(
            Entity.is_access_type,
            allocated_type.matching_formal_type(Entity.accessed_type)
        )

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()], public=True)
    def canonical_type():
        """
        Return the canonical type declaration for this type declaration. For
        subtypes, it will return the base type declaration.
        """
        return imprecise_fallback.bind(
            False, Entity.canonical_part.cast(T.BaseTypeDecl)
        )

    @langkit_property(memoized=True, memoize_in_populate=True,
                      ignore_warn_on_node=True)
    def classwide_type_node():
        return T.ClasswideTypeDecl.new(name=Self.name)

    @langkit_property(
        memoized=True, memoize_in_populate=True, ignore_warn_on_node=True
    )
    def scalar_base_subtype_node():
        """
        Helper for scalar_base_subtype. Return the interned node for the
        subtype entity.
        """
        return DiscreteBaseSubtypeDecl.new(name=Self.name)

    @langkit_property()
    def scalar_base_subtype():
        """
        Return the base subtype for this type. Note that this is only legal for
        scalar types.
        """
        return Self.scalar_base_subtype_node().as_entity

    @langkit_property(public=True,
                      return_type=T.BaseTypeDecl.entity,
                      memoized=True)
    def previous_part(go_to_incomplete=(Bool, True)):
        """
        Returns the previous part for this type decl.
        """
        return If(
            Self.is_generic_formal,

            # A generic formal type never has a previous part
            No(T.BaseTypeDecl.entity),

            # Otherwise look for the previous part in the immediate enclosing
            # declarative region.
            Self.name.then(
                lambda type_name:

                Entity.semantic_parent.immediate_declarative_region.get(
                    symbol=type_name.name_symbol,
                    from_node=Self,
                    lookup=LK.minimal
                ).then(lambda pp: pp.find(lambda pp: Or(
                    And(Entity.is_in_private_part,
                        pp.cast(T.BaseTypeDecl)._.is_private),
                    And(go_to_incomplete,
                        pp.is_a(T.IncompleteTypeDecl)),
                ))).cast(T.BaseTypeDecl)
            )
        )

    @langkit_property(public=True, return_type=T.BaseTypeDecl.entity,
                      memoized=True)
    def next_part():
        """
        Returns the next part for this type decl.

        .. note:: Since this property returns a ``BaseTypeDecl``, it cannot be
            used to retrieve the next part of ``TaskTypeDecl`` and
            ``ProtectedTypeDecl`` nodes as their next part is actually a
            ``Body``. Use ``BasicDecl.next_part_for_decl`` for those instead.
        """

        return Entity.match(
            lambda itd=T.IncompleteTypeDecl:
            # The next part of a (non-private) incomplete type declaration must
            # either be in the same declarative scope...
            itd.declarative_scope.then(
                lambda s: itd.find_next_part_in(s.as_entity)
            )

            # Or in the particular case of taft-amendment types where the
            # incomplete decl is in the private part of the package spec,
            # the next part can be found in the package's body (RM 3.10.1).
            ._or(Entity.is_in_private_part.then(
                lambda _:
                Entity.declarative_scope.parent
                .cast_or_raise(T.BasePackageDecl).as_entity.body_part.then(
                    lambda p: itd.find_next_part_in(p.decls)
                )
            )),

            lambda _: If(
                Entity.is_private
                & Not(Entity.is_generic_formal),
                Entity.private_completion,
                No(T.BaseTypeDecl.entity)
            )

        )

    @langkit_property(return_type=T.BaseTypeDecl.entity, public=True)
    def full_view():
        """
        Return the full completion of this type.
        """
        return Entity.next_part.then(
            lambda np: np.full_view,
            default_val=Entity
        )

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_origin()], public=True)
    def is_definite_subtype():
        """
        Returns whether this is a definite subtype.

        For convenience, this will return ``False`` for incomplete types, even
        though the correct answer is more akin to "non applicable".
        """
        return Entity.match(
            lambda _=T.IncompleteTypeDecl: False,
            lambda td=T.TypeDecl: td.discriminants.is_null & td.type_def.match(
                lambda dtd=T.DerivedTypeDef:
                Not(dtd.subtype_indication.constraint.is_null)
                | dtd.base_type.is_definite_subtype,

                lambda atd=T.ArrayTypeDef:
                atd.indices.is_a(T.ConstrainedArrayIndices),
                lambda _: True,
            ),
            lambda st=T.SubtypeDecl:
            Not(st.subtype.constraint.is_null)
            | st.get_type.is_definite_subtype,
            lambda _=T.ClasswideTypeDecl: False,
            lambda ttd=T.TaskTypeDecl: ttd.discriminants.is_null,
            lambda ptd=T.ProtectedTypeDecl: ptd.discriminants.is_null,
            lambda _: True
        )

    is_private = Property(
        False,
        doc="""
        Whether node is a private view of corresponding type.
        """,
        public=True,
    )

    @langkit_property(return_type=BaseFormalParamDecl.entity.array,
                      dynamic_vars=[default_origin()],
                      kind=AbstractKind.abstract, public=True)
    def discriminants_list():
        """
        Return the list of all discriminants of this type. If this type has no
        discriminant or only unknown discriminants, an empty list is returned.
        """
        pass

    @langkit_property(return_type=T.BasicDecl.entity, dynamic_vars=[origin],
                      memoized=True)
    def corresponding_char_literal():
        """
        If Self denotes the declaration of a character type (i.e. an enum type
        with character literals) and origin is bound to a character literal,
        return the EnumLiteralDecl that symbolically corresponds to the
        literal, or synthesize one if the enum type is one of the standard
        Character types (we need to synthesize them since we cannot declare
        them all in our standard package implementation because of their
        number).
        """
        root = Var(Entity.root_type)
        enum_type = Var(root.cast(TypeDecl)._.type_def.cast(EnumTypeDef))
        sym = Var(origin.cast(CharLiteral).then(lambda l: l.symbol))

        char_lit = Var(enum_type._.enum_literals.find(
            lambda lit_decl: lit_decl.name.name_is(sym)
        ))

        return If(
            # If we didn't find a char literal and the enum type is one of the
            # standard characters types, synthesize the corresponding character
            # enum literal.
            char_lit.is_null & enum_type.is_std_char_type,

            SyntheticCharEnumLit.new(
                # Ideally, name should take a SyntheticDefiningName, built from
                # the symbol `sym`, but that would mean that name's parent is
                # Self here rather than the SyntheticCharEnumLit we are
                # creating. It matters for the DefiningName.basic_decl property
                # for example. To workaround that issue, we also pass the
                # symbol to the SyntheticCharEnumLit node in order to build the
                # SyntheticDefiningName there, afterwards.
                name=No(T.DefiningName),
                char_symbol=sym,
                enum_type_decl=enum_type.parent.cast(TypeDecl),
            ).as_entity,

            char_lit
        )

    root_type = Property(
        Entity,
        dynamic_vars=[default_origin()],
        doc="""
        Return the type that is at the root of the derivation hierarchy
        (ignoring secondary interfaces derivations for tagged types)
        """,
        public=True,
    )

    @langkit_property()
    def next_part_for_decl():
        return Entity.match(
            # SingleTaskTypeDecl next part is its parent SingleTaskDecl next
            # part.
            lambda sttd=T.SingleTaskTypeDecl:
            sttd.parent_basic_decl.basic_decl_next_part_for_decl,
            lambda ttd=T.TaskTypeDecl: ttd.basic_decl_next_part_for_decl,
            lambda _: Entity.next_part.cast(T.BasicDecl.entity)
        )

    @langkit_property(return_type=Shape.array, dynamic_vars=[default_origin()],
                      public=True)
    def shapes(include_discriminants=(Bool, True)):
        """
        Must be called on a record (sub-)type declaration. Return all the
        possible shapes that a value of this record type can take. For example,
        consider the following record definition:

        .. code::

            type R (A : Integer; B : Integer) is record
                X : Integer;
                case A is
                    when 1 .. 10 =>
                        Y_1 : Integer;
                        case B is
                            when 1 .. 10 =>
                                Z_1 : Integer;
                            when others => null;
                        end case;
                    when 11 .. 20 =>
                        Y_2 : Integer;
                        case B is
                            when 1 .. 10 =>
                                Z_2 : Integer;
                            when others => null;
                        end case;
                    when others => null;
                end case;
            end record;

        For this instance, this property will return the following results:

        .. code::

            [
                [X, Y_1, Z_1],
                [X, Y_1],
                [X, Y_2, Z_2],
                [X, Y_2],
                [X]
            ]

        .. ATTENTION::
            This property is inaccurate when called on a record extension which
            defines components under a certain condition C, and this same
            condition is used to define some components in the parent record:
            in that case, any feasible shape will in practice contain either
            both the components defined under condition C in the child record
            and the parent record, or none of them.

            However, due to the simplified algorithm we use here to compute the
            feasible shapes, we will also return shapes that include the
            components of the child record but not the parent record, and
            conversely.
        """
        rdef = Var(Entity.record_def)
        comps = Var(rdef.components)

        parent_type = Var(comps.type_def.cast(T.DerivedTypeDef)._.base_type)

        parent_record = Var(parent_type._.record_def)

        own_shapes = Var(comps.shapes)

        # include parent shapes only if view on base type is indeed a record
        # (i.e. parent_record is not null).
        all_shapes = Var(parent_record.then(
            lambda _: parent_type.shapes(include_discriminants=False).mapcat(
                lambda parent_shape: own_shapes.map(
                    lambda own_shape: Shape.new(
                        components=parent_shape.components.concat(
                            own_shape.components
                        ),
                        discriminants_values=parent_shape.discriminants_values
                        .concat(
                            own_shape.discriminants_values
                        )
                    )
                )
            ),
            default_val=own_shapes
        ))

        discrs = Var(comps.type_decl.discriminants_list)
        return If(
            include_discriminants,
            all_shapes.map(
                lambda s: Shape.new(
                    components=discrs.concat(s.components),
                    discriminants_values=s.discriminants_values
                ),
            ),
            all_shapes
        )

    @lazy_field(return_type=T.LexicalEnv)
    def dottable_subps_env():
        """
        The environment that contains all subprograms that can be called with
        the dot-notation on values of this type.
        """
        return DynamicLexicalEnv(
            assocs_getter=BaseTypeDecl.dottable_subps,
            transitive_parent=False
        )

    @langkit_property(return_type=T.inner_env_assoc.array, memoized=True)
    def dottable_subps():
        """
        Return the list of all subprograms that can be called with the dot-
        notation on values of this type. We look for them in the public part,
        private part and body part of the package this type is declared in.
        """
        scope = Var(Entity.declarative_scope)
        pkg = Var(
            scope._.parent.cast(PackageBody).then(
                lambda body: imprecise_fallback.bind(
                    False,
                    body.as_entity.previous_part
                ).cast(BasePackageDecl),
                default_val=scope._.parent.cast(BasePackageDecl).as_entity
            )
        )
        return Cond(
            Not(Entity.is_tagged_type),
            No(T.inner_env_assoc.array),

            pkg.is_null,
            No(T.inner_env_assoc.array),

            Array([
                pkg.public_part.cast(DeclarativePart),
                pkg.private_part.cast(DeclarativePart),
                pkg.body_part._.decls
            ]).mapcat(
                lambda dp: dp._.decls.as_array
            ).filtermap(
                lambda decl: Let(
                    lambda bd=decl.cast(BasicDecl): T.inner_env_assoc.new(
                        key=bd.defining_name.name_symbol,
                        value=bd.node,
                        metadata=T.Metadata.new(dottable_subp=True)
                    )
                ),

                lambda decl:
                decl.cast(BasicDecl)
                ._.subp_spec_or_null._.dottable_subp_of == Entity
            )
        )

    @langkit_property(return_type=T.EnvRebindings, memoized=True,
                      dynamic_vars=[origin])
    def find_base_type_rebindings(target=T.BaseTypeDecl):
        """
        Given Self & a target type node, browse the inheritance hierarchy of
        Self until the target is found, and return its associated rebindings.
        For example, consider the following snippet.

        .. code::

            package My_Vectors is new Ada.Containers.Vectors (...);

            type My_Vector is new My_Vectors.Vector;

        Calling this property on ``My_Vector`` with the target type
        ``Ada.Containers.Vectors.Vector`` will return the rebindings
        corresponding to the instantiation in the first line of the snippet
        (package My_Vectors).
        """
        return If(
            Self == target,
            Entity.info.rebindings,
            Entity.base_type.then(
                lambda bt: bt.find_base_type_rebindings(target)
            )
        )

    @langkit_property(return_type=Bool)
    def has_base_type_impl(target=T.BaseTypeDecl):
        """
        Implementation of ``has_base_type``, which assumes that ``target`` has
        been canonicalized.
        """
        return Or(
            Entity.canonical_type.node == target,
            Entity.base_types.any(
                lambda bt: bt.has_base_type_impl(target)
            )
        )

    @langkit_property(return_type=Bool)
    def has_base_type(target=T.BaseTypeDecl):
        """
        Return whether the given type is amongst the bases types (direct or
        indirect) of self.

        .. note:: Unlike ``is_derived_type``, we don't care here about the
            rebindings of ``target``, meaning any instance of ``target`` will
            be accepted.
        """
        return Entity.full_view.has_base_type_impl(
            target.as_bare_entity.canonical_type.node
        )


@synthetic
class ClasswideTypeDecl(BaseTypeDecl):
    """
    Synthetic node (not parsed, generated from a property call). Refers to the
    classwide type for a given tagged type (:rmlink:`3.4.1`).
    """
    # We don't want to add the classwide type to the environment
    env_spec = EnvSpec()

    aspects = NullField()

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
    is_private = Property(Entity.typedecl.is_private)
    is_in_private_part = Property(Entity.typedecl.is_in_private_part)

    @langkit_property()
    def get_aspect_assoc(name=Symbol):
        return Entity.typedecl.get_aspect_assoc(name)

    is_interface_type = Property(Entity.typedecl.is_interface_type)

    discriminants_list = Property(Entity.typedecl.discriminants_list)

    @langkit_property(public=True, return_type=T.BaseTypeDecl.entity,
                      memoized=True)
    def previous_part(go_to_incomplete=(Bool, True)):
        return Entity.typedecl.previous_part(go_to_incomplete).then(
            lambda pp: pp.classwide_type
        )

    canonical_type = Property(Entity.typedecl.canonical_type.then(
        # The canonical type should be classwide whenever it makes sense (e.g.
        # if the canonical type is a tagged record type.) Otherwise return
        # a non-classwide type.
        lambda t: t.classwide_type._or(t),
    ))


class TypeDecl(BaseTypeDecl):
    """
    Type declarations that embed a type definition node. Corresponds to the
    ARM's full type declarations (:rmlink:`3.2.1`).
    """
    discriminants = Field(type=T.DiscriminantPart)
    type_def = Field(type=T.TypeDef)
    aspects = Field(type=T.AspectSpec)

    is_iterable_type = Property(
        # TODO: Need to implement on:
        #
        #   * Spark iterable types (Iterable aspect).
        Or(
            Entity.is_array,
            Not(Entity.get_aspect_spec_expr('Iterator_Element').is_null),
            Not(Entity.get_aspect_spec_expr('Iterable').is_null),
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
        ie = Var(Entity.get_aspect_spec_expr('Iterator_Element'))
        it = Var(Entity.get_aspect_spec_expr('Iterable'))

        return imprecise_fallback.bind(False, Cond(
            Entity.is_array, Entity.comp_type,

            Not(ie.is_null), ie.cast(T.Name).then(
                lambda name: env.bind(name.node_env, name.designated_type_impl)
            ),

            Not(it.is_null),
            it.cast(T.Aggregate).assocs.unpacked_params.find(
                lambda sa: sa.name.name_is('Element')
            ).assoc.expr.cast_or_raise(T.Name)
            .referenced_decl.expr_type,

            Entity.type_def.match(
                lambda dtd=T.DerivedTypeDef:
                dtd.base_type.then(lambda bt: bt.iterable_comp_type),
                lambda _: No(T.BaseTypeDecl.entity)
            ),
        )._or(Entity.previous_part(False)
              .then(lambda pp: pp.iterable_comp_type)))

    @langkit_property()
    def discrete_range():
        return Entity.type_def.discrete_range

    @langkit_property()
    def discriminants_list():
        # TODO: investigate if below origin.bind is valid
        base_type = Var(Entity.base_type)
        self_discs = Var(Entity.discriminants.then(
            lambda d: d.abstract_formal_params)
        )

        return Cond(
            Entity.is_access_type,
            Entity.accessed_type.discriminants_list,

            self_discs.length > 0, self_discs,
            Not(base_type.is_null), Entity.base_type.discriminants_list,
            No(T.BaseFormalParamDecl.entity.array)
        )

    @lazy_field(return_type=LexicalEnv)
    def direct_primitives_env():
        """
        The environment that contains all subprograms that are direct
        primitives of this type, that is, primitives that are not inherited.
        """
        return DynamicLexicalEnv(
            assocs_getter=TypeDecl.direct_primitive_subps,
            transitive_parent=False
        )

    @lazy_field(return_type=T.inner_env_assoc.array)
    def direct_primitive_subps():
        """
        Return the list of all subprograms that are direct primitives of this
        type. We look for them in the public part and private part of the
        package this type is declared in.
        """
        scope = Var(Self.declarative_scope)
        is_derived_tagged = Var(Self.as_bare_entity.is_derived_tagged_type)
        decl_parts = Var(scope._.parent.as_bare_entity._.match(
            lambda pkg_decl=BasePackageDecl:
            # Self is declared in a package scope, we should check all the
            # declarative parts of it.
            pkg_decl.public_part.decls.as_array.concat(
                pkg_decl.private_part._.decls.as_array
            ),
            lambda _:
            # Self is not declared in a package scope: the only way that there
            # can be a primitive of Self here is if Self is a derived tagged
            # type.
            If(
                is_derived_tagged,
                Self.parent.parent.cast(DeclarativePart).then(
                    lambda dp: dp.as_bare_entity.decls.as_array
                ),
                No(AdaNode.entity.array)
            )
        ))
        enum_lits = Var(Self.type_def.cast(EnumTypeDef).then(
            lambda etf: etf.enum_literals.map(
                lambda lit: T.inner_env_assoc.new(
                    key=lit.name.name_symbol,
                    value=lit,
                    metadata=T.Metadata.new(primitive=Self)
                )
            )
        ))
        prim_subps = Var(decl_parts.filter(
            lambda decl: decl.cast(BasicDecl)._.subp_spec_or_null.then(
                lambda spec: spec.candidate_primitive_subp_types.contains(
                    Self.as_bare_entity
                ) & If(
                    # For candidate primitives not declared in a package decl,
                    # we must further check if they are overriding a parent
                    # primitive.
                    # This check is not done in `candidate_type_for_primitive`
                    # because it may cause infinite recursions if the parent
                    # type is declared in the same scope as Self.
                    Not(scope.parent.is_a(BasePackageDecl)),
                    Self.as_bare_entity.parent_primitives_env.get(
                        spec.name.name_symbol
                    ).any(
                        lambda x:
                        x.cast(BasicDecl).subp_spec_or_null.match_signature(
                            spec, match_name=False, use_entity_info=True
                        )
                    ),
                    True
                )
            )
        ).mapcat(
            lambda decl: Let(
                lambda bd=decl.cast(BasicDecl): T.inner_env_assoc.new(
                    key=bd.defining_name.name_symbol,
                    value=bd.node,
                    metadata=T.Metadata.new(primitive=Self)
                ).singleton.concat(If(
                    bd.defining_name.name_is('"="'),
                    T.inner_env_assoc.new(
                        key='"/="',
                        value=bd.node,
                        metadata=T.Metadata.new(primitive=Self)
                    ).singleton,
                    No(T.inner_env_assoc.array)
                ))
            )
        ))

        # Also add this types' predefined operators to the list of primitives
        predefined_ops = Var(Self.predefined_operators.map(
            lambda assoc: T.inner_env_assoc.new(
                key=assoc.key,
                value=assoc.value,
                metadata=T.Metadata.new(primitive=Self)
            )
        ))

        return enum_lits.concat(prim_subps).concat(predefined_ops)

    array_ndims = Property(Entity.type_def.array_ndims)

    is_real_type = Property(Entity.type_def.is_real_type)
    is_float_type = Property(Entity.type_def.is_float_type)
    is_fixed_point = Property(Entity.type_def.is_fixed_point)
    is_int_type = Property(Entity.type_def.is_int_type)
    is_access_type = Property(Self.as_bare_entity.type_def.is_access_type)
    is_static_decl = Property(Self.as_bare_entity.type_def.is_static)

    @langkit_property()
    def accessed_type():
        imp_deref = Var(Entity.get_imp_deref)

        return If(
            imp_deref.is_null,
            Entity.type_def.accessed_type,

            # Here, we need to call defining_env on TypeDef, in order to not
            # recurse for ever (accessed_type is called by defining_env).
            include_ud_indexing.bind(
                False,
                Entity.type_def.defining_env.get_first(
                    imp_deref.cast(T.Name).name_symbol, categories=no_prims
                )
            )

            # We cast to BaseFormalParamDecl. Following Ada's legality rule,
            # you need to implicit deref on a discriminant, but I see no reason
            # to enforce that here.
            .cast_or_raise(T.BaseFormalParamDecl).formal_type.accessed_type
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

    is_derived_tagged_type = Property(
        Entity.type_def.is_tagged_type
        & Entity.type_def.is_a(T.DerivedTypeDef, T.InterfaceTypeDef)
    )

    array_def = Property(Entity.type_def.match(
        lambda atd=T.ArrayTypeDef: atd,
        lambda dtd=T.DerivedTypeDef: dtd.base_type.array_def,
        lambda _: No(T.ArrayTypeDef.entity)
    ))

    root_type = Property(Entity.type_def.match(
        lambda dtd=T.DerivedTypeDef: dtd.base_type.root_type,
        lambda _: Entity
    ))

    @langkit_property()
    def defining_env():
        imp_deref = Var(Entity.get_imp_deref)

        # Evaluating in type env, because the defining environment of a type
        # is always its own.
        self_env = Var(Entity.type_def.defining_env)

        return Cond(
            Not(imp_deref.is_null),
            Array([self_env, Entity.accessed_type.defining_env]).env_group(),

            include_ud_indexing & Entity.has_ud_indexing,
            include_ud_indexing.bind(
                False,
                Entity.constant_indexing_fns
                .concat(Entity.variable_indexing_fns)
                .map(lambda fn: fn.defining_env)
                .concat([self_env]).env_group()
            ),

            self_env,
        )

    @langkit_property(return_type=T.env_assoc.array, memoized=True)
    def predefined_operators():
        """
        Return all the predefined operators for this type, as an array of env
        associations ready to be added to a lexical environment.

        Note that the universal int and universal real types are not real
        type declarations and do not have their own operators
        (:rmlink:`3.4.1` - 7).
        """
        return If(
            Self.any_of(Self.universal_int_type.node,
                        Self.universal_real_type.node),
            No(T.env_assoc.array),
            Self.type_def.predefined_operators
        )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_to_env(Self.predefined_operators),
        add_env(),
        handle_children(),
        reference(
            Self.cast(AdaNode).singleton,
            through=T.TypeDecl.refined_parent_primitives_env,
            kind=RefKind.transitive,
            dest_env=Self.node_env,
            cond=Self.type_def.is_a(T.DerivedTypeDef, T.InterfaceTypeDef),
            category="inherited_primitives"
        )
    )

    record_def = Property(
        Entity.type_def.match(
            lambda r=T.RecordTypeDef: r.record_def,
            # If the derived type is tagged, then return its own record def. If
            # it isn't tagged, return the base type's record def.
            lambda d=T.DerivedTypeDef: If(
                Entity.is_tagged_type,
                d.record_extension,
                d.base_type._.record_def
            ),
            lambda _: No(T.BaseRecordDef.entity)
        )
    )

    xref_entry_point = Property(True)

    @langkit_property(return_type=Equation)
    def xref_equation():
        # TODO: Handle discriminants
        return Entity.type_def.sub_equation

    is_discrete_type = Property(Entity.type_def.is_discrete_type)

    @langkit_property(return_type=LexicalEnv)
    def own_primitives_env(with_rebindings=T.EnvRebindings):
        """
        Return the environment containing the primitives for Self, rebound
        using the given rebindings.
        """
        return Entity.direct_primitives_env.rebind_env(with_rebindings)

    @langkit_property(return_type=LexicalEnv.array)
    def own_primitives_envs(with_rebindings=T.EnvRebindings):
        """
        Return the environments containing the primitives for Self and its
        previous parts, if there are some. All returned environments are
        rebound using the given rebindings.
        """
        # If self has a previous part, it might have primitives too
        return Entity.previous_part(False).cast(T.TypeDecl).then(
            lambda pp: Array([
                Entity.own_primitives_env(with_rebindings),
                pp.own_primitives_env(with_rebindings)
            ]),
            default_val=Entity.own_primitives_env(with_rebindings).singleton
        )

    @langkit_property(return_type=LexicalEnv.array)
    def primitives_envs(with_rebindings=T.EnvRebindings,
                        stop_at=BaseTypeDecl.entity.array,
                        include_self=(Bool, False)):
        """
        Return the environments containing the primitives for Self (if
        ``include_self`` is True) and all its base types up to ``stop_at``:
        upon rewinding the base type chain, if we stumble on one of the types
        included in the ``stop_at`` set, we stop the recusion of that branch.
        All returned environments are rebound using the given rebindings.
        """
        # TODO: Not clear if the below origin.bind is correct, investigate
        # later.
        return origin.bind(Self, Entity.base_types.mapcat(lambda t: t.match(
            lambda td=T.TypeDecl: td,
            lambda std=T.BaseSubtypeDecl: origin.bind(
                std.node.origin_node, std.get_type.cast(T.TypeDecl)
            ),
            lambda _: No(T.TypeDecl.entity),
        ).then(
            lambda bt: If(
                stop_at.contains(bt),
                No(LexicalEnv.array),
                bt.own_primitives_envs(with_rebindings)
                .concat(bt.primitives_envs(with_rebindings, stop_at, False)))
            )
        ).concat(
            If(include_self,
               Entity.own_primitives_envs(with_rebindings),
               No(LexicalEnv.array))
        ))

    @langkit_property(memoized=True)
    def compute_primitives_env(
        include_self=(Bool, True),
        stop_at=(BaseTypeDecl.entity.array, No(BaseTypeDecl.entity.array))
    ):
        """
        Return a environment containing all primitives accessible to Self,
        with the adjusted ``primitive_real_type`` metadata field.
        """
        return Entity.primitives_envs(
            with_rebindings=Entity.info.rebindings,
            stop_at=stop_at,
            include_self=include_self
        ).env_group(
            with_md=T.Metadata.new(primitive_real_type=Self)
        )

    @langkit_property()
    def parent_primitives_env():
        return If(
            Self.type_def.is_a(T.DerivedTypeDef, T.InterfaceTypeDef),
            Entity.compute_primitives_env(include_self=False),
            Self.empty_env
        )

    @langkit_property()
    def refined_parent_primitives_env():
        """
        Return a lexical environment containing the primitives inherited by
        this type. This makes sure not to re-include primitives which have
        already been inherited by the previous part of this type, so as to:

         - Not overload lexical envs with useless entries (when one has view
           on this part, it necessarily has view on its previous part).

         - But most importantly, to fix a visibility issue arising when
           resolving a reference to a subprogram overriden in the public part
           of a package if the type has a refined declaration in its private
           part, in which case the inherited subprogram would take precedence
           over the overriden one (see testcase precise_override_2, U817-024).

           This change fixes this issue because, by construction, if the
           overriden subprogram lies in the public part, it means the public
           type declaration already has a view on the inherited subprogram,
           which means we won't include it in the environment computed here
           for the private view.
        """
        return Entity.compute_primitives_env(
            include_self=False,
            stop_at=Entity.previous_part._.base_types
        )

    @langkit_property()
    def primitives_env():
        return Entity.compute_primitives_env(include_self=True)

    @langkit_property(public=True, return_type=T.BasicDecl.entity.array)
    def get_primitives(only_inherited=(Bool, False),
                       include_predefined_operators=(Bool, False)):
        """
        Return the list of all primitive operations that are available on this
        type. If ``only_inherited`` is True, it will only return the primitives
        that are implicitly inherited by this type, discarding those explicitly
        defined on this type. Predefined operators are included in the result
        iff ``include_predefined_operators`` is True. It defaults to False.
        """
        prim_env = Var(If(
            only_inherited,
            Entity.parent_primitives_env,
            Entity.primitives_env
        ))

        all_prims = Var(prim_env.get(symbol=No(T.Symbol)).map(
            lambda t: t.cast(BasicDecl)
        ))

        bds = Var(If(
            include_predefined_operators,
            all_prims,
            all_prims.filter(lambda p: Not(p.is_a(SyntheticSubpDecl)))
        ))

        # Make sure to return only one instance of each primitive: the most
        # "overriding" one.
        return bds.filter(lambda i, a: Let(
            lambda
            a_spec=a.subp_spec_or_null,
            a_prim=a.info.md.primitive.as_bare_entity.cast(BaseTypeDecl):

            bds.all(lambda j, b: Let(
                lambda b_prim=b.info.md.primitive.cast(BaseTypeDecl):
                Or(
                    # Note that we don't want to use `match_name=True` in the
                    # call to `match_signature` below because it also compares
                    # the name of the parameters which we don't want to take
                    # into account here. Therefore, we first compare the names
                    # of the subprogram separately.
                    Not(a.defining_name.node.matches(b.defining_name.node)),

                    # If two primitives have the same signature...
                    Not(a_spec.match_signature(
                        b.subp_spec_or_null,
                        match_name=False, use_entity_info=True
                    )),

                    Let(lambda b_prim_ent=b_prim.as_bare_entity: If(
                        # Test if the type of the first primitive (a) derives
                        # from the type of the second primitive (b)...
                        a_prim.has_base_type(b_prim_ent.node),

                        # Case a derives from b...

                        # If b also derives from a, it means the types are
                        # equal: both primitives are in fact the same
                        # subprogram, but the first one is the declaration and
                        # the second one is the body. In that case we decide to
                        # keep the body.
                        # Else if b does not derive from a, it means the
                        # primitive on a overrides the primitive on b, so
                        # return True.
                        (i >= j) | Not(b_prim_ent.has_base_type(a_prim.node)),

                        # Case a does *not* derive from b...

                        # If b also does not derive from a, the two base types
                        # are unrelated, it means that the primitives are
                        # merged in a single one (remember their signature
                        # match). We keep the one that is inherited first with
                        # respect to the list of parents.
                        # But if b derives from a, we return False as we don't
                        # want to keep this primitive: we will keep the most
                        # inherited one (defined on b) later instead.
                        (i <= j) & Not(b_prim_ent.has_base_type(a_prim.node))
                    ))
                )
            ))
        ))

    get_imp_deref = Property(
        Entity.get_aspect_spec_expr('Implicit_Dereference')
    )

    has_ud_indexing = Property(
        Not(Entity.get_aspect_spec_expr('Constant_Indexing').is_null)
        | Not(Entity.get_aspect_spec_expr('Variable_Indexing').is_null)
    )

    @langkit_property()
    def constant_indexing_fns():
        return (
            Entity.get_aspect_spec_expr('Constant_Indexing')
            ._.cast_or_raise(T.Name).all_env_elements_internal(seq=False)
            .filtermap(
                lambda e: e.cast(T.BasicDecl),
                lambda env_el:
                env_el.cast_or_raise(T.BasicDecl).subp_spec_or_null.then(
                    lambda ss:
                    origin.bind(
                        Self.origin_node,
                        ss.unpacked_formal_params.at(0)
                        ._.formal_decl.formal_type.matching_formal_type(Entity)
                    )
                )
            )
        )

    @langkit_property()
    def variable_indexing_fns():
        return origin.bind(
            Self.origin_node,
            Entity.get_aspect_spec_expr('Variable_Indexing').then(
                lambda a: a.cast_or_raise(T.Name)
                .all_env_elements_internal(seq=False).filtermap(
                    lambda e: e.cast(T.BasicDecl),
                    lambda env_el:
                    env_el.cast_or_raise(T.BasicDecl).subp_spec_or_null.then(
                        lambda ss:
                        ss.unpacked_formal_params.at(0)
                        ._.formal_decl.formal_type.matching_formal_type(Entity)
                        & ss.return_type.is_implicit_deref
                    )
                )
            )
        )


class AnonymousTypeDecl(TypeDecl):
    """
    Anonymous type declaration (for anonymous array or access types). This
    class has no RM existence, and anonymous (sub)types are refered to
    implicitly in the RM.
    """

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def access_def_matches(other=BaseTypeDecl.entity, for_assignment=Bool):
        """
        Returns whether:

        1. Self and other are both access types.
        2. Their access def matches structurally. If for_assignment is True,
           matching_assign_type is used instead of matching_type to compare
           the two access defs.
        """

        self_subp_access = Var(Entity.type_def.cast(AccessToSubpDef))
        other_subp_access = Var(other.access_def.cast(AccessToSubpDef))

        # If the anonymous type is an access type definition, then verify if
        #  the accessed type corresponds to other's accessed type.
        return Cond(
            Not(self_subp_access.is_null) & Not(other_subp_access.is_null),
            other_subp_access.subp_spec.match_signature(
                self_subp_access.subp_spec, False
            ),

            self_subp_access.is_null & other_subp_access.is_null,
            Entity.type_def.cast(AccessDef)._.accessed_type.then(
                lambda ast: other.accessed_type.then(
                    lambda oat: Let(
                        lambda
                        # Forget the classwide view: GNAT always allows
                        # comparison/assignment between access-to-T and
                        # access-to-T'Class.
                        exp=ast.cast(ClasswideTypeDecl)._.typedecl._or(ast),
                        act=oat.cast(ClasswideTypeDecl)._.typedecl._or(oat):
                        If(
                            for_assignment,

                            # Pass the possibly-classwide view of the expected
                            # type here, because matching_assign_type will
                            # handle this case specifically.
                            act.matching_assign_type(ast),

                            act.matching_type(exp)
                        ),
                    )
                )
            ),

            False
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
    """
    Type definition for enumerations (:rmlink:`3.5.1`).
    """
    enum_literals = Field(type=T.EnumLiteralDecl.list)

    is_char_type = Property(Self.enum_literals.any(
        lambda lit: lit.name.name.is_a(T.CharLiteral)
    ))

    is_enum_type = Property(True)

    xref_equation = Property(LogicTrue())

    is_static = Property(True)

    @langkit_property()
    def is_std_char_type():
        self_type = Var(Self.parent.cast(TypeDecl))
        return self_type.any_of(
            Self.std_char_type.node,
            Self.std_wide_char_type.node,
            Self.std_wide_wide_char_type.node
        )

    @langkit_property(memoized=True)
    def predefined_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)

        defaults = Var([
            Self.create_binop_assoc('"<"', self_type, self_type, bool_type),
            Self.create_binop_assoc('"<="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type),
            Self.create_binop_assoc('">"', self_type, self_type, bool_type),
            Self.create_binop_assoc('">="', self_type, self_type, bool_type),
        ])

        # The boolean type has four additional builtin operations
        specials = Var(If(
            self_type == bool_type,
            [Self.create_binop_assoc('"and"', self_type, self_type, self_type),
             Self.create_binop_assoc('"or"', self_type, self_type, self_type),
             Self.create_binop_assoc('"xor"', self_type, self_type, self_type),
             Self.create_unop_assoc('"not"', self_type, self_type)],
            No(T.env_assoc.array)
        ))

        return defaults.concat(specials)


class FloatingPointDef(RealTypeDef):
    """
    Type definition for floating-point numbers (:rmlink:`3.5.7`).
    """
    num_digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)
    is_float_type = Property(True)

    @langkit_property()
    def xref_equation():
        """
        Build an equation for a floating point type definition.
        """
        return And(
            # As per RM 3.5.7, the num_digits expression is expected to be of
            # any integer type.
            Entity.universal_int_bind(Entity.num_digits.expected_type_var),
            Entity.num_digits.sub_equation,
            Entity.num_digits.matches_expected_type,

            # Expressions from the range specification are expected to be of
            # any real type, the types need not be the same.
            Entity.range.then(
                lambda r:
                Entity.universal_real_bind(r.range.expected_type_var)
                & r.range.sub_equation
                & r.range.matches_expected_type,
                default_val=LogicTrue()
            )
        )


class OrdinaryFixedPointDef(RealTypeDef):
    """
    Type definition for ordinary fixed-point numbers (:rmlink:`3.5.9`).
    """
    delta = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)

    is_fixed_point = Property(True)

    @langkit_property()
    def xref_equation():
        """
        Build an equation for an ordinary fixed point type definition.
        """
        return And(
            # As per RM 3.5.9, the delta expression is expected to be of any
            # real type.
            Entity.universal_real_bind(Entity.delta.expected_type_var),
            Entity.delta.sub_equation,
            Entity.delta.matches_expected_type,

            # Expressions from the range specification are expected to be of
            # any real type, the types need not be the same.
            Entity.range.then(
                lambda r:
                Entity.universal_real_bind(r.range.expected_type_var)
                & r.range.sub_equation
                & r.range.matches_expected_type,
                default_val=LogicTrue()
            )
        )

    @langkit_property(memoized=True)
    def predefined_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)
        int_type = Var(Self.int_type.node)

        return [
            Self.create_binop_assoc('"+"', self_type, self_type, self_type),
            Self.create_binop_assoc('"-"', self_type, self_type, self_type),
            Self.create_binop_assoc('"*"', self_type, self_type, self_type),
            Self.create_binop_assoc('"/"', self_type, self_type, self_type),

            Self.create_binop_assoc('"*"', int_type, self_type, self_type),
            Self.create_binop_assoc('"*"', self_type, int_type, self_type),
            Self.create_binop_assoc('"/"', self_type, int_type, self_type),
            Self.create_binop_assoc('"**"', self_type, int_type, self_type),

            Self.create_binop_assoc('"<"', self_type, self_type, bool_type),
            Self.create_binop_assoc('"<="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type),
            Self.create_binop_assoc('">"', self_type, self_type, bool_type),
            Self.create_binop_assoc('">="', self_type, self_type, bool_type),

            Self.create_unop_assoc('"+"', self_type, self_type),
            Self.create_unop_assoc('"-"', self_type, self_type),
            Self.create_unop_assoc('"abs"', self_type, self_type),
        ]


class DecimalFixedPointDef(RealTypeDef):
    """
    Type definition for decimal fixed-point numbers (:rmlink:`3.5.9`).
    """
    delta = Field(type=T.Expr)
    digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)

    is_fixed_point = Property(True)

    @langkit_property()
    def xref_equation():
        """
        Build an equation for a decimal fixed point type definition.
        """
        return And(
            # As per RM 3.5.9, the delta expression is expected to be of any
            # real type.
            Entity.universal_real_bind(Entity.delta.expected_type_var),
            Entity.delta.sub_equation,
            Entity.delta.matches_expected_type,

            # The digits expression is expected to be of any integer type
            Entity.universal_int_bind(Entity.digits.expected_type_var),
            Entity.digits.sub_equation,
            Entity.digits.matches_expected_type,

            # Expressions from the range specification are expected to be of
            # any real type, the types need not be the same.
            Entity.range.then(
                lambda r:
                Entity.universal_real_bind(r.range.expected_type_var)
                & r.range.sub_equation
                & r.range.matches_expected_type,
                default_val=LogicTrue()
            )
        )

    @langkit_property(memoized=True)
    def predefined_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)
        int_type = Var(Self.int_type.node)

        return [
            Self.create_binop_assoc('"+"', self_type, self_type, self_type),
            Self.create_binop_assoc('"-"', self_type, self_type, self_type),
            Self.create_binop_assoc('"*"', self_type, self_type, self_type),
            Self.create_binop_assoc('"/"', self_type, self_type, self_type),

            Self.create_binop_assoc('"*"', int_type, self_type, self_type),
            Self.create_binop_assoc('"*"', self_type, int_type, self_type),
            Self.create_binop_assoc('"/"', self_type, int_type, self_type),
            Self.create_binop_assoc('"**"', self_type, int_type, self_type),

            Self.create_binop_assoc('"<"', self_type, self_type, bool_type),
            Self.create_binop_assoc('"<="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type),
            Self.create_binop_assoc('">"', self_type, self_type, bool_type),
            Self.create_binop_assoc('">="', self_type, self_type, bool_type),

            Self.create_unop_assoc('"+"', self_type, self_type),
            Self.create_unop_assoc('"-"', self_type, self_type),
            Self.create_unop_assoc('"abs"', self_type, self_type),
        ]


@abstract
class BaseAssoc(AdaNode):
    """
    Abstract class for a key/value association, where the value is an
    expression.
    """
    assoc_expr = AbstractProperty(
        type=T.Expr.entity, public=True,
        doc="Returns the expression side of this assoc node."
    )


@abstract
class Constraint(AdaNode):
    """
    Base class for type constraints (:rmlink:`3.2.2`).
    """
    subtype = Property(origin.bind(
        Self.origin_node,
        Self.parent.cast_or_raise(T.SubtypeIndication)
        .as_entity.designated_type
    ))

    @langkit_property(dynamic_vars=[default_imprecise_fallback()])
    def is_static():
        return Entity.match(
            lambda rc=RangeConstraint: rc.range.range.is_static_expr,
            lambda cc=CompositeConstraint: If(
                cc.is_index_constraint,
                cc.constraints.all(
                    lambda c: c.cast(CompositeConstraintAssoc).constraint_expr
                    .match(
                        lambda st=SubtypeIndication: st.is_static_subtype,
                        lambda e=Expr: e.is_static_expr,
                        lambda _: False
                    )
                ),
                cc.constraints.all(
                    lambda c: c.expr.is_static_expr
                )
            ),
            lambda dc=DigitsConstraint: dc.range.then(
                lambda range: range.range.is_static_expr,
                default_val=True
            ),
            lambda dc=DeltaConstraint: dc.range.then(
                lambda range: range.range.is_static_expr,
                default_val=True
            )
        )


class RangeConstraint(Constraint):
    """
    Range-based type constraint (:rmlink:`3.5`).
    """
    range = Field(type=T.RangeSpec)

    @langkit_property()
    def xref_equation():
        return And(
            Bind(Self.range.range.expected_type_var,
                 Entity.subtype.base_subtype),
            Entity.range.sub_equation,
            Entity.range.range.matches_expected_type
        )


class DigitsConstraint(Constraint):
    """
    Digits and range type constraint (:rmlink:`3.5.9`).
    """
    digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)

    xref_equation = Property(
        Entity.digits.sub_equation & Entity.range.then(
            lambda range: range.sub_equation,
            default_val=LogicTrue()
        )
    )


class DeltaConstraint(Constraint):
    """
    Delta and range type constraint (:rmlink:`J.3`).
    """
    digits = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)

    xref_equation = Property(
        Entity.digits.sub_equation & Entity.range.then(
            lambda range: range.sub_equation,
            default_val=LogicTrue()
        )
    )


class CompositeConstraint(Constraint):
    """
    Constraint for a composite type (:rmlink:`3.6.1`). Due to ambiguities in
    the Ada grammar, this could be either a list of index constraints, if the
    owning type is an array type, or a list of discriminant constraints, if the
    owning type is a discriminated record type.
    """

    constraints = Field(type=T.AssocList)

    @langkit_property(public=True)
    def is_index_constraint():
        """
        Whether this composite constraint is an index constraint.
        """
        return Or(
            Entity.subtype.is_array_type,
            Entity.subtype.is_access_type
            & Entity.subtype.accessed_type.is_array_type
        )

    @langkit_property(public=True)
    def is_discriminant_constraint():
        """
        Whether this composite constraint is a discriminant constraint.
        """
        return Not(Entity.is_index_constraint)

    @langkit_property()
    def xref_equation():
        typ = Var(Entity.subtype)
        return If(
            Entity.is_index_constraint,
            Entity.constraints.logic_all(lambda i, c: Let(
                lambda ex=c.cast(T.CompositeConstraintAssoc).constraint_expr:
                # If the index constraint is an expression (which means it
                # is either a BinOp (first .. last) or an AttributeRef
                # (X'Range)), we assign to the type of that expression the
                # type of the index which we are constraining, or else it
                # would be resolved without any context and we could get
                # erroneous types in some cases.  Consider for example
                # ``subtype T is List ('A' .. 'B')``: here, 'A' and 'B'
                # could type to e.g. ``Character`` although the index type
                # of ``List`` is for example ``My_Character``. But if we
                # bind the type of ``'A' .. 'B'`` to ``My_Character`` as we
                # now do, the type will be propagated to both 'A' and 'B'
                # and therefore they will get the correct types.

                # Note that it's currently necessary to first assign the
                # expected type to the range before recursively
                # constructing its xref equations, as we have cases (e.g.
                # BinOp) where resolution takes different paths depending
                # on its operands' types (e.g. whether it's a universal
                # type or not).
                ex.cast(T.Expr).then(
                    lambda e:
                    Bind(e.expected_type_var, typ.index_type(i).base_subtype),
                    default_val=LogicTrue()
                ) & ex.sub_equation
            )),

            # Regular discriminant constraint case
            Self.match_formals(
                typ.discriminants_list, Entity.constraints, False
            ).logic_all(
                lambda pm:
                Bind(pm.actual.assoc.expr.expected_type_var,
                     pm.formal.formal_decl.formal_type)
                & pm.actual.assoc.expr.sub_equation
                & pm.actual.assoc.expr.matches_expected_formal_type
                & pm.actual.name.then(
                    lambda name: Bind(name.ref_var, pm.formal.formal_decl),
                    default_val=LogicTrue()
                )
            )
        )


@abstract
@has_abstract_list
class BasicAssoc(AdaNode):
    """
    Association of one or several names to an expression.
    """
    expr = AbstractProperty(type=T.Expr.entity)
    names = AbstractProperty(type=T.AdaNode.array)

    @langkit_property(public=True,
                      return_type=T.DefiningName.entity.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_params():
        """
        Return the list of parameters that this association refers to.
        """
        return (
            Entity.parent.cast_or_raise(T.AssocList).zip_with_params.filtermap(
                lambda m: m.param,
                lambda m: m.actual == Entity.expr
            )
        )


class CompositeConstraintAssoc(BasicAssoc):
    """
    Association of discriminant names to an expression (:rmlink:`3.7.1`).
    """
    ids = Field(type=T.DiscriminantChoiceList)
    constraint_expr = Field(type=T.AdaNode)

    expr = Property(Entity.constraint_expr.cast_or_raise(T.Expr))
    names = Property(Self.ids.map(lambda i: i.cast(T.AdaNode)))


class DerivedTypeDef(TypeDef):
    """
    Type definition for a derived type (:rmlink:`3.4`).
    """
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
        origin.bind(Self.origin_node,
                    Entity.subtype_indication.designated_type)
    )

    base_interfaces = Property(
        Entity.interfaces.map(lambda i: i.name_designated_type)
    )

    is_int_type = Property(Entity.base_type.is_int_type)
    is_access_type = Property(Self.as_bare_entity.base_type.is_access_type)
    is_char_type = Property(Entity.base_type.is_char_type)
    is_float_type = Property(Entity.base_type.is_float_type)
    is_fixed_point = Property(Entity.base_type.is_fixed_point)
    accessed_type = Property(Entity.base_type.accessed_type)
    is_tagged_type = Property(
        Not(Entity.record_extension.is_null) | Entity.has_with_private.as_bool
    )

    is_enum_type = Property(Entity.base_type.is_enum_type)
    is_static = Property(Entity.subtype_indication.is_static_subtype)

    @langkit_property(return_type=Equation)
    def xref_equation():
        # We want to make discriminants accessible, so need to evaluate this in
        # Self's children_env.
        return env.bind(Entity.children_env, (
            Entity.subtype_indication.xref_equation
            & Entity.interfaces.logic_all(lambda ifc: ifc.xref_equation)
        ))

    @langkit_property()
    def discrete_range():
        return Entity.subtype_indication.discrete_range


class PrivateTypeDef(TypeDef):
    """
    Type definition for a private type.

    Libadalang diverges from the ARM here, treating private types like regular
    type declarations that have an embedded type definition. This type
    definition hence corresponds to :rmlink:`7.3`.
    """
    has_abstract = Field(type=Abstract)
    has_tagged = Field(type=Tagged)
    has_limited = Field(type=Limited)

    is_tagged_type = Property(Self.has_tagged.as_bool)

    xref_equation = Property(LogicTrue())

    @langkit_property(memoized=True)
    def predefined_operators():
        return If(
            Self.has_limited.as_bool,
            No(T.env_assoc.array),
            Self.predefined_equality_operators
        )


class SignedIntTypeDef(TypeDef):
    """
    Type definition for a signed integer type (:rmlink:`3.5.4`).
    """
    range = Field(type=T.RangeSpec)
    is_int_type = Property(True)

    xref_equation = Property(Entity.range.xref_equation)

    @langkit_property()
    def discrete_range():
        return Entity.range.range.discrete_range

    @langkit_property(memoized=True)
    def predefined_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)
        int_type = Var(Self.int_type.node)

        defaults = Var([
            Self.create_binop_assoc('"+"', self_type, self_type, self_type),
            Self.create_binop_assoc('"-"', self_type, self_type, self_type),
            Self.create_binop_assoc('"*"', self_type, self_type, self_type),
            Self.create_binop_assoc('"/"', self_type, self_type, self_type),
            Self.create_binop_assoc('"mod"', self_type, self_type, self_type),
            Self.create_binop_assoc('"rem"', self_type, self_type, self_type),

            Self.create_binop_assoc('"**"', self_type, int_type, self_type),

            Self.create_binop_assoc('"<"', self_type, self_type, bool_type),
            Self.create_binop_assoc('"<="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type),
            Self.create_binop_assoc('">"', self_type, self_type, bool_type),
            Self.create_binop_assoc('">="', self_type, self_type, bool_type),

            Self.create_unop_assoc('"+"', self_type, self_type),
            Self.create_unop_assoc('"-"', self_type, self_type),
            Self.create_unop_assoc('"abs"', self_type, self_type),
        ])

        return defaults

    is_static = Property(Entity.range.range.is_static_expr)


class ModIntTypeDef(TypeDef):
    """
    Type definition for a modular integer type (:rmlink:`3.5.4`).
    """
    expr = Field(type=T.Expr)
    is_int_type = Property(True)

    xref_equation = Property(Entity.expr.sub_equation)

    is_static = Property(Entity.expr.is_static_expr)

    @langkit_property()
    def discrete_range():
        return DiscreteRange.new(low_bound=No(T.Expr.entity),
                                 high_bound=Entity.expr)

    @langkit_property(memoized=True)
    def predefined_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)
        int_type = Var(Self.int_type.node)

        return [
            Self.create_binop_assoc('"+"', self_type, self_type, self_type),
            Self.create_binop_assoc('"-"', self_type, self_type, self_type),
            Self.create_binop_assoc('"*"', self_type, self_type, self_type),
            Self.create_binop_assoc('"/"', self_type, self_type, self_type),
            Self.create_binop_assoc('"mod"', self_type, self_type, self_type),
            Self.create_binop_assoc('"rem"', self_type, self_type, self_type),
            Self.create_binop_assoc('"and"', self_type, self_type, self_type),
            Self.create_binop_assoc('"or"', self_type, self_type, self_type),

            Self.create_binop_assoc('"**"', self_type, int_type, self_type),

            Self.create_binop_assoc('"<"', self_type, self_type, bool_type),
            Self.create_binop_assoc('"<="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type),
            Self.create_binop_assoc('">"', self_type, self_type, bool_type),
            Self.create_binop_assoc('">="', self_type, self_type, bool_type),

            Self.create_unop_assoc('"+"', self_type, self_type),
            Self.create_unop_assoc('"-"', self_type, self_type),
            Self.create_unop_assoc('"abs"', self_type, self_type),
            Self.create_unop_assoc('"not"', self_type, self_type),
        ]


@abstract
class ArrayIndices(AdaNode):
    """
    Specification for array indexes (:rmlink:`3.6`).
    """
    ndims = AbstractProperty(
        type=Int,
        doc="""Number of dimensions described in this node."""
    )

    @langkit_property(return_type=Equation, dynamic_vars=[origin],
                      kind=AbstractKind.abstract)
    def constrain_index_expr(index_expr=T.Expr.entity, dim=Int):
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
    def index_type(dim=Int):
        pass

    is_static = AbstractProperty(
        type=Bool, dynamic_vars=[default_imprecise_fallback()],
        doc="""Return True iff all index types are static."""
    )


class UnconstrainedArrayIndices(ArrayIndices):
    """
    Unconstrained specification for array indexes (:rmlink:`3.6`).
    """
    types = Field(type=T.UnconstrainedArrayIndex.list)
    ndims = Property(Self.types.length)

    @langkit_property(return_type=Equation)
    def constrain_index_expr(index_expr=T.Expr.entity, dim=Int):
        return And(
            Bind(index_expr.expected_type_var, Entity.index_type(dim)),
            index_expr.matches_expected_type
        )

    @langkit_property()
    def index_type(dim=Int):
        return Entity.types.at(dim)._.designated_type

    @langkit_property()
    def xref_equation():
        return Entity.types.logic_all(
            lambda typ: typ.subtype_indication.sub_equation
        )

    is_static = Property(Entity.types.all(
        lambda t: t.subtype_indication.is_static_subtype
    ))


class ConstrainedArrayIndices(ArrayIndices):
    """
    Constrained specification for array indexes (:rmlink:`3.6`).
    """
    list = Field(type=T.ConstraintList)

    ndims = Property(Self.list.length)

    @langkit_property(return_type=Equation)
    def constrain_index_expr(index_expr=T.Expr.entity, dim=Int):
        return And(
            Bind(index_expr.expected_type_var, Entity.index_type(dim)),
            index_expr.matches_expected_type
        )

    @langkit_property()
    def xref_equation():
        return Entity.list.logic_all(
            lambda index:
            index.sub_equation
            & index.cast(T.Expr).then(
                lambda expr:
                Predicate(BaseTypeDecl.is_not_null, expr.type_var)
                & Predicate(BaseTypeDecl.is_discrete_type, expr.type_var)
                & Predicate(BaseTypeDecl.is_not_root_int_type, expr.type_var),
                default_val=LogicTrue()
            )
        )

    @langkit_property(dynamic_vars=[origin])
    def index_type(dim=Int):
        # We might need to solve self's equation to get the index type
        ignore(Var(Self.parents.find(
            lambda p: p.xref_entry_point).as_entity.resolve_names
        ))

        return Entity.list.at(dim)._.match(
            lambda st=T.SubtypeIndication: st.designated_type,
            lambda e=T.Expr: e.type_val.cast(T.BaseTypeDecl.entity),
            lambda _: No(T.BaseTypeDecl.entity)
        )

    is_static = Property(Entity.list.all(
        lambda t: t.match(
            lambda st=T.SubtypeIndication: st.is_static_subtype,
            lambda e=T.BinOp: e.left.is_static_expr & e.right.is_static_expr,
            lambda _: False
        )
    ))


class ComponentDef(AdaNode):
    """
    Definition for a component (:rmlink:`3.6`).
    """
    has_aliased = Field(type=Aliased)
    has_constant = Field(type=Constant)
    type_expr = Field(type=T.TypeExpr)

    @langkit_property()
    def xref_equation():
        return Entity.type_expr.sub_equation


class ArrayTypeDef(TypeDef):
    """
    Type definition for an array (:rmlink:`3.6`).
    """
    indices = Field(type=T.ArrayIndices)
    component_type = Field(type=T.ComponentDef)

    @langkit_property(dynamic_vars=[origin])
    def comp_type():
        """Returns the type stored as a component in the array."""
        return Entity.component_type.type_expr.designated_type

    @langkit_property(dynamic_vars=[origin])
    def index_type(dim=Int):
        return Entity.indices.index_type(dim)

    array_ndims = Property(Self.indices.ndims)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.indices.sub_equation,
            Entity.component_type.sub_equation
        )

    @langkit_property(memoized=True)
    def predefined_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)
        comp_type_expr = Var(Self.component_type.type_expr)

        # Note: here, we define the `and`, `or`, `xor` and `not` operators
        # for all array types (even if they don't make sense) because we have
        # no way to know at this stage if the component type is a boolean type
        # or not (e.g. the component type designates a generic formal).
        # This does not seem to cause any problem for now in practice, but in
        # theory it could hide user-defined operators in certain circumstances.
        # TODO: This could be fixed by filtering out invalid operators when
        # resolving names, somewhere the Entity info is available.
        return [
            Self.create_binop_assoc('"<"', self_type, self_type, bool_type),
            Self.create_binop_assoc('"<="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type),
            Self.create_binop_assoc('">"', self_type, self_type, bool_type),
            Self.create_binop_assoc('">="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"and"', self_type, self_type, self_type),
            Self.create_binop_assoc('"or"', self_type, self_type, self_type),
            Self.create_binop_assoc('"xor"', self_type, self_type, self_type),
            Self.create_unop_assoc('"not"', self_type, self_type),

            # The 4 predefined array concatenation operators
            Self.create_binop_assoc_l_r_expr(
                '"&"', comp_type_expr, comp_type_expr, self_type
            ),
            Self.create_binop_assoc_l_expr(
                '"&"', comp_type_expr, self_type, self_type
            ),
            Self.create_binop_assoc_r_expr(
                '"&"', self_type, comp_type_expr, self_type
            ),
            Self.create_binop_assoc(
                '"&"', self_type, self_type, self_type
            ),
        ]

    is_static = Property(Entity.indices.is_static)

    xref_entry_point = Property(True)


class InterfaceKind(AdaNode):
    """
    Kind of interface type.
    """
    enum_node = True
    alternatives = ["limited", "task", "protected", "synchronized"]


class InterfaceTypeDef(TypeDef):
    """
    Type definition for an interface (:rmlink:`3.9.4`).
    """
    interface_kind = Field(type=InterfaceKind)
    interfaces = Field(type=T.ParentList)

    is_tagged_type = Property(True)

    base_interfaces = Property(
        Entity.interfaces.map(lambda i: i.name_designated_type)
    )

    @langkit_property(return_type=Equation)
    def xref_equation():
        return Entity.interfaces.logic_all(lambda ifc: ifc.xref_equation)


@abstract
class BaseSubtypeDecl(BaseTypeDecl):
    """
    Base class for subtype declarations (:rmlink:`3.2.2`).
    """

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def from_type_bound():
        # TODO: This is a hack, to avoid making all of the predicates on types
        # take an origin. But ultimately, for semantic correctness, it will be
        # necessary to remove this, and migrate every property using it to
        # having a dynamic origin parameter.
        return origin.bind(Self.origin_node, Entity.get_type)

    @langkit_property(kind=AbstractKind.abstract,
                      return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()],
                      public=True)
    def get_type():
        """
        Get the type for this subtype.
        """
        pass

    primitives_env = Property(Entity.from_type_bound.primitives_env)

    array_ndims = Property(Entity.get_type.array_ndims)
    defining_env = Property(Entity.get_type.defining_env)

    canonical_type = Property(Entity.get_type.canonical_type)
    record_def = Property(Entity.get_type.record_def)
    accessed_type = Property(Entity.get_type.accessed_type)
    is_int_type = Property(Entity.get_type.is_int_type)
    is_discrete_type = Property(Entity.get_type.is_discrete_type)

    is_real_type = Property(Entity.get_type.is_real_type)
    is_float_type = Property(Entity.get_type.is_float_type)
    is_fixed_point = Property(Entity.get_type.is_fixed_point)
    is_enum_type = Property(Entity.get_type.is_enum_type)
    is_access_type = Property(Entity.get_type.is_access_type)
    access_def = Property(Entity.get_type.access_def)
    is_char_type = Property(Entity.get_type.is_char_type)
    is_tagged_type = Property(Entity.get_type.is_tagged_type)
    base_type = Property(Entity.get_type.base_type)
    base_interfaces = Property(Entity.get_type.base_interfaces)
    base_types = Property(Entity.get_type.base_types)
    array_def = Property(Entity.get_type.array_def)
    is_classwide = Property(Entity.from_type_bound.is_classwide)
    discriminants_list = Property(Entity.get_type.discriminants_list)
    is_iterable_type = Property(Entity.get_type.is_iterable_type)
    iterable_comp_type = Property(Entity.get_type.iterable_comp_type)
    is_record_type = Property(Entity.get_type.is_record_type)
    is_private = Property(Entity.from_type_bound.is_private)
    root_type = Property(Entity.get_type.root_type)

    has_ud_indexing = Property(
        Entity.from_type_bound.has_ud_indexing
    )
    constant_indexing_fns = Property(
        Entity.from_type_bound.constant_indexing_fns
    )
    variable_indexing_fns = Property(
        Entity.from_type_bound.variable_indexing_fns
    )


class SubtypeDecl(BaseSubtypeDecl):
    """
    Subtype declaration (:rmlink:`3.2.2`).
    """
    subtype = Field(type=T.SubtypeIndication)
    aspects = Field(type=T.AspectSpec)

    @langkit_property(return_type=T.BaseTypeDecl.entity, dynamic_vars=[origin])
    def get_type():
        return Entity.subtype.designated_type.match(
            lambda st=T.SubtypeDecl: st.get_type,
            lambda t: t
        )

    @langkit_property()
    def discrete_range():
        return Entity.subtype.discrete_range

    @langkit_property()
    def xref_equation():
        return Entity.subtype.sub_equation

    is_static_decl = Property(Entity.subtype.is_static_subtype)
    xref_entry_point = Property(True)


@synthetic
class DiscreteBaseSubtypeDecl(BaseSubtypeDecl):
    """
    Specific ``BaseSubtypeDecl`` synthetic subclass for the base type of scalar
    types.
    """
    aspects = NullField()

    is_static_decl = Property(
        True  # TODO: If base subtype is from a formal type, then False
    )

    get_type = Property(
        Self.parent.cast_or_raise(T.BaseTypeDecl).as_entity
    )


class TaskDef(AdaNode):
    """
    Type definition for a task type (:rmlink:`9.1`).
    """
    interfaces = Field(type=T.ParentList)
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)
    end_name = Field(type=T.EndName)

    @langkit_property()
    def xref_equation():
        return Entity.interfaces.logic_all(lambda ifc: ifc.xref_equation)


class ProtectedDef(AdaNode):
    """
    Type definition for a protected object (:rmlink:`9.4`).
    """
    public_part = Field(type=T.PublicPart)
    private_part = Field(type=T.PrivatePart)
    end_name = Field(type=T.EndName)


class TaskTypeDecl(BaseTypeDecl):
    """
    Declaration for a task type (:rmlink:`9.1`).
    """
    discriminants = Field(type=T.DiscriminantPart)
    aspects = Field(type=T.AspectSpec)
    definition = Field(type=T.TaskDef)
    is_task_type = Property(True)

    base_interfaces = Property(
        Entity.definition.interfaces.map(lambda i: i.name_designated_type)
    )

    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        return Self.top_level_env_name.then(
            lambda fqn: fqn.to_symbol.singleton
        )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env(names=Self.env_names)
    )

    defining_env = Property(Entity.children_env)

    discriminants_list = Property(Entity.discriminants.abstract_formal_params)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        return Entity.definition.then(
            lambda d: d.xref_equation,
            default_val=LogicTrue()
        )

    @langkit_property()
    def declarative_parts():
        return Entity.definition.then(
            lambda tdef:
            tdef.public_part.cast(DeclarativePart).singleton.concat(
                tdef.private_part.cast(DeclarativePart)._.singleton
            )
        )


class SingleTaskTypeDecl(TaskTypeDecl):
    """
    Type declaration for a single task (:rmlink:`9.1`).
    """
    env_spec = EnvSpec(
        # In this case, we don't want to add this type to the env, because it's
        # the single task that contains this type decl that will be added to
        # the env. So we don't call the inherited env spec.
        add_env()
    )


class ProtectedTypeDecl(BaseTypeDecl):
    """
    Declaration for a protected type (:rmlink:`9.4`).
    """
    discriminants = Field(type=T.DiscriminantPart)
    aspects = Field(type=T.AspectSpec)
    interfaces = Field(type=T.ParentList)
    definition = Field(type=T.ProtectedDef)

    discriminants_list = Property(Entity.discriminants.abstract_formal_params)

    defining_env = Property(Entity.children_env)

    base_interfaces = Property(
        Entity.interfaces.map(lambda i: i.name_designated_type)
    )

    @langkit_property()
    def next_part_for_decl():
        return Entity.basic_decl_next_part_for_decl

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        return Entity.interfaces.logic_all(lambda ifc: ifc.xref_equation)

    @langkit_property()
    def declarative_parts():
        pdef = Var(Entity.definition)
        return pdef.public_part.cast(DeclarativePart).singleton.concat(
            pdef.private_part.cast(DeclarativePart)._.singleton
        )

    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        return Self.top_level_env_name.then(
            lambda fqn: fqn.to_symbol.singleton
        )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env(names=Self.env_names)
    )


@abstract
class AccessDef(TypeDef):
    """
    Base class for access type definitions (:rmlink:`3.10`).
    """
    has_not_null = Field(type=NotNull)

    is_access_type = Property(True)

    @langkit_property(memoized=True)
    def predefined_operators():
        return Self.predefined_equality_operators


class AccessToSubpDef(AccessDef):
    """
    Type definition for accesses to subprograms (:rmlink:`3.10`).
    """
    has_protected = Field(type=Protected)
    subp_spec = Field(type=T.SubpSpec)

    xref_equation = Property(LogicTrue())

    accessed_type = Property(Entity.subp_spec.return_type)

    # We need to add an env to contain the subp_spec's parameters, so that they
    # don't leak in the external scope.
    env_spec = EnvSpec(add_env())


@abstract
class BaseTypeAccessDef(AccessDef):
    """
    Base class for access type definitions (:rmlink:`3.10`).
    """
    pass


class TypeAccessDef(BaseTypeAccessDef):
    """
    Syntactic type definition for accesses.
    """
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
    """
    Type definition for discrete types in generic formals
    (:rmlink:`12.5.2`).
    """
    xref_equation = Property(LogicTrue())

    is_discrete_type = Property(True)

    @langkit_property(memoized=True)
    def predefined_operators():
        self_type = Var(Self.parent.cast(TypeDecl))
        bool_type = Var(Self.bool_type.node)
        return [
            Self.create_binop_assoc('"<"', self_type, self_type, bool_type),
            Self.create_binop_assoc('"<="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"="', self_type, self_type, bool_type),
            Self.create_binop_assoc('"/="', self_type, self_type, bool_type),
            Self.create_binop_assoc('">"', self_type, self_type, bool_type),
            Self.create_binop_assoc('">="', self_type, self_type, bool_type),
        ]


class NullComponentDecl(AdaNode):
    """
    Placeholder for the ``null`` in lists of components (:rmlink:`3.8`).
    """
    pass


class WithClause(AdaNode):
    """
    With clause (:rmlink:`10.1.2`).
    """
    has_limited = Field(type=Limited)
    has_private = Field(type=Private)
    packages = Field(type=T.Name.list)

    @langkit_property(return_type=Equation)
    def child_unit_xref_equation(name=T.Name):
        """
        Given a name that fully qualified a library-level declaration (i.e.
        a name in a with clause), return an xref equation that binds every
        part of the name to its corresponding library-level declarations.
        """
        self_eq = Var(Bind(name.ref_var, Self.withed_unit_helper(name)._.decl))
        return self_eq & name.cast(DottedName).then(
            lambda dn: Entity.child_unit_xref_equation(dn.prefix),
            default_val=LogicTrue()
        )

    xref_entry_point = Property(True)
    xref_equation = Property(
        Self.packages.logic_all(
            lambda p: Entity.child_unit_xref_equation(p)
        )
    )

    env_spec = EnvSpec(
        set_initial_env(named_env('Standard'))
    )


@abstract
class UseClause(AdaNode):
    """
    Base class for use clauses (:rmlink:`10.1.2`).
    """
    xref_entry_point = Property(True)

    @langkit_property(return_type=T.LexicalEnv)
    def used_envs():
        """
        Return the environment grouping all environments that are referred
        to by this use clause.
        """
        return Entity.match(
            lambda upc=T.UsePackageClause: upc.designated_envs.env_group(),
            lambda utc=T.UseTypeClause:
            utc.types.map(lambda n: n.name_designated_type_env).env_group()
        )

    @langkit_property(return_type=T.DesignatedEnv)
    def initial_env():
        """
        Return the initial env for a use clause. Always the standard package
        for top level use clauses.
        """
        return If(
            Self.parent.parent.is_a(CompilationUnit),
            named_env('Standard'),
            current_env()
        )


class UsePackageClause(UseClause):
    """
    Use clause for packages (:rmlink:`8.4`).
    """
    packages = Field(type=T.Name.list)

    env_spec = EnvSpec(
        set_initial_env(Self.initial_env),

        # Run PLE on the children (i.e. the names of USE'd packages) so that we
        # can run name resolution on them in the call to reference() below.
        handle_children(),
        reference(
            Self.packages.map(lambda n: n.cast(AdaNode)),
            T.Name.use_package_name_designated_env,
            cond=Not(Self.parent.parent.is_a(T.CompilationUnit))
        )
    )

    @langkit_property(return_type=LexicalEnv)
    def designated_env(index=T.Int):
        """
        Return the lexical env designated by the index'th package name in this
        use clause.
        """
        pkg = Var(Self.packages.at(index))
        return env.bind(
            Entity.node_env,
            origin.bind(
                pkg.origin_node,
                pkg.as_bare_entity.designated_env
            )
        )

    @langkit_property(return_type=LexicalEnv.array)
    def designated_envs():
        """
        Return the array of designated envs corresponding to each package name.

        It is very important for this property to be memoized, as it is used a
        lot during lexical environment lookups.
        """
        return Self.packages.map(
            lambda i, _: Entity.designated_env(i)
        )

    xref_equation = Property(
        Entity.packages.logic_all(lambda p: p.xref_no_overloading)
    )


class UseTypeClause(UseClause):
    """
    Use clause for types (:rmlink:`8.4`).
    """
    has_all = Field(type=All)
    types = Field(type=T.Name.list)

    env_spec = EnvSpec(
        set_initial_env(Self.initial_env),

        # Run PLE on the children (i.e. the names of USE'd packages) so that we
        # can run name resolution on them in the call to reference() below.
        handle_children(),
        reference(
            Self.types.map(lambda n: n.cast(AdaNode)),
            T.Name.name_designated_type_env,
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

    This node has no ARM correspondence.
    """

    array_ndims = Property(
        origin.bind(Self.origin_node, Entity.designated_type.array_ndims)
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

    @langkit_property(dynamic_vars=[origin, include_ud_indexing])
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
        origin.bind(Self.origin_node, Entity.designated_type),
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
        return origin.bind(origin_node.node.origin_node,
                           Entity.designated_type)

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

    @langkit_property(return_type=BaseTypeDecl.entity, dynamic_vars=[origin],
                      warn_on_unused=False)
    def canonical_type():
        return Entity.designated_type._.canonical_type


@synthetic
class EnumLitSynthTypeExpr(TypeExpr):
    """
    Synthetic node. Represents the type expression for an enum literal.
    """
    designated_type = Property(
        Entity.parent.cast(T.EnumLiteralDecl).enum_type
    )

    custom_id_text = Property(
        # The custom_id_text is the combination of the enum type name and of
        # the enum literal name.
        origin.bind(
            Self,
            Entity.designated_type.canonical_fully_qualified_name
            .concat(String("."))
            .concat(
                Entity.sym_join(
                    Entity.parent.cast(T.EnumLiteralDecl)
                    .defining_name.as_symbol_array,
                    String("")
                )
            )
        )
    )


class AnonymousType(TypeExpr):
    """
    Container for inline anonymous array and access types declarations.
    """
    type_decl = Field(type=T.AnonymousTypeDecl)

    designated_type = Property(Entity.type_decl)
    xref_equation = Property(Entity.type_decl.sub_equation)

    # TODO: This implementation is not satisfying, because the formatting will
    # be the original source formatting, but will do for the moment.
    # Ideally we would compute a properly formatted version of the anonymous
    # type declaration. Using unparsing in order to avoid duplicating logic
    # between parsing/unparsing.
    custom_id_text = Property(Entity.type_decl.text)


class ParamActual(Struct):
    """
    Data structure used by zip_with_params, Name.call_params,
    GenericInstantiation.inst_params, BaseAggregate.aggregate_params,
    SubtypeIndication.subtype_constraints, and EnumRepClause.params
    properties. Associates an expression (the actual) to a formal param
    declaration (the parameter).
    """
    param = UserField(type=T.DefiningName.entity)
    actual = UserField(type=T.Expr.entity)


class SubtypeIndication(TypeExpr):
    """
    Reference to a type by name (:rmlink:`3.2.2`).
    """
    has_not_null = Field(type=NotNull)
    name = Field(type=T.Name)
    constraint = Field(type=T.Constraint)

    # The name for this type has to be evaluated in the context of the
    # SubtypeIndication node itself: we don't want to use whatever lexical
    # environment the caller is using. However we need to inherit the
    # visibility (origin node) of the caller.
    designated_type = Property(
        env.bind(Entity.node_env, Entity.name.designated_type_impl)
    )

    @langkit_property(return_type=T.CompletionItem.array)
    def complete_items():
        """
        Return possible completions for a type indication at this point in the
        file. Completions for a type indication are more likely coming from a
        type declaration. PackageDecls have a medium weigth in order to provide
        completion of fully qualified names.
        """
        return Entity.children_env.get(No(Symbol)).map(
            lambda n: CompletionItem.new(
                decl=n.cast(T.BasicDecl),
                is_dot_call=n.info.md.dottable_subp,
                is_visible=Self.has_with_visibility(n.unit),
                weight=n.match(
                    lambda btd=T.BaseTypeDecl: If(
                        # Do not promote Self as a possible completion for
                        # itself::
                        #
                        #     type My_Type is new M
                        #                          ^ set My_Type's weight to 0
                        And(
                            Entity.parent.is_a(T.DerivedTypeDef),
                            Entity.parent.parent == btd
                        ),
                        0,
                        100,
                    ),
                    lambda _=T.PackageDecl: 50,
                    lambda _: 0
                )
            )
        )

    @langkit_property(public=True, return_type=ParamActual.array)
    def subtype_constraints():
        """
        Returns an array of pairs, associating formal parameters to actual or
        default expressions.
        """
        constraints = Var(
            Entity.constraint._.cast(CompositeConstraint).constraints
        )
        # Build a discriminants list with their default expressions
        discrs = Var(
            Entity.designated_type_decl._.discriminants_list.mapcat(
                lambda d: Let(
                    lambda ds=d.cast(DiscriminantSpec):
                    ds.ids.map(
                        lambda i: ParamActual.new(
                            param=i,
                            actual=ds.default_expr
                        )
                    )
                )
            )
        )

        # Update the constraints expressions if some are provided
        return constraints.then(
            lambda c: discrs.map(
                lambda i, dp: ParamActual.new(
                    param=dp.param,
                    actual=c.actual_for_param_at(dp.param, i, dp.actual)
                )
            ),
            default_val=discrs
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
        return rc.then(
            lambda r: r.range.range.discrete_range,
            # If no additional range constraint is specified, the range is
            # that of the indicated subtype.
            default_val=Entity.designated_type_decl.discrete_range
        )

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_imprecise_fallback()],
                      public=True)
    def is_static_subtype():
        """
        Returns whether Self denotes a static subtype or not.
        """
        return origin.bind(Self.origin_node, Entity.constraint.then(
            lambda c: c.is_static,
            default_val=Entity.designated_type.is_static_decl
        ))

    custom_id_text = Property(origin.bind(
        Self,
        Entity.designated_type.canonical_fully_qualified_name
    ))


class ConstrainedSubtypeIndication(SubtypeIndication):
    """
    Reference to a type with a range constraint.
    """
    pass


class DiscreteSubtypeIndication(SubtypeIndication):
    """
    Reference to a type with a general constraint.
    """
    pass


class Mode(AdaNode):
    """
    Syntactic indicators for passing modes in formals (:rmlink:`6.1`).
    """
    enum_node = True
    alternatives = ["in", "out", "in_out", "default"]

    @langkit_property()
    def is_writable():
        """
        Return whether this mode allows the qualified entity to be written or
        not.
        """
        return Self.is_a(Mode.alt_out, Mode.alt_in_out)


class ParamSpec(BaseFormalParamDecl):
    """
    Specification for a parameter (:rmlink:`6.1`).
    """
    ids = Field(type=T.DefiningName.list)
    has_aliased = Field(type=Aliased)
    mode = Field(type=Mode)
    type_expr = Field(type=T.TypeExpr)
    default_expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    is_mandatory = Property(Self.default_expr.is_null)
    defining_names = Property(Self.ids.map(lambda id: id.as_entity))

    env_spec = EnvSpec(
        add_to_env(Self.env_mappings(Self.ids, Self))
    )

    type_expression = Property(Entity.type_expr)

    @langkit_property(return_type=Bool)
    def is_constant_object():
        return Self.mode.is_a(Mode.alt_in, Mode.alt_default)

    @langkit_property()
    def defining_env():
        return Entity.type_expr.defining_env

    @langkit_property(return_type=Equation)
    def constrain_prefix(prefix=T.Expr):
        # If a dotted name refers to a parameter, it's necessarily because of
        # fully qualified name access, and thus the prefix of the dotted name
        # is the enclosing subprogram and must:
        #  - not have a type.
        #  - not have a called subp spec.
        return And(
            Bind(prefix.type_var, No(BaseTypeDecl.entity)),
            prefix.cast(Name).then(
                lambda name: Bind(name.subp_spec_var,
                                  No(BaseFormalParamHolder.entity)),
                default_val=LogicFalse()
            )
        )

    @langkit_property()
    def xref_equation():
        typ = Var(Entity.expr_type)
        return (
            Entity.type_expr.sub_equation

            & Entity.default_expr.then(
                lambda de:
                Bind(de.expected_type_var, typ)
                & de.sub_equation
                & de.matches_expected_assign_type,
                default_val=LogicTrue()
            )
        )

    xref_entry_point = Property(True)


class AspectSpec(AdaNode):
    """
    List of aspects in a declaration (:rmlink:`13.1.1`).
    """
    aspect_assocs = Field(type=T.AspectAssoc.list)


class Overriding(AdaNode):
    """
    Syntactic indicators for subprogram overriding modes.
    """
    enum_node = True
    alternatives = ["overriding", "not_overriding", "unspecified"]


@abstract
class BasicSubpDecl(BasicDecl):
    """
    Base class for subprogram declarations.
    """

    defining_names = Property(Entity.subp_decl_spec.name.singleton)

    defining_env = Property(Entity.subp_decl_spec.defining_env)

    type_expression = Property(
        Entity.subp_decl_spec.returns, doc="""
        The expr type of a subprogram declaration is the return type of the
        subprogram if the subprogram is a function.
        """
    )

    @langkit_property(dynamic_vars=[default_imprecise_fallback()])
    def get_body_in_env(env=T.LexicalEnv):
        elements = Var(
            env.get(Entity.name_symbol, LK.flat, categories=no_prims)
        )
        precise = Var(elements.find(
            lambda ent:
            # Discard the rebindings of Entity before trying to match
            # against the tentative body, as those do not carry that info.
            ent.node.as_bare_entity.cast(T.Body)
            ._.formal_param_holder_or_null.match_other(
                Entity.subp_decl_spec.node.as_bare_entity, True
            )
        ).cast(T.Body))

        result = Var(If(
            precise.is_null & imprecise_fallback,
            elements.find(lambda e: e.is_a(T.Body)).cast(T.Body),
            precise
        ))

        # If found, reuse the rebindings of the decl on the body
        return result.node.as_entity._.without_md.cast(Body)

    @langkit_property(return_type=T.BasicDecl.entity)
    def next_part_for_decl():
        decl_scope = Var(Entity.declarative_scope)
        parent_decl = Var(decl_scope.as_entity.then(
            lambda ds: ds.semantic_parent.cast(T.BasicDecl)
        ))

        default_next_part = Var(Entity.basic_decl_next_part_for_decl)

        return Cond(
            # If __nextpart is registered in the decl's env, simply return
            # that.
            Not(default_next_part.is_null),
            default_next_part,

            # Self is declared in a private part
            decl_scope.is_a(T.PrivatePart),
            parent_decl.next_part_for_decl.then(
                lambda np: Entity.get_body_in_env(np.children_env)
            ),

            # Self is declared in a public part
            decl_scope.is_a(T.PublicPart),

            # Search in private part
            parent_decl.decl_private_part.then(
                lambda dpp: Entity.get_body_in_env(dpp.children_env),
            )
            # If not found, search in body
            ._or(parent_decl.body_part_for_decl.then(
                lambda np: Entity.get_body_in_env(np.children_env)
            )),

            # No declarative scope: Bail out!
            decl_scope.is_null, No(T.Body.entity),

            # Self is declared in any other declarative scope. Search for decl
            # in it directly.
            Entity.get_body_in_env(decl_scope.children_env)
        )

    @langkit_property()
    def constrain_prefix(prefix=T.Expr):
        return If(
            # If self is a dottable subprogram, then we want to constrain the
            # prefix so that it's type is the type of the first parameter of
            # self.
            Entity.info.md.dottable_subp,
            Bind(prefix.expected_type_var,
                 Entity.subp_decl_spec
                 .unpacked_formal_params.at(0)._.formal_decl.formal_type)
            & prefix.matches_expected_prefix_type,
            LogicTrue()
        )

    @langkit_property()
    def expr_type():
        return Entity.subp_spec_or_null._.return_type

    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        return Self.top_level_env_name.then(
            lambda fqn: fqn.to_symbol.singleton
        )

    subp_decl_spec = AbstractProperty(
        type=T.BaseSubpSpec.entity, public=True,
        doc='Return the specification for this subprogram'
    )

    env_spec = EnvSpec(
        # Call the env hook to parse eventual parent unit
        do(Self.env_hook),

        set_initial_env(
            # TODO: This is wrong (should take into account whether the entity
            # is private or not), but we have no example of cases where this is
            # a problem yet.
            Self.child_decl_initial_env(True)
        ),

        add_to_env(Entity.child_decl_env_assocs),

        add_env(names=Self.env_names),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        handle_children(),

        do(Self.populate_body_unit)
    )


@abstract
class ClassicSubpDecl(BasicSubpDecl):
    """
    This is an intermediate abstract class for subprogram declarations with a
    common structure: overriding indicator, ``SubpSpec``, aspects,
    <other fields>.
    """
    overriding = Field(type=Overriding)
    subp_spec = Field(type=T.SubpSpec)

    subp_decl_spec = Property(Entity.subp_spec)

    @langkit_property(public=True, return_type=T.BaseSubpBody.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def body_part():
        """
        Return the BaseSubpBody corresponding to this node.
        """
        return Entity.body_part_for_decl.cast_or_raise(BaseSubpBody)


class SubpDecl(ClassicSubpDecl):
    """
    Regular subprogram declaration (:rmlink:`6.1`).
    """
    aspects = Field(type=T.AspectSpec)

    is_constant_object = Property(True)


class AbstractSubpDecl(ClassicSubpDecl):
    """
    Declaration for an abstract subprogram (:rmlink:`3.9.3`).
    """
    aspects = Field(type=T.AspectSpec)


class Pragma(AdaNode):
    """
    Class for pragmas (:rmlink:`2.8`). Pragmas are compiler directives,
    that can be language or compiler defined.
    """
    id = Field(type=T.Identifier)
    args = Field(type=T.BaseAssoc.list)

    xref_entry_point = Property(True)

    @langkit_property(public=True)
    def is_ghost_code():
        """
        Return whether this pragma is ghost code or not. See SPARK RM 6.9.
        """
        # We only consider pragmas that can be in lists of statements for the
        # moment.
        return Entity.id.sym.any_of(
            'Assert', 'Assert_And_Cut', 'Assume', 'Loop_Invariant'
        )

    @langkit_property()
    def xref_initial_env():
        """
        Contract pragmas such as ``Precondition`` have full visibility on their
        associated subprogram's formals although they are not in an internal
        scope. We handle that by overriding this ``xref_initial_env`` property
        which will make sure that name resolution uses the env returned by this
        property when constructing xref_equations.
        """
        return If(
            Entity.id.name_symbol.any_of(
                "Pre", "Post", "Pre'Class", "Post'Class",
                "Precondition", "Postcondition",
                "Precondition'Class", "Postcondition'Class",
                "Test_Case", "Contract_Cases"
            ),
            Entity.associated_entities.at(0).children_env,
            Entity.children_env
        )

    @langkit_property()
    def xref_equation():
        return Cond(
            Or(
                Entity.id.name_is('Assert'),
                Entity.id.name_is('Loop_Invariant'),
                Entity.id.name_is('Compile_Time_Warning'),
                Entity.id.name_is('Compile_Time_Error'),
            ),
            Let(
                lambda expr=Entity.args.at(0).assoc_expr:
                Bind(expr.expected_type_var, Self.bool_type)
                & expr.sub_equation
                & expr.matches_expected_formal_prim_type
            ) & Entity.args.at(1)._.assoc_expr.then(
                lambda msg:
                Bind(msg.expected_type_var, Self.std_entity('String'))
                & msg.sub_equation
                & msg.matches_expected_type,
                default_val=LogicTrue()
            ),

            Entity.id.name_is('Unreferenced'),
            Entity.args.logic_all(
                lambda assoc:
                assoc.assoc_expr.cast_or_raise(T.Name).xref_no_overloading
            ),

            Entity.id.name_symbol.any_of(
                'Import', 'Export', 'Interface', 'Convention',
                'Pack', 'Pure', 'Preelaborate', 'Elaborate_Body',
                'Inline', 'Volatile'
            ),
            Entity.associated_entity_names.logic_all(
                lambda n: n.xref_no_overloading
            ),

            Entity.id.name_is('Warnings'),
            Entity.args.at(1)._.assoc_expr.then(
                lambda expr: If(
                    expr.is_a(T.Identifier),

                    expr.sub_equation,

                    Bind(expr.expected_type_var, Self.std_entity("String"))
                    & expr.sub_equation
                    & expr.matches_expected_type
                ),
                default_val=LogicTrue()
            ),

            # Pragmas we want to deliberately not resolve, either because there
            # is nothing to resolve in there, or because we don't know how to
            # resolve them and don't want to spend effort implementing
            # resolution for them (for example, other compilers implementation
            # defined pragmas).
            Entity.id.name_symbol.any_of(
                'Style_Checks', 'Import_Function', 'Import_Procedure'
            ),
            LogicTrue(),

            Entity.id.name_symbol.any_of(
                "Pre", "Post", "Pre'Class", "Post'Class",
                "Precondition", "Postcondition",
                "Precondition'Class", "Postcondition'Class"
            ),
            Entity.args.at(0).assoc_expr.then(
                lambda expr:
                Bind(expr.expected_type_var, Self.bool_type)
                & expr.sub_equation
                & expr.matches_expected_formal_prim_type,
                default_val=LogicFalse()
            ),

            Entity.id.name_is("Test_Case"),
            Entity.args.filter(
                lambda arg: arg.cast(PragmaArgumentAssoc).then(
                    lambda parg: parg.name.name_symbol.any_of(
                        "Requires", "Ensures"
                    )
                )
            ).logic_all(
                lambda arg:
                Bind(arg.assoc_expr.expected_type_var, Self.bool_type)
                & arg.assoc_expr.sub_equation
                & arg.assoc_expr.matches_expected_formal_prim_type
            ),

            Entity.id.name_is("Contract_Cases"),
            Entity.args.at(0).assoc_expr.cast(BaseAggregate).assocs.logic_all(
                lambda assoc:
                assoc.cast(AggregateAssoc).contract_cases_assoc_equation
            ),

            Entity.id.name_is("Debug"),
            If(
                # If we have two arguments, the first one is a conditional
                # expression.
                Entity.args.length == 2,
                Let(
                    lambda expr=Entity.args.at(0).assoc_expr:
                    Bind(expr.expected_type_var, Self.bool_type)
                    & expr.sub_equation
                    & expr.matches_expected_formal_prim_type
                ),
                LogicTrue()
            ) & Let(
                lambda
                proc=Entity.args.at(Entity.args.length - 1)._.assoc_expr:
                Bind(proc.type_var, No(BaseTypeDecl.entity))
                & proc.sub_equation
            ),

            Entity.args.logic_all(
                # In the default case, we try to resolve every associated
                # expression, but we never fail, in order to not generate
                # diagnostics for unknown/implementation defined pragmas.
                lambda a: Or(a.assoc_expr.sub_equation, LogicTrue())
            )
        )

    @langkit_property(return_type=T.Name.entity.array)
    def associated_entity_names():
        return Cond(
            Entity.id.name_symbol.any_of(
                'Import', 'Export', 'Interface', 'Convention'
            ),
            Entity.args.at(1).assoc_expr.cast_or_raise(T.Name).singleton,

            Entity.id.name_is('Inline'),
            Entity.args.map(lambda a: a.assoc_expr.cast(T.Name)),

            Entity.id.name_symbol.any_of(
                'Pack', 'Pure', 'Preelaborate', 'Elaborate_Body',
                'Volatile', 'Volatile_Components', 'Unchecked_Union',
                'Atomic', 'Atomic_Components', 'No_Return', "Discard_Names",
                "Independent", "Independent_Components", "Asynchronous",
                "Interrupt_Handler", "Attach_Handler",
            ),
            Entity.args.at(0)._.assoc_expr.cast(T.Name)._.singleton,

            No(T.Name.entity.array),
        )

    @langkit_property()
    def associated_entities_helper():
        return Entity.associated_entity_names.mapcat(
            # Find the scope in which this pragma lies by fetching the closest
            # lexical scope. We don't use ``declarative_scope`` here, as some
            # decls do not lie in a DeclarativePart, such as ComponentDecls.
            lambda name: Entity.semantic_parent.then(
                # Get entities in it
                lambda parent: parent.children_env.get(
                    name.name_symbol, lookup=LK.flat, categories=no_prims
                )
                # Map to the public view, to work on the instantiation nodes
                # instead of the Generic*Internal nodes.
                .map(
                    lambda node:
                    node.cast(T.BasicDecl).wrap_public_reference
                    .defining_names.find(
                        lambda dn: dn.name_is(name.name_symbol)
                    )
                )
            )

            # Only get entities that are after self in the *same* source
            .filter(lambda ent: And(ent.unit == Self.unit, ent.node < Self))
        )

    @langkit_property(public=True)
    def associated_entities():
        """
        Return an array of ``BasicDecl`` instances associated with this pragma,
        or an empty array if non applicable.
        """
        top_level_decl = Var(Self.parent.parent.cast(T.CompilationUnit).then(
            lambda cu: cu.body.cast_or_raise(T.LibraryItem)
            .item.as_entity.defining_name.singleton,
            default_val=No(DefiningName.entity.array)
        ))

        enclosing_program_unit = Var(Self.parents.find(
            lambda p: p.is_a(T.BasicDecl)
        ).cast(T.BasicDecl).as_entity)

        # TODO: This should be using a ._or, but is waiting on a fix for
        # R903-028.

        # NOTE: The whole reason we have to implement custom resolution for
        # decls associated to a pragma, is because there can be several
        # associated decls, so the regular crossref mechanism is not
        # sufficient, as in the following example::
        #
        #     procedure Foo;
        #     procedure Foo (A : Integer);
        #     pragma Inline (Foo);
        return Entity.associated_entity_names.then(
            lambda names: Let(
                lambda p=Entity.associated_entities_helper._or(top_level_decl):
                If(
                    Not(p.equals(No(T.DefiningName.entity.array))),
                    p,
                    enclosing_program_unit.then(lambda epu: If(
                        And(
                            names.length == 1,
                            names.at(0).referenced_decl() == epu
                        ),
                        epu.defining_name.singleton,
                        No(DefiningName.entity.array)
                    ), default_val=top_level_decl)
                )
            ),
            default_val=If(
                # If no name, either it's a contract pragma...
                Self.is_contract_aspect(Entity.id.name_symbol),

                # in which case they are attached to the closest subprogram
                # above it.
                Entity.declarative_scope.then(
                    lambda decl_scope: decl_scope.decls.filter(
                        lambda decl: decl.is_a(BasicDecl) & (decl < Self)
                    ).then(
                        lambda decls: decls.at(decls.length - 1)
                    ).cast(BasicDecl).as_entity._.singleton
                )._or(
                    # Or else to the closest parent subprogram
                    enclosing_program_unit.singleton
                ),

                # Or else it 's necessarily a program unit pragma
                enclosing_program_unit.singleton
            ).map(lambda bd: bd.defining_name)
        )

    @langkit_property(return_type=T.DesignatedEnv)
    def initial_env():
        """
        Return the initial env name for a pragma clause. We use the
        Standard package for top level use clauses.
        """
        return If(
            Self.parent.parent.is_a(CompilationUnit),
            named_env('Standard'),
            current_env()
        )

    env_spec = EnvSpec(
        set_initial_env(Self.initial_env)
    )


class PragmaArgumentAssoc(BaseAssoc):
    """
    Argument assocation in a pragma.
    """
    name = Field(type=T.Name)
    expr = Field(type=T.Expr)
    assoc_expr = Property(Entity.expr)


@abstract
class AspectClause(AdaNode):
    """
    Base class for aspect clauses.
    """
    xref_entry_point = Property(True)


class EnumRepClause(AspectClause):
    """
    Representation clause for enumeration types (:rmlink:`13.4`).
    """
    type_name = Field(type=T.Name)
    aggregate = Field(type=T.BaseAggregate)

    @langkit_property()
    def xref_equation():
        # TODO: resolve names in ``aggregate``
        return Entity.type_name.xref_no_overloading

    @langkit_property(public=True, return_type=ParamActual.array)
    def params():
        """
        Returns an array of pairs, associating enum literals to representation
        clause actuals.
        """
        # Get the enum literals
        el = Var(Entity.type_name.referenced_decl().cast(T.BaseTypeDecl)
                 .root_type.cast(T.TypeDecl).type_def.cast(T.EnumTypeDef)
                 .enum_literals)
        # Get the representation clause actuals
        ra = Var(Entity.aggregate.assocs)

        return el.map(
            lambda i, l: ParamActual.new(
                param=l.name,
                actual=ra.actual_for_param_at(l.name, i)
            )
        )


class AttributeDefClause(AspectClause):
    """
    Clause for an attribute definition (``for ...'Attribute use ...;``)
    (:rmlink:`13.3`).
    """
    attribute_expr = Field(type=T.Name)
    expr = Field(type=T.Expr)

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

            rel_name.any_of('Put_Image'),
            Entity.expr.cast_or_raise(T.Name).xref_no_overloading(all_els=True)
            & Predicate(
                BasicDecl.is_put_image_subprogram_for_type,
                Entity.expr.cast(T.Name).ref_var,
                attr.prefix.name_designated_type
            ),

            Entity.expr.sub_equation & Cond(
                rel_name == "External_Tag",
                Bind(Self.expr.expected_type_var, Self.std_entity("String")),

                LogicTrue()
            )
        ) & attr.then(
            lambda ar: ar.prefix.sub_equation, default_val=LogicTrue()
        )


class ComponentClause(AdaNode):
    """
    Representation clause for a single component (:rmlink:`13.5.1`).
    """
    id = Field(type=T.Identifier)
    position = Field(type=T.Expr)
    range = Field(type=T.RangeSpec)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        # Find the record representation clause in which the component clause
        # appears.
        rep_clause = Var(Entity.parent.parent.cast_or_raise(T.RecordRepClause))

        # rep_clause.name must refer to a subtype, so it's safe to use
        # designated_env_no_overloading.
        record_env = Var(rep_clause.name.designated_env_no_overloading)

        return And(
            # Resolve `id` in the environment of the original record
            env.bind(record_env, Entity.id.xref_equation),
            Entity.position.sub_equation,
            Entity.range.sub_equation
        )


class RecordRepClause(AspectClause):
    """
    Representation clause for a record type (:rmlink:`13.5.1`).
    """
    name = Field(type=T.Name)
    at_expr = Field(type=T.Expr)
    components = Field(type=T.AdaNode.list)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.name.xref_no_overloading,
            Entity.at_expr.then(
                lambda e: e.sub_equation,
                default_val=LogicTrue()
            )
        )


class AtClause(AspectClause):
    """
    Representation clause (``for .. use at ...;``) (:rmlink:`13.5.1`).
    """
    name = Field(type=T.BaseId)
    expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return Entity.name.sub_equation & Entity.expr.sub_equation


class SingleTaskDecl(BasicDecl):
    """
    Declaration for a single task (:rmlink:`9.1`).
    """
    task_type = Field(type=T.SingleTaskTypeDecl)
    aspects = NullField()

    defining_names = Property(Entity.task_type.defining_names)
    expr_type = Property(Entity.task_type)

    defining_env = Property(Entity.task_type.defining_env)

    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        return Self.top_level_env_name.then(
            lambda fqn: fqn.to_symbol.singleton
        )

    env_spec = EnvSpec(
        add_to_env_kv(Self.name_symbol, Self),
        add_env(names=Self.env_names)
    )


class SingleProtectedDecl(BasicDecl):
    """
    Declaration for a single protected object (:rmlink:`9.4`).
    """
    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    interfaces = Field(type=T.ParentList)
    definition = Field(type=T.ProtectedDef)

    defining_names = Property(Entity.name.singleton)

    defining_env = Property(Entity.children_env)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        return Entity.interfaces.logic_all(lambda ifc: ifc.xref_equation)

    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        return Self.top_level_env_name.then(
            lambda fqn: fqn.to_symbol.singleton
        )

    @langkit_property()
    def declarative_parts():
        pdef = Var(Entity.definition)
        return pdef.public_part.cast(DeclarativePart).singleton.concat(
            pdef.private_part.cast(DeclarativePart)._.singleton
        )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env(names=Self.env_names)
    )


class AspectAssoc(AdaNode):
    """
    Name/expression association in an aspect.
    """
    id = Field(type=T.Name)
    expr = Field(type=T.Expr)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        target = Var(Self.parent.parent.parent)
        return Cond(
            # Iterable aspect
            Entity.id.name_is('Iterable'),
            Entity.expr.cast(T.Aggregate).assocs.unpacked_params.logic_all(
                lambda sa:
                sa.assoc.expr
                .cast_or_raise(T.Name).xref_no_overloading(sequential=False)
            ),

            # Contracts
            Entity.id.name_symbol.any_of(
                'Pre', 'Post', 'Refined_Post', 'Type_Invariant', 'Invariant',
                'Predicate', 'Static_Predicate', 'Dynamic_Predicate'
            ),
            Let(
                lambda expr_equation_env=If(
                    # Ada 2022 allows Pre and Post aspects for
                    # access-to-subprogram types. In such case, visibility
                    # rules change. The TypeDecl environment should be
                    # considered for name resolution.
                    Entity.id.name_symbol.any_of('Pre', 'Post')
                    & target.cast(TypeDecl)._.type_def.is_a(AccessToSubpDef),
                    target.cast(TypeDecl).type_def.children_env,
                    Entity.node_env
                ):
                Bind(Self.expr.expected_type_var, Self.bool_type)
                & env.bind(expr_equation_env,
                           Entity.expr.sub_equation)
                & Self.expr.matches_expected_formal_prim_type
            ),

            Entity.id.name_is('Contract_Cases'),
            Entity.expr.sub_equation,

            # Put_Image aspect
            Entity.id.name_is('Put_Image'),
            Entity.expr.cast_or_raise(T.Name).xref_no_overloading(
                sequential=False, all_els=True
            ) & Predicate(
                BasicDecl.is_put_image_subprogram_for_type,
                Entity.expr.cast(T.Name).ref_var,
                target.cast_or_raise(BaseTypeDecl).as_entity
            ),

            # Global aspect. Depends is always an aggregate, so doesn't need an
            # entry.
            Entity.id.name_is('Global') | Entity.id.name_is('Refined_Global'),
            If(
                Entity.expr.is_a(NullLiteral),
                LogicTrue(),
                Entity.expr.sub_equation
            ),

            # Do not resolve anything inside those aspect, as identifiers act
            # as reserved words. For example, we do not want to resolve `C`
            # in `Convention => C` to the first visible entity named C.
            Entity.id.name_is('Convention'), LogicTrue(),

            Entity.id.name_is('Stable_Properties'),
            Entity.stable_properties_assoc_equation,

            # Default resolution: For the moment we didn't encode specific
            # resolution rules for every aspect, so by default at least try to
            # name resolve the expression.
            Entity.expr.then(lambda e: e.sub_equation, default_val=LogicTrue())
            | LogicTrue()
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def stable_properties_assoc_equation():
        """
        Equation for the case where this is an aspect assoc for a
        Stable_Properties aspect.
        """
        # Get the list of names defined by the aspect
        identifiers = Var(
            Entity.expr.match(
                # AspectAssoc is of the form: (name1, name2, ...)
                lambda a=T.Aggregate: a.assocs.map(lambda i: i.expr),
                # AspectAssoc is of the form: (name)
                lambda pe=T.ParenExpr: pe.expr.singleton,
                lambda _: No(T.Expr.entity).singleton
            ).map(
                lambda e:
                # Ignore the `not` keyword (useless for nameres)
                e.cast(T.UnOp).then(
                    lambda uo: If(uo.op.is_a(Op.alt_not), uo.expr, e),
                    default_val=e
                )
            ).map(
                # Names defined by the assoc can only be `Identifier`s
                lambda e: e.cast_or_raise(T.Identifier)
            )
        )

        return identifiers.logic_all(
            lambda i: Self.env_get(
                env,
                i.sym,
                lookup=LK.recursive,
                from_node=Self.origin_node,
                categories=all_categories
            ).filter(
                # It can only refer to a SubpDecl or an ExprFunction
                lambda f: f.is_a(T.SubpDecl, T.ExprFunction)
                & i.denotes_the_property_function(
                    f.cast(T.BasicDecl).subp_spec_or_null
                )
            ).logic_any(
                lambda f: Bind(i.ref_var, f)
            )
        )

    @langkit_property(return_type=T.String)
    def aspect_name(n=T.Name.entity):
        """
        Return the string representation of the given name, which must be a
        Name that can appear in an aspect association id.
        """
        # TODO: would be cleaner to implement a general "image" function in
        # class Name directly.
        return n.match(
            lambda bid=T.BaseId: bid.sym.image,
            lambda ar=T.AttributeRef: Self.aspect_name(ar.prefix)
                                          .concat(String("'"))
                                          .concat(ar.attribute.sym.image),
            lambda _: PropertyError(
                T.String,
                "aspect_name called on an invalid aspect name"
            ),
        )

    @langkit_property(public=True)
    def is_ghost_code():
        """
        Return whether this aspect is ghost code or not. See SPARK RM 6.9.
        """
        return Entity.id.name_symbol.any_of('Pre', 'Post', 'Contract_Cases')


class NumberDecl(BasicDecl):
    """
    Declaration for a static constant number (:rmlink:`3.3.2`).
    """
    ids = Field(type=T.DefiningName.list)
    expr = Field(type=T.Expr)
    aspects = NullField()

    defining_names = Property(Entity.ids.map(lambda id: id))

    env_spec = EnvSpec(add_to_env(Self.env_mappings(Self.ids, Self)))

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

    is_static_decl = Property(True)

    xref_equation = Property(
        Bind(Self.expr.expected_type_var, No(AdaNode))
        & Entity.expr.sub_equation
    )

    is_constant_object = Property(True)


class ObjectDecl(BasicDecl):
    """
    Base class for Ada object declarations (:rmlink:`3.3.1`). Ada object
    declarations are variables/constants declarations that can be declared in
    any declarative scope.
    """

    ids = Field(type=T.DefiningName.list)
    has_aliased = Field(type=Aliased)
    has_constant = Field(type=Constant)
    mode = Field(type=Mode)
    type_expr = Field(type=T.TypeExpr)
    default_expr = Field(type=T.Expr)
    renaming_clause = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)

    env_spec = EnvSpec(add_to_env(Self.env_mappings(Self.ids, Self)))

    defining_names = Property(Entity.ids.map(lambda id: id))
    defining_env = Property(Entity.type_expr.defining_env)
    type_expression = Property(Entity.type_expr)

    @langkit_property(return_type=Bool)
    def is_constant_object():
        return Or(
            Entity.has_constant.then(lambda c: c.as_bool),

            # A GenericFormalObjDecl is constant if the Mode is `in`
            Entity.mode.then(
                lambda m:
                Self.parent.is_a(T.GenericFormalObjDecl)
                & (m.is_a(Mode.alt_in, Mode.alt_default))
            ),

            # Renaming clause is constant if the renamed object is constant
            Entity.renaming_clause._.renamed_object.is_constant,

            # Constant if the object is protected
            Entity.type_expr.designated_type_decl.is_a(ProtectedTypeDecl)
        )

    @langkit_property(public=True, return_type=Bool)
    def is_static_decl():
        return Or(
            Self.has_constant.as_bool & Entity.default_expr.then(
                lambda expr: expr.is_static_expr
            ),
            Entity.renaming_clause._.renamed_object.is_static_expr
        )

    @langkit_property()
    def xref_equation():
        typ = Var(Entity.expr_type)
        return (
            Entity.type_expr.sub_equation
            & Entity.default_expr.then(
                lambda de:
                Bind(de.expected_type_var, typ)
                & de.sub_equation
                & de.matches_expected_assign_type,
                default_val=LogicTrue()
            )
            & Entity.renaming_clause.then(
                lambda rc:
                Bind(rc.renamed_object.expected_type_var, typ)
                & rc.renamed_object.sub_equation
                & rc.renamed_object.matches_expected_assign_type,
                default_val=LogicTrue()
            )
        )

    @langkit_property()
    def next_part_for_name(sym=T.Symbol):
        return If(
            Entity.is_in_public_part & Self.has_constant.as_bool,
            Self.declarative_scope.parent
            .cast(T.BasePackageDecl).private_part._.children_env
            .get_first(sym, LK.minimal, categories=no_prims)
            .cast(T.BasicDecl),
            No(T.BasicDecl.entity)
        )

    @langkit_property()
    def previous_part_for_name(sym=T.Symbol):
        return If(
            Entity.is_in_private_part & Self.has_constant.as_bool,
            Self.declarative_scope.parent
            .cast(T.BasePackageDecl).public_part.children_env
            .get_first(sym, LK.minimal, categories=no_prims)
            .cast(T.BasicDecl),
            No(T.BasicDecl.entity)
        )

    @langkit_property(return_type=T.BasicDecl.entity, public=True)
    def private_part_decl():
        """
        If this object decl is the constant completion of an object decl in the
        public part, return the object decl from the public part.
        """
        return Entity.next_part_for_name(
            Entity.defining_name_or_raise.name_symbol
        )

    @langkit_property(return_type=T.BasicDecl.entity, public=True)
    def public_part_decl():
        """
        If this object decl is the incomplete declaration of a constant in a
        public part, return its completion in the private part.
        """
        return Entity.previous_part_for_name(
            Entity.defining_name_or_raise.name_symbol
        )

    @langkit_property()
    def next_part_for_decl():
        return Entity.private_part_decl

    @langkit_property()
    def previous_part_for_decl():
        return Entity.public_part_decl

    xref_entry_point = Property(True)


@synthetic
class AnonymousExprDecl(BasicDecl):
    """
    Represents a anonymous declaration that holds an expression.

    This is used to store the results of queries such as ``referenced_decl``
    called on references to object formals from inside a instantiated generic
    in order to return the relevant actual.

    Indeed, ``referenced_decl`` must return a ``BasicDecl``, but actuals of
    generic instantiations are ``Expr``. This wrapper node is therefore a
    way to both satisfy the ``BasicDecl`` interface, and provide to the user
    the expression of the actual through the ``expr`` field.
    """
    expr = Field(
        type=T.Expr,
        doc="Return the expression wrapped by this declaration."
    )

    aspects = NullField()
    defining_names = Property(No(T.DefiningName.entity.array))
    defining_env = Property(Entity.type_expression.defining_env)

    @langkit_property(return_type=T.DefiningName.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()],
                      memoized=True)
    def get_formal():
        """
        Return the generic formal object declaration corresponding to this
        actual.
        """
        assoc = Var(Entity.expr.parent.cast(BasicAssoc))
        return assoc.get_params.at(0)

    @langkit_property()
    def type_expression():
        """
        Internal property that actually retrieves the type expression. Since
        this property requires non-trivial computation and is used during
        name resolution, it's important for the ``get_formal`` to be memoized.
        """
        return Entity.get_formal.basic_decl.type_expression

    @langkit_property(public=True, return_type=Bool)
    def is_static_decl():
        return Entity.expr.is_static_expr


class ExtendedReturnStmtObjectDecl(ObjectDecl):
    """
    Object declaration that is part of an extended return statement
    (:rmlink:`6.5`).
    """
    pass


class NoTypeObjectRenamingDecl(ObjectDecl):
    """
    Object declaration without subtype indication. This node has been
    introduced to cover a special case for ``ObjectDecl``, where
    ``type_expr`` is made optional (AI12-0275), and therefore cannot
    fit in an ``ObjectDecl``.
    """
    @langkit_property()
    def xref_equation():
        return Entity.renaming_clause.renamed_object.sub_equation


class DeclarativePart(AdaNode):
    """
    List of declarations (:rmlink:`3.11`).
    """
    annotations = Annotations(snaps=True)

    decls = Field(type=T.AdaNode.list)

    @langkit_property()
    def use_clauses_envs():
        """
        Returns the envs for all the use clauses declared in this declarative
        part.
        """
        return Entity.decls.children.filtermap(
            lambda u: u.cast(T.UseClause).used_envs,
            lambda c: c.is_a(T.UseClause)
        ).env_group()


class PrivatePart(DeclarativePart):
    """
    List of declarations in a private part.
    """
    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        """
        A private part allows for a named env iff its parent package is a
        library item, in which case it will be ``.__privatepart`` appended to
        that package's top_level_env_name.
        """
        return Self.parent.cast(BasePackageDecl).then(
            lambda pkg: pkg.top_level_env_name.then(
                lambda name: Array([
                    name.concat(String(".__privatepart")).to_symbol
                ])
            )
        )

    @langkit_property(return_type=T.LexicalEnv)
    def immediate_declarative_region():
        return Entity.semantic_parent.immediate_declarative_region

    env_spec = EnvSpec(
        add_to_env_kv('__privatepart', Self),
        add_env(transitive_parent=True, names=Self.env_names)
    )


class PublicPart(DeclarativePart):
    """
    List of declarations in a public part.
    """
    pass


@abstract
class BasePackageDecl(BasicDecl):
    """
    Base class for package declarations. This will be used
    both for non-generic package declarations (via :typeref:`PackageDecl`) and
    for generic ones (via :typeref:`GenericPackageInternal`).
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
        return imprecise_fallback.bind(
            False,
            Entity.body_part_for_decl()
            .cast(T.PackageBody)
        )

    @langkit_property()
    def declarative_parts():
        return Entity.public_part.cast(DeclarativePart).singleton.concat(
            Entity.private_part.cast(DeclarativePart)._.singleton
        )

    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        """
        Return the env names that this package defines. Make sure to include
        the ``.__privatepart`` env name if this package doesn't have a private
        part, as some construct will always try to register themselves in the
        private part and therefore expect it to always be defined.
        """
        fqn = Var(Self.top_level_env_name)
        return fqn.then(
            lambda fqn: If(
                Not(Self.private_part.is_null),
                Array([
                    fqn.to_symbol
                ]),
                Array([
                    fqn.to_symbol,
                    fqn.concat(String(".__privatepart")).to_symbol
                ])
            )
        )


class PackageDecl(BasePackageDecl):
    """
    Non-generic package declarations (:rmlink:`7.1`).
    """
    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(
            # TODO: This is wrong (should take into account whether the entity
            # is private or not), but we have no example of cases where this is
            # a problem yet.
            Self.child_decl_initial_env(True)
        ),

        add_to_env(Entity.child_decl_env_assocs),

        add_env(names=Self.env_names),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
    )


class ExceptionDecl(BasicDecl):
    """
    Exception declarations (:rmlink:`11.1`).
    """
    ids = Field(type=T.DefiningName.list)
    renames = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)
    defining_names = Property(Entity.ids.map(lambda id: id))

    @langkit_property()
    def next_part_for_decl():
        """
        An exception declaration never has a next part.
        """
        return No(BasicDecl.entity)

    env_spec = EnvSpec(add_to_env(Self.env_mappings(Self.ids, Self)))

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        return Entity.renames.then(
            lambda c: c.renamed_object.xref_no_overloading,
            default_val=LogicTrue()
        )


@abstract
class GenericInstantiation(BasicDecl):
    """
    Instantiations of generics (:rmlink:`12.3`).
    """
    @lazy_field(return_type=LexicalEnv)
    def instantiation_env():
        return DynamicLexicalEnv(
            assocs_getter=GenericInstantiation.instantiation_bindings,
            transitive_parent=False,
            assoc_resolver=AdaNode.resolve_generic_actual
        )

    @langkit_property(return_type=T.inner_env_assoc.array)
    def instantiation_bindings():
        # We force the computation of the corresponding generic declaration
        # in this non-memoized property to avoid a false "infinite recursion"
        # property error that can happen when the computation is only done in
        # instantiation_bindings_internal.
        ignore(Var(Self.nonbound_generic_decl))
        return Entity.instantiation_bindings_internal

    @langkit_property(return_type=T.inner_env_assoc.array, memoized=True,
                      memoize_in_populate=True)
    def instantiation_bindings_internal():
        return If(
            Entity.is_any_formal,
            No(T.inner_env_assoc.array),
            env.bind(
                Self.default_initial_env,
                Self.nonbound_generic_decl._.formal_part.match_param_list(
                    Entity.generic_inst_params, False
                ).filter(
                    lambda pm: Not(pm.actual.assoc.expr.is_a(BoxExpr))
                ).map(
                    lambda i, pm: T.inner_env_assoc.new(
                        key=pm.formal.name.name_symbol,
                        value=If(
                            pm.formal.formal_decl.is_a(T.GenericFormalObjDecl),
                            Entity.actual_expr_decls.at(i),
                            pm.actual.assoc.expr.node
                        ),
                        metadata=T.Metadata.new()
                    )
                )
            )
        )

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
        Entity.generic_inst_params.at(0)._.expr.is_a(T.BoxExpr)
    )

    nonbound_generic_decl = Property(
        Self.as_bare_entity.generic_entity_name
        .all_env_elements_internal(
            seq=True, seq_from=Self, categories=no_prims
        ).find(
            lambda e: Self.has_visibility(e)
        )._.match(
            lambda b=Body: imprecise_fallback.bind(False, b.decl_part),
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

    @lazy_field(return_type=T.AnonymousExprDecl.array,
                ignore_warn_on_node=True)
    def actual_expr_decls():
        return Self.match(
            lambda subp=T.GenericSubpInstantiation: subp.params,
            lambda pkg=T.GenericPackageInstantiation: pkg.params
        ).map(
            lambda assoc: AnonymousExprDecl.new(
                expr=assoc.as_bare_entity.expr.node
            )
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
                Entity.generic_inst_params, False
            ).filter(
                lambda pm: Not(pm.actual.assoc.expr.is_a(BoxExpr))
            ).logic_all(lambda pm: Let(
                lambda actual_name=pm.actual.assoc.expr.cast(T.Name):
                pm.formal.formal_decl.cast(T.GenericFormal).decl.match(
                    lambda _=T.TypeDecl: actual_name.xref_no_overloading,

                    lambda subp_decl=T.FormalSubpDecl.entity:
                    If(
                        actual_name.is_a(AttributeRef),
                        actual_name.sub_equation,
                        actual_name.xref_no_overloading(all_els=True)
                        & Predicate(BasicDecl.subp_decl_match_signature,
                                    actual_name.ref_var,
                                    subp_decl.cast(T.BasicDecl))
                        | LogicTrue()
                    ),

                    lambda obj_decl=T.ObjectDecl:
                    Bind(pm.actual.assoc.expr.expected_type_var,
                         obj_decl.expr_type)
                    & pm.actual.assoc.expr.sub_equation
                    & pm.actual.assoc.expr.matches_expected_assign_type,

                    lambda _: LogicTrue(),
                ) & pm.actual.name.then(
                    lambda n:
                    Bind(n.ref_var,
                         pm.formal.node.as_bare_entity.basic_decl),
                    default_val=LogicTrue()
                )
            ))
        )
    )

    @langkit_property(public=True, return_type=ParamActual.array)
    def inst_params():
        """
        Returns an array of pairs, associating formal parameters to actual or
        default expressions.
        """
        ap = Var(Entity.generic_inst_params)

        return Entity.nonbound_generic_decl._.formal_part.decls.mapcat(
            # Unpack generic formals with their default expressions
            lambda d: d.match(
                lambda t=GenericFormalPackage: ParamActual.new(
                    param=t.decl.cast(GenericPackageInstantiation).name,
                    actual=No(Expr.entity)
                ).singleton,
                lambda t=GenericFormalTypeDecl: ParamActual.new(
                    param=t.decl.cast(TypeDecl).name,
                    actual=No(Expr.entity)
                ).singleton,
                lambda o=GenericFormalObjDecl:
                o.decl.cast(ObjectDecl).ids.map(
                    lambda i: ParamActual.new(
                        param=i,
                        actual=o.decl.cast(ObjectDecl).default_expr
                    )
                ),
                lambda sp=GenericFormalSubpDecl: Let(
                    lambda subp=sp.decl.cast(FormalSubpDecl):

                    ParamActual.new(
                        param=sp.defining_name,
                        actual=If(
                            subp.default_expr.is_a(BoxExpr),
                            subp.designated_subprogram_from(
                                inst=Entity
                            )._.defining_name,
                            subp.default_expr
                        )
                    )
                ).singleton,
                lambda _: No(ParamActual).singleton
            )
        ).map(
            # Update actuals from instantiation params
            lambda i, dp: ParamActual.new(
                param=dp.param,
                actual=ap.actual_for_param_at(dp.param, i, dp.actual)
            )
        )


class GenericSubpInstantiation(GenericInstantiation):
    """
    Instantiations of a generic subprogram .
    """

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
    def expr_type():
        return Entity.subp_spec_or_null._.return_type

    defining_env = Property(Entity.subp_spec_or_null._.defining_env)

    @langkit_property(public=True)
    def designated_subp():
        """
        Return the subprogram decl designated by this instantiation.
        """
        return Self.nonbound_generic_decl.then(
            lambda p: BasicSubpDecl.entity.new(
                node=p.node.cast(GenericSubpDecl).subp_decl,
                info=T.entity_info.new(
                    md=p.info.md,
                    rebindings=Entity.info.rebindings
                    # Append the rebindings from the decl
                    .concat_rebindings(p._.decl.info.rebindings)
                    .append_rebinding(
                        p.node.children_env, Self.instantiation_env
                    ),
                    from_rebound=p.info.from_rebound
                )
            )
        )

    designated_generic_decl = Property(
        Entity.designated_subp.parent.cast(T.BasicDecl)
    )

    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(Self.child_decl_initial_env),

        add_to_env_kv(
            key=Entity.name_symbol,
            value=Self
        ),

        add_env(),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        )
    )


class GenericPackageInstantiation(GenericInstantiation):
    """
    Instantiations of a generic package.
    """

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
                node=p.node.cast(GenericPackageDecl).package_decl,
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
                    # p.node.children_env.
                    .append_rebinding(p.node.children_env,
                                      Self.instantiation_env),
                    from_rebound=p.info.from_rebound
                ),
            )
        )

    designated_generic_decl = Property(
        Entity.designated_package.parent.cast(T.BasicDecl)
    )

    @langkit_property(return_type=LexicalEnv, dynamic_vars=[origin])
    def defining_env_impl(inst_from_formal=(Bool, False)):
        """
        Specialized function for getting the defining env for this generic
        instantiation.

        If ``inst_from_formal`` is True, we know that this generic package
        instantiation is coming from a rebound formal package, and that we need
        visibility on the formals.
        """
        dp = Var(Entity.designated_package)
        return If(
            # In some specific scenarios on valid Ada code, dp can be null
            # here (see U630-018). For example, consider the following snippet.
            #
            # .. code::
            #   use U; -- U contains the declaration of G
            #   package A is new G;
            #   use A;
            #
            # Now consider an env lookup attempting to traverse the "use U"
            # clause. When calling the use clause's resolver, we lookup "U" and
            # we end up traversing "use A" (this is the critical part: although
            # the use clause is located after, we still traverse it in order
            # to cache further lookups to "U", no matter their origin). When
            # resolving "use A", we need to lookup G. However, since U is
            # already being visited, our lexical env mechanism preventing
            # infinite recursion returns an empty vector, which in turn makes
            # ``Entity.designated_package`` be null here.
            # Fortunately, returning EmptyEnv in this case is fine, because
            # there would be no way for U to be found through A anyways.
            dp.is_null,
            EmptyEnv,
            Array([
                If(
                    Self.is_formal | inst_from_formal,
                    Array([
                        dp.children_env,
                        dp.parent.children_env
                    ]).env_group(),
                    dp.children_env
                ),
                # The environment of the instantiation needs to be available,
                # because library unit generic package instantiations can be
                # nested, and so need to be available, such as in::
                #
                #     --  a.ads
                #     package A is new Gen_A;
                #
                #     --  a-b.ads
                #     package A.B is new A.Gen_B;
                #
                Entity.children_env
            ]).env_group()
        )

    @langkit_property(return_type=LexicalEnv)
    def defining_env():
        return Entity.defining_env_impl

    @langkit_property(return_type=T.Symbol)
    def initial_env_name():
        return If(
            Self.is_library_item,
            Self.child_decl_initial_env_name,
            No(T.Symbol)
        )

    @langkit_property(return_type=T.Symbol.array)
    def env_names():
        return Self.top_level_env_name.then(
            lambda fqn: fqn.to_symbol.singleton
        )

    defining_names = Property(Entity.name.singleton)

    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(
            named_env(
                Self.initial_env_name,
                or_current=True
            )
        ),

        add_to_env_kv(Entity.name_symbol, Self),

        add_env(names=Self.env_names),

        do(Self.populate_dependent_units),
        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        )
    )


class RenamingClause(AdaNode):
    """
    Renaming clause, used everywhere renamings are valid.
    """
    renamed_object = Field(type=T.Name)


@synthetic
class SyntheticRenamingClause(RenamingClause):
    """
    Synthetic renaming clause. Used to synthesize object decls with renamings.
    (See to_anonymous_object_decl).
    """
    pass


class PackageRenamingDecl(BasicDecl):
    """
    Declaration for a package renaming (:rmlink:`8.5.3`).
    """

    name = Field(type=T.DefiningName)
    renames = Field(type=RenamingClause)
    aspects = Field(type=T.AspectSpec)

    @langkit_property(return_type=T.BasicDecl.entity, public=True)
    def renamed_package():
        """
        Return the declaration of the package that is renamed by Self.
        """
        # Workaround for V714-016. We perform an initial "dummy" env get query
        # to prepare the referenced envs that will be traversed by the next
        # query by allowing `Name.use_package_name_designated_env` to get
        # memoized.
        node_env = Var(Entity.node_env)
        ignore(Var(node_env.get("__dummy", lookup=LK.recursive)))

        # We can then safely perform the actual query which will not trigger
        # the infinite recursion.
        return env.bind(
            node_env,
            Entity.renames.renamed_object.env_elements.at(0)._.cast(BasicDecl)
        )

    @langkit_property(return_type=T.BasicDecl.entity, public=True)
    def final_renamed_package():
        """
        Return the declaration of the package that is ultimately renamed by
        Self, skipping through all intermediate package renamings.
        """
        pkg = Var(Entity.renamed_package)
        return pkg.cast(PackageRenamingDecl).then(
            lambda r: r.final_renamed_package,
            default_val=pkg
        )

    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(Self.child_decl_initial_env),

        add_to_env_kv(
            key=Entity.name_symbol,
            value=Self
        ),

        add_env(),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        )
    )

    defining_names = Property(Entity.name.singleton)
    defining_env = Property(Entity.renamed_package.defining_env)

    xref_entry_point = Property(True)
    xref_equation = Property(
        Entity.renames.renamed_object.xref_no_overloading
    )


@abstract
class GenericRenamingDecl(BasicDecl):
    """
    Base node for all generic renaming declarations (:rmlink:`8.5.5`).
    """
    renaming_name = AbstractProperty(type=T.Name.entity)

    resolve = Property(
        # We must use `all_env_elements_internal` here and not `env_elements`,
        # as the latter assumes the Name is used in an expression context,
        # which is not the case here.
        Entity.renaming_name.all_env_elements_internal(
            seq=True, seq_from=Self, categories=no_prims
        ).at(0)._.match(
            lambda gd=T.GenericDecl: gd,
            lambda grd=T.GenericRenamingDecl: grd.resolve,
            lambda _: No(T.GenericDecl.entity)
        ),
        type=T.GenericDecl.entity,
        doc="Resolve the GenericDecl this renaming decl is pointing at"
    )

    xref_entry_point = Property(True)
    xref_equation = Property(Entity.renaming_name.xref_no_overloading)


class GenericPackageRenamingDecl(GenericRenamingDecl):
    """
    Declaration for a generic package renaming (:rmlink:`8.5.5`).
    """
    name = Field(type=T.DefiningName)
    renames = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)
    defining_env = Property(Entity.resolve.defining_env)
    renaming_name = Property(Entity.renames)

    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(Self.child_decl_initial_env),

        add_to_env_kv(
            key=Entity.name_symbol,
            value=Self
        ),

        add_env(),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        )
    )


class SubpKind(AdaNode):
    """
    Qualifier for a subprogram kind.
    """
    enum_node = True
    alternatives = ["procedure", "function"]


class GenericSubpRenamingDecl(GenericRenamingDecl):
    """
    Declaration for a generic subprogram renaming.
    """
    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(Self.child_decl_initial_env),

        add_to_env_kv(
            key=Entity.name_symbol,
            value=Self
        ),

        add_env(),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        )
    )

    kind = Field(type=T.SubpKind)
    name = Field(type=T.DefiningName)
    renames = Field(type=T.Name)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)
    renaming_name = Property(Entity.renames)


@abstract
class FormalSubpDecl(ClassicSubpDecl):
    """
    Formal subprogram declarations, in generic declarations formal parts
    (:rmlink:`12.6`).
    """
    default_expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.subp_spec.name.singleton)

    xref_equation = Property(
        Entity.default_expr.then(lambda e: e.match(
            lambda _=T.NullLiteral: LogicTrue(),

            lambda _=T.BoxExpr: LogicTrue(),

            lambda n=T.Name: And(
                And(n.xref_no_overloading(all_els=True),
                    Predicate(BasicDecl.subp_decl_match_signature,
                              n.ref_var, Entity.cast(T.BasicDecl))),

                # TODO: change this once we synthesize function attributes
                # Do not crash on attributes which denotes functions. Ssee
                # equivalent logic in SubpRenamingDecl's xref_equation.
                If(n.is_a(AttributeRef), LogicFalse(), LogicTrue())
            ),

            lambda _: PropertyError(Equation, "Should not happen")
        ), default_val=LogicTrue())
    )

    @langkit_property(return_type=T.BasicDecl.entity)
    def designated_subprogram_from(inst=T.GenericInstantiation.entity):
        """
        Return the first visible subprogram that can match this formal subp in
        the context of an instantiation. This is used to find the matching
        subprogram in an instantiation for a formal subp with a box expr.
        """
        subps = Var(Self.env_get(
            env=inst.node_env,
            symbol=Entity.defining_name.name_symbol,
            from_node=inst.node
        ))

        # Create a subp spec for the formal subprogram in the context of the
        # instantiation, e.g. for the function ``Foo`` in ``G_Inst`` in the
        # following code::
        #
        #    generic
        #       type T is private;
        #       function Foo (Self: T) return T;
        #    package G is ...
        #
        #    package G_Inst is new G (Integer);
        #
        # return ``function Foo (Self : Integer) return Integer``.
        actual_spec = Var(SubpSpec.entity.new(

            node=Self.subp_spec,
            info=T.entity_info.new(
                md=Entity.info.md,
                rebindings=Entity.info.rebindings
                # Append the given generic instantiation
                .append_rebinding(
                    Self.parent.node_env, inst.instantiation_env
                ),
                from_rebound=Entity.info.from_rebound
            )
        ))

        # Search for the first subprogram that matches the instantiated profile
        found = Var(subps.find(
            lambda subp: subp.cast(BasicDecl).subp_spec_or_null.then(
                lambda spec: actual_spec.match_signature(
                    other=spec,

                    # Names must already match due to the env_get call
                    match_name=False
                )
            )
        ).cast(BasicDecl))

        # ``found`` can be null, for example when it is supposed to designate
        # a builtin operator.
        return found._.wrap_public_reference


class ConcreteFormalSubpDecl(FormalSubpDecl):
    """
    Formal declaration for a concrete subprogram (:rmlink:`12.6`).
    """

    pass


class AbstractFormalSubpDecl(FormalSubpDecl):
    """
    Formal declaration for an abstract subprogram (:rmlink:`12.6`).
    """

    pass


class GenericFormalPart(BaseFormalParamHolder):
    """
    List of declaration for generic formals (:rmlink:`12.1`).
    """
    decls = Field(type=T.AdaNode.list)

    abstract_formal_params = Property(Entity.decls.keep(BaseFormalParamDecl))

    @langkit_property(return_type=T.LexicalEnv)
    def use_clauses_envs():
        """
        Returns the envs for all the use clauses declared in this generic
        formal part.
        """
        return Entity.decls.filtermap(
            lambda u: u.cast(T.UseClause).used_envs,
            lambda c: c.is_a(T.UseClause)
        ).env_group()


@abstract
class GenericFormal(BaseFormalParamDecl):
    """
    Enclosing declaration for a generic formal. The real declaration is
    accessible via the ``decl`` field.
    """
    decl = Field(T.BasicDecl)
    aspects = NullField()

    defining_names = Property(Entity.decl.defining_names)
    type_expression = Property(Entity.decl.type_expression)

    xref_entry_point = Property(True)
    xref_equation = Property(Entity.decl.xref_equation)


class GenericFormalObjDecl(GenericFormal):
    """
    Formal declaration for an object.
    """
    pass


class GenericFormalTypeDecl(GenericFormal):
    """
    Formal declaration for a type (:rmlink:`12.1`).
    """

    pass


class GenericFormalSubpDecl(GenericFormal):
    """
    Formal declaration for a subprogram (:rmlink:`12.1`).
    """

    pass


class GenericFormalPackage(GenericFormal):
    """
    Formal declaration for a package (:rmlink:`12.1`).
    """

    pass


class GenericSubpInternal(BasicSubpDecl):
    """
    Internal node for generic subprograms.
    """
    subp_spec = Field(type=T.SubpSpec)
    aspects = Field(type=T.AspectSpec)

    subp_decl_spec = Property(Entity.subp_spec)

    env_spec = EnvSpec(add_env(names=Self.env_names))


@abstract
class GenericDecl(BasicDecl):
    """
    Base class for generic declarations (:rmlink:`12.1`).
    """
    formal_part = Field(type=T.GenericFormalPart)
    decl = AbstractProperty(type=T.BasicDecl.entity)

    annotations = Annotations(rebindable=True)

    @langkit_property()
    def get_aspect_assoc(name=Symbol):
        # The aspect is actually on the Generic*Internal node, so forward
        # the call to it.
        return Entity.decl.get_aspect_assoc(name)

    @langkit_property()
    def next_part_for_decl():
        return Entity.decl.next_part_for_decl


class GenericSubpDecl(GenericDecl):
    """
    Generic subprogram declaration (:rmlink:`12.1`).
    """

    subp_decl = Field(type=T.GenericSubpInternal)
    aspects = NullField()

    defining_names = Property(Entity.subp_decl.subp_spec.name.singleton)

    @langkit_property(public=True, dynamic_vars=[default_imprecise_fallback()])
    def body_part():
        """
        Return the BaseSubpBody corresponding to this node.
        """
        return Entity.body_part_for_decl.cast(BaseSubpBody)

    env_spec = EnvSpec(
        # Call the env hook to parse eventual parent unit
        do(Self.env_hook),

        set_initial_env(
            # TODO: This is wrong (should take into account whether the entity
            # is private or not), but we have no example of cases where this is
            # a problem yet.
            Self.child_decl_initial_env(True)
        ),

        add_to_env(Entity.child_decl_env_assocs),

        add_env(),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        handle_children(),

        do(Self.populate_body_unit)
    )

    decl = Property(Entity.subp_decl)

    # Overriding properties forwarding to internal subp decl
    is_imported = Property(Entity.subp_decl.is_imported)

    @langkit_property()
    def next_part_for_decl():
        return Entity.subp_decl.next_part_for_decl


class GenericPackageInternal(BasePackageDecl):
    """
    This class denotes the internal package contained by a GenericPackageDecl.
    """
    # Implementation note: This exists so that we can insert an environment to
    # distinguish between formal parameters and the package's contents.

    @langkit_property(return_type=T.Bool)
    def is_library_item():
        """
        Return whether this is a top-level element.
        """
        return Self.parent.parent.is_a(LibraryItem)

    env_spec = EnvSpec(add_env(names=Self.env_names))


class GenericPackageDecl(GenericDecl):
    """
    Generic package declaration (:rmlink:`12.1`).
    """
    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(
            # TODO: This is wrong (should take into account whether the entity
            # is private or not), but we have no example of cases where this is
            # a problem yet.
            Self.child_decl_initial_env(True)
        ),

        add_to_env(Entity.child_decl_env_assocs),

        add_env(),

        do(Self.populate_dependent_units),
        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        )
    )

    package_decl = Field(type=GenericPackageInternal)
    aspects = NullField()

    defining_env = Property(Entity.package_decl.defining_env)

    defining_names = Property(
        Self.package_decl.package_name.as_entity.singleton
    )

    @langkit_property(public=True)
    def body_part():
        """
        Return the PackageBody corresponding to this node, or null if there is
        none.
        """
        return Entity.package_decl.body_part

    decl = Property(Entity.package_decl)


class Substitution(Struct):
    """
    Represent a substitution of a BasicDecl by a given value. This can then
    be used as part of an environment in the eval_as_*_in_env property. See
    the declaration of those properties for more details.
    """

    from_decl = UserField(type=T.BasicDecl.entity,
                          doc="The declaration to substitute.")

    # TODO: once we can call expr_eval from the DSL and get an actual
    # discriminated type, use that type instead of BigInt.
    # For now however, we only ever need to do BigInt substitutions.
    to_value = UserField(type=T.BigInt,
                         doc="The value by which to substitute the"
                             " declaration.")

    value_type = UserField(type=T.BaseTypeDecl.entity,
                           doc="The type of the substituted value.")


@abstract
@has_abstract_list
class Expr(AdaNode):
    """
    Base class for expressions (:rmlink:`4.4`).
    """

    logic_vars = UserField(type=T.Address, public=False)

    @langkit_property(return_type=LogicVar, external=True,
                      uses_entity_info=False, uses_envs=False)
    def type_var():
        pass

    @langkit_property(return_type=LogicVar, external=True,
                      uses_entity_info=False, uses_envs=False)
    def expected_type_var():
        pass

    type_val = Property(Self.type_var.get_value)

    expression_type = Property(
        Self.logic_val(Entity, Self.type_var)
            .value.cast_or_raise(T.BaseTypeDecl),
        public=True,
        doc="""
        Return the declaration corresponding to the type of this expression
        after name resolution.
        """
    )

    expected_expression_type = Property(
        Self.logic_val(Entity, Self.expected_type_var)
            .value.cast_or_raise(T.BaseTypeDecl),
        public=True,
        doc="""
        Return the declaration corresponding to the expected type of this
        expression after name resolution.
        """
    )

    xref_stop_resolution = Property(
        # Pause resolution of ForLoopSpecs' iterator filter expression, so
        # that the indexing variable's type can be fully inferred first.
        # Otherwise, any reference to the indexing variable appearing in the
        # filter expression will cause an infinite xref_equation recursion.
        Self.parent.cast(T.ForLoopSpec).then(
            lambda spec: spec.iter_filter == Self
        )
    )

    @langkit_property(return_type=T.Bool)
    def has_context_free_type():
        """
        This is an internal helper for name resolution: return True if the
        type of this expression can be determined without context, i.e. that
        when constructing its xref equation, we never need to use its expected
        type to find its type. For example, an integer literal can always be
        typed to universal integer, so ``has_context_free_type`` returns True
        for it. However, the type of a binary operation in general cannot be
        determined solely by looking at its operands' types. This is used
        to optimize the xref equations we construct for some nodes. See
        ``BinOp.no_overload_equation`` for an example.
        """
        return True

    @langkit_property(return_type=T.Equation, dynamic_vars=[origin])
    def matches_expected_type():
        return Predicate(
            BaseTypeDecl.matching_type,
            Self.type_var,
            Self.expected_type_var
        )

    @langkit_property(return_type=T.Equation, dynamic_vars=[origin])
    def matches_expected_assign_type():
        return Predicate(
            BaseTypeDecl.matching_assign_type,
            Self.type_var,
            Self.expected_type_var
        )

    @langkit_property(return_type=T.Equation, dynamic_vars=[origin])
    def matches_expected_formal_type():
        return Predicate(
            BaseTypeDecl.matching_formal_type,
            Self.type_var,
            Self.expected_type_var
        )

    @langkit_property(return_type=T.Equation, dynamic_vars=[origin])
    def matches_expected_formal_prim_type():
        return Predicate(
            BaseTypeDecl.matching_formal_prim_type,
            Self.type_var,
            Self.expected_type_var
        )

    @langkit_property(return_type=T.Equation, dynamic_vars=[origin])
    def matches_expected_prefix_type():
        return Predicate(
            BaseTypeDecl.matching_prefix_type,
            Self.type_var,
            Self.expected_type_var
        )

    @langkit_property(public=True, dynamic_vars=[default_imprecise_fallback()],
                      return_type=T.Bool)
    def is_dynamically_tagged():
        """
        Returns whether this expression is dynamically tagged (See
        :rmlink:`3.9.2`).
        """
        # See ARM 3.9.2 for the rules
        return origin.bind(Self.origin_node, Or(
            Entity.expression_type.is_classwide,
            Entity.expression_type.accessed_type._.is_classwide,

            Entity.match(
                lambda qual_expr=QualExpr:
                qual_expr.suffix.is_dynamically_tagged,

                # If expr is a call with a controlling result which has at
                # least one dynamically tagged controlling operand, then it's
                # dynamically tagged.
                lambda n=Name: And(
                    n.is_direct_call,
                    n.called_subp_spec.cast(T.BaseSubpSpec).then(
                        lambda spec:
                        spec.has_controlling_result
                        & n.has_dynamic_controlling_operand(spec)
                    )
                ),

                lambda cond_expr=CondExpr: cond_expr.dependent_exprs.all(
                    lambda e: e.is_dynamically_tagged
                ),

                lambda decl_expr=DeclExpr:
                decl_expr.expr.is_dynamically_tagged,

                lambda paren_expr=ParenExpr:
                paren_expr.expr.is_dynamically_tagged,

                lambda _: False
            )

        ))

    @langkit_property(return_type=T.ExpectedTypeForExpr.array,
                      dynamic_vars=[default_imprecise_fallback()],
                      kind=AbstractKind.abstract_runtime_check)
    def potential_actuals_for_dispatch(spec=T.BaseSubpSpec.entity):
        """
        Assuming Self is a call to a subprogram, return an array of pairs
        (expected_type, expression) for each expression in the call that could
        be used for performing a dynamic dispatch for this call.

        .. note:: Implementations should not check that the call is done in the
           RHS of an assign statement in order to take into account return type
           dispatching, as this logic does not depend on the node kind and
           is therefore factorized in ``is_dispatching_call_impl``.
        """
        pass

    @langkit_property(return_type=T.Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def has_dynamic_controlling_operand(spec=T.BaseSubpSpec.entity):
        """
        Return whether this call has at least one controlling operand which is
        dynamically tagged.
        """
        # Retrieve the candidate expressions on which the tag check could
        # be made, together with the expected type for them.
        # Then, check that there is a pair (``formal``, ``actual``)
        # where ``formal`` is a controlling formal parameter of the
        # primitive subprogram ``decl``, and ``actual`` is a dynamically
        # tagged expression used for this parameter.
        return Entity.potential_actuals_for_dispatch(spec).any(
            lambda c:
            spec.get_candidate_type_for_primitive(c.expected_type).then(
                lambda typ: And(
                    # We don't need accurate tagged-visibility information on
                    # `typ` at the callsite, we just want to abort early here
                    # to avoid having to handle nonsensical types in the
                    # `is_dynamically_tagged` property.
                    typ.full_view.is_tagged_type,
                    c.expr.is_dynamically_tagged
                )
            )
        )

    @langkit_property(return_type=T.Expr.entity)
    def parent_candidate_dispatching_call():
        """
        Return the closest parent expression that can be a dispatching call and
        which can control the tag value of this expression according to Ada
        semantics (see :rmlink:`3.9.2` 17/2).
        """
        return Entity.parent.match(
            lambda ce=CallExpr: If(
                Entity == ce.name,
                ce.parent_candidate_dispatching_call,
                ce
            ),
            lambda dn=DottedName: If(
                Entity == dn.suffix,
                dn.parent_candidate_dispatching_call,
                dn
            ),
            lambda uo=UnOp: If(
                Entity == uo.op,
                uo.parent_candidate_dispatching_call,
                uo
            ),
            lambda bo=BinOp: If(
                Entity == bo.op,
                bo.parent_candidate_dispatching_call,
                bo
            ),
            lambda ce=CondExpr: If(
                ce.dependent_exprs.contains(Entity),
                ce.parent_candidate_dispatching_call,
                No(Expr.entity)
            ),
            lambda pe=ParenExpr: pe.parent_candidate_dispatching_call,
            lambda qe=QualExpr: qe.parent_candidate_dispatching_call,
            lambda de=DeclExpr: de.parent_candidate_dispatching_call,
            lambda pa=ParamAssoc: pa.parent.parent.cast(CallExpr),
            lambda _: No(Expr.entity)
        )

    @langkit_property(return_type=T.Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def has_dynamic_context():
        """
        Return whether the tag value for this tag-indeterminate expression can
        be determined from the enclosing context, that is, whether this is the
        RHS of an assign statement which destination has a classwide type, or a
        controlling operand of an enclosing call which is itself dispatching.
        """
        return Entity.parent.cast(AssignStmt).then(
            # If we're the RHS of an assignment, RM 3.9.2 (18.1/2) applies:
            # The controlling operand is the LHS of the assignment.
            lambda a: And(
                # This only applies to the RHS of the assign statement
                a.expr == Entity,
                Entity.expected_expression_type.is_classwide
            ),

            # If we're an argument of an enclosing dispatching call, then
            # RM 3.9.2 (18/2) applies: The controlling tag value of this
            # call is the controlling tag value of the enclosing call.
            default_val=Entity.parent_candidate_dispatching_call
            ._.is_dispatching_call
        )

    @langkit_property(return_type=T.Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_dispatching_call_impl(decl=T.BasicDecl.entity):
        """
        Common logic for the implementation of is_dispatching_call on the
        various node types. ``decl`` should be the declaration of the
        subprogram being called.
        """
        spec = Var(decl.canonical_part.subp_spec_or_null(follow_generic=True))
        return Or(
            # A call to an abstract formal subprogram is necessarily
            # dispatching (see RM 12.6 8.5/2).
            decl.is_a(AbstractFormalSubpDecl),

            # Alternatively, check if there is a controlling operand that is
            # dynamically tagged.
            Entity.has_dynamic_controlling_operand(spec),

            # Otherwise, it means that all controlling operands are statically
            # tagged or tag-indeterminate. In the latter case, we need to check
            # that the called primitive has a controlling result and that the
            # tag can be determined from context.
            spec.has_controlling_result & Entity.has_dynamic_context
        )

    @langkit_property(public=True, return_type=T.Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_dispatching_call():
        """
        Returns True if this ``Name`` corresponds to a dispatching call,
        including:

        - Calls done through subprogram access types.
        - Calls to dispatching subprograms, in the object-oriented sense.

        .. note:: This is an experimental feature. There might be some
            discrepancy with the GNAT concept of "dispatching call".

        .. note:: This should only be called on a ``Name`` and ``UnOp``
            or a ``BinOp``.
        """
        return PropertyError(
            T.Bool,
            "Invalid node type: expected Name, UnOp or BinOp"
        )

    @langkit_property(public=True, return_type=Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_static_expr():
        """
        Return whether this expression is static according to the ARM
        definition of static. See :rmlink:`4.9`.
        """
        return origin.bind(Self.origin_node, Entity.match(
            lambda _=NumLiteral: True,
            lambda _=StringLiteral: True,

            lambda ar=AttributeRef: ar.prefix.is_static_expr & Or(
                Not(ar.prefix.name_designated_type
                    ._.root_type._.is_formal)
                & (ar.attribute.name_symbol == 'Base'),

                ar.prefix.name_designated_type
                ._.is_static_decl & ar.attribute.name_symbol.any_of(
                    'First', 'Last', 'Range', 'Val'
                ),

                ar.prefix.referenced_decl._.is_array
                & ar.attribute.name_symbol.any_of(
                    'First', 'Last', 'Length', 'Range'
                )
            ) & ar.args_list.then(
                # No matter the attribute, if it has arguments they must all be
                # static for the whole thing to be considered static.
                lambda args: args.all(lambda arg: arg.expr.is_static_expr),
                default_val=True
            ),

            lambda ce=CallExpr:
            ce.name.is_static_expr
            & ce.params.all(lambda pa: pa.expr.is_static_expr),

            lambda qe=QualExpr:
            qe.prefix.name_designated_type._.is_static_decl
            & qe.suffix.is_static_expr,

            lambda n=Name:
            n.referenced_decl._.is_static_decl,

            lambda me=MembershipExpr:
            me.expr.is_static_expr & me.membership_exprs.all(
                lambda e: e.is_static_expr
            ),

            lambda bo=BinOp:
            bo.left.is_static_expr
            & bo.right.is_static_expr
            & bo.op.referenced_decl.then(
                lambda decl: decl.is_static_decl,
                default_val=True
            ),

            lambda uo=UnOp:
            uo.expr.is_static_expr
            & uo.op.referenced_decl.then(
                lambda decl: decl.is_static_decl,
                default_val=True
            ),

            lambda i=IfExpr:
            i.cond_expr.is_static_expr & i.then_expr.is_static_expr
            & i.alternatives.all(
                lambda a:
                a.cond_expr.is_static_expr
                & a.then_expr.is_static_expr
            )
            & i.else_expr.is_static_expr,

            lambda pe=ParenExpr: pe.expr.is_static_expr,

            lambda _: False
        ))

    @langkit_property(public=True)
    def first_corresponding_decl():
        """
        Return the first decl that is lexically named like self in self's
        scope.
        """
        return No(T.BasicDecl.entity)

    @langkit_property(return_type=T.BigInt, public=True)
    def eval_as_int():
        """
        Statically evaluates self, and returns the value of the evaluation as
        an integer.

        .. note::
            In order for a call to this not to raise, the expression needs to
            be a static expression, as specified in :rmlink:`4.9`. You
            can verify whether an expression is static with the
            ``is_static_expr`` property.

        .. ATTENTION::
            This is an experimental feature, so even if it is exposed to allow
            experiments, it is totally unsupported and the API and behavior are
            very likely to change in the future.
        """
        return Entity.eval_as_int_in_env(No(Substitution.array))

    @langkit_property(external=True, uses_entity_info=True, uses_envs=False,
                      return_type=T.BigInt, public=True)
    def eval_as_int_in_env(env=T.Substitution.array):
        """
        Statically evaluates self, and returns the value of the evaluation as
        an integer. The given environment is used to substitute references
        to declarations by actual values.

        .. note::
            In order for a call to this not to raise, the expression needs to
            be a static expression, as specified in :rmlink:`4.9`. You
            can verify whether an expression is static with the
            ``is_static_expr`` property.

        .. ATTENTION::
            This is an experimental feature, so even if it is exposed to allow
            experiments, it is totally unsupported and the API and behavior are
            very likely to change in the future.
        """
        pass

    @langkit_property(return_type=T.String, public=True)
    def eval_as_string():
        """
        Statically evaluates self, and returns the value of the evaluation as
        a string.

        .. note::
            In order for a call to this not to raise, the expression needs to
            be a static expression, as specified in :rmlink:`4.9`. You
            can verify whether an expression is static with the
            ``is_static_expr`` property.

        .. ATTENTION::
            This is an experimental feature, so even if it is exposed to allow
            experiments, it is totally unsupported and the API and behavior are
            very likely to change in the future.
        """
        return Entity.eval_as_string_in_env(No(Substitution.array))

    @langkit_property(external=True, uses_entity_info=True, uses_envs=False,
                      return_type=T.String, public=True)
    def eval_as_string_in_env(env=T.Substitution.array):
        """
        Statically evaluates self, and returns the value of the evaluation as
        a string. The given environment is used to substitute references
        to declarations by actual values.

        .. note::
            In order for a call to this not to raise, the expression needs to
            be a static expression, as specified in :rmlink:`4.9`. You
            can verify whether an expression is static with the
            ``is_static_expr`` property.

        .. ATTENTION::
            This is an experimental feature, so even if it is exposed to allow
            experiments, it is totally unsupported and the API and behavior are
            very likely to change in the future.
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
                low_bound=bo.left, high_bound=bo.right
            ),
            lambda _: No(DiscreteRange)
        )

    @langkit_property(return_type=LexicalEnv,
                      dynamic_vars=[env, origin, default_no_visibility()])
    def designated_env_no_overloading():
        """
        Returns the lexical environment designated by this name, assuming
        that this name cannot be overloaded.

        If ``no_visibility``, discard visibility checks.
        """
        return Entity.designated_env

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      return_type=LexicalEnv,
                      dynamic_vars=[env, origin, default_no_visibility()])
    def designated_env():
        """
        Returns the lexical environment designated by this name.

        If this name involves overloading, this will return a combination of
        the various candidate lexical environments.

        If ``no_visibility``, discard visibility checks.
        """
        pass

    env_elements = Property(
        Entity.env_elements_impl.filter(lambda e: Self.has_visibility(e)),
        dynamic_vars=[env, default_no_visibility()]
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
    """
    Single association for the ``Contract_Case`` aspect.
    """
    guard = Field(type=T.AdaNode)
    consequence = Field(type=T.Expr)

    assoc_expr = Property(Entity.consequence)


class DeclExpr(Expr):
    """
    Declare expression (Ada 2020, :rmlink:`4.5.9`).
    """

    decls = Field(type=T.BasicDecl.list)
    expr = Field(type=T.Expr)

    env_spec = EnvSpec(
        add_env()
    )

    has_context_free_type = Property(Self.expr.has_context_free_type)

    @langkit_property()
    def xref_equation():
        return And(
            env.bind(Entity.children_env, Entity.expr.sub_equation),
            Bind(Self.expr.expected_type_var, Self.expected_type_var),
            Bind(Self.expr.type_var, Self.type_var)
        )


class ContractCases(Expr):
    """
    List of associations for the ``Contract_Case`` aspect.

    Contract cases is a non standard Ada extension that's mainly useful in
    SPARK. See `the SPARK RM <https://docs.adacore.com/spark2014-docs/>`_ for
    more details.
    """
    contract_cases = Field(ContractCaseAssoc.list)


class ParenExpr(Expr):
    """
    Parenthesized expression.
    """
    expr = Field(type=T.Expr)

    has_context_free_type = Property(Self.expr.has_context_free_type)

    @langkit_property()
    def xref_equation():
        return (
            Entity.expr.sub_equation
            & Bind(Self.expr.expected_type_var, Self.expected_type_var)
            & Bind(Self.expr.type_var, Self.type_var)
        )


class UnOp(Expr):
    """
    Unary expression.

    This encompasses several ARM expressions, because it is used for every
    unary operator in Ada. Those expressions are all documented in
    :rmlink:`4.4`.
    """

    op = Field(type=T.Op)
    expr = Field(type=T.Expr)

    has_context_free_type = Property(False)

    @langkit_property()
    def xref_equation():
        return Entity.expr.sub_equation & If(
            Self.in_aspect('Depends') | Self.in_aspect('Refined_Depends'),
            LogicTrue(),
            Or(Entity.overload_equation,
               Bind(Self.expected_type_var, Self.expr.expected_type_var)
               & Bind(Self.type_var, Self.expr.type_var))
        )

    @langkit_property(dynamic_vars=[origin, env])
    def overload_equation():
        return Entity.op.subprograms.logic_any(
            lambda subp: Entity.entity_eq(subp)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[origin, env])
    def entity_eq(subp=T.BasicDecl.entity):
        spec = Var(subp.subp_spec_or_null)
        ps = Var(spec.unpacked_formal_params)
        return If(
            ps.length == 1,
            # The subprogram's first argument must match Self's left
            # operand.
            spec.call_argument_equation(ps.at(0).formal_decl, Entity.expr)

            # The subprogram's return type is the type of Self
            & Bind(Self.type_var, spec.return_type)

            # The operator references the subprogram
            & Bind(Self.op.ref_var, subp)
            & Bind(Self.op.subp_spec_var, spec),
            LogicFalse()
        )

    @langkit_property()
    def potential_actuals_for_dispatch(spec=T.BaseSubpSpec.entity):
        return ExpectedTypeForExpr.new(
            expected_type=spec.abstract_formal_params.at(0).type_expression,
            expr=Entity.expr
        ).singleton

    @langkit_property()
    def is_dispatching_call():
        return Entity.op.referenced_decl.then(
            lambda decl: Entity.is_dispatching_call_impl(decl)
        )


class BinOp(Expr):
    """
    Binary expression.

    This encompasses several ARM expressions, because it is used for every
    binary expression in Ada, all documented in ::rmlink:`4.4`.
    """

    left = Field(type=T.Expr)
    op = Field(type=T.Op)
    right = Field(type=T.Expr)

    @langkit_property()
    def has_context_free_type():
        return Self.op.is_a(Op.alt_and_then, Op.alt_or_else)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.left.sub_equation,
            Entity.right.sub_equation
        ) & Cond(
            Self.op.is_a(Op.alt_double_dot),
            Entity.double_dot_equation,

            Self.op.is_a(Op.alt_and_then, Op.alt_or_else),
            Self.test_eq,

            Entity.overload_equation | Entity.no_overload_equation
        )

    @langkit_property(return_type=Equation, dynamic_vars=[origin, env])
    def double_dot_equation():
        return Or(
            LogicTrue(),
            Bind(Self.expected_type_var, Self.root_int_type),
            Bind(Self.expected_type_var, Self.int_type)
        ) & And(
            Predicate(AdaNode.is_not_null, Self.expected_type_var),
            Bind(Self.expected_type_var, Self.type_var),
            Bind(Self.type_var, Self.left.expected_type_var),
            Bind(Self.type_var, Self.right.expected_type_var)
        ) & Or(
            # Expected type was given explicitly, so we can simply check that
            # the type inferred for the operands matches. This is generally
            # the case for ranges in component representation clauses or
            # in subtype indications' constraints.
            Self.left.matches_expected_formal_type
            & Self.right.matches_expected_formal_type,

            # Expected was not given explicitly, so we must infer it here.
            # This is generally the case for ranges in for loop specs.
            Entity.numeric_type_from_operands_equation(Self.type_var)
        )

    @langkit_property(dynamic_vars=[origin])
    def test_eq():
        return And(
            Bind(Self.left.expected_type_var, Self.bool_type),
            Bind(Self.right.expected_type_var, Self.bool_type),

            Self.left.matches_expected_formal_prim_type,
            Self.right.matches_expected_formal_prim_type,

            Bind(Self.type_var, Self.left.type_var)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[origin, env])
    def arguments_eq(spec=T.BaseSubpSpec.entity):
        ps = Var(spec.unpacked_formal_params)
        return If(
            ps.length == 2,

            # The subprogram's first argument must match Self's left
            # operand.
            spec.call_argument_equation(ps.at(0).formal_decl, Entity.left)

            # The subprogram's second argument must match Self's right
            # operand.
            & spec.call_argument_equation(ps.at(1).formal_decl, Entity.right)

            # The subprogram's return type is the type of Self
            & Bind(Self.type_var, spec.return_type),

            LogicFalse()
        )

    @langkit_property(return_type=Equation, dynamic_vars=[origin, env])
    def entity_eq(subp=T.BasicDecl.entity):
        spec = Var(subp.subp_spec_or_null)
        return And(
            Entity.arguments_eq(spec),

            # The operator references the subprogram
            Bind(Self.op.ref_var, subp),
            Bind(Self.op.subp_spec_var, spec)
        )

    @langkit_property(dynamic_vars=[origin, env])
    def overload_equation():
        return Entity.op.subprograms.logic_any(
            lambda subp: Entity.entity_eq(subp)
        )

    @langkit_property(dynamic_vars=[origin])
    def no_overload_equation():
        """
        When no subprogram is found for this node's operator, use this property
        to construct the xref equation for this node.
        """
        return Or(
            # Use the type we are about to infer for Self as the expected types
            # for the left and right operands. This is to handle the general
            # shape: `function "op" (X, Y : T) return T`.
            And(
                Bind(Self.type_var, Self.left.expected_type_var),
                Bind(Self.type_var, Self.right.expected_type_var)
            ),

            # But there are some other cases to handle:
            Cond(
                # For multiplication operators, we must also handle the shape
                # `function "op" (X : Integer, Y : T) return T` as well as
                # `function "op" (X : T, Y : Integer) return T`.
                Self.op.is_a(Op.alt_mult, Op.alt_div),
                Or(
                    Bind(Self.left.expected_type_var, Self.int_type)
                    & Bind(Self.right.expected_type_var, Self.type_var),

                    Bind(Self.right.expected_type_var, Self.int_type)
                    & Bind(Self.left.expected_type_var, Self.type_var)
                ),

                # For power operators, we must also handle the shape
                # `function "op" (X : T, Y : Integer) return T`.
                Self.op.is_a(Op.alt_pow),
                Bind(Self.right.expected_type_var, Self.int_type)
                & Bind(Self.left.expected_type_var, Self.type_var),

                LogicFalse()
            )
        ) & Or(
            # If the expected type is known, use it to infer the type of Self.
            # This will allow Self's type to be used in the next disjunction to
            # infer the type of the operands if necessary.
            Predicate(AdaNode.is_not_null, Self.expected_type_var)
            & Bind(Self.expected_type_var, Self.type_var,
                   conv_prop=BaseTypeDecl.derefed_base_subtype),

            # Otherwise, we cannot infer the type of Self from its expected
            # type, so we will infer it from one of the operands, since at
            # least one of them necessarily has a context-free type (otherwise
            # this wouldn't be valid Ada code).
            Bind(Self.expected_type_var, No(BaseTypeDecl.entity))
        ) & Or(
            # If the expected is known, it was assigned to the type of Self,
            # there might be nothing more to do.
            Predicate(AdaNode.is_not_null, Self.expected_type_var)
            & Self.left.matches_expected_formal_type
            & Self.right.matches_expected_formal_type,

            # If we're trying the following option, it means we must infer
            # Self's type from one of the operands. We assign it to the first
            # one that is *not* a universal type (the result of a binary
            # operation cannot be a universal type).
            Entity.numeric_type_from_operands_equation(Self.type_var)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[origin])
    def numeric_type_from_operands_equation(dest_var=T.LogicVar):
        """
        Construct the xref equation that must bind the given variable to the
        type of this binary operation's operands, assuming we are dealing with
        numeric types and arithmetic operators.
        """
        left_ctx_free = Var(Self.left.has_context_free_type)
        right_ctx_free = Var(Self.right.has_context_free_type)
        return If(
            # If both operands have a context free type, we can use an
            # N-Propagate equation to assign `dest_var` because we know for
            # sure that Self.left.type_var and Self.right.type_var will be
            # given a value explicitly.
            left_ctx_free & right_ctx_free,

            Predicate(BaseTypeDecl.one_non_universal,
                      Self.left.type_var, Self.right.type_var)
            & NPropagate(dest_var,
                         BaseTypeDecl.non_universal_base_subtype,
                         Self.left.type_var, Self.right.type_var)
            & Self.left.matches_expected_formal_type
            & Self.right.matches_expected_formal_type,

            Let(
                lambda
                infer_left=Predicate(BaseTypeDecl.is_not_universal_type,
                                     Self.left.type_var)
                & Bind(Self.left.type_var, dest_var,
                       conv_prop=BaseTypeDecl.derefed_base_subtype)
                & Self.left.matches_expected_formal_type,

                infer_right=Predicate(BaseTypeDecl.is_not_universal_type,
                                      Self.right.type_var)
                & Bind(Self.right.type_var, dest_var,
                       conv_prop=BaseTypeDecl.derefed_base_subtype)
                & Self.right.matches_expected_formal_type:

                If(left_ctx_free,
                   infer_left | infer_right,
                   infer_right | infer_left)
            )
        )

    @langkit_property()
    def potential_actuals_for_dispatch(spec=T.BaseSubpSpec.entity):
        params = Var(spec.unpacked_formal_params)
        return Array([
            ExpectedTypeForExpr.new(
                expected_type=params.at(0).formal_decl.type_expression,
                expr=Entity.left
            ),
            ExpectedTypeForExpr.new(
                expected_type=params.at(1).formal_decl.type_expression,
                expr=Entity.right
            )
        ])

    @langkit_property()
    def is_dispatching_call():
        return Entity.op.referenced_decl.then(
            lambda decl: Entity.is_dispatching_call_impl(decl)
        )


class ConcatOp(Expr):
    """
    Concatenation expression.

    Since concatenation expression can be huge in practice, this node handles
    them as a list of operands rather than a deep tree of binary operators, in
    order to avoid crashes while parsing of running name resolution on such
    huge expression.

    The purpose of this node is to replace the arbitraty too deep tree of
    binary operators (which can lead to a stack overflow), as for example with
    ``"A & B & C & D & E"``:

    .. code::

        BinOp(
          Binop(
            BinOp(
              BinOp(A, &, B), & , C), &, D), &, E)

    by a single operator, handling a list of operands that can be processed
    without having to perform deep recursions:

    .. code::

        ConcatOp(A,
          ConcatOperand(&, B),
          ConcatOperand(&, C),
          ConcatOperand(&, D),
          ConcatOperand(&, E))

    """
    first_operand = Field(type=T.Expr)
    other_operands = Field(type=T.ConcatOperand.list)

    operands = Property(
        Entity.first_operand.singleton.concat(
            Entity.other_operands.map(lambda oo: oo.operand)
        ),
        public=True,
        doc="Return the operands of this concatenation expression"
    )

    @langkit_property()
    def has_context_free_type():
        return False

    @langkit_property()
    def xref_equation():
        last_operand = Var(
            Self.other_operands.at(Self.other_operands.length - 1))
        concat_subprograms = Var(
            Entity.other_operands.at(0).operator.subprograms)

        # Perform expression resolution from left to right
        return (
            Entity.first_operand.sub_equation
            & Entity.other_operands.logic_all(
                lambda pos, concat_operand: Let(
                    # left operand expression is first_operand to begin with,
                    # then, left operands are the result of the previous
                    # operator resolution.
                    lambda left=If(
                        pos > 0,
                        Entity.other_operands.at(pos - 1),
                        Entity.first_operand
                    ),
                    right=concat_operand.operand:

                    right.sub_equation &
                    Or(
                        # Find the subprogram corresponding to:
                        # "&" (left, right).
                        concat_subprograms.logic_any(
                            lambda subp: Let(
                                lambda spec=subp.subp_spec_or_null:

                                And(
                                    Entity.arguments_eq(spec, left,
                                                        concat_operand, right),
                                    Bind(concat_operand.operator.ref_var,
                                         subp),
                                    Bind(concat_operand.operator.subp_spec_var,
                                         spec)
                                )
                            )
                        ),
                        # When no subprogram is found for this concat operator,
                        # use this equation to infer it's type depending on the
                        # context.
                        Entity.operator_no_subprogram_equation(left,
                                                               concat_operand,
                                                               right)
                    )
                )
            )
            # Just propagate last operand's type/expected_type to Self
            & Bind(Self.type_var, last_operand.type_var)
            & Bind(Self.expected_type_var, last_operand.expected_type_var)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[origin, env])
    def arguments_eq(spec=T.BaseSubpSpec.entity,
                     left=T.Expr.entity,
                     op=T.ConcatOperand.entity,
                     right=T.Expr.entity):

        ps = Var(spec.unpacked_formal_params)
        return If(
            ps.length == 2,

            # The subprogram's first argument must match left operand
            spec.call_argument_equation(ps.at(0).formal_decl, left)

            # The subprogram's second argument must match right operand
            & spec.call_argument_equation(ps.at(1).formal_decl, right)

            # The subprogram's return type is the type of op
            & Bind(op.type_var, spec.return_type),

            LogicFalse()
        )

    @langkit_property(dynamic_vars=[origin])
    def operator_no_subprogram_equation(left=T.Expr.entity,
                                        op=T.ConcatOperand.entity,
                                        right=T.Expr.entity):
        """
        When no subprogram is found for this operator, use this property to
        construct the xref equation for this node.
        """
        return (
            Predicate(BaseTypeDecl.is_array_def_with_deref_or_null,
                      op.expected_type_var)
            & Predicate(BaseTypeDecl.is_array_def_with_deref, op.type_var)
            & Or(
                # If the expected is not null, use it to infer self's type
                # and the type of the operands.
                Predicate(BaseTypeDecl.is_not_null, op.expected_type_var)
                & Entity.array_concat_expected_type_equation(left, op, right),

                # If the expected type is null (e.g. we are in a type
                # conversion), we must infer Self's type from the operands.
                # Since we assume Ada code, either the LHS or the RHS will have
                # a context-free type which we can use to infer the rest.
                Bind(op.expected_type_var, No(BaseTypeDecl))
                & Bind(left.type_var, left.expected_type_var)
                & Bind(right.type_var, right.expected_type_var)
                & Or(
                    Bind(left.type_var, op.type_var),
                    Self.comp_bind(op.type_var, left.type_var)
                )
                & Or(
                    Bind(right.type_var, op.type_var),
                    Self.comp_bind(op.type_var, right.type_var)
                )
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[origin])
    def array_concat_expected_type_equation(left=T.Expr.entity,
                                            op=T.ConcatOperand.entity,
                                            right=T.Expr.entity):
        """
        Assume this operator represents a predefined array concatenation
        operator, construct the xref equation that must bind the operator's
        type to the type of the result of the concatenation, using the type of
        the operands or the type from the context.
        """
        left_ctx_free = Var(left.has_context_free_type)
        right_ctx_free = Var(right.has_context_free_type)

        # Generate different xref equations depending on which operands have a
        # context-free type. This helps reduce the final number of disjunction
        # compared to handling all the cases in a single equation.
        return Cond(
            left_ctx_free & right_ctx_free,
            Or(
                # Both operands have a context-free type, so use their types to
                # find the type of the result. This equation handles the
                # following operators:
                #  - "&" (Array_Of_T, T) -> Array_Of_T
                #  - "&" (T, Array_Of_T) -> Array_Of_T
                #  - "&" (Array_Of_T, Array_Of_T) -> Array_Of_T.
                NPropagate(op.type_var,
                           BaseTypeDecl.array_concat_result_type,
                           left.type_var, right.type_var)
                & NPropagate(left.expected_type_var,
                             BaseTypeDecl.expected_array_concat_operand_type,
                             op.type_var, left.type_var)
                & NPropagate(right.expected_type_var,
                             BaseTypeDecl.expected_array_concat_operand_type,
                             op.type_var, right.type_var),

                # But we need another disjunction to handle the case:
                #  - "&" (T, T) -> Array_Of_T
                # For this case, we necessarily need to use the type of the
                # context even though both operands have a context-free type.
                Bind(op.expected_type_var, op.type_var,
                     conv_prop=BaseTypeDecl.derefed_base_subtype)
                & Self.comp_bind(op.type_var, left.expected_type_var)
                & Self.comp_bind(op.type_var, right.expected_type_var)
                & left.matches_expected_formal_type
                & right.matches_expected_formal_type
            ),

            left_ctx_free,
            Or(
                # Left operand has a context free, use to infer the type of the
                # result. This can handle the cases:
                #  - "&" (Array_Of_T, *) -> Array_Of_T.
                Bind(left.type_var, op.type_var,
                     conv_prop=BaseTypeDecl.derefed_base_subtype)
                & Bind(op.type_var, left.expected_type_var),

                # We need another disjunction to handle the remaining cases:
                #  - "&" (T, *) -> Array_Of_T
                # For both these cases, we need to use the type of the context
                # even though LHS has a context-free type.
                Bind(op.expected_type_var, op.type_var,
                     conv_prop=BaseTypeDecl.derefed_base_subtype)
                & Self.comp_bind(op.type_var, left.expected_type_var)
                & left.matches_expected_formal_type
            )
            # The type of op has been inferred from the LHS, use it to
            # determine the type of the RHS.
            & Or(
                Bind(op.type_var, right.expected_type_var),
                Self.comp_bind(op.type_var, right.expected_type_var)
            )
            & right.matches_expected_formal_type,

            right_ctx_free,
            Or(
                # Right operand has a context free, use to infer the type of
                # the result. This can handle the cases:
                #  - "&" (*, Array_Of_T) -> Array_Of_T.
                Bind(right.type_var, op.type_var,
                     conv_prop=BaseTypeDecl.derefed_base_subtype)
                & Bind(op.type_var, right.expected_type_var),

                # We need another disjunction to handle the remaining cases:
                #  - "&" (*, T) -> Array_Of_T
                # For both these cases, we need to use the type of the context
                # even though RHS has a context-free type.
                Bind(op.expected_type_var, op.type_var,
                     conv_prop=BaseTypeDecl.derefed_base_subtype)
                & Self.comp_bind(op.type_var, right.expected_type_var)
                & right.matches_expected_formal_type
            )
            # The type of op has been inferred from the RHS, use it to
            # determine the type of the LHS.
            & Or(
                Bind(op.type_var, left.expected_type_var),
                Self.comp_bind(op.type_var, left.expected_type_var)
            )
            & left.matches_expected_formal_type,

            # None of the operands have a context-free type, so we necessarily
            # have an expected type (otherwise it wouldn't be valid Ada code).
            # Use it to determine the type of the operands.
            Bind(op.expected_type_var, op.type_var,
                 conv_prop=BaseTypeDecl.derefed_base_subtype)
            & Or(
                Bind(op.type_var, left.expected_type_var),
                Self.comp_bind(op.type_var, left.expected_type_var)
            )
            & Or(
                Bind(op.type_var, right.expected_type_var),
                Self.comp_bind(op.type_var, right.expected_type_var)
            )
            & left.matches_expected_formal_type
            & right.matches_expected_formal_type
        )


class ConcatOperand(Expr):
    """
    A concatenation operator and its RHS operand.

    This node is used to represent the tuple ("&", operand) used by the
    ``ConcatOp`` node to store its ``other_operands`` list.
    """
    operator = Field(type=T.Op.Concat)
    operand = Field(type=T.Expr)

    @langkit_property()
    def has_context_free_type():
        return False


class RelationOp(BinOp):
    """
    Binary operation that compares two value, producing a boolean
    (:rmlink:`4.4`).
    """
    has_context_free_type = Property(True)

    no_overload_equation = Property(
        Bind(Self.type_var, Self.bool_type)
        & Bind(Self.left.expected_type_var, Self.right.expected_type_var)
        & Entity.numeric_type_from_operands_equation(
            Self.left.expected_type_var
        )
    )


class MembershipExpr(Expr):
    """
    Represent a membership test (in/not in operators) (:rmlink:`4.4`).

    Note that we don't consider them as binary operators since multiple
    expressions on the right hand side are allowed.
    """
    expr = Field(type=T.Expr)
    op = Field(type=T.Op)
    membership_exprs = Field(type=T.ExprAlternativesList)

    xref_equation = Property(
        Bind(Self.type_var, Self.bool_type)
        & Entity.expr.sub_equation
        & Entity.membership_exprs.logic_all(
            lambda m: m.cast(T.Name)._.name_designated_type.then(
                # Tagged type check or subtype membership check
                lambda typ:
                m.cast(T.Name).xref_no_overloading
                & Cond(
                    # If testing a specific tagged type membership, the
                    # expected type of the tested expression is the type at the
                    # root of the tagged type hierarchy.
                    # TODO: This is currently not possible to express because
                    # of an unrelated bug (see V408-038), but we can at least
                    # constrain the resulting type to be tagged after implicit
                    # dereference.
                    typ.is_tagged_type,
                    Bind(Self.expr.expected_type_var, No(BaseTypeDecl.entity))
                    & Predicate(BaseTypeDecl.is_tagged_type_with_deref,
                                Self.expr.type_var),

                    # This is a simple subtype membership test, so the expected
                    # type of the tested expression is the subtype's base type.
                    Bind(Self.expr.expected_type_var,
                         typ.base_subtype)
                    & Self.expr.matches_expected_formal_type
                ),

                # Regular membership check
                default_val=And(
                    Bind(m.expected_type_var, Self.expr.type_var),
                    m.sub_equation,
                    m.matches_expected_type
                )
            )
        ),
    )


class MultidimAggregateInfo(Struct):
    """
    Struct enclosing information about aggregates for multidimensional array
    types.
    """
    agg = UserField(type=T.BaseAggregate.entity, doc="the top level aggregate")
    typ = UserField(type=T.BaseTypeDecl.entity, doc="the type of the array")
    rank = UserField(type=Int, doc="the rank of the original sub-aggregate")


@abstract
class BaseAggregate(Expr):
    """
    Base class for aggregates (:rmlink:`4.3`).
    """

    ancestor_expr = Field(type=T.Expr)
    assocs = Field(type=T.AssocList)

    has_context_free_type = Property(False)

    @langkit_property()
    def xref_equation():
        # An aggregate (or more precisely, its associations) are resolved
        # separately from the rest of an expression. However,resolution of the
        # containing expression can leverage the knowledge that self is an
        # aggregate, by accepting only type that can be represented by an
        # aggregate (e.g. records and arrays).
        type_constraint = Var(If(
            # In the following cases, the aggregate is not a real expression:
            # it's merely re-used as a pure syntactic construct to aggregate
            # information.
            Self.in_aspect('Global') | Self.in_aspect('Refined_Global')
            | Self.in_aspect('Depends') | Self.in_aspect('Refined_Depends')
            | Self.in_aspect('Test_Case') | Self.in_aspect('Refined_State')
            # Careful: normal aggregates can appear inside a contract_cases
            # aspect's expression, so we must only special case the direct
            # aggregate of that aspect.
            | Self.is_contract_cases_base_aggregate,
            LogicTrue(),
            origin.bind(
                Self.origin_node,
                Predicate(BaseTypeDecl.is_array_or_rec, Self.expected_type_var)
                & Bind(Self.expected_type_var, Self.type_var)
            )
        ))

        return type_constraint & Entity.ancestor_expr.then(
            lambda ae: ae.sub_equation, default_val=LogicTrue()
        ) & Entity.assocs.logic_all(
            lambda assoc: assoc.sub_equation
        )

    @langkit_property(return_type=Bool)
    def is_contract_cases_base_aggregate():
        """
        Return whether this is the aggregate directly used as the RHS of the
        ``Contract_Cases`` aspect.
        """
        return Self.parent.cast(AspectAssoc).then(
            lambda aspect: aspect.id.name_is('Contract_Cases')
        )

    @langkit_property(return_type=MultidimAggregateInfo, dynamic_vars=[origin],
                      memoized=True, call_memoizable=True)
    def multidim_root_aggregate(r=(Int, 0)):
        """
        Return the root parent aggregate if Self is part of a multidimensional
        array aggregate (either the root or a sub-aggregate).
        """
        # Nested aggregates of a multidimensional array have no types, so we're
        # searching for the first aggregate with a type inside type_val.
        return Entity.type_val.cast(BaseTypeDecl).then(
            lambda tv: If(
                # If we have a multidimensional array type here, return all the
                # needed info (rank, root aggregate and type of the array).
                tv.array_ndims > 1,
                MultidimAggregateInfo.new(agg=Entity, typ=tv, rank=r),

                # If we're here, we found a type, and it's not a multidim
                # array: Stop there.
                No(T.MultidimAggregateInfo)
            ),
            # If we're here, there is a parent aggregate and no type_val:
            # recurse up.
            default_val=Entity.parent.parent.parent.cast(T.Aggregate)
            ._.multidim_root_aggregate(r + 1)
        )

    @langkit_property(return_type=T.BaseFormalParamDecl.entity.array,
                      dynamic_vars=[origin],
                      memoized=True, call_memoizable=True)
    def all_discriminants():
        """
        Return the list of all discriminants that must be associated by this
        aggregate.

        .. attention::
            This property is part of the name resolution algorithm for
            AggregateAssocs and therefore is probably not what you're looking
            for, as it makes several assumptions on the content of logic vars.
            Find more details in ``AggregateAssoc.record_assoc_equation``.

        .. note::
            This property must be memoized because all AggregateAssocs that are
            children of this aggregate will call it during their name
            resolution routine.
        """
        td = Var(Self.type_val.cast(BaseTypeDecl))
        record_decl = Var(td.record_def.comps.type_decl)
        return record_decl.discriminants_list

    @langkit_property(return_type=T.BaseFormalParamDecl.entity.array,
                      dynamic_vars=[origin, env],
                      memoized=True, call_memoizable=True)
    def all_components():
        """
        Return the list of all components that must be associated by this
        aggregate.

        .. attention::
            This property is part of the name resolution algorithm for
            AggregateAssocs. More details under ``all_discriminants``.
        """
        td = Var(Self.type_val.cast(BaseTypeDecl))
        comp_list = Var(td.record_def.comps)
        return If(
            Entity.is_a(T.DeltaAggregate),
            # For delta aggregates, get all the components regardless of the
            # discriminant values.
            comp_list.abstract_formal_params_for_delta_assocs,
            comp_list.abstract_formal_params_for_assocs(Entity.assocs)
        )

    @langkit_property(return_type=T.ParamMatch.array, dynamic_vars=[origin],
                      memoized=True)
    def matched_discriminants():
        """
        Return the list of all discriminants specified by this aggregate,
        together with the actual used for it.

        .. attention::
            This property is part of the name resolution algorithm for
            AggregateAssocs. More details under ``all_discriminants``.
        """
        return Self.match_formals(
            Entity.all_discriminants, Entity.assocs, False
        )

    @langkit_property(return_type=T.ParamMatch.array,
                      dynamic_vars=[origin, env], memoized=True)
    def matched_components():
        """
        Return the list of all components specified by this aggregate,
        together with the actual used for it.

        .. attention::
            This property is part of the name resolution algorithm for
            AggregateAssocs. More details under ``all_discriminants``.
        """
        return Self.match_formals(
            Entity.all_components, Entity.assocs, False
        )

    @langkit_property(return_type=T.DefiningName.entity,
                      dynamic_vars=[origin, env])
    def first_unmatched_formal():
        """
        Return the first discriminant or component that is not matched
        explicitly.

        .. attention::
            This property is part of the name resolution algorithm for
            AggregateAssocs. More details under ``all_discriminants``.
        """
        # Try to find an unmatched discriminant first
        unmatched_discr = Var(
            Self.unpack_formals(Entity.all_discriminants).find(
                lambda f: Not(Entity.matched_discriminants.any(
                    lambda m: m.formal == f
                ))
            )
        )
        return If(
            Not(unmatched_discr.is_null),
            unmatched_discr,

            # If there is no unmatched discriminant, this means all of them
            # are specified, so the shape of the record is known: we can now
            # try to find the unmatched formal.
            # WARNING: for the same reason stated in
            # AggregateAssoc.record_assoc_equation, this must be done in this
            # order.
            Self.unpack_formals(Entity.all_components).find(
                lambda f: Not(Entity.matched_components.any(
                    lambda m: m.formal == f
                ))
            )
        )

    @langkit_property(public=True, return_type=T.ParamActual.array)
    def aggregate_params():
        """
        Returns an array of pairs, associating formal parameters to actual
        expressions. See ``zip_with_params``.
        """
        return Entity.assocs.zip_with_params

    @langkit_property(public=True, return_type=T.Bool)
    def is_subaggregate():
        """
        Return whether this aggregate is actually a subaggregate of a
        multidimensional array aggregate, as described in :rmlink:`4.3.3`.
        """
        # The `multidim_root_aggregate` property assumes that the top-level
        # aggregate's type_var has been set, so run nameres beforehand.
        ignore(Var(Entity.resolve_names_from_closest_entry_point))
        return origin.bind(Self, Entity.multidim_root_aggregate.rank > 0)


class Aggregate(BaseAggregate):
    """
    Aggregate that is not a ``null record`` aggregate (:rmlink:`4.3`).
    """


class NullRecordAggregate(BaseAggregate):
    """
    Aggregate for ``null record`` (:rmlink:`4.3`).
    """


class BracketAggregate(Aggregate):
    """
    Bracket array or container aggregate (Ada 2020, :rmlink:`4.3`).
    """


class DeltaAggregate(BaseAggregate):
    """
    Aggregate for delta aggregate (Ada 2022, :rmlink:`4.3`).
    """
    pass


class BracketDeltaAggregate(DeltaAggregate):
    """
    Bracket delta aggregate (Ada 2020, :rmlink:`4.3`).
    """
    pass


class ExpectedTypeForExpr(Struct):
    """
    Struct used by ``potential_actuals_for_dispatch`` to store an expression
    together with the type that is expected for it.
    """
    expected_type = UserField(type=T.TypeExpr.entity)
    expr = UserField(type=T.Expr.entity)


class RefResultKind(Enum):
    """
    Kind for the result of a cross reference operation.

    - ``no_ref`` is for no reference, it is the null value for this enum.
    - ``precise`` is when the reference result is precise.
    - ``imprecise`` is when there was an error computing the precise result,
      and a result was gotten in an imprecise fashion.
    - ``error`` is for unrecoverable errors (either there is no imprecise path
      for the request you made, or the imprecise path errored out too.
    """
    no_ref = EnumValue(is_default=True)
    precise = EnumValue()
    imprecise = EnumValue()
    error = EnumValue()


class RefdDecl(Struct):
    """
    Result for a cross reference query returning a referenced decl.
    """
    decl = UserField(type=T.BasicDecl.entity,
                     default_value=No(T.BasicDecl.entity))
    kind = UserField(type=RefResultKind, default_value=RefResultKind.no_ref)


class RefdDef(Struct):
    """
    Result for a cross reference query returning a referenced defining name.
    """
    def_name = UserField(type=T.DefiningName.entity,
                         default_value=No(T.DefiningName.entity))
    kind = UserField(type=RefResultKind, default_value=RefResultKind.no_ref)


class RefResult(Struct):
    """
    Result for a cross reference query returning a reference.
    """
    ref = UserField(type=T.BaseId.entity)
    kind = UserField(type=RefResultKind, default_value=RefResultKind.no_ref)


@abstract
class Name(Expr):
    """
    Base class for names (:rmlink:`4.1`).
    """

    enclosing_defining_name = Property(
        Entity.parents.find(lambda p: p.is_a(T.DefiningName))
        .cast(T.DefiningName),
        public=True, doc="""
        If this name is part of a defining name, return the enclosing defining
        name node.
        """,
    )

    @langkit_property(public=True, return_type=T.Bool)
    def is_defining():
        """
        Return True if this name is part of a defining name.
        """
        return Or(
            # Obvious case
            Self.is_a(T.DefiningName),

            # The whole Identifier/DottedName contained in the defining name
            # is always considered defining.
            Self.parent.is_a(T.DefiningName),

            # And in case of a dotted name, the suffix of the outermost dotted
            # name is also considered defining.
            Self.parent.parent.is_a(T.DefiningName)
            & (Self.parent.cast(T.DottedName)._.suffix == Self)
        )

    parent_scope = AbstractProperty(
        type=LexicalEnv, runtime_check=True,
        dynamic_vars=[env],
        doc="""
        Returns the lexical environment that is the scope in which the
        entity designated by this name is defined/used.
        """
    )

    @langkit_property(public=True, return_type=T.Bool)
    def name_is(sym=(T.Symbol)):
        """
        Helper. Check that this name matches ``sym``.
        """
        return Self.name_symbol.then(lambda ns: ns == sym)

    @langkit_property(public=True, return_type=T.Bool)
    def is_direct_call():
        """
        Return True iff this name represents a call to a subprogram which is
        referred by its defining name. (i.e. not through a subprogram access).
        """
        return And(
            Entity.is_call,
            Not(Entity.called_subp_spec.parent.is_a(AccessToSubpDef))
        )

    @langkit_property(public=True, return_type=T.Bool)
    def is_access_call():
        """
        Return True iff this name represents a call to subprogram through
        an access type.
        """
        return And(
            Entity.is_call,
            Entity.called_subp_spec.parent.is_a(AccessToSubpDef)
        )

    @langkit_property(public=True, return_type=T.Bool)
    def is_call():
        """
        Returns True if this Name corresponds to a call.
        """
        return And(
            Not(Entity.is_defining),
            Not(Entity.called_subp_spec.is_null)
        )

    @langkit_property(public=True, return_type=T.Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_dot_call():
        """
        Returns True if this Name corresponds to a dot notation call.
        """
        return And(
            Not(Entity.is_defining),
            Entity.referenced_decl.info.md.dottable_subp
        )

    @langkit_property(return_type=RefdDef, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def failsafe_referenced_def_name():
        """
        Failsafe version of ``referenced_defining_name``. Returns a
        ``RefdDef``, which can be precise, imprecise, or error.
        """
        ref_decl = Var(Entity.failsafe_referenced_decl)

        def_name = Var(ref_decl.decl.then(
            lambda ref_decl: Entity.name_symbol.then(
                lambda rel_name: ref_decl.defining_names.find(
                    lambda dn: dn.name_is(rel_name)
                )
            )._or(ref_decl.defining_name),
            default_val=No(T.DefiningName.entity)
        ))

        return RefdDef.new(def_name=def_name, kind=ref_decl.kind)

    @langkit_property(public=True, return_type=T.DefiningName.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def referenced_defining_name():
        """
        Like ``referenced_decl``, but will return the defining identifier for
        the decl, rather than the basic declaration node itself.
        """
        ref_decl = Var(Entity.referenced_decl)
        return ref_decl.then(lambda ref_decl: Entity.name_symbol.then(
            lambda rel_name: ref_decl.defining_names.find(
                lambda dn: dn.name_is(rel_name)
            )
        )._or(ref_decl.defining_name), default_val=No(T.DefiningName.entity))

    @langkit_property(return_type=T.DefiningName.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def gnat_xref_decl():
        """
        Implementation helper for DefiningName.gnat_xref. TODO: Get rid of that
        by inlining in DefiningName.gnat_xref.
        """
        dn = Var(Entity.is_defining.then(
            lambda _: Entity.enclosing_defining_name
        ))
        bd = Var(dn.then(lambda dn: dn.basic_decl))

        return Cond(
            bd.then(lambda bd: bd.is_a(T.ParamSpec)),
            bd.cast(T.ParamSpec).decl_param(dn),

            bd.then(lambda bd: bd.is_a(Body)),
            bd.cast(T.Body).decl_part._.defining_name,

            bd.then(lambda bd: bd.is_a(BaseTypeDecl)
                    & bd.cast(T.BaseTypeDecl).is_in_private_part),
            bd.cast(T.BaseTypeDecl).previous_part(True)._.defining_name,

            bd.then(lambda bd: bd.is_a(ObjectDecl)),
            # TODO: Implement jumping to full object decl view for constant
            # object decls with no value.
            No(T.DefiningName.entity),

            bd.then(lambda bd: bd.is_a(NumberDecl)),
            # Number decls cannot have a next part, always return None
            No(T.DefiningName.entity),

            bd.then(lambda bd: bd.is_a(BasicDecl)),
            bd.body_part_for_decl.then(lambda bpe: bpe.defining_name)
            ._or(bd.defining_name),

            Entity.referenced_defining_name
        )

    @langkit_property(return_type=AdaNode.entity.array,
                      kind=AbstractKind.abstract_runtime_check,
                      dynamic_vars=[env, origin])
    def all_env_els_impl(
            seq=(Bool, True),
            seq_from=(AdaNode, No(T.AdaNode)),
            categories=(T.RefCategories, all_categories)
    ):
        pass

    @langkit_property(return_type=AdaNode.entity.array)
    def all_env_elements_internal(
            seq=(Bool, True),
            seq_from=(AdaNode, No(T.AdaNode)),
            categories=(T.RefCategories, all_categories)
    ):
        """
        Internal property like all_env_elements, but accepting an additional
        ``categories`` parameter for internal uses.
        """
        return origin.bind(
            Self.origin_node,
            env.bind(
                Entity.node_env,
                Entity.all_env_els_impl(
                    seq=seq,
                    seq_from=seq_from,
                    categories=categories
                )
            )
        )

    @langkit_property(public=True)
    def all_env_elements(
            seq=(Bool, True),
            seq_from=(AdaNode, No(T.AdaNode))
    ):
        """
        Return all elements in self's scope that are lexically named like Self.
        """
        return Entity.all_env_elements_internal(seq, seq_from)

    @langkit_property(public=True)
    def first_corresponding_decl():
        return If(
            Self.parent.is_a(DottedName) & Self.is_suffix,
            Entity.parent.cast(DottedName).first_corresponding_decl,
            Entity.all_env_elements_internal.at(0).cast(T.BasicDecl)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def all_args_xref_equation(root=T.Name):
        """
        Constructs the xref equations for all the argument lists of CallExprs
        appearing between ``Entity`` and ``root``. This is done in a single
        distinct pass instead of directly inside the ``parent_name_equation``
        & co. properties as we were accidentally duplicating the construction
        of xref equations for some arguments lists and fixing this inside these
        properties proved to be difficult.
        """
        return Entity.cast(CallExpr).then(
            lambda ce: ce.params._.logic_all(
                lambda pa: pa.expr.sub_equation
            ),
            default_val=LogicTrue()
        ) & Entity.parent_name(root).then(
            lambda name: name.all_args_xref_equation(root),
            default_val=LogicTrue()
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def bottom_up_name_equation():
        return Self.innermost_name.as_entity.match(
            lambda ce=T.CallExpr: ce.general_xref_equation(Self),
            lambda ed=T.ExplicitDeref: ed.general_xref_equation(Self),
            lambda qe=T.QualExpr: qe.general_xref_equation(Self),
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

        ``Self.innermost_name`` will return the node corresponding to
        ``Self.name.name``.
        """
        name = Var(Self.match(
            lambda ce=T.CallExpr: ce.name,
            lambda ed=T.ExplicitDeref: ed.prefix,
            lambda _: No(T.Name)

        ))

        return Cond(
            name.is_a(T.CallExpr, T.ExplicitDeref), name.innermost_name,
            name.is_a(T.QualExpr), name,
            Self
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def parent_name_equation(typ=T.BaseTypeDecl.entity, root=T.Name):
        """
        Construct the xref equation for the chain of parent nested names.
        """
        as_subp_access = Var(typ._.access_def.cast(AccessToSubpDef))
        is_paramless = Var(as_subp_access._.subp_spec.paramless(
            dottable_subp=False,
            can_be=False
        ))
        can_be_paramless = Var(as_subp_access._.subp_spec.paramless(
            dottable_subp=False,
            can_be=True
        ))
        comp_type = Var(
            typ._.comp_type(is_subscript=Not(Self.is_a(ExplicitDeref)))
        )
        return If(
            typ.is_null,
            LogicFalse(),

            Self.match(
                lambda ce=T.CallExpr:
                ce.as_entity.subscriptable_type_equation(typ),

                lambda ed=T.ExplicitDeref:
                ed.as_entity.eq_for_type(typ)

                # If ``typ`` is an access to subprogram, it means Self (an
                # ExplicitDeref) is actually a call to that subprogram. So,
                # bind its subp_spec_var to the subprogram spec of the access.
                & Bind(Self.subp_spec_var, as_subp_access._.subp_spec),

                lambda _: Bind(Self.type_var, No(T.AdaNode.entity).node),
            ) & Entity.parent_name(root).then(
                lambda pn: Cond(
                    Self.is_a(T.ExplicitDeref) & Not(as_subp_access.is_null),

                    # If Self is an explicit deref of a subprogram access type,
                    # we need to handle several cases:
                    Cond(
                        # The subprogram doesn't take parameters, in which case
                        # the explicit dereference necessarily means accessing
                        # the component type of the access type (it represents
                        # the call).
                        is_paramless,
                        pn.parent_name_equation(comp_type, root),

                        # The subprogram can be called without parameters, in
                        # which case we don't know for sure whether the
                        # explicit dereference accesses the component type or
                        # if it is the parent CallExpr that will.
                        can_be_paramless,
                        Or(
                            pn.parent_name_equation(comp_type, root),
                            pn.parent_name_equation(typ, root),
                        ),

                        # The subprogram must be called with parameters, in
                        # which case the parent CallExpr will expect the non-
                        # dereferenced type.
                        pn.parent_name_equation(typ, root)
                    ),

                    # If Self is an array slice, we recurse with the same type
                    Entity.cast(CallExpr)._.check_array_slice(typ),
                    pn.parent_name_equation(typ, root),

                    # Otherwise the name necessarily accesses the
                    # component type of typ.
                    pn.parent_name_equation(comp_type, root)
                ),
                default_val=LogicTrue()
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def subtype_indication_equation():
        return Entity.xref_no_overloading

    @langkit_property(return_type=Bool)
    def can_designate_primitive():
        """
        Return True if this name can refer to a primitive subprogram.
        This is used in env lookups to avoid visiting referenced primitive envs
        when it is not relevant.

        .. note:: This is not an optimisation, it actually prevents potential
            infinite recursions during lookups.
        """
        return Self.parent.match(
            lambda n=Name: n.can_designate_primitive,
            lambda r=RenamingClause: Not(r.parent.is_a(PackageRenamingDecl)),
            lambda _: True
        )

    @langkit_property(return_type=T.Name.entity)
    def parent_name(stop_at=T.Name):
        """
        Will return the parent name until the stop point.
        """
        return If(stop_at.is_null | (Self == stop_at),
                  No(T.Name.entity),
                  Entity.parent.cast(T.Name))

    @langkit_property(return_type=T.CallExpr.entity)
    def parent_callexpr():
        """
        If this name qualifies the prefix in a call expression, this returns
        the corresponding CallExpr node. Return null otherwise. For example::

            C (12, 15);
            ^ parent_callexpr = <CallExpr>

            A.B.C (12, 15);
                ^ parent_callexpr = <CallExpr>

            A.B.C (12, 15);
              ^ parent_callexpr = null

            C (12, 15);
               ^ parent_callexpr = null
        """
        return Entity.parents.take_while(lambda p: Or(
            p.is_a(CallExpr),
            p.is_a(DottedName, BaseId) & p.parent.match(
                lambda pfx=DottedName: pfx.suffix == p,
                lambda ce=CallExpr: ce.name == p,
                lambda _: False
            )
        )).find(lambda p: p.is_a(CallExpr)).cast(CallExpr)

    @langkit_property(return_type=Bool)
    def is_range_attribute():
        """
        Predicate that returns True if self is a range attribute ref.
        """
        return Self.cast(T.AttributeRef).then(
            lambda attr_ref:
            attr_ref.as_bare_entity.attribute.name_is('Range')
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

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      return_type=LogicVar)
    def ref_var():
        """
        This property proxies the logic variable that points to the entity that
        this name refers to. For example, for a simple dotted name::

            A.B

        The dotted name's ref var is the one of the SingleTokNode B.
        """
        pass

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      return_type=LogicVar)
    def subp_spec_var():
        """
        This logic variable holds the specification of the subprogram or
        subprogram access that is being called by this exact Name.
        """
        pass

    @langkit_property(return_type=T.Bool)
    def defines_subp_spec_var():
        # A null logic variable could have been used instead of this additional
        # property to indicate that an AST node does not define subp_spec_var.
        # Unfortunately, No(LogicVar) is not a valid dsl expression. Therefore,
        # we provide a default implementation for this property, which is then
        # overriden in relevant child classes to indicate that one can call
        # p_subp_spec_var.
        return False

    @langkit_property(public=True, return_type=T.BaseFormalParamHolder.entity)
    def called_subp_spec():
        """
        Return the subprogram specification of the subprogram or subprogram
        access that is being called by this exact Name, if relevant.
        """
        return If(
            Self.defines_subp_spec_var,
            Self.logic_val(
                from_node=Entity,
                lvar=Self.subp_spec_var
            ).value.cast(BaseFormalParamHolder),
            No(BaseFormalParamHolder.entity)
        )

    @langkit_property(public=True, return_type=T.BasicDecl.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def referenced_decl():
        """
        Return the declaration this node references after name resolution.
        If imprecise_fallback is True, errors raised during resolution of the
        xref equation are catched and a fallback mechanism is triggered, which
        tries to find the referenced declaration in an ad-hoc way.
        """
        return Entity.referenced_decl_internal.decl

    @langkit_property(public=True, return_type=RefdDecl,
                      dynamic_vars=[default_imprecise_fallback()])
    def failsafe_referenced_decl():
        """
        Failsafe version of ``referenced_decl``. Returns a ``RefdDecl``, which
        can be precise, imprecise, or error.
        """
        return Try(
            Entity.referenced_decl_internal,
            RefdDecl.new(kind=RefResultKind.error)
        )

    @langkit_property(public=True, return_type=RefdDecl,
                      dynamic_vars=[default_imprecise_fallback()])
    def referenced_decl_internal():
        """
        Return the declaration this node references. Try not to run name res if
        already resolved.

        .. warning:: INTERNAL USE ONLY.
        """
        return If(
            # First, check whether the name is defining, in which case it
            # cannot be a reference.
            Entity.is_defining,
            No(RefdDecl),

            If(
                imprecise_fallback,

                # The imprecise_fallback path cannot raise
                Let(lambda v=Try(
                    Self.logic_val(Entity, Self.ref_var),
                    LogicValResult.new(success=False, value=No(AdaNode.entity))
                ): If(
                    v.success,
                    # If we resolved correctly using full nameres, return a
                    # precise result.
                    RefdDecl.new(
                        decl=v.value.cast(T.BasicDecl.entity),
                        kind=RefResultKind.precise
                    ),

                    # Else, just take the first corresponding declaration,
                    # return as an imprecise result.
                    Entity._.first_corresponding_decl.then(
                        lambda fcd:
                        RefdDecl.new(decl=fcd, kind=RefResultKind.imprecise),
                        default_val=RefdDecl.new(kind=RefResultKind.error)
                    )
                )),

                # No fallback path
                Let(lambda v=Self.logic_val(Entity, Self.ref_var): If(
                    v.success,
                    RefdDecl.new(
                        decl=v.value.cast_or_raise(T.BasicDecl.entity),
                        kind=RefResultKind.precise
                    ),
                    RefdDecl.new(
                        decl=No(BasicDecl.entity),
                        kind=RefResultKind.error
                    )
                ))
            ).then(lambda res: RefdDecl.new(
                decl=res.decl._.wrap_public_reference,
                kind=res.kind
            ).then(lambda res: If(
                # Never return a null node with a Precise result: this
                # indicates that it should be a no_ref (e.g. builtin operator).
                res.decl.is_null & (res.kind == RefResultKind.precise),
                No(RefdDecl),
                res
            )))
        )

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
                 origin.bind(Self.origin_node, Entity.designated_type_impl)),
        doc="""
        Like SubtypeIndication.designated_type, but on names, since because of
        Ada's ambiguous grammar, some subtype indications will be parsed as
        names.
        """,
        public=True
    )

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_imprecise_fallback()],
                      public=True)
    def is_static_subtype():
        """
        Returns whether Self denotes a static subtype or not.
        """
        return Entity.name_designated_type.is_static_decl

    @langkit_property(memoized=True)
    def name_designated_type_env():
        return Entity.name_designated_type._.primitives_env

    @langkit_property()
    def referenced_unit(kind=AnalysisUnitKind,
                        not_found_is_error=(Bool, True)):
        """
        Return the compilation unit referenced by this name and for the given
        unit kind, if there is one.
        """
        return Self.get_unit(
            Self.as_symbol_array, kind, True, not_found_is_error
        )

    @langkit_property(return_type=Bool)
    def matches(n=T.Name):
        """
        Return whether two names match each other.

        This compares the symbol for Identifier and StringLiteral nodes. We
        consider that there is no match for all other node kinds.
        """
        return Self.match(
            lambda id=SyntheticIdentifier: n.cast(Identifier).then(
                # We only need to check the case where `n` is an `Identifier`
                # as `SyntheticIdentifier`s can only be compared to those
                # when matching formals (see AdaNode.match_formals). This case
                # will happen when the formal comes from a synthetic operator
                # definition, and the actual from a call with a named
                # parameter (which will necessarily be an `Identifier`).
                lambda other_id: id.sym.equals(other_id.sym)
            ),
            lambda id=Identifier: n.cast(Identifier).then(
                lambda other_id: id.sym.equals(other_id.sym)
            ),
            lambda sl=StringLiteral: n.cast(StringLiteral).then(
                lambda other_sl: sl.sym.equals(other_sl.sym)
            ),
            lambda dn=DottedName: n.cast(DottedName).then(
                lambda sn: And(
                    sn.prefix.matches(dn.prefix),
                    sn.suffix.matches(dn.suffix)
                )
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
        return Self.matches(n.node)

    @langkit_property(memoized=True)
    def use_package_name_designated_env():
        """
        Assuming Self is a name that is the direct child of a
        UsePackageClause's package name list, return the memoized designated
        environment for it.
        """
        return (Entity.parent.parent.cast_or_raise(T.UsePackageClause)
                .designated_env(Self.child_index))

    relative_name = AbstractProperty(
        type=T.SingleTokNode.entity, runtime_check=True, public=True,
        doc="""
        Returns the relative name of this instance. For example,
        for a prefix ``A.B.C``, this will return ``C``.
        """
    )

    name_symbol = Property(Self.as_bare_entity.relative_name.symbol)

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def xref_no_overloading(sequential=(Bool, True),
                            all_els=(Bool, False)):
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
                all_els & Self.is_suffix,
                i.ref_var.domain(
                    Self.env_get(
                        env,
                        i.name_symbol,
                        from_node=If(sequential, Entity.node, No(T.Name)),
                        lookup=If(Self.is_prefix, LK.recursive, LK.flat),
                    ),
                ),
                Bind(
                    i.ref_var,
                    i.env_get_first_visible(
                        env,
                        from_node=If(sequential, Entity.node, No(T.Name)),
                        lookup_type=If(Self.is_prefix, LK.recursive, LK.flat)
                    )
                )
            ),

            # xref_no_overloading can be used to resolve type references in
            # generic instantiations. In that case, we might encounter a 'Class
            # attribute.
            lambda ar=T.AttributeRef:
            ar.prefix.xref_no_overloading(sequential, all_els)
            & Cond(
                ar.attribute.sym == 'Class',
                Bind(ar.prefix.ref_var, ar.ref_var,
                     conv_prop=BaseTypeDecl.classwide_type),

                ar.attribute.sym == 'Base',
                Bind(ar.prefix.ref_var, ar.ref_var,
                     conv_prop=BaseTypeDecl.scalar_base_type),

                LogicTrue()
            ),

            lambda _: LogicFalse()
        )

    @langkit_property(return_type=T.Bool, memoized=True)
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

    @langkit_property(return_type=T.Bool, memoized=True)
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

    @langkit_property(public=True)
    def is_operator_name():
        """
        Return whether the name that Self designates is an operator.
        """
        return Entity.name_symbol.any_of(
            '"="',  '"="', '"/="', '"<"', '"<="', '">"', '">="', '"and"',
            '"or"', '"xor"', '"abs"', '"*"', '"/"', '"mod"', '"rem"', '"+"',
            '"-"', '"&"' '"+"', '"-"', '"not"', '"abs"'
        )

    @langkit_property(public=True, return_type=T.Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_write_reference():
        """
        Whether this name is a write reference.

        For example, ``X`` is a write reference in the following cases:

        1. ``X := 2;``
        2. ``X (2) := 2;``
        3. ``P(F => X)`` where F is declared ``out`` or ``in out``.
        4. ``P(F => T (X))`` where F is declared ``out`` or ``in out``
        5. ``X'Access``.
        6. ``X.C := 2``, ``R.X := 2``
        7. ``X.P`` where the formal for X is declared ``out`` or ``in out``.

        .. note:: This is an experimental feature. There might be some
            discrepancy with the GNAT concept of "write reference".
        """
        return Entity.parent.match(
            # Handle assignment case::
            #     X := 2;
            lambda a=T.AssignStmt: a.dest == Entity,

            # Handle assignment to component case::
            #     X (2) := 2;
            lambda c=T.CallExpr: And(
                c.name == Entity,  # Self is the name of component access
                c.is_write_reference
            ),

            # Handle assignment to component case::
            #    X.C := 2
            #    R.X := 2
            #
            # As well as calls using the dot notation with out/inout operand.
            lambda d=T.DottedName: Or(
                # Component writes
                d.is_write_reference,

                # Dot calls with "out" first parameter
                (d.prefix == Entity) & d.suffix.called_subp_spec.then(
                    lambda spec:
                    spec.info.md.dottable_subp
                    & spec.abstract_formal_params.at(0)
                    .cast(T.ParamSpec)._.mode._.is_writable
                )
            ),

            # Handle out/inout param case
            lambda p=T.ParamAssoc: If(
                p.parent.parent.cast(CallExpr)._.is_type_conversion,
                p.parent.parent.cast(CallExpr).is_write_reference,
                p.get_params.any(
                    lambda m:
                    m.basic_decl.cast(T.ParamSpec)._.mode._.is_writable
                )
            ),

            # handle 'Access case
            lambda a=T.AttributeRef: (a.prefix == Entity) & a.is_access_attr,

            lambda l=T.AlternativesList: l.parent.is_a(AggregateAssoc),

            lambda _: False
        )

    @langkit_property()
    def potential_actuals_for_dispatch(spec=T.BaseSubpSpec.entity):
        """
        Implementation for calls done via a CallExpr, a DottedName
        or an Identifier. The result includes the prefix of the call in case
        the dot-notation is used.
        """
        # Handle calls done using the dot notation. Retrieve the prefix and
        # match it with the type of the first parameter of the called
        # subprogram.
        prefix = Var(
            Entity.is_dot_call.then(lambda _: Entity.match(
                lambda c=T.CallExpr: c.name.cast_or_raise(T.DottedName),
                lambda d=T.DottedName: d,
                lambda i=T.Identifier: i.parent.cast_or_raise(T.DottedName),
                lambda _: No(T.DottedName.entity)
            ).then(lambda d: ExpectedTypeForExpr.new(
                expected_type=spec.unpacked_formal_params.at(0)
                .formal_decl.type_expression,
                expr=d.prefix
            ).singleton))
        )

        # Handle the rest of the arguments if this is a CallExpr, matching
        # them with the types of the parameters of the called subprogram.
        args = Var(
            Entity.cast(T.CallExpr)._.params._.zip_with_params.map(
                lambda pm: ExpectedTypeForExpr.new(
                    expected_type=pm.param.basic_decl.type_expression,
                    expr=pm.actual
                )
            )
        )

        return prefix.concat(args)

    @langkit_property()
    def is_dispatching_call():
        return Or(
            Entity.is_access_call,
            Entity.is_direct_call
            & Entity.parent_callexpr.cast(T.Name.entity)
            ._or(Entity).is_dispatching_call_impl(Entity.referenced_decl)
        )

    @langkit_property(public=True, return_type=T.Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_static_call():
        """
        Returns True if this Name corresponds to a static non-dispatching call.
        In other words, this will return True if and only if the target of the
        call is known statically.

        .. note:: This is an experimental feature. There might be some
            discrepancy with the GNAT concept of "static call".
        """
        return Entity.is_call & Not(Entity.is_dispatching_call)

    @langkit_property(return_type=T.SingleTokNode.array)
    def as_single_tok_node_array():
        """
        Return the array of SingleTokNode nodes that compose this name.

        Only simple name kinds are allowed: Identifer, DottedName and
        DefiningName. Any other kind will trigger a PropertyError.
        """
        return Self.match(
            lambda dname=T.DefiningName: dname.name.as_single_tok_node_array,
            lambda tok=T.SingleTokNode: tok.singleton,
            lambda dot=T.DottedName:
            dot.prefix.as_single_tok_node_array.concat(
                dot.suffix.as_single_tok_node_array
            ),
            lambda _: PropertyError(T.SingleTokNode.array),
        )

    @langkit_property(public=True, return_type=Symbol.array)
    def as_symbol_array():
        """
        Turn this name into an array of symbols.

        For instance, a node with name ``A.B.C`` is turned into
        ``['A', 'B', 'C']``.

        Only simple name kinds are allowed: Identifer, DottedName and
        DefiningName. Any other kind will trigger a PropertyError.
        """
        return Self.as_single_tok_node_array.map(lambda t: t.symbol)

    @langkit_property(public=True, return_type=Symbol)
    def canonical_text():
        """
        Return a canonicalized version of this name's text.

        Only simple name kinds are allowed: Identifer, DottedName and
        DefiningName. Any other kind will trigger a PropertyError.
        """
        return Self.sym_join(Self.as_symbol_array, String(".")).to_symbol

    @langkit_property(kind=AbstractKind.abstract_runtime_check,
                      public=True, return_type=Bool)
    def is_constant():
        """
        Return whether this name denotes a constant value.
        """
        pass

    @langkit_property(public=True, return_type=ParamActual.array)
    def call_params():
        """
        Returns an array of pairs, associating formal parameters to actual or
        default expressions.
        """
        return If(
            Entity.is_call,

            Let(
                lambda offset=If(Entity.is_dot_call, 1, 0),
                # Get the actuals of this call expression if any
                aparams=Entity.cast(CallExpr)._.params,
                # Create an array of pairs from the subprogram formals and
                # default expressions.
                dparams=Entity.called_subp_spec
                .cast(SubpSpec)._.subp_params._.params.mapcat(
                    lambda i, p: p.defining_names.map(
                        lambda n: ParamActual.new(
                            param=n,
                            actual=If(
                                # Handling dot notation (first actual is
                                # denoted by the prefix of the dot call).
                                And(Entity.is_dot_call, i == 0),

                                Entity.cast(CallExpr).then(
                                    lambda c: c.name.cast(DottedName).prefix,
                                    default_val=Entity.cast(DottedName).prefix
                                ),

                                p.default_expr
                            )
                        )
                    )
                ):

                # Create a new array by updating the actuals if the call
                # expression provides some.
                aparams.then(
                    lambda ap: dparams.map(
                        lambda i, dp: ParamActual.new(
                            param=dp.param,
                            # Search if a named param expression exists for
                            # this formal param in the call assoc list.
                            actual=If(
                                # Handling dot notation (do not update first
                                # actual).
                                And(Entity.is_dot_call, i == 0),

                                dp.actual,

                                ap.actual_for_param_at(
                                    dp.param, i-offset, dp.actual
                                )
                            )
                        )
                    ),
                    default_val=dparams
                )
            ),

            PropertyError(
                T.ParamActual.array,
                "this name doesn't reference a call expression"
            )
        )


class DiscreteSubtypeName(Name):
    """
    Subtype name for membership test expressions (:rmlink:`3.6`).
    """

    subtype = Field(type=T.DiscreteSubtypeIndication)


class TargetName(Name):
    """
    Name for Ada 2020 ``@`` (:rmlink:`5.2.1`).
    """

    r_ref_var = UserField(LogicVar, public=False)
    ref_var = Property(Self.r_ref_var)

    assign_statement = Property(
        Self.parents.find(lambda p: p.is_a(T.AssignStmt))
        .cast_or_raise(T.AssignStmt),
        ignore_warn_on_node=True
    )

    relative_name = Property(
        Self.assign_statement.dest.as_entity.relative_name
    )

    @langkit_property()
    def xref_equation():
        return And(
            Bind(Self.type_var, Self.assign_statement.dest.type_var),
            Bind(Self.ref_var, Self.assign_statement.dest.ref_var)
        )


class CallExprKind(Enum):
    """
    Kind of CallExpr type.

    - ``call`` is when the CallExpr is a procedure or function call.
    - ``array_slice``, ``array_index`` is when the CallExpr is in fact an
      array slice or an array subcomponent access expression, respectively.
    - ``type_conversion`` is when the CallExpr is a type conversion.
    """
    call = EnumValue()
    array_slice = EnumValue()
    array_index = EnumValue()
    type_conversion = EnumValue()


class CallExpr(Name):
    """
    Represent a syntactic call expression.

    At the semantic level, this can be either a subprogram call, an array
    subcomponent access expression, an array slice or a type conversion, all
    described in :rmlink:`4.1`, except for subprogram call statements,
    described in :rmlink:`6.4`.
    """
    name = Field(type=T.Name)
    suffix = Field(type=T.AdaNode)

    ref_var = Property(Self.name.ref_var)

    r_called_spec = UserField(LogicVar, public=False)

    subp_spec_var = Property(Self.r_called_spec)
    defines_subp_spec_var = Property(True)

    relative_name = Property(Entity.name.relative_name)

    @langkit_property(public=True)
    def kind():
        """
        Return whether this expression is a subprogram call, an array
        subcomponent access expression, an array slice or a type conversion.
        """
        return Cond(
            Entity.is_call,
            CallExprKind.call,

            Entity.is_array_slice,
            CallExprKind.array_slice,

            origin.bind(
                Self.origin_node,
                Not(Entity.name.expression_type.array_def_with_deref.is_null)
            ),
            CallExprKind.array_index,

            # Case for type conversion: CallExpr has one
            # argument and its name denotes a type declaration.
            And(
                Entity.params.length == 1,
                Entity.name.referenced_decl.is_a(T.TypeDecl)
            ),
            CallExprKind.type_conversion,

            # Should not happen
            PropertyError(CallExprKind, "undetermined CallExpr kind")
        )

    @langkit_property()
    def is_constant():
        return Cond(
            Entity.kind == CallExprKind.type_conversion,
            Entity.params.at(0).expr.match(
                # View conversion: constant if the object is constant (value
                # conversion is always constant).
                lambda n=Name: n.is_constant,
                lambda _: True
            ),

            # General case that handles subprogram call, array
            # subcomponent access expression, and array slice.
            Entity.referenced_decl.is_constant_object
        )

    @langkit_property()
    def designated_env():
        typ = Var(Entity.name.name_designated_type)

        return If(
            Not(typ.is_null),

            typ.defining_env,

            # Since we are in a CallExpr, we need to include user-defined
            # indexing in defining_env of the prefix, as it might actually be
            # used here.
            include_ud_indexing.bind(
                True,
                Entity.env_elements.map(lambda e: e.match(
                    lambda bd=BasicDecl.entity:       bd.defining_env,
                    lambda _:                         EmptyEnv,
                )).env_group()
            )
        )

    @langkit_property()
    def env_elements_impl():
        return Entity.name.env_elements_impl

    # CallExpr can appear in type expressions: they are used to create implicit
    # subtypes for discriminated records or arrays.
    @langkit_property()
    def designated_type_impl():
        # Retrieve the type designated by the prefix
        prefix_tpe = Var(Entity.name.designated_type_impl)

        # Check that this CallExpr is a valid type, which in this context
        # is the case if and only if the arguments of this CallExpr match
        # the discriminant list of the type designated by the prefix.
        matches_formals = Var(Entity.params.then(
            lambda ps: Self.match_formals(
                prefix_tpe._.discriminants_list, ps, False
            ).all(
                lambda pm: And(
                    pm.has_matched,
                    pm.formal.formal_decl.formal_type.matching_type(
                        pm.actual.assoc.expr.cast(Name)._.name_designated_type
                    )
                )
            ),
            default_val=True
        ))

        # Make sure to not return the type designated by the prefix if this
        # CallExpr does not designate a type!
        return If(matches_formals, prefix_tpe, No(BaseTypeDecl.entity))

    params = Property(Entity.suffix.cast(T.AssocList))

    @langkit_property(return_type=Bool, dynamic_vars=[origin])
    def check_array_slice(typ=T.BaseTypeDecl.entity):
        """
        Return whether this CallExpr can correspond to taking a slice of the
        given array type.
        """
        atd = Var(typ.then(lambda t: t.array_def_with_deref))
        return And(
            Not(atd.is_null),
            Entity.suffix.then(
                lambda sfx: Or(
                    # array slice using the ``(A .. B)`` notation
                    sfx.is_a(BinOp),
                    # array slice using the ``(X'Range)`` notation
                    sfx.is_a(AttributeRef),
                    # array slice using the ``(Subtype range ..)`` notation
                    sfx.is_a(SubtypeIndication),
                    # array slice using the ``(Subtype)`` notation
                    sfx.cast(AssocList).then(
                        lambda al: And(
                            al.length == 1,
                            al.at(0).expr.cast(Name).then(
                                lambda n: Not(n.name_designated_type.is_null)
                            )
                        )
                    )
                )
            )
        )

    @langkit_property(public=True, return_type=Bool)
    def is_array_slice():
        """
        Return whether this CallExpr is actually an access to a slice of
        the array denoted by the prefix of this CallExpr.
        """
        return origin.bind(
            Self.origin_node,
            Entity.check_array_slice(Entity.name.expression_type)
        )

    @langkit_property(return_type=Equation)
    def xref_equation():
        return Entity.bottom_up_name_equation

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def type_conv_self_xref_equation():
        """
        Helper for xref_equation and stop_resolution_equation, handles the
        construction of the equation in type conversion cases, without the
        recursion on the argument.
        """
        return And(
            Entity.name.subtype_indication_equation,
            Bind(Self.name.ref_var, Self.name.type_var),
            Bind(Self.type_var, Self.name.ref_var)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def entry_equation(e=T.EntryDecl.entity, root=T.Name):
        """
        Build the xref equation in case this node represents a call to the
        given entry declaration.
        """
        return If(
            e.has_family,

            # Handle calls to entry families
            e.family_type.then(
                lambda ft:
                Bind(Entity.params.at(0).expr.expected_type_var, ft)
                & Entity.params.at(0).expr.matches_expected_type,

                # If the family type is None, it means it is an anonymous range
                # in which case we don't need to constrain it further.
                default_val=LogicTrue()
            )
            & Entity.parent_name(root).cast(T.CallExpr).then(
                lambda c:
                c.params.logic_all(lambda pa: pa.expr.sub_equation)
                & c.entity_equation(e, root),

                # The parent name can be null if the entry declaration has no
                # parameter section besides the family type section.
                default_val=LogicTrue()
            ),

            # If this entry decl declares no family we can treat it the same
            # way as a subprogram call.
            Entity.entity_equation(e, root)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def entity_equation(s=T.BasicDecl.entity, root=T.Name):
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
                        s.info.md.dottable_subp
                    ),
                    Entity.parent_name(root).then(
                        lambda pn:
                        pn.parent_name_equation(s.expr_type, root),
                        default_val=LogicTrue()
                    )
                )
            ),

            And(
                Entity.subprogram_equation(
                    s.subp_spec_or_null,
                    s.info.md.dottable_subp
                ),
                Entity.parent_name(root).then(
                    lambda pn:
                    pn.parent_name_equation(s.expr_type, root),
                    default_val=LogicTrue()
                )
            )
        )

    @langkit_property(return_type=Bool)
    def is_type_conversion():
        """
        Return whether this CallExpr actually represents a type conversion.
        """
        return And(
            Not(Entity.name.is_a(QualExpr)),
            Not(Entity.name.name_designated_type.is_null)
        )

    xref_stop_resolution = Property(
        Entity.super() | Entity.is_type_conversion
    )

    stop_resolution_equation = Property(If(
        Entity.is_type_conversion,
        Entity.type_conv_self_xref_equation,
        Entity.super()
    ))

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def general_xref_equation(root=T.Name):
        """
        Helper for xref_equation, handles construction of the equation in
        subprogram call cases.
        """
        return Cond(
            Entity.is_type_conversion,

            # Type conversion case
            Entity.type_conv_self_xref_equation
            & Bind(Entity.params.at(0).expr.expected_type_var,
                   No(BaseTypeDecl.entity))
            & Entity.params.at(0).expr.sub_equation
            & Entity.parent_name(root).then(
                lambda pn: pn.parent_name_equation(
                    Entity.name.name_designated_type,
                    root
                ),
                default_val=LogicTrue()
            ),

            # Attribute ref case: we can always resolve the AttributeRef first
            # without ambiguity. This allows us to use its type in order to
            # solve the rest of the expression.
            Entity.name.is_a(AttributeRef),
            Entity.name.resolve_names_internal.then(lambda _: And(
                Entity.all_args_xref_equation(root),
                Entity.name.type_val.cast(BaseTypeDecl).then(
                    lambda typ: Entity.parent_name_equation(typ, root),

                    # If the attribute has no type, it must necessarily
                    # reference a subprogram. Therefore, handle the rest as
                    # if it was an entity call.
                    default_val=Entity.entity_equation(
                        Entity.name.ref_var.get_value.cast_or_raise(BasicDecl),
                        root
                    )
                )),
                default_val=LogicFalse()
            ),

            And(
                Entity.all_args_xref_equation(root),

                # For each potential entity match, we want to express the
                # following constraints:
                Let(lambda subps=Entity.env_elements: And(
                    subps.logic_any(
                        lambda s: s.cast(EntryDecl).then(
                            lambda e: Entity.entry_equation(e, root),
                            default_val=Entity.entity_equation(
                                s.cast_or_raise(BasicDecl),
                                root
                            )
                        )
                    ),
                    Entity.name.sub_equation
                ))
                # TODO: Bug here: if operator equation, then parent equation is
                # not called!
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def subscriptable_type_equation(typ=T.BaseTypeDecl.entity):
        """
        Construct an equation verifying if Self is conformant to the type
        designator passed in parameter.
        """
        atd = Var(typ.then(lambda t: t.array_def_with_deref))
        real_typ = Var(typ.then(
            lambda t: If(t.is_implicit_deref, t.accessed_type, t))
        )

        return Cond(
            # First handle the case where this is an access to subprogram
            typ.access_def.is_a(AccessToSubpDef),
            typ.access_def.cast(AccessToSubpDef).then(
                lambda asd:
                Entity.subprogram_equation(asd.subp_spec, False),
                default_val=LogicFalse(),
            ),

            Not(atd._.indices.is_null), Entity.suffix.match(
                lambda _=T.AssocList: Or(
                    # Either an array slice through subtype indication
                    Entity.params.at(0)._.expr.cast(Name).then(
                        lambda name: If(
                            name.name_designated_type.is_null,
                            LogicFalse(),
                            name.xref_no_overloading
                            & Bind(Self.type_var, real_typ)
                        ),
                        default_val=LogicFalse()
                    ),

                    # Or a regular array access
                    Entity.params._.logic_all(
                        lambda i, pa:
                        atd.indices.constrain_index_expr(pa.expr, i)
                    )
                    & Bind(Self.type_var, atd.comp_type)
                ),

                # Explicit slice access
                lambda bo=T.BinOp:
                atd.indices.constrain_index_expr(bo.left, 0)
                & atd.indices.constrain_index_expr(bo.right, 0)
                & Bind(bo.expected_type_var, bo.right.expected_type_var)
                & Bind(Self.type_var, real_typ)
                & bo.left.sub_equation
                & bo.right.sub_equation,

                # Range attribute
                lambda ar=T.AttributeRef:
                ar.sub_equation
                & atd.indices.constrain_index_expr(ar, 0)
                & Bind(Self.type_var, real_typ),

                # Subtype indication
                lambda st=T.SubtypeIndication:
                st.sub_equation
                & Bind(Self.type_var, real_typ),

                lambda _: LogicFalse(),
            ),

            # Type has user defined indexing
            Not(typ.is_null) & typ.has_ud_indexing,
            typ.constant_indexing_fns.concat(typ.variable_indexing_fns)
            .logic_any(lambda fn: Let(
                lambda
                formal=fn.subp_spec_or_null.unpacked_formal_params.at(1),
                ret_type=fn.subp_spec_or_null.return_type,
                param=Entity.params.at(0).expr:

                Bind(Self.type_var, ret_type)
                & Bind(param.expected_type_var, formal.formal_decl.formal_type)
                & param.matches_expected_type
            )),

            LogicFalse()
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def subprogram_equation(subp_spec=T.BaseFormalParamHolder.entity,
                            dottable_subp=Bool):
        return subp_spec.then(
            lambda subp_spec:
            # The type of the expression is the expr_type of the
            # subprogram.
            Bind(Self.type_var, subp_spec.cast(BaseSubpSpec)._.return_type)

            # This node represents a call to a subprogram which specification
            # is given by ``subp_spec``.
            & Bind(Self.subp_spec_var, subp_spec)

            # For each parameter, the type of the expression matches
            # the expected type for this subprogram.
            & subp_spec.match_param_list(
                Entity.params, dottable_subp
            ).logic_all(
                lambda pm: If(
                    pm.has_matched,
                    subp_spec.call_argument_equation(
                        pm.formal.formal_decl,
                        pm.actual.assoc.expr
                    ) & If(
                        # Bind actuals designators to parameters if there
                        # are designators.
                        pm.actual.name.is_null,
                        LogicTrue(),
                        Bind(
                            pm.actual.name.ref_var,
                            Let(lambda n=pm.formal.formal_decl:
                                Entity.entity_no_md(
                                    n.node,
                                    n.info.rebindings,
                                    n.info.from_rebound
                                ))
                        )
                    ),
                    LogicFalse()
                )
            ),
            default_val=LogicFalse()
        )

    @langkit_property(return_type=Bool, dynamic_vars=[env, origin])
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

        return origin.bind(Self.origin_node, typ.then(lambda typ: And(
            Or(
                # Arrays
                atd.then(lambda _: Self.suffix.match(
                    # Array indexing case
                    lambda al=AssocList: atd.array_ndims == al.length,

                    # Array slice cases
                    lambda _=BinOp: atd.array_ndims == 1,
                    lambda _=SubtypeIndication: atd.array_ndims == 1,
                    lambda _=AttributeRef: atd.array_ndims == 1,

                    lambda _: False
                ), default_val=False),

                # Accesses to subprograms
                typ.access_def.cast(T.AccessToSubpDef).then(
                    lambda sa:
                    sa.subp_spec.is_matching_param_list(Entity.params, False)
                ),

                # Types with user defined indexing
                typ.has_ud_indexing
                & Self.suffix.cast(T.AssocList).then(lambda al: al.length == 1)
            ),

            Entity.parent.cast(T.CallExpr).then(
                # Since the result type of Self is ``typ``, the result type of
                # its parent CallExpr (if it exists) must be the component type
                # of ``typ``, except in case of an array slice.
                # Note: we use subscript=True because a CallExpr will
                # dereference implicitly.
                lambda ce: ce.check_for_type(If(
                    Entity.check_array_slice(typ),
                    typ,
                    typ.comp_type(is_subscript=True)
                )),

                # We are done if the parent is not a CallExpr. We could
                # actually do more here by considering ExplicitDerefs, but
                # this should be sufficient for the current purpose of
                # check_for_type (e.g. to preemptively discard inadequate
                # candidates in env_elements_baseid).
                default_val=True
            )
        )))


class ParamAssoc(BasicAssoc):
    """
    Assocation (X => Y) used for parameter associations (:rmlink:`6.4`).
    """
    designator = Field(type=T.AdaNode)
    r_expr = Field(type=T.Expr)

    expr = Property(Entity.r_expr)
    names = Property(If(Self.designator.is_null,
                        No(T.AdaNode.array), Self.designator.singleton))

    xref_entry_point = Property(Self.is_static_attribute_assoc)
    xref_equation = Property(If(
        Self.xref_entry_point,
        Entity.expr.sub_equation,
        LogicFalse()
    ))

    @langkit_property(return_type=Bool)
    def is_static_attribute_assoc():
        return Self.parent.parent.cast(AttributeRef).then(
            lambda ar: ar.attribute.name_symbol.any_of(
                'First', 'Last', 'Range', 'Length'
            )
        )


class AggregateAssoc(BasicAssoc):
    """
    Assocation (X => Y) used for aggregates associations (:rmlink:`4.3`).
    """
    designators = Field(type=T.AlternativesList)
    r_expr = Field(type=T.Expr)

    expr = Property(Entity.r_expr)
    names = Property(Self.designators.map(lambda d: d))

    xref_stop_resolution = Property(True)

    base_aggregate = Property(
        Entity.parent.parent.cast_or_raise(BaseAggregate)
    )

    @langkit_property()
    def xref_equation():
        agg = Var(Entity.base_aggregate)

        mra = Var(agg.multidim_root_aggregate)

        # If we're part of a multidim aggregate, then take the root aggregate's
        # type. Else, this is a regular aggregate. In this case grab the type
        # in type_val.
        td = Var(If(
            Not(mra.is_null),
            mra.typ,
            Entity.base_aggregate.type_val.cast(BaseTypeDecl),
        ))

        atd = Var(td._.array_def)

        return Cond(
            agg.in_aspect('Global') | agg.in_aspect('Refined_Global'),
            Entity.globals_assoc_equation,

            agg.in_aspect('Depends') | agg.in_aspect('Refined_Depends'),
            Entity.depends_assoc_equation,

            agg.in_aspect('Test_Case'),
            Entity.test_case_assoc_equation,

            agg.in_aspect('Refined_State'),
            # Simply resolve all names present in the Refined_State aspect,
            # as they must all refer to existing declarations.
            Entity.exprs_assoc_equation,

            agg.in_aspect('Contract_Cases'),
            Entity.contract_cases_assoc_equation,

            agg.parent.is_a(AspectClause, AspectAssoc, PragmaArgumentAssoc),
            LogicTrue(),

            atd.is_null,
            Entity.record_assoc_equation,
            Entity.array_assoc_equation(atd, mra)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def record_assoc_equation():
        """
        Equation for the case where this is an aggregate assoc for a record
        type.
        """
        agg = Var(Entity.base_aggregate)

        # First, try to find all the discriminants matched by this assoc
        discr_matches = Var(agg.matched_discriminants.filter(
            lambda pm: pm.actual.assoc == Entity
        ))

        # If there are none, this assoc matches one or several components of
        # the record, so gather them.
        # WARNING: It is important to gather these components ONLY IF this
        # association is not for specifying a discriminant. Indeed,
        # discriminants can (and must) be resolved separately once the type of
        # the aggregate is known. Otherwise, name resolution will enter an
        # infinite loop when trying to match an available component for this
        # association, as it requires statically evaluating discriminants which
        # involves doing name resolution on them, thus introducing a cycle.
        matches = Var(If(
            Not(discr_matches.is_null),

            discr_matches,

            agg.matched_components.filter(
                lambda pm: pm.actual.assoc == Entity
            )
        ))

        # Whether this is the `others => ...` association
        is_others_assoc = Var(Entity.names.any(
            lambda n: n.is_a(OthersDesignator)
        ))

        return If(
            Not(is_others_assoc),

            matches.logic_all(lambda match: And(
                Bind(
                    match.actual.assoc.expr.expected_type_var,
                    match.formal.formal_decl.type_expression.designated_type
                ),
                match.actual.assoc.expr.sub_equation,
                match.actual.assoc.expr.matches_expected_type,
                match.actual.name.then(
                    lambda n: Bind(n.ref_var, match.formal.formal_decl),
                    LogicTrue()
                )
            )),

            # Since all the formals designated by "others" should have the same
            # type, we look for the first formal that was not yet matched and
            # use its type as the type of the expression associated to
            # "others".
            agg.first_unmatched_formal.then(
                lambda unmatched_formal:
                Bind(
                    Entity.expr.expected_type_var,
                    unmatched_formal.formal_decl
                    .type_expression.designated_type
                )
                & Entity.expr.sub_equation
                & Entity.expr.matches_expected_type,
                default_val=LogicTrue()
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def array_assoc_equation(atd=ArrayTypeDef.entity,
                             mra=MultidimAggregateInfo):
        """
        Equation for the case where this is an aggregate assoc for an array
        type.
        """
        return And(
            If(
                # If the array is monodimensional, or we're on the last
                # dimension of a multidimensional array ..
                Or(mra.is_null, mra.rank == atd.array_ndims - 1),

                # .. Then we want to match the component type
                Bind(Entity.expr.expected_type_var, atd.comp_type)
                & Entity.expr.sub_equation
                & Entity.expr.matches_expected_type,

                # .. Else we're on an intermediate dimension of a
                # multidimensional array: do nothing.
                LogicTrue()
            ),

            Entity.designators.logic_all(
                lambda n: n.sub_equation & If(
                    Not(n.cast(Name)._.name_designated_type.is_null),

                    n.cast(Name).xref_no_overloading,

                    n.cast(T.Expr).then(
                        lambda n:
                        Bind(n.expected_type_var, atd.index_type(mra.rank))
                        & n.matches_expected_type,
                        default_val=LogicTrue()
                    )
                )
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def globals_assoc_equation():
        """
        Equation for the case where this is an aggregate assoc for a Globals
        aspect.
        """
        # Assoc expr can either be a name or an aggregate. If a name, then
        # resolve. If an aggregate, resolution will be handled recursively
        # by solve.
        return Entity.expr.sub_equation

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def depends_assoc_equation():
        """
        Equation for the case where this is an aggregate assoc for a Depends
        aspect.
        """
        return And(
            # For both the name and the expr, same as in `globals_equation`, we
            # call sub_equation: If it's a name it will resolve the name. If
            # it's an aggregate it will return LogicTrue() and the content will
            # be resolved separately.
            Entity.expr.sub_equation,

            # Here, we go fetch the first element of the list of names. Since
            # we parse this as an aggregate, the list is elements separated by
            # pipes (alternatives_list), which will ever only have one element
            # in this case. We make sure to only resolve Identifiers, because
            # the ``null`` literal is also possible here and we don't want
            # to resolve it.
            Entity.names.at(0).cast(Identifier).as_entity.then(
                lambda n: n.sub_equation, default_val=LogicTrue()
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def test_case_assoc_equation():
        """
        Equation for the case where this is an aggregate assoc for a Test_Case
        aspect.
        """
        return If(
            # Only resolve the right-hand side of `Requires` and `Ensures`,
            # the other associations (Name and Mode) need not be resolved.
            Entity.names.at(0).cast(BaseId).then(
                lambda name: name.name_symbol.any_of(
                    'Requires', 'Ensures'
                )
            ),
            Entity.expr.sub_equation,
            LogicTrue()
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def contract_cases_assoc_equation():
        """
        Equation for the case where this is an aggregate assoc for a
        Contract_Cases aspect. Both the ``guard`` and the ``consequence`` must
        be of type Boolean.
        """
        return And(
            Entity.designators.logic_all(
                lambda d: d.cast(Expr).then(
                    lambda e:
                    Bind(e.expected_type_var, Self.bool_type)
                    & e.sub_equation
                    & e.matches_expected_formal_prim_type,

                    # Nothing to do for `others =>`
                    default_val=LogicTrue()
                )
            ),
            Bind(Entity.expr.expected_type_var, Self.bool_type),
            Entity.expr.sub_equation,
            Entity.expr.matches_expected_formal_prim_type
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def exprs_assoc_equation():
        """
        Return the xref equation for the case where this is an aggregate assoc
        in which all the designator as well as the RHS are usual expressions
        which can be recursively resolved.
        """
        return And(
            Entity.designators.logic_all(lambda d: d.sub_equation),
            Entity.expr.cast(Name).then(
                lambda n: n.sub_equation,
                default_val=LogicTrue()
            )
        )


class IteratedAssoc(BasicAssoc):
    """
    Iterated association (Ada 2020, :rmlink:`4.3.3`).
    """
    spec = Field(type=T.ForLoopSpec)
    r_expr = Field(type=T.Expr)

    expr = Property(Entity.r_expr)
    names = Property(No(T.AdaNode.array))

    base_aggregate = Property(
        Entity.parent.parent.cast_or_raise(BaseAggregate)
    )

    xref_stop_resolution = Property(Entity.parent.parent.is_a(BaseAggregate))

    @langkit_property()
    def xref_equation():
        aggregate = Var(Entity.base_aggregate)

        root_agg = Var(aggregate.multidim_root_aggregate)

        # If we're part of a multidim aggregate, then take the root aggregate's
        # type. Else, this is a regular aggregate. In this case grab the type
        # in type_val.
        type_decl = Var(If(
            Not(root_agg.is_null),
            root_agg.typ,
            aggregate.type_val.cast(BaseTypeDecl),
        ))

        array_type_def = Var(type_decl._.array_def)

        # NOTE: we need to resolve the spec first so that the indexing variable
        # has a type when resolving `r_expr`.
        # NOTE: if the form of the iterated_component_association is
        # `for I in ..`, Ada requires the type of I to be the index type of the
        # array (taking dimension into account) which we are building an
        # aggregate for.
        spec_success = Var(Entity.spec.resolve_names_internal_with_eq(
            If(Self.spec.loop_type.is_a(IterType.alt_in),
               Bind(Entity.spec.iter_expr.cast(Expr).expected_type_var,
                    array_type_def.index_type(root_agg.rank))
               & Entity.spec.iter_expr.cast(Expr).matches_expected_type,
               LogicTrue())
        ))

        return If(
            spec_success,
            If(
                # If the array is monodimensional, or we're on the last
                # dimension of a multidimensional array ..
                Or(root_agg.is_null,
                   root_agg.rank == array_type_def.array_ndims - 1),

                # .. Then we want to match the component type
                Entity.expr.sub_equation
                & Bind(Entity.expr.expected_type_var, array_type_def.comp_type)
                & Entity.expr.matches_expected_type,

                # .. Else we're on an intermediate dimension of a
                # multidimensional array: do nothing.
                LogicTrue()
            ),
            LogicFalse()
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def xref_equation_for_reduce():
        """
        Equation specialization for ``ValueSequence`` name resolution (part of
        ``ReduceAttributeRef``).
        """
        # The expected type is the expected type of the ReduceAttributeRef
        # holding this iterated assoc.
        expected_typ = Var(Entity.parent.cast(ValueSequence).iter_assoc.expr
                           .expected_type_var)

        # See Self.xref_equation for more details
        spec_success = Var(Entity.spec.resolve_names_internal_with_eq(
            If(Self.spec.loop_type.is_a(IterType.alt_in),
               Bind(Entity.spec.iter_expr.cast(Expr).expected_type_var,
                    expected_typ)
               & Entity.spec.iter_expr.cast(Expr).matches_expected_type,
               LogicTrue())
        ))

        return If(spec_success,
                  Entity.expr.sub_equation
                  & Bind(Entity.expr.expected_type_var, expected_typ)
                  & Entity.expr.matches_expected_type,
                  LogicFalse())


class MultiDimArrayAssoc(AggregateAssoc):
    """
    Association used for multi-dimension array aggregates.
    """
    pass


class AssocList(BasicAssoc.list):
    """
    List of associations.
    """

    @langkit_property(return_type=T.Expr.entity)
    def actual_for_param_at(param=T.DefiningName.entity,
                            pos=T.Int,
                            default_expr=(T.Expr.entity, No(T.Expr.entity))):
        """
        Return the actual expression for ``param`` if any, ``default_expr``
        otherwise.
        """
        up = Var(Entity.unpacked_params)

        return up.find(
            # Search expression for parameter `param` if a named one exists
            lambda p: p.name._.matches(param.name.node)
        ).then(
            lambda a: a.assoc.expr,
            # Otherwise, get the parameter using its position if any
            default_val=If(
                Or(
                    up.at(pos).is_null,
                    Not(up.at(pos).assoc.names.is_null)
                ),
                # None was found, either by name or by position, return
                # default expression.
                default_expr,
                # Use expression for param by position
                up.at(pos).assoc.expr
            )
        )

    @langkit_property(memoized=True)
    def unpacked_params():
        """
        Given the list of ParamAssoc, that can in certain case designate
        several actual parameters at once, create an unpacked list of
        SingleActual instances.
        """
        return Entity.mapcat(lambda pa: Let(lambda names=pa.names: If(
            names.length == 0,
            SingleActual.new(name=No(Identifier), assoc=pa).singleton,
            names.filtermap(
                lambda i: SingleActual.new(name=i.cast(T.BaseId), assoc=pa),
                lambda n: n.is_a(T.BaseId),
            )
        )))

    @langkit_property(public=True, return_type=ParamActual.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def zip_with_params():
        """
        Returns an array of pairs, associating formal parameters to actual
        expressions. The formals to match are retrieved by resolving the call
        which this AssocList represents the actuals of.
        """
        # Bind imprecise_fallback to False for now because
        # first_corresponding_decl is not implemented on CallExpr.
        is_dottable_subp = Var(imprecise_fallback.bind(
            False, Entity.parent.cast(T.Name).then(lambda e: e.is_dot_call)
        ))

        params = Var(Entity.parent._.match(
            lambda e=T.CallExpr: e.called_subp_spec._.abstract_formal_params,

            lambda i=T.GenericInstantiation:
            i.generic_entity_name.referenced_decl.cast(T.GenericDecl)
            ._.formal_part.abstract_formal_params,

            lambda c=T.CompositeConstraint:
            c.subtype._.discriminants_list,

            lambda a=T.BaseAggregate: origin.bind(Self, env.bind(
                Self.node_env,
                a.expression_type.record_def
                ._.components.abstract_formal_params_for_assocs(Entity),
            )),

            lambda _: No(T.BaseFormalParamDecl.entity.array)
        ))

        others_assoc = Var(Entity.find(
            lambda assoc: assoc.names.any(
                lambda n: n.is_a(OthersDesignator)
            )
        ))

        explicit_matches = Var(params.then(
            lambda _: Self.match_formals(params, Entity, is_dottable_subp).map(
                lambda m: ParamActual.new(
                    param=m.formal,
                    actual=m.actual.assoc.expr
                ),
            ))
        )

        default_subp_matches = Var(params.then(lambda _: params.filtermap(
            # Append implicit actuals of formal subprograms that have a
            # default value (box expression of explicit reference).
            lambda p: Let(
                lambda
                decl=p.cast(GenericFormalSubpDecl),
                subp=p.cast(GenericFormalSubpDecl).decl.cast(FormalSubpDecl):

                ParamActual.new(
                    param=decl.defining_name,
                    actual=If(
                        subp.default_expr.is_a(Name),
                        subp.default_expr,
                        subp.designated_subprogram_from(
                            inst=Entity.parent.cast(GenericInstantiation)
                        )._.defining_name
                    )
                )
            ),
            lambda p: p.cast(GenericFormalSubpDecl).then(
                lambda fd: fd.decl.cast(FormalSubpDecl).then(
                    lambda subp: And(
                        # Generate a new match for formal subprogram which
                        # have a default value.
                        subp.default_expr.is_a(BoxExpr, Name),

                        # unless they have already have an explicit match
                        Not(explicit_matches.any(
                            lambda m: m.param == subp.defining_name
                        ))
                    )
                )
            )
        )))

        given_matches = Var(explicit_matches.concat(default_subp_matches))

        others_matches = Var(others_assoc.then(
            lambda oa: Self.unpack_formals(params).filtermap(
                lambda p: ParamActual.new(
                    param=p,
                    actual=oa.expr
                ),
                lambda p: Not(given_matches.any(lambda m: m.param == p))
            )
        ))

        return given_matches.concat(others_matches)


class DeclList(AdaNode.list):
    """
    List of declarations.
    """
    pass


class StmtList(AdaNode.list):
    """
    List of statements.
    """
    pass


class ExplicitDeref(Name):
    """
    Explicit dereference expression (``.all``) (:rmlink:`4.1`).
    """

    prefix = Field(type=T.Name)
    ref_var = Property(Self.prefix.ref_var)

    r_called_spec = UserField(LogicVar, public=False)

    subp_spec_var = Property(Self.r_called_spec)
    defines_subp_spec_var = Property(True)

    relative_name = Property(Entity.prefix.relative_name)

    @langkit_property()
    def designated_env():
        # Since we have implicit dereference in Ada, everything is directly
        # accessible through the prefix, so we just use the prefix's env.
        return Entity.prefix.designated_env

    @langkit_property()
    def env_elements_impl():
        prefix = Var(Entity.prefix)
        env_els = Var(prefix.env_elements_impl)
        return If(
            prefix.cast(AttributeRef)._.is_access_attr,
            env_els,
            origin.bind(Self.origin_node, env_els.filter(
                # Env elements for access derefs need to be of an access type
                lambda e: e.cast(BasicDecl)._.expr_type.then(
                    lambda t: t.is_access_type
                )
            ))
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def eq_for_type(typ=T.BaseTypeDecl.entity):
        return If(
            typ.is_access_type,

            Bind(Self.prefix.expected_type_var, typ)
            & Entity.prefix.matches_expected_type
            & Bind(Self.type_var, typ.accessed_type),

            LogicFalse()
        )

    @langkit_property()
    def xref_equation():
        return Entity.bottom_up_name_equation

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def general_xref_equation(root=(T.Name, No(T.Name))):
        return And(
            Entity.all_args_xref_equation(root),
            Entity.prefix.sub_equation,
            Entity.env_elements.logic_any(
                lambda el: el.cast(T.BasicDecl).expr_type.then(
                    lambda typ:
                    Bind(Self.ref_var, el)
                    & Bind(Self.ref_var, Self.prefix.ref_var)
                    & Entity.parent_name_equation(
                        If(Entity.prefix.cast(AttributeRef)._.is_access_attr,
                           typ.anonymous_access_type,
                           typ),
                        root
                    ),
                    default_val=LogicFalse()
                )
            )
        )

    @langkit_property()
    def is_constant():
        # The dereference expression is constant if its access type is
        # constant.
        return origin.bind(
            Self,
            Entity.prefix.expression_type._.access_def
            .cast(T.TypeAccessDef)._.has_constant.as_bool
        )


class BoxExpr(Expr):
    """
    Box expression (``<>``).

    This is not an expression per-se in Ada, but treating it as one helps us
    keep coherent types in some cases, like aggregates expressions.
    """
    xref_equation = Property(Bind(Self.type_var, Self.expected_type_var))


class OthersDesignator(AdaNode):
    """
    ``other`` designator.
    """
    xref_equation = Property(LogicTrue())


@abstract
class CondExpr(Expr):
    """
    Base class for a conditional expressions (:rmlink:`4.5.7`).
    """

    @langkit_property(public=True,
                      return_type=T.Expr.entity.array,
                      kind=AbstractKind.abstract)
    def dependent_exprs():
        """
        Return the dependent expressions for this conditional expression.
        """
        pass


class IfExpr(CondExpr):
    """
    ``if`` expression (:rmlink`4.5.7`).
    """

    cond_expr = Field(type=T.Expr)
    then_expr = Field(type=T.Expr)
    alternatives = Field(type=T.ElsifExprPart.list)
    else_expr = Field(type=T.Expr)

    has_context_free_type = Property(False)

    @langkit_property()
    def dependent_exprs():
        return Entity.then_expr.singleton.concat(
            Entity.alternatives.map(lambda a: a.then_expr)
        ).concat(Entity.else_expr.singleton)

    @langkit_property()
    def xref_equation():
        return (
            # Construct sub equations for common sub exprs
            Bind(Self.cond_expr.expected_type_var, Self.bool_type)
            & Entity.cond_expr.sub_equation
            & Entity.cond_expr.matches_expected_formal_prim_type

            # Construct the equation for the then branch
            & Entity.then_expr.sub_equation

            & Entity.alternatives.logic_all(
                lambda elsif:
                # Build the sub equations for cond and then exprs
                Bind(elsif.cond_expr.expected_type_var, Self.bool_type)
                & elsif.cond_expr.sub_equation
                & elsif.cond_expr.matches_expected_formal_prim_type
                & elsif.then_expr.sub_equation
            )

            & If(
                Not(Self.else_expr.is_null),

                # If there is an else, then construct sub equation
                Entity.else_expr.sub_equation,

                # If no else, then the then_expression has type bool
                Bind(Self.then_expr.expected_type_var, Self.bool_type)
                & Entity.then_expr.matches_expected_formal_prim_type
            )

            & Or(
                Predicate(AdaNode.is_not_null, Self.expected_type_var)
                & Entity.expected_type_equation,

                Bind(Self.expected_type_var, No(BaseTypeDecl))
                & Entity.no_expected_type_equation
            )
        )

    @langkit_property(dynamic_vars=[env, origin])
    def expected_type_equation():
        """
        Return the equation to use in the case where the expected type for this
        if-expression is known. In that case, we can use it to infer the
        branches' types.
        """
        return Bind(Self.type_var, Self.expected_type_var) & If(
            Not(Self.else_expr.is_null),

            Bind(Self.else_expr.expected_type_var, Self.expected_type_var)
            & Bind(Self.then_expr.expected_type_var, Self.expected_type_var)
            & Entity.else_expr.matches_expected_formal_type
            & Entity.then_expr.matches_expected_formal_type,

            LogicTrue()
        ) & Entity.alternatives.logic_all(
            lambda elsif:
            Bind(elsif.then_expr.expected_type_var, Self.expected_type_var)
            & elsif.then_expr.matches_expected_formal_type
        )

    @langkit_property(dynamic_vars=[env, origin])
    def no_expected_type_equation():
        """
        Return the equation to use when the expected type is not known, for
        example if we are inside a type conversion. In that case, we'll infer
        the type of the if expression by taking the common base subtype of the
        context-free types of all the sub-branches.
        """
        return Self.then_expr.singleton.concat(
            Self.alternatives.map(lambda a: a.then_expr)
        ).concat(
            Self.else_expr._.singleton
        ).filter(
            lambda e: e.has_context_free_type
        ).logic_all(
            lambda e:
            Predicate(BaseTypeDecl.is_universal_type, e.type_var)
            | Bind(e.type_var, Self.type_var,
                   conv_prop=BaseTypeDecl.base_subtype)
        )


class ElsifExprPart(AdaNode):
    """
    ``elsif`` block, part of an ``if`` expression.
    """
    cond_expr = Field(type=T.Expr)
    then_expr = Field(type=T.Expr)


class CaseExpr(CondExpr):
    """
    ``case`` expression (:rmlink:`4.5.7`).
    """
    expr = Field(type=T.Expr)
    cases = Field(type=T.CaseExprAlternative.list)

    has_context_free_type = Property(False)

    @langkit_property()
    def dependent_exprs():
        return Entity.cases.map(lambda c: c.expr)

    @langkit_property()
    def xref_equation():
        # We solve Self.expr separately because it is not dependent on the rest
        # of the semres.
        ignore(Var(Entity.expr.resolve_names_internal_with_eq(
            Predicate(BaseTypeDecl.is_discrete_type, Self.expr.type_var)
        )))

        return And(
            Bind(Self.type_var, Self.expected_type_var),
            Entity.cases.logic_all(
                lambda alt:

                alt.choices.logic_all(lambda c: c.match(
                    # Expression case
                    lambda e=T.Expr: If(
                        Not(e.cast(Name)._.name_designated_type.is_null),

                        e.cast(Name).xref_no_overloading,

                        Bind(e.expected_type_var, Self.expr.type_val)
                        & e.sub_equation
                        & e.matches_expected_type
                    ),

                    # SubtypeIndication case (``when Color range Red .. Blue``)
                    lambda t=T.SubtypeIndication: t.xref_equation,

                    lambda _=T.OthersDesignator: LogicTrue(),

                    lambda _: PropertyError(T.Equation, "Should not happen")
                ))

                # Equations for the dependent expressions
                & Bind(Self.expected_type_var, alt.expr.expected_type_var)
                & alt.expr.sub_equation
                & alt.expr.matches_expected_type
            )
        )


class CaseExprAlternative(Expr):
    """
    Alternative in a ``case`` expression (``when ... => ...``).
    """
    choices = Field(type=T.AlternativesList)
    expr = Field(type=T.Expr)


@abstract
class SingleTokNode(Name):
    """
    Base class for nodes that are made up of a single token.
    """

    token_node = True

    relative_name = Property(Entity)

    @langkit_property(return_type=LogicVar, external=True,
                      uses_entity_info=False, uses_envs=False)
    def subp_spec_var():
        pass

    @langkit_property(return_type=LogicVar, external=True,
                      uses_entity_info=False, uses_envs=False)
    def ref_var():
        pass

    defines_subp_spec_var = Property(True)

    sym = Property(
        Self.symbol,
        doc="""
        Shortcut to get the symbol of this node. We keep this short form, even
        though the public property canonical_text is equivalent because it is
        very used inside of the internal properties
        """
    )

    @langkit_property()
    def canonical_text():
        return Self.sym

    @langkit_property(dynamic_vars=[default_no_visibility()])
    def env_get_first_visible(lex_env=LexicalEnv,
                              lookup_type=LK,
                              from_node=T.AdaNode):
        """
        Like env.get_first, but returning the first visible element in the Ada
        sense.

        If ``no_visibility``, discard visibility checks.
        """
        return Self.env_get(
            lex_env,
            Self.symbol,
            lookup=lookup_type,
            from_node=from_node,
            categories=no_prims
            # If no_visibility, then don't check visibility, (so return the
            # first).
        ).find(lambda el: no_visibility | Self.has_visibility(el))


class DefiningName(Name):
    """
    Name that defines an entity (:rmlink:`3.1`).
    """
    name = Field(type=T.Name)

    parent_scope = Property(Self.name.parent_scope)
    scope = Property(Self.name.scope)
    relative_name = Property(Entity.name.relative_name)
    ref_var = Property(Self.name.ref_var)
    env_elements_impl = Property(Entity.name.env_elements_impl)

    @langkit_property(return_type=T.BaseFormalParamDecl.entity)
    def formal_decl():
        """
        Return the parent ``BaseFormalParamDecl`` of this ``DefiningName``.
        Raise an error otherwise.
        """
        return Entity.parents.find(
            lambda n: n.is_a(BaseFormalParamDecl)
        ).cast_or_raise(T.BaseFormalParamDecl)

    @langkit_property(public=True, return_type=T.String)
    def canonical_fully_qualified_name():
        """
        Return a canonical representation of the fully qualified name
        corresponding to this defining name.
        """
        return Entity.basic_decl.canonical_fully_qualified_name_impl(dn=Entity)

    @langkit_property(public=True, return_type=T.String)
    def unique_identifying_name():
        """
        Return a unique identifying name for this defining name, provided this
        declaration is a public declaration. In the case of subprograms, this
        will include the profile.

        .. attention::
            This will only return a unique name for public declarations.
            Notably, anything nested in an unnamed declare block won't be
            handled correctly.
        """
        return Entity.basic_decl.unique_identifying_name_impl(Entity)

    @langkit_property(public=True, return_type=T.Symbol.array)
    def fully_qualified_name_array():
        """
        Return the fully qualified name corresponding to this defining name, as
        an array of symbols.
        """
        return Entity.basic_decl.fully_qualified_name_impl(
            dn=Entity
        ).map(lambda t: t.to_symbol)

    @langkit_property(public=True, return_type=T.String)
    def fully_qualified_name():
        """
        Return the fully qualified name corresponding to this defining name.
        """
        return String(".").join(
            Entity.basic_decl.fully_qualified_name_impl(dn=Entity)
        )

    @langkit_property()
    def all_env_els_impl(
            seq=(Bool, True),
            seq_from=(AdaNode, No(T.AdaNode)),
            categories=(T.RefCategories, all_categories)
    ):
        return Entity.name.all_env_els_impl(seq, seq_from, categories)

    basic_decl = Property(
        Self.parents.find(lambda p: p.is_a(T.BasicDecl))
        .cast_or_raise(T.BasicDecl).as_entity,
        public=True, memoized=True,
        doc="Returns this DefiningName's basic declaration"
    )

    basic_decl_no_internal = Property(
        Entity.basic_decl.then(
            lambda bd: If(
                bd.is_a(GenericPackageInternal, GenericSubpInternal,
                        SingleTaskTypeDecl),
                bd.parent.cast_or_raise(BasicDecl),
                bd
            )
        ),
        doc="Returns this DefiningName's basic declaration but discard "
            "intermediate internal nodes."
    )

    @langkit_property(public=True, return_type=T.RefResult.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def find_refs(root=T.AdaNode.entity):
        """
        Find all references to this defining name in the given ``root`` and its
        children.
        """
        return Entity.canonical_part.find_refs_impl(root, Self)

    @langkit_property(return_type=T.RefResult.array,
                      dynamic_vars=[imprecise_fallback])
    def find_refs_impl(root=T.AdaNode.entity, skip_name=T.DefiningName):
        """
        Internal implementation for find_refs. Find all references to Self in
        the given ``root``. The ``skip_name`` is used to filter out a
        DefiningName from the result (typically, the name of Self in order to
        avoid to report a reference to itself).
        """
        # TODO: Factor the traversal between this and `find_derived_types`
        return root.children.then(
            lambda c: c.filter(
                lambda n: Not(n.is_null | (n.node == skip_name))
            ).mapcat(lambda n: Entity.find_refs_impl(n, skip_name))
        ).concat(
            root.cast(BaseId).then(
                lambda id: Entity.is_referenced_by(id)
                .then(lambda ref_kind: If(
                    ref_kind.any_of(
                        RefResultKind.precise, RefResultKind.imprecise
                    ),
                    RefResult.new(ref=id, kind=ref_kind).singleton,
                    No(RefResult.array)
                ))
            )
        )

    @langkit_property(return_type=Bool, memoized=True)
    def is_derivable_equal():
        """
        Return whether this is a name that defines an "=" operator which
        implicitly declares an "/=" operator giving the complementary result,
        which is True iff this "=" declaration returns a Boolean
        (:rmlink:`6.6` 6/3).
        """
        return And(
            Self.name_is('"="'),
            Entity.basic_decl.subp_spec_or_null.then(
                lambda s: s.returns._.designated_type_decl == Entity.bool_type
            )
        )

    @langkit_property(return_type=Bool, memoized=True)
    def is_potential_reference(symbol=T.Symbol):
        """
        Return whether the given symbol could be a reference to this defining
        name.
        """
        return Or(
            Self.name_is(symbol),
            Entity.is_derivable_equal & (symbol == '"/="')
        )

    @langkit_property(return_type=RefResultKind,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_referenced_by(id=T.BaseId.entity):
        """
        Returns True iff the given node is an identifier referring to Self.
        Note that this takes into account both direct references as well as
        potential references.

        Potential references can occur in the context of dispatching calls: an
        identifier having for direct reference the declaration of an
        overridable subprogram is considered a potential reference to all
        subprograms that override it if the identifier appears in a dispatching
        call.
        """
        return If(
            Entity.is_potential_reference(id.name_symbol),

            If(id.is_defining,
               RefdDef.new(def_name=id.enclosing_defining_name,
                           kind=RefResultKind.precise),
               id.failsafe_referenced_def_name)

            .then(
                lambda def_res: Let(
                    lambda canon=def_res.def_name._.canonical_part._.node:

                    If(
                        Or(
                            # Either `id` is a direct reference
                            (canon == Self),

                            # Or `id` refers to one of the base subprograms of
                            # defined by Self, and `x` appears in a dispatching
                            # call context.
                            Entity.basic_decl.base_subp_declarations.then(
                                lambda decls: decls.any(
                                    lambda d: d.defining_name.node == canon
                                )
                                & id.is_dispatching_call
                            ),
                        ),
                        def_res.kind,
                        RefResultKind.no_ref
                    )
                )
            ),

            RefResultKind.no_ref
        )

    @langkit_property(public=True, return_type=T.RefResult.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def find_all_references(units=AnalysisUnit.array,
                            follow_renamings=(Bool, False)):
        """
        Searches all references to this defining name in the given list of
        units.

        If ``follow_renamings`` is True, also this also includes references
        that ultimately refer to this defining name, by unwinding renaming
        clauses.
        """
        dn = Var(Entity.canonical_part)

        # If `dn` defines a subprogram which overrides some subprogram P, we
        # need to do the unit filtering from the declaration of P so that we
        # don't omit units in which we may have potential references to Self
        # through dispatching calls. This is valid because all units that would
        # import `dn` will necessarily import `base` as well, as `dn`
        # necessarily imports `base` to define its overriding subprogram.
        # This only works if filter_is_imported_by is called with transitive
        # set to True.
        bases = Var(origin.bind(
            Self,
            dn.basic_decl.root_subp_declarations._or(dn.basic_decl.singleton)
        ))

        all_units = Var(bases.mapcat(
            lambda base: base.filter_is_imported_by(units, True)
        ).unique)

        refs = Var(all_units.mapcat(lambda u: u.root.then(
            lambda r: dn.find_refs_impl(r.as_bare_entity, Self)
        )))

        return If(
            follow_renamings,
            refs.concat(
                refs.filter(
                    # Get the all renaming clauses *for which the renamed
                    # entity is self* (it is possible to find a reference
                    # inside a renaming clause but that this clause does not
                    # rename self, such as `X` in  `... renames X.Y`, in which
                    # case we don't want to recursively find its references!).
                    lambda f: f.ref.parents.find(
                        lambda p: p.is_a(RenamingClause)
                    ).cast(RenamingClause).then(
                        lambda r:
                        r.renamed_object.referenced_defining_name.node
                        == dn.node
                    )
                ).mapcat(
                    # Since a renaming clause is always part of a BasicDecl,
                    # retrieve the BasicDecl from the renaming clauses and
                    # recursively find all references on those.
                    lambda f: f.ref.parents.find(
                        lambda p: p.is_a(RenamingClause)
                    ).parent.cast_or_raise(BasicDecl).then(
                        lambda bd: bd.defining_name.find_all_references(
                            units=units,
                            follow_renamings=True
                        )
                    )
                )
            ),
            refs
        )

    @langkit_property()
    def find_matching_name(bd=BasicDecl.entity):
        """
        Helper for navigation proxies. Will return the defining name matching
        Self on the given BasicDecl.
        """
        return bd._.defining_names.find(
            lambda di: Entity.name.name_is(di.name_symbol)
        )

    @langkit_property(public=True, return_type=T.RefResult.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def find_all_calls(units=AnalysisUnit.array,
                       follow_renamings=(Bool, False)):
        """
        Return the list of all possible calls to the subprogram which Self is
        the defining name of.

        This will return the name corresponding to the call, excluding the
        parameters if there are any. For instance, it will return ``A`` for the
        ``A (B)`` call.

        .. note:: This does not yet support calls done inside generics.
        """
        return Entity.find_all_references(units, follow_renamings).filter(
            lambda r: r.ref.is_direct_call
        )

    next_part = Property(
        Entity.find_matching_name(
            Entity.basic_decl.next_part_for_name(Entity.name_symbol)
        ),
        public=True,
        doc="Like ``BasicDecl.next_part_for_decl`` on a defining name",
        dynamic_vars=[default_imprecise_fallback()]
    )

    previous_part = Property(
        Entity.find_matching_name(
            Entity.basic_decl.previous_part_for_name(Entity.name_symbol)
        ),
        public=True,
        doc="Like ``BasicDecl.previous_part_for_decl`` on a defining name",
        dynamic_vars=[default_imprecise_fallback()]
    )

    canonical_part = Property(
        Entity.find_matching_name(
            Entity.basic_decl.canonical_part_for_name(Entity.name_symbol)
        ),
        public=True,
        doc="Like ``BasicDecl.canonical_part`` on a defining name",
        dynamic_vars=[default_imprecise_fallback()]
    )

    @langkit_property(return_type=T.DefiningName.entity, public=True,
                      dynamic_vars=[origin, default_imprecise_fallback()])
    def most_visible_part():
        """
        Given an origin node and the entity represented by Self, this property
        returns the most visible completion of Self that can be seen by origin,
        according to Ada's visibility rules.
        """
        return Entity.find_matching_name(
            Entity.basic_decl.most_visible_part_for_name(Entity.name_symbol)
        )

    @langkit_property(return_type=T.DefiningName.entity.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def all_previous_parts():
        """
        Return all previous parts of this entity, where the first part
        is at the beginning of the array.
        """
        return Entity.previous_part.then(
            lambda pp: If(
                Entity == pp,
                No(DefiningName.entity.array),
                pp.all_previous_parts.concat(pp.singleton)
            )
        )

    @langkit_property(return_type=T.DefiningName.entity.array,
                      dynamic_vars=[default_imprecise_fallback()])
    def all_next_parts():
        """
        Return all next parts of this entity, where the last part is at the
        end of the array.
        """
        return Entity.next_part.then(
            lambda np: If(
                Entity == np,
                No(DefiningName.entity.array),
                np.singleton.concat(np.all_next_parts)
            )
        )

    @langkit_property(return_type=T.DefiningName.entity.array, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def all_parts():
        """
        Return all parts that define this entity, sorted from first part to
        last part.
        """
        prevs = Var(Entity.all_previous_parts)
        nexts = Var(Entity.all_next_parts)
        return prevs.concat(Entity.singleton).concat(nexts)

    @langkit_property(return_type=Aspect,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_aspect_impl(name=Symbol):
        """
        Return the aspect with the name ``name`` associated to this specific
        entity part.
        """
        return Entity.get_pragma(name).then(
            lambda p: Aspect.new(
                exists=True, node=p, value=p.args._.at(1)._.assoc_expr
            )
        )._or(Entity.basic_decl.get_aspect_assoc(name).then(
            lambda aa: Aspect.new(exists=True, node=aa, value=aa.expr)
        ))._or(Entity.get_representation_clause(name).then(
            lambda rc: Aspect.new(exists=True, node=rc, value=rc.expr)
        ))._or(If(
            name == 'Address',
            Entity.get_at_clause.then(
                lambda atc: Aspect.new(exists=True, node=atc, value=atc.expr)
            ),
            No(Aspect)
        ))

    @langkit_property(return_type=Aspect, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_aspect(name=Symbol):
        """
        Return the aspect with name ``name`` associated to entity that this
        name defines.

        Aspects are properties of entities that can be specified by the Ada
        program, either via aspect specifications, pragmas, or attributes.

        This will return the syntactic node corresponding to attribute
        directly.

        Note: for some aspects (e.g. ``Inline``), Libadalang will check if they
        are defined on any part of the entity.
        """
        parts_to_check = Var(If(
            name.any_of(
                'Inline',
                # For the following aspects, an aspect only on the body is
                # illegal, but we don't care about illegal cases, and this
                # allows us to auto propagate the aspect from spec to body.
                'Ghost', 'Default_Initial_Condition'
            ),
            Entity.all_parts,
            Entity.singleton
        ))
        return parts_to_check.map(
            lambda p: p.get_aspect_impl(name)
        ).find(
            lambda a: a.exists
        )

    @langkit_property(return_type=Bool, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def has_aspect(name=Symbol):
        """
        Returns whether the boolean aspect named ``name`` is set on the entity
        represented by this node.

        "Aspect" is used as in RM terminology (see :rmlink:`13.1`).
        """
        a = Var(Entity.get_aspect(name))

        return a.exists & If(
            Self.is_contract_aspect(name),

            # We don't want to evaluate the predicate condition to determine
            # if its present.
            True,

            a.value.then(lambda val: Or(
                # Only check the value of the expression if it is determined to
                # be of a boolean type, so we don't erroneously try to cast a
                # value to bool when it would be wrong.
                Not(val.expression_type == Self.bool_type),
                val.eval_as_int == BigIntLiteral(1)
            ), default_val=True)
        )

    @langkit_property(return_type=T.Bool)
    def is_valid_pragma_for_name(name=Symbol, decl=AdaNode.entity):
        """
        Helper property for ``get_pragma``. Used to check that ``decl`` is a
        pragma declaration that has the given name and is a valid pragma for
        the entity defined by this defining name.
        """
        return decl.cast(T.Pragma).then(
            lambda p: And(
                # Check pragma's name
                p.id.name_is(name),
                # Check that it's associated to self
                Not(p.associated_entities.find(lambda d: d == Entity)
                    .is_null),
                # Check that the pragma is after the decl
                (Self < p.node)
            )
        )

    @langkit_property(return_type=T.Pragma.entity, public=True)
    def get_pragma(name=Symbol):
        """
        Return the pragma with name ``name`` associated to this entity.

        Please use the ``p_get_aspects`` property instead if you are interested
        in aspects, i.e. information that can be represented by either aspect
        specification nodes, pragma nodes or attribute definition nodes.
        """
        bd = Var(Entity.basic_decl_no_internal)
        # First look at library level pragmas if Self is a library item
        return bd.library_item_pragmas.then(
            # Check pragma's name
            lambda plist: plist.find(lambda p: p.id.name_is(name)),
        )._or(
            # First look in the scope where Self is declared. We don't use
            # ``declarative_scope`` here, as this BasicDecl may not necessarily
            # be in a DeclarativePart, as is the case for ComponentDecls.
            # Instead, we simply look among this node's siblings.
            bd.parent.cast(AdaNode.list)._.find(
                lambda d: Entity.is_valid_pragma_for_name(name, d)
            )

            # Then, if entity is declared in the public part of a package or
            # protected def, corresponding pragma might be in the private part.
            ._or(bd.declarative_scope.cast(T.PublicPart).then(
                lambda pp: pp.parent.match(
                    lambda pkg=T.BasePackageDecl: pkg.private_part,
                    lambda ptd=T.ProtectedDef: ptd.private_part,
                    lambda _: No(T.PrivatePart)
                )._.decls.as_entity.find(
                    lambda d: Entity.is_valid_pragma_for_name(name, d)
                )
            ))

            # Then, look inside decl, in the first declarative region of decl
            ._or(bd.declarative_parts.at(0)._.decls.find(
                lambda d: Entity.is_valid_pragma_for_name(name, d)
            ))

            .cast(T.Pragma)
        )

    @langkit_property(return_type=T.AttributeDefClause.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_representation_clause(name=Symbol):
        """
        Return the representation clause associated to this entity that
        defines the given attribute name.
        """
        return Entity.declarative_scope._.decls.as_entity.find(
            lambda d: d.cast(T.AttributeDefClause).then(
                lambda p: Let(
                    lambda attr=p.attribute_expr.cast_or_raise(T.AttributeRef):
                        And(attr.attribute.name_is(name),
                            attr.prefix.referenced_defining_name == Entity)
                )
            )
        ).cast(T.AttributeDefClause.entity)

    @langkit_property(return_type=T.AtClause.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def get_at_clause():
        """
        Return the at clause associated to this entity.
        """
        return Entity.declarative_scope._.decls.as_entity.find(
            lambda d: d.cast(AtClause).then(
                lambda p: p.name.referenced_defining_name == Entity
            )
        ).cast(AtClause.entity)

    @langkit_property(public=True, return_type=T.Bool)
    def is_imported():
        """
        Whether this entity defined by this name is imported from another
        language.
        """
        return Entity.has_aspect('Import') | Entity.has_aspect('Interface')

    @langkit_property(public=True, return_type=T.Bool, memoized=True)
    def is_ghost_code():
        """
        Return whether the entity defined by this name is ghost or not.
        See SPARK RM 6.9.
        """
        bd = Var(Entity.basic_decl_no_internal)
        return Or(
            Entity.has_aspect('Ghost'),
            bd.parent_basic_decl._.is_ghost_code(),

            # Instantiation of generic ghost entity is ghost code
            bd.cast(GenericInstantiation).then(
                lambda gi: gi.designated_generic_decl.is_ghost_code()
            ),

            # Renaming of ghost entity is ghost code
            bd.match(
                lambda sr=SubpRenamingDecl: sr.renames.renamed_object,
                lambda pr=PackageRenamingDecl: pr.renames.renamed_object,
                lambda gr=GenericRenamingDecl: gr.renaming_name,
                lambda _: No(Name.entity)
            ).then(lambda c: c.referenced_defining_name._.is_ghost_code())
        )

    @langkit_property()
    def xref_equation():
        # The name field of a defining name must be an Identifier or a
        # DottedName. So we can special case the construction of the xref
        # equation here.
        return Entity.name.cast(T.DottedName).then(
            lambda dn: dn.prefix.xref_equation,
            default_val=LogicTrue()
        )

    # There are names to resolve in a defining name only if its name field is
    # a dotted name, in which case we must resolve its prefix.
    xref_entry_point = Property(Self.name.is_a(T.DottedName))


class EndName(Name):
    """
    Entity name in ``end ...;`` syntactic constructs.
    """

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

    @langkit_property()
    def xref_equation():
        return Entity.parent.cast(T.AcceptStmtWithStmts).then(
            lambda stmt: Bind(Self.ref_var, stmt.designated_entry),
            default_val=Bind(Self.ref_var, Entity.basic_decl)
        ) & Entity.name.cast(T.DottedName).then(
            # Also resolve the prefix of the dotted name, in case this
            # subprogram/package is a child unit: the fully qualified name must
            # be resolved as seen from the standard package.
            lambda dn: env.bind(
                Entity.std_env,
                dn.prefix.xref_no_overloading
            ),
            default_val=LogicTrue()
        )

    xref_entry_point = Property(True)


@abstract
class BaseId(SingleTokNode):
    """
    Base class for identifiers.
    """

    annotations = Annotations(custom_short_image=True)

    @langkit_property(memoized=True)
    def scope():
        elt = Var(env.get_first(
            Self,
            lookup=If(Self.is_prefix, LK.recursive, LK.flat),
            categories=no_prims
        ))
        ret = Var(If(
            Not(elt.is_null) & elt.node.is_a(
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

    @langkit_property()
    def designated_env_no_overloading():
        return Self.env_get_first_visible(
            env,
            lookup_type=If(Self.is_prefix, LK.recursive, LK.flat),
            from_node=Self.origin_node
        ).cast(T.BasicDecl).then(
            lambda bd: If(
                bd._.is_package, Entity.pkg_env(bd), bd.defining_env
            )
        )

    @langkit_property()
    def designated_env():
        """
        Decoupled implementation for designated_env, specifically used by
        DottedName when the parent is a library level package.
        """
        bd = Var(Self.parents.find(
            lambda p: p.is_a(GenericPackageInstantiation)
        ))

        env_el = Var(Self.env_get_first_visible(
            env,
            lookup_type=If(Self.is_prefix, LK.recursive, LK.flat),
            from_node=Self.origin_node,
        )).cast(T.BasicDecl)

        return If(
            # If first element is a package, then return the pkg env
            env_el._.is_package & Not(env_el.node == bd),
            Entity.pkg_env(env_el),

            Entity.env_elements_baseid.then(
                lambda all_env_els:
                all_env_els.filter(lambda e: And(
                    # Exclude own generic package instantiation from the lookup
                    Not(e.node == bd),

                    Self.has_visibility(e)
                ))
            ).map(lambda e: e.cast(BasicDecl).defining_env).env_group(),
        )

    @langkit_property(dynamic_vars=[env, origin])
    def pkg_env(from_pkg=T.BasicDecl.entity):
        """
        Return the lexical environment for this identifier, should it be a
        package. This method handles resolving to the most visible part of a
        package - private or body - if necessary. It also unwinds package
        renamings if necessary.

        If ``inst_from_formal`` is True, we know that bd is a generic package
        instantiation coming from a rebound formal package, and that we need
        visibility on the formals.
        """

        # If the given package is a renaming (after potentially several levels
        # of renamings) of another package P, do the rest of the work on P
        # instead.
        pkg = Var(from_pkg.cast(PackageRenamingDecl).then(
            lambda r: r.final_renamed_package,
            default_val=from_pkg
        ))

        bd = Var(
            # If pkg is a generic package (non instantiated) and it is
            # rebound somewhere in the context of Self's rebindings, then
            # we want to put back those rebindings on it, because it means
            # we are inside a generic instantiation, so refering to the
            # generic package actually means referring to the
            # instantiation.
            pkg.unshed_rebindings(Entity.info.rebindings)
        )
        is_inst_from_formal = Var(pkg.is_a(T.GenericPackageInstantiation) &
                                  from_pkg.info.from_rebound)

        env = Var(If(
            bd.is_a(T.GenericPackageInstantiation) & is_inst_from_formal,
            bd.cast(T.GenericPackageInstantiation).defining_env_impl(True),
            bd.defining_env
        ))

        # If the basic_decl is a package decl with a private part, we get it.
        # Else we keep the defining env.
        private_part_env = Var(
            env.get('__privatepart', LK.flat, categories=no_prims).at(0).then(
                lambda pp: pp.children_env, default_val=env
            )
        )

        package_body_env = Var(
            private_part_env.get('__nextpart', LK.flat, categories=no_prims)
            .at(0).then(
                lambda pb: If(
                    # If the package is implemented as a separate, we need to
                    # jump through one more link to get to the body.
                    pb.is_a(PackageBodyStub),

                    pb.children_env
                    .get('__nextpart', LK.flat, categories=no_prims)
                    .at(0).then(lambda pb: pb.children_env),

                    pb.children_env
                ), default_val=EmptyEnv
            )
        )

        formals_env = Var(bd.cast(GenericPackageDecl).then(
            lambda pkg_g: pkg_g.formal_part.children_env,
            default_val=EmptyEnv
        ))

        return Cond(

            # If we're looking from the body, return a group of all the
            # relevant envs together.
            Not(package_body_env.equals(EmptyEnv))
            & Self.is_children_env(package_body_env,
                                   (origin._or(Self)).node_env),
            Array([
                package_body_env, private_part_env, env, formals_env
            ]).env_group(),

            # If we're looking from the private part, return a group of private
            # part + public part.
            Self.is_children_env(private_part_env,
                                 (origin._or(Self)).node_env),
            Array([private_part_env, env, formals_env]).env_group(),

            # TODO: Probably some special handling for separates here, because
            # they'll have full visibility on the package body in which they're
            # defined.

            env
        )

    parent_scope = Property(env)

    @langkit_property()
    def designated_type_impl_get_real_type(n=AdaNode.entity):
        """
        Helper property for ``designated_type_impl``. Returns the actual type
        defined by the given node, if any.
        """
        return n.match(
            lambda t=T.BaseTypeDecl.entity: t,
            lambda tb=T.TaskBody.entity: tb.task_type,
            lambda _: No(BaseTypeDecl.entity)
        )

    @langkit_property()
    def designated_type_impl():
        # This is the view of the type where it is referenced
        des_type_1 = Var(Self.env_get_first_visible(
            env,
            from_node=Self,
            lookup_type=If(Self.is_prefix, LK.recursive, LK.minimal),
        ).then(
            lambda env_el: Self.designated_type_impl_get_real_type(env_el)
        ))

        # This is the view of the type where it is used
        des_type_2 = Var(Self.env_get_first_visible(
            env,
            from_node=origin,
            lookup_type=If(Self.is_prefix, LK.recursive, LK.minimal),
        ).then(
            lambda env_el: Self.designated_type_impl_get_real_type(env_el)
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

        # We might have a more complete view of the type at the origin point,
        # so look for every entity named like the type, to see if any is a
        # completer view of the type.
        completer_view = Var(origin.then(lambda o: Self.env_get(
            o.children_env, Self.symbol, from_node=origin, categories=no_prims
        )).filtermap(
            lambda n: n.cast(BaseTypeDecl),
            lambda n:
            n.is_a(BaseTypeDecl)
            & des_type.then(lambda d: d.is_view_of_type(n.cast(BaseTypeDecl)))
        ).at(0))

        # If completer_view is a more complete view of the type we're looking
        # up, then return completer_view. Else return des_type.
        return If(
            Not(completer_view.is_null),
            completer_view,
            des_type
        )

    @langkit_property(dynamic_vars=[env])
    def env_elements_impl():
        return Entity.env_elements_baseid

    @langkit_property()
    def all_env_els_impl(
            seq=(Bool, True),
            seq_from=(AdaNode, No(T.AdaNode)),
            categories=(T.RefCategories, all_categories)
    ):
        return Self.env_get(
            env,
            Self.name_symbol,
            lookup=If(Self.is_prefix, LK.recursive, LK.flat),
            from_node=If(seq, If(Not(seq_from.is_null), seq_from, Self),
                         No(T.AdaNode)),
            categories=categories
        )

    @langkit_property(dynamic_vars=[env], memoized=True)
    def env_elements_baseid():
        """
        Decoupled implementation for env_elements_impl, specifically used by
        designated_env when the parent is a library level package.
        """
        items = Var(Self.env_get(
            env,
            Self.symbol,
            lookup=If(Self.is_prefix, LK.recursive, LK.flat),
            # If we are in an aspect, then lookup is not sequential
            from_node=Self.origin_node,
            categories=If(
                Self.can_designate_primitive,
                all_categories,
                no_prims
            )
        ))

        # TODO: there is a big smell here: We're doing the filtering for parent
        # expressions in the baseid env_elements. We should solve that.

        pc = Var(Entity.parent_callexpr)
        is_prefix = Var(Not(Self.is_suffix))

        return origin.bind(Self.origin_node, Cond(
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

                    # This name can refer to the enclosing subprogram to
                    # create a qualified name to a local variable. This is only
                    # possible if this name is the prefix of a dotted name.
                    is_prefix & e.cast(T.BaseSubpBody)._.in_scope
                )
            )),

            # This identifier is the name for a called subprogram or an array.
            # So only keep:
            # * subprograms for which the actuals match
            # * arrays for which the number of dimensions match
            # * any type that has a user defined indexing aspect.

            pc.suffix.cast(AssocList).then(
                lambda params: items.filter(lambda e: e.match(
                    # Type conversion case
                    lambda _=BaseTypeDecl: params.length == 1,

                    lambda b=BasicDecl:
                    b.subp_spec_or_null.then(
                        lambda spec: Let(
                            lambda real_pc=If(
                                spec.cast(T.EntrySpec)._.family_type.is_null,
                                pc, pc.parent.cast(T.CallExpr)
                            ):
                            # ``real_pc`` can be null if we are handling a
                            # paramless entry decl that has an entry family,
                            # in which case the subsequent checks are not
                            # relevant.
                            real_pc.is_null

                            # Either the subprogram is matching the CallExpr's
                            # parameters.
                            | And(
                                spec.is_matching_param_list(
                                    params, b.info.md.dottable_subp
                                ),
                                real_pc.parent.cast(T.CallExpr).then(
                                    lambda ce: ce.check_for_type(b.expr_type),
                                    default_val=True
                                )
                            )

                            # Or the entity is parameterless, and the returned
                            # component (s) matches the callexpr (s).
                            | And(real_pc.check_for_type(b.expr_type),
                                  spec.paramless(b.info.md.dottable_subp)),

                        ),
                        # In the case of ObjectDecls/CompDecls in general,
                        # verify that the callexpr is valid for the given
                        # type designator.
                        default_val=pc.check_for_type(b.expr_type)
                    ),

                    lambda _: False
                )),

                # Discard BaseTypeDecls when resolving a CallExpr that cannot
                # be a type conversion.
                default_val=items.filter(lambda e: Not(e.is_a(BaseTypeDecl)))
            )
        ))

    @langkit_property(return_type=Bool)
    def denotes_the_property_function(subp_spec=T.BaseSubpSpec.entity):
        # Return true whether this node can refer to a property function
        # detoned by `subp_spec`. (see RM 7.3.4 about stable properties of a
        # type). This equation has to be called in the scope of the
        # `Stable_Properties` aspect name resolution.

        primitive_types = Var(subp_spec.primitive_subp_types())

        # ``subp_decl`` is a property function of this node if it comes from a
        # `Stable_Properties` AspectAssoc and:
        return And(
            # It only has one single parameter (mode in but not checked here)
            subp_spec.params.length == 1,

            # It matches the type for which the Stable_Properties is defined.
            # There are two cases:
            Entity.parent_basic_decl.match(
                # Either the Stable_Properties aspect is defined within a
                # TypeDecl.
                lambda td=T.TypeDecl: Not(
                        primitive_types.find(
                            lambda t: t == td
                        ).is_null
                    ),
                # Or within a SubpDecl
                lambda sd=T.SubpDecl: Not(
                        sd.subp_spec.primitive_subp_types().filter(
                            lambda t1: Not(
                                primitive_types.find(
                                    lambda t2: t1 == t2
                                ).is_null
                            )
                        ).is_null
                    ),
                lambda _: False
            )
        )

    @langkit_property()
    def xref_equation():
        return Entity.base_id_xref_equation()

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def base_id_xref_equation():
        env_els = Var(Entity.env_elements)
        is_prefix = Var(Not(Self.is_suffix))

        return env_els.logic_any(
            lambda e:
            Bind(Self.ref_var, e)
            & If(
                # If this BaseId refers to an enclosing subprogram and is
                # the prefix of a dotted name, then it is not a call.
                is_prefix & e.cast(T.BaseSubpBody)._.in_scope,

                Bind(Self.type_var, No(BaseTypeDecl.entity)),

                # If this BaseId represents a call, the called subprogram will
                # be held in Self.ref_var, in which case subp_spec_or_null will
                # return the specification of the called subprogram. If ref_var
                # does not contain a subprogram, this BaseId cannot be a call,
                # and subp_spec_or_null would indeed return null in this case.
                Bind(Self.ref_var, Self.type_var,
                     conv_prop=BasicDecl.expr_type)
                & Bind(Self.ref_var, Self.subp_spec_var,
                       conv_prop=BasicDecl.subp_spec_or_null)
            )
        )


class Op(BaseId):
    """
    Operation in a binary expression.

    Note that the ARM does not consider "double_dot" ("..") as a binary
    operator, but we process it this way here anyway to keep things simple.
    """
    enum_node = True
    alternatives = ["and", "or", "or_else", "and_then", "xor", "in",
                    "not_in", "abs", "not", "pow", "mult", "div", "mod",
                    "rem", "plus", "minus", "concat", "eq", "neq", "lt",
                    "lte", "gt", "gte", "double_dot"]

    @langkit_property(return_type=T.Symbol)
    def subprogram_symbol():
        """
        Return the symbol that needs to be used to define an overload of this
        operator.
        """
        return Self.match(
            lambda _=Op.alt_and:    '"and"',
            lambda _=Op.alt_or:     '"or"',
            lambda _=Op.alt_xor:    '"xor"',
            lambda _=Op.alt_abs:    '"abs"',
            lambda _=Op.alt_not:    '"not"',
            lambda _=Op.alt_pow:    '"**"',
            lambda _=Op.alt_mult:   '"*"',
            lambda _=Op.alt_div:    '"/"',
            lambda _=Op.alt_mod:    '"mod"',
            lambda _=Op.alt_rem:    '"rem"',
            lambda _=Op.alt_plus:   '"+"',
            lambda _=Op.alt_minus:  '"-"',
            lambda _=Op.alt_concat: '"&"',
            lambda _=Op.alt_eq:     '"="',
            lambda _=Op.alt_neq:    '"/="',
            lambda _=Op.alt_lt:     '"<"',
            lambda _=Op.alt_lte:    '"<="',
            lambda _=Op.alt_gt:     '">"',
            lambda _=Op.alt_gte:    '">="',
            lambda _:               '<<>>',
        )

    @langkit_property(return_type=T.BasicDecl.entity.array)
    def subprograms_for_symbol(sym=T.Symbol, from_node=T.AdaNode.entity):
        """
        Return the list of all operator definitions for the given operator
        symbol. Note that corresponding operators of root types are returned
        first in the list, so as to implement the "preference" behavior
        described in :rmlink:`8.6` - 29 in BinOp and UnOp xref_equation.
        """
        return Self.root_type_ops(sym).concat(Self.env_get(
            from_node.node_env, sym, from_node=from_node.node
        ).filtermap(
            lambda e: e.cast_or_raise(T.BasicDecl),
            lambda e: And(
                e.cast_or_raise(T.BasicDecl).is_subprogram,

                # Note: we do not use synthesized operators for BinOp/UnOp
                # resolution for now as it introduces a significant performance
                # regression.

                # TODO: retry when new solver is available
                Not(e.is_a(SyntheticSubpDecl))
            )
        ))

    subprograms = Property(
        Self.subprograms_for_symbol(Self.subprogram_symbol, Entity),
        doc="""
        Return the subprograms corresponding to this operator accessible in the
        lexical environment.
        """
    )

    name_symbol = Property(Self.subprogram_symbol)

    @langkit_property()
    def xref_equation():
        # An Op can only be a field of a BinOp or UnOp, so its ref var will
        # be bound in the xref equations of these two types.
        return LogicFalse()

    @langkit_property()
    def is_dispatching_call():
        # The "is dispatching call" logic is implemented on the BinOp/UnOp
        # itself.
        return Entity.parent.cast(Expr).is_dispatching_call


@has_abstract_list
class Identifier(BaseId):
    """
    Regular identifier (:rmlink:`2.3`).
    """

    annotations = Annotations(repr_name="Id")

    # Some attributes return functions in Ada. However, LAL incorrectly parses
    # an "AttributeRef with arguments" as something magical rather than a
    # regular call (which is why AttributeRef has an `args` field.
    #
    # Additionally, resolution for a number of them was implemented as "magic
    # attributes" rather than built-in functions. This is wrong and needs to be
    # fixed (see S910-057). However, for the moment, we parse them as
    # ``AttributeRef (pfx, attr, args)``, and resolve them specially
    # rather than  ``CallExpr (AttrRef (pfx, attr), args)``.
    #
    # For other args, we deactivate this parsing, so that they're correctly
    # parsed as ``CallExpr (AttrRef (pfx, attr), args)``.
    is_attr_with_args = Property(
        Self.symbol.any_of('First', 'Last', 'Range', 'Length')
    )

    @langkit_property(return_type=T.CompletionItem.array)
    def complete_items():
        return Entity.parent.complete_items

    @langkit_property()
    def is_constant():
        rd = Var(Entity.referenced_decl)
        return Or(
            # Check if the referenced declaration is constant (filter out
            # subprograms as it makes no sense to call is_constant_object
            # on them, except for EnumLiteralDecls).
            If(
                Or(Not(rd.is_subprogram), rd.is_a(EnumLiteralDecl)),
                rd.is_constant_object,
                False
            ),

            # An instance of a protected variable is constant within
            # a function body of the corresponding protected unit.
            Entity.parents.find(
                # We just check that the variable is used within a
                # function first, then we ensure that it is protected
                # by looking at its declaration, which should be inside
                # the private part of the corresponding protected unit.
                lambda n: n.cast(T.BaseSubpBody)._.subp_spec
                .subp_kind.is_a(SubpKind.alt_function)
            ).then(
                lambda _: And(
                    rd.is_in_private_part,
                    Not(
                        rd.parents.find(
                            lambda n: n.is_a(T.ProtectedTypeDecl)
                        ).is_null
                    )
                )
            )
        )


class StringLiteral(BaseId):
    """
    String literal (:rmlink:`2.6`).
    """

    annotations = Annotations(repr_name="Str")

    @langkit_property(return_type=T.String, external=True, public=True,
                      uses_entity_info=False, uses_envs=False)
    def denoted_value():
        """
        Return the value that this literal denotes.
        """
        pass

    has_context_free_type = Property(False)

    @langkit_property()
    def xref_equation():
        return If(
            # StringLiteral can be in a name, if it is an operator, in which
            # case we don't want to constrain its type.
            Self.parent.is_a(Name),
            Entity.base_id_xref_equation,
            Predicate(BaseTypeDecl.allows_string_literal,
                      Self.expected_type_var)
            & Bind(Self.expected_type_var, Self.type_var)
        )


class EnumLiteralDecl(BasicSubpDecl):
    """
    Declaration for an enumeration literal (:rmlink:`3.5.1`).
    """

    name = Field(type=T.DefiningName)
    aspects = NullField()

    is_static_decl = Property(True)

    is_constant_object = Property(True)

    @langkit_property(public=True)
    def enum_type():
        """
        Return the enum type corresponding to this enum literal.
        """
        return Self.parents.find(
            lambda p: p.is_a(TypeDecl)
        ).as_entity.cast(TypeDecl)

    defining_names = Property(Entity.name.singleton)

    @langkit_property(memoized=True)
    def synth_type_expr():
        return EnumLitSynthTypeExpr.new().as_entity

    @langkit_property(memoized=True)
    def subp_decl_spec():
        return T.EnumSubpSpec.new().as_entity

    env_spec = EnvSpec(

        add_to_env_kv(Self.name_symbol, Self,
                      dest_env=direct_env(Entity.enum_type.node_env)),

        # We add an env here so that parent_basic_decl/semantic_parent on the
        # enum subp spec work correctly and returns the EnumLiteralDecl rt. the
        # type decl.
        add_env()
    )

    enum_decl_name = Property(Entity.name)


@synthetic
class SyntheticCharEnumLit(EnumLiteralDecl):
    """
    Synthetic character enum literal declaration.
    """
    char_symbol = UserField(type=T.Symbol, public=False)
    enum_type_decl = UserField(type=T.TypeDecl.entity, public=False)

    enum_type = Property(Entity.enum_type_decl)

    expr = Property(Entity.defining_names.at(0), public=True, doc="""
    Return the CharLiteral expression corresponding to this enum literal.
    """)
    enum_decl_name = Property(Entity.expr)

    defining_names = Property(
        [Self.synthesize_defining_name(Self.char_symbol).as_entity]
    )


class CharLiteral(BaseId):
    """
    Character literal (:rmlink:`4.1`).
    """

    annotations = Annotations(repr_name="Chr")

    @langkit_property(return_type=T.Character, external=True, public=True,
                      uses_entity_info=False, uses_envs=False)
    def denoted_value():
        """
        Return the value that this literal denotes.
        """
        pass

    has_context_free_type = Property(False)

    @langkit_property()
    def xref_equation():
        return And(
            Predicate(BaseTypeDecl.is_non_null_char_type,
                      Self.expected_type_var),
            Bind(Self.expected_type_var, Self.type_var),

            # Ada RM 4.2 (3): since the expected type of the `CharLiteral` is
            # known in this case (the predicates above let us through), we can
            # use it to determine what the literal refers to. Hackish: we use
            # the `origin` dynamic variable to pass an additional argument to
            # the conversion property ``corresponding_char_literal``.
            # TODO: fix this once we can pass explicit parameters to conversion
            # properties.
            origin.bind(
                Self,
                Bind(Self.type_var, Self.ref_var,
                     conv_prop=BaseTypeDecl.corresponding_char_literal)
            )
        )


@abstract
class NumLiteral(SingleTokNode):
    """
    Base class for number literals (:rmlink:`2.4`).
    """

    annotations = Annotations(repr_name="Num")

    is_constant = Property(True)


class RealLiteral(NumLiteral):
    """
    Literal for a real number (:rmlink:`2.4`).
    """

    annotations = Annotations(repr_name="Real")

    @langkit_property()
    def xref_equation():
        return Self.universal_real_bind(Self.type_var)


class IntLiteral(NumLiteral):
    """
    Literal for an integer (:rmlink:`2.4`).
    """

    annotations = Annotations(repr_name="Int")

    @langkit_property()
    def xref_equation():
        return Self.universal_int_bind(Self.type_var)

    @langkit_property(return_type=T.BigInt, external=True, public=True,
                      uses_entity_info=False, uses_envs=False)
    def denoted_value():
        """
        Return the value that this literal denotes.
        """
        pass


class NullLiteral(SingleTokNode):
    """
    The ``null`` literal (:rmlink:`4.4`).
    """

    annotations = Annotations(repr_name="Null")

    has_context_free_type = Property(False)

    @langkit_property()
    def xref_equation():
        return And(
            Predicate(BaseTypeDecl.is_access_type_predicate,
                      Self.expected_type_var),
            Bind(Self.expected_type_var, Self.type_var)
        )


class SingleActual(Struct):
    name = UserField(type=BaseId)
    assoc = UserField(type=T.BasicAssoc.entity)


class ParamMatch(Struct):
    """
    Helper data structure to implement SubpSpec/ParamAssocList matching.

    Each value relates to one ParamAssoc.
    """
    has_matched = UserField(type=Bool, doc="""
        Whether the matched ParamAssoc a ParamSpec.
    """)
    actual = UserField(type=SingleActual)
    formal = UserField(type=DefiningName.entity)


@abstract
class BaseSubpSpec(BaseFormalParamHolder):
    """
    Base class for subprogram specifications (:rmlink:`6.1`).
    """

    name = AbstractProperty(type=T.DefiningName.entity)
    returns = AbstractProperty(
        type=T.TypeExpr.entity, public=True, doc="""
        Syntax property. Return the type expression node corresponding to the
        return of this subprogram spec.
        """
    )

    @langkit_property(return_type=T.ParamSpec.entity.array, public=True,
                      kind=AbstractKind.abstract_runtime_check)
    def params():
        """
        Returns the array of parameters specification for this subprogram spec.
        """

    abstract_formal_params = Property(
        Entity.params.map(lambda p: p.cast(BaseFormalParamDecl))
    )

    @langkit_property(return_type=Bool)
    def match_return_type(other=T.BaseSubpSpec.entity):
        # Check that the return type is the same. Caveat: it's not because
        # we could not find the canonical type that it is null!
        #
        # TODO: simplify this code when SubpSpec provides a kind to
        # distinguish functions and procedures.
        self_ret = Var(origin.bind(Self.origin_node, Entity.return_type))
        other_ret = Var(origin.bind(other.node.origin_node, other.return_type))
        return Or(
            And(Entity.returns.is_null, other.returns.is_null),
            And(
                Not(Entity.returns.is_null), Not(other.returns.is_null),
                origin.bind(Self.origin_node,
                            self_ret._.matching_type(other_ret))
            )
        )

    @langkit_property(return_type=Bool)
    def match_signature(other=T.BaseSubpSpec.entity, match_name=Bool,
                        use_entity_info=(Bool, True),
                        ignore_first_param=(Bool, False)):
        """
        Return whether SubpSpec's signature matches Self's.

        Note that the comparison for types isn't just a name comparison: it
        compares the canonical types.

        If match_name is False, then the name of subprogram will not be
        checked.

        If use_entity_info is True and Entity's metadata has values for fields
        ``primitive`` and ``primitive_real_type`` (e.g. if it was retrieved
        from a primitive_env), those will be taken into account and
        match_signature will return True if ``other`` overrides ``Entity``.

        If ignore_first_param is True, do the signature match by ignoring the
        Self's first parameter. This can be used for example when matching a
        TaskType's procedure with one of its parent interface primitives,
        because the subprogram from the task has an implicit Self parameter
        which does not appear in the subprogram specification.
        """
        ent = Var(If(use_entity_info, Entity, Self.as_bare_entity))
        return And(
            # Check that the names are the same
            Not(match_name) | ent.name.node.matches(other.name.node),
            ent.match_return_type(other),
            ent.match_formal_params(other, match_name, ignore_first_param),
        )

    @langkit_property(return_type=Bool)
    def match_other(other=T.BaseFormalParamHolder.entity,
                    match_names=(Bool, True)):
        return Entity.match_signature(
            other.cast_or_raise(BaseSubpSpec), match_names
        )

    @langkit_property(return_type=LexicalEnv,
                      dynamic_vars=[origin])
    def defining_env():
        """
        Helper for BasicDecl.defining_env.
        """
        return If(Entity.returns.is_null,
                  EmptyEnv, Entity.return_type.defining_env)

    @langkit_property(return_type=BaseTypeDecl.entity, dynamic_vars=[origin])
    def potential_dottable_type():
        """
        If self meets the criteria for being a subprogram callable via the dot
        notation, return the type of dottable elements.
        """
        return Entity.abstract_formal_params.at(0).then(
            lambda p: p.type_expression._.element_type
        )

    @langkit_property(return_type=BaseTypeDecl.entity)
    def get_candidate_type_for_primitive(
        type_expr=T.TypeExpr.entity, canonicalize=(T.Bool, True)
    ):
        """
        If the given type expression designates a type of which Self is a
        primitive, return that designated type. Otherwise return null.

        If ``canonicalize`` is true, then the returned type will be
        canonicalized first. Else, the most complete part of the type will be
        returned.
        """
        decl_scope = Var(Self.parent.parent.parent.cast(DeclarativePart))

        typ = Var(origin.bind(Self.origin_node, type_expr.match(
            lambda at=T.AnonymousType: at.element_type.then(
                # TODO: remove this check once S918-021 is done, since it will
                # be checked below in any case.
                lambda et: Not(et.is_classwide).then(
                    lambda _: et
                )
            ),
            lambda other: other.designated_type
        )))

        # Canonicalize if requested
        canon_type = Var(If(
            canonicalize,
            origin.bind(Self.origin_node, typ._.canonical_type),
            typ
        ))

        final_type = Var(canon_type.cast(IncompleteTypeDecl).then(
            lambda i: i.next_part,
            default_val=canon_type
        ))

        type_scope = Var(final_type.then(
            lambda typ: typ.node.parent.parent.cast(DeclarativePart)
        ))

        return If(
            And(
                Or(
                    # Either both the subprogram and the type are declared in
                    # in a package declaration...
                    And(
                        type_scope._.parent.is_a(BasePackageDecl),
                        decl_scope._.parent.is_a(BasePackageDecl)
                    ),

                    # Or, in case of a derived tagged type, a subprogram
                    # defined in the same scope may be a primitive even in a
                    # non-package scope if that subprogram overrides a previous
                    # primitive.
                    # Therefore the correct behavior here would be to compute
                    # the primitives of `final_type` and check if one of its
                    # primitives matches the signature of this subprogram.
                    # Unfortunately we cannot do that here, because it would
                    # trigger infinite recursions if the parent is defined
                    # in the same scope. Therefore, the final filtering is done
                    # in `direct_primitive_subps` and the result of this
                    # property is not 100% accurate.
                    final_type.cast(TypeDecl)._.is_derived_tagged_type
                ),

                # A subprogram may not be a primitive of a classwide type
                Not(final_type._.is_classwide),

                # A subprogram may not be a primitive of a type which is not
                # declared in the same declarative scope as Self, or in the
                # private part of the package in which Self is defined.
                type_scope.then(lambda ds: ds.any_of(
                    decl_scope,
                    decl_scope._.parent.cast(BasePackageDecl)._.public_part
                ))
            ),
            final_type,
            No(BaseTypeDecl.entity)
        )

    @langkit_property(return_type=BaseTypeDecl.entity.array, memoized=True)
    def candidate_primitive_subp_types(canonicalize=(T.Bool, True)):
        """
        Return the types of which this subprogram is a candidate primitive of.
        If ``canonicalize`` is true, then the returned types will be
        canonicalized.
        """
        # TODO: This might be improved by checking for spelling before looking
        # up every type.

        params = Var(Entity.unpacked_formal_params)
        types = Var(params.map(lambda p: p.formal_decl.type_expression).concat(
            Entity.returns._.singleton
        ))

        return types.map(
            lambda t: Entity.get_candidate_type_for_primitive(
                t, canonicalize=canonicalize
            )
        ).filter(
            lambda t: Not(t.is_null)
        ).map(
            lambda t: t.cast(IncompleteTypeDecl).then(
                lambda i: i.next_part,
                default_val=t
            )
        ).unique

    @langkit_property(return_type=BaseTypeDecl.entity)
    def candidate_primitive_subp_first_type():
        """
        Return the first type of which this subprogram is a candidate
        primitive of.
        """
        return Entity.candidate_primitive_subp_types.then(lambda p: p.at(0))

    @langkit_property(return_type=BaseTypeDecl.entity, memoized=True)
    def candidate_primitive_subp_tagged_type(canonicalize=(T.Bool, True)):
        """
        If this subprogram is a primitive for a tagged type, then return this
        type. If ``canonicalize`` is true, then the returned types will be
        canonicalized.
        """
        return origin.bind(
            Self, Entity.candidate_primitive_subp_types(canonicalize).find(
                lambda t: t.full_view.is_tagged_type
            )
        )

    @langkit_property(return_type=T.BaseSubpSpec.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def decl_spec(follow_generic=Bool):
        """
        If this subp spec is that of the body of an entity, this property
        returns the subp spec of the declaration of that entity. It returns
        itself otherwise.

        If ``follow_generic`` is set to False, we explicitly return null if
        this spec is part of a generic subprogram declaration. See
        ``primitive_decl_spec``.
        """
        # The ``name`` field can be null, for example if this subp spec is part
        # of an access-to-subprogram type declaration.
        bd = Var(Entity.name._.basic_decl)
        return bd._.canonical_part.then(
            lambda dp: If(
                Not(follow_generic) & dp.is_a(GenericSubpInternal),
                No(BaseSubpSpec.entity),
                dp.subp_spec_or_null(follow_generic=follow_generic)
            ),
            default_val=Entity
        )

    @langkit_property(return_type=T.BaseSubpSpec.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def primitive_decl_spec():
        """
        Return the subp spec of the declaration of this potential primitive.
        Since a generic subprogram cannot be a primitive, we explicitly
        set ``follow_generic`` to False to filter out those early.
        """
        return Entity.decl_spec(follow_generic=False)

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def as_primitive_subp_type(typ=T.BaseTypeDecl.entity):
        """
        Given a type that was retrieved by one of the ``candidate_primitive_*``
        properties, return itself if the subp spec actually corresponds to a
        primitive of this type, otherwise return null. This is needed because
        the result of the ``candidate_primitive_*`` properties is an
        approximation.
        """
        # `Entity.name` can be null for access-to-subprogram specifications
        return Entity.name.then(
            lambda name:
            # This is node is indeed a primitive if we can find it in `typ`'s
            # primitives env.
            typ._.primitives_env.get(name.name_symbol).any(
                lambda b: b.node == name.basic_decl.node
            ).then(
                # If seen from an inherited primitive, make sure the returned
                # type corresponds to the derived type and not the base type
                # by using `real_type`.
                lambda _: Entity.real_type(typ)
            )
        )

    @langkit_property(return_type=BaseTypeDecl.entity.array, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def primitive_subp_types():
        """
        Return the types of which this subprogram is a primitive of.
        """
        return Entity.primitive_decl_spec.then(
            lambda spec: spec.candidate_primitive_subp_types.filter(
                lambda c: Not(spec.as_primitive_subp_type(c).is_null)
            )
        )

    @langkit_property(return_type=BaseTypeDecl.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def primitive_subp_first_type():
        """
        Return the first type of which this subprogram is a primitive of.
        """
        return Entity.primitive_decl_spec.then(
            lambda spec: spec.as_primitive_subp_type(
                spec.candidate_primitive_subp_first_type
            )
        )

    @langkit_property(return_type=BaseTypeDecl.entity, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def primitive_subp_tagged_type():
        """
        If this subprogram is a primitive for a tagged type, then return this
        type.
        """
        return Entity.primitive_decl_spec.then(
            lambda spec: spec.as_primitive_subp_type(
                spec.candidate_primitive_subp_tagged_type
            )
        )

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def has_controlling_result():
        """
        Return whether this subprogram has a controlling result, i.e. that
        it is the primitive of a tagged type ``T`` and its return type is
        ``T`` as well.
        """
        typ = Var(Entity.candidate_primitive_subp_tagged_type)
        return Not(typ.is_null) & (typ == Entity.return_type)

    @langkit_property(return_type=BaseTypeDecl.entity, memoized=True)
    def dottable_subp_of():
        """
        Returns whether the subprogram containing this spec is a subprogram
        callable via the dot notation.
        """
        return origin.bind(Entity.name.origin_node, If(
            Entity.nb_max_params > 0,
            Entity.potential_dottable_type.then(lambda t: Cond(
                t.is_a(ClasswideTypeDecl),
                t.cast(ClasswideTypeDecl).typedecl,

                # NOTE: We are not actually implementing the correct Ada
                # semantics here, because you can call primitives via the dot
                # notation on private types with a tagged completion.
                # However, since private types don't have components, this
                # should not ever be a problem with legal Ada.
                t.full_view.is_tagged_type,
                t,

                No(T.BaseTypeDecl.entity)
            )),
            No(T.BaseTypeDecl.entity)
        ))

    @langkit_property(return_type=T.BaseTypeDecl.entity,
                      dynamic_vars=[default_origin()], public=True)
    def return_type():
        """
        Returns the return type of Self, if applicable (e.g. if Self is a
        subprogram). Else, returns null.
        """
        return Entity.returns.then(
            lambda rt: Entity.real_designated_type(rt)
        )

    xref_entry_point = Property(True)
    xref_equation = Property(Entity.returns.then(lambda r: r.sub_equation,
                                                 default_val=LogicTrue()))


@synthetic
class EnumSubpSpec(BaseSubpSpec):
    """
    Synthetic node for the abstract subprogram spec of an enum literal.

    NOTE: This has no existence in the ARM. While enum literals are functions
    semantically, they're not such syntactically.
    """
    enum_decl = Property(Self.parent.cast(T.EnumLiteralDecl).as_entity)

    name = Property(Entity.enum_decl.enum_decl_name)
    returns = Property(Entity.enum_decl.synth_type_expr)
    params = Property(No(T.ParamSpec.entity.array))


@synthetic
class SyntheticIdentifier(Name):
    """
    Synthetic identifier.
    """
    sym = UserField(public=False, type=T.Symbol)
    name_symbol = Property(Self.sym)


@synthetic
class SyntheticDefiningName(DefiningName):
    """
    Synthetic DefiningName.
    """
    # it is not possible to override Name.relative_name (which name_symbol is
    # defined in terms of), so we override name_symbol directly.
    name_symbol = Property(Self.name.name_symbol)

    as_symbol_array = Property(Self.name_symbol.singleton)


@synthetic
class SyntheticTypeExpr(TypeExpr):
    """
    Synthetic type expression. The designated type is already known at
    instantiation time and is to be given in the ``target_type`` field.
    """
    target_type = Field(type=BaseTypeDecl)

    @langkit_property()
    def designated_type():
        # The `target_type` field stores the bare designated BaseTypeDecl,
        # but Entity may be carrying rebindings that need to be put back on
        # the bare node.
        # However, all of Entity's rebinding may not be relevant. For example,
        # if `target_type` is the definition of `Standard.Boolean`, no
        # rebindings will ever be relevant.
        # In order to find which rebindings are relevant, we first need to
        # find the closest rebindable parent of `target_type`. From there, we
        # can inspect Entity's rebindings, and retrieve them as soon as we find
        # a parent rebinding that rebinds the closest rebindable parent of
        # `target_type`.

        # First we need to find the closest rebindable parent
        gd = Var(Self.target_type.parents.find(
            lambda p: p.is_a(GenericDecl)
        ).cast(GenericDecl).as_bare_entity)

        # Extract the relevant rebindings
        relevant_rebindings = Var(gd._.unshed_rebindings(
            Entity.info.rebindings
        ).then(
            lambda fixed: fixed.info.rebindings,
            default_val=No(T.EnvRebindings)
        ))

        # Return a rebound `target_type`
        return T.BaseTypeDecl.entity.new(
            node=Self.target_type,
            info=T.entity_info.new(
                rebindings=relevant_rebindings,
                md=No(T.Metadata),
                from_rebound=Entity.info.from_rebound
            )
        )


@synthetic
class SyntheticFormalParamDecl(BaseFormalParamDecl):
    """
    Synthetic parameter declaration.
    """
    param_name = UserField(type=T.Symbol, public=False)
    param_type = Field(type=T.TypeExpr)
    aspects = NullField()
    defining_names = Property(
        [Self.synthesize_defining_name(Self.param_name).as_entity]
    )

    type_expression = Property(Entity.param_type)


@synthetic
class SyntheticUnarySpec(BaseSubpSpec):
    """
    Synthetic subprogram specification for unary operators.
    """
    subp_symbol = UserField(type=T.Symbol, public=False)
    right_param = Field(type=T.SyntheticFormalParamDecl)
    return_type_expr = Field(type=T.SyntheticTypeExpr)

    name = Property(Self.synthesize_defining_name(Self.subp_symbol).as_entity)
    returns = Property(Entity.return_type_expr)

    @langkit_property()
    def abstract_formal_params():
        return [Entity.right_param.cast(BaseFormalParamDecl)]


@synthetic
class SyntheticBinarySpec(BaseSubpSpec):
    """
    Synthetic subprogram specification for binary operators.
    """
    subp_symbol = UserField(type=T.Symbol, public=False)
    left_param = Field(type=T.SyntheticFormalParamDecl)
    right_param = Field(type=T.SyntheticFormalParamDecl)
    return_type_expr = Field(type=T.TypeExpr)

    name = Property(Self.synthesize_defining_name(Self.subp_symbol).as_entity)
    returns = Property(Entity.return_type_expr)

    @langkit_property()
    def abstract_formal_params():
        return [
            Entity.left_param.cast(BaseFormalParamDecl),
            Entity.right_param.cast(BaseFormalParamDecl)
        ]


@synthetic
class SyntheticSubpDecl(BasicSubpDecl):
    """
    Synthetic subprogram declaration.

    Is used to represent predefined operators. This should also be usable
    for synthesizing function attributes.
    """
    aspects = NullField()
    spec = Field(type=T.BaseSubpSpec)

    subp_decl_spec = Property(Entity.spec)

    @langkit_property()
    def next_part_for_decl():
        return No(BasicDecl.entity)

    @langkit_property()
    def is_static_decl():
        return True


class SubpSpec(BaseSubpSpec):
    """
    Subprogram specification (:rmlink:`6.1`).
    """
    subp_kind = Field(type=T.SubpKind)
    subp_name = Field(type=T.DefiningName)
    subp_params = Field(type=T.Params)
    subp_returns = Field(type=T.TypeExpr)

    name = Property(Entity.subp_name)
    params = Property(Entity.subp_params._.params.map(lambda p: p))

    returns = Property(Entity.subp_returns)


class EntryDecl(BasicSubpDecl):
    """
    Entry declaration (:rmlink:`9.4`).
    """
    overriding = Field(type=Overriding)
    spec = Field(type=T.EntrySpec)
    aspects = Field(type=T.AspectSpec)

    subp_decl_spec = Property(Entity.spec)

    defining_names = Property(Entity.spec.name.singleton)

    @langkit_property(public=True, return_type=T.EntryBody.entity,
                      dynamic_vars=[default_imprecise_fallback()])
    def body_part():
        """
        Return the entry body associated to this entry declaration.
        """
        return Entity.body_part_for_decl.cast_or_raise(EntryBody)

    @langkit_property(return_type=Bool)
    def has_family():
        """
        Return whether this actually declares a family of entries.
        """
        return Not(Self.spec.family_type.is_null)

    @langkit_property(return_type=T.BaseTypeDecl.entity, dynamic_vars=[origin])
    def family_type():
        """
        Return the type designated by the family type, if relevant. Note that
        the family type may be an anonymous range, in which case this property
        returns None.
        """
        return Entity.spec.family_type.cast(TypeExpr)._.designated_type

    @langkit_property(public=True, return_type=T.AcceptStmt.entity.array)
    def accept_stmts():
        """
        Return an array of accept statements corresponding to this entry.
        """
        return Entity.find_accept_stmts(
            # Find the body part of the task declaration for the current entry
            Entity.parent_basic_decl.match(
                lambda st=T.SingleTaskTypeDecl.entity: st.parent_basic_decl,
                lambda tt=T.TaskTypeDecl: tt,
                lambda _: PropertyError(
                    T.BasicDecl.entity,
                    "Can only be called on EntryDecl in Tasks"
                )
            ).body_part_for_decl
        )

    @langkit_property(return_type=T.AcceptStmt.entity.array)
    def find_accept_stmts(root=T.AdaNode.entity):
        """
        Find all accept statements in all children of ``root`` that correspond
        to this entry declaration.
        """
        return root.children.then(
            lambda child: child.filter(lambda n: Not(n.is_null))
            .mapcat(lambda n: Entity.find_accept_stmts(n))
        ).concat(
            root.cast(T.AcceptStmt).then(
                lambda stmt: If(
                    stmt.corresponding_entry == Entity,
                    stmt.singleton,
                    No(AcceptStmt.entity.array)
                )
            )
        )

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env()
    )


class EntrySpec(BaseSubpSpec):
    """
    Entry specification.

    This node does not have ARM existence, because in the RM subprogram
    specifications don't encompass the ad-hoc specifications that happen in
    entry declarations. Entry declarations are described in
    :rmlink:`9.5.2`.
    """
    entry_name = Field(type=T.DefiningName)
    family_type = Field(type=T.AdaNode)
    entry_params = Field(type=T.Params)

    name = Property(Entity.entry_name)
    params = Property(
        Entity.entry_params.then(
            lambda p: p.params.map(lambda p: p),
            default_val=No(T.ParamSpec.entity.array)
        )
    )
    returns = Property(No(T.TypeExpr.entity))

    xref_equation = Property(Entity.family_type.then(
        lambda r: r.sub_equation,
        default_val=LogicTrue()
    ))


class Quantifier(AdaNode):
    """
    Type for quantified expressions.
    """
    enum_node = True
    alternatives = ["all", "some"]


class IterType(AdaNode):
    """
    Iteration type for ``for`` loops.
    """
    enum_node = True
    alternatives = ["in", "of"]


@abstract
class LoopSpec(AdaNode):
    """
    Base class for loop specifications (:rmlink:`5.5`).
    """
    pass


class ForLoopVarDecl(BasicDecl):
    """
    Declaration for the controlling variable in a ``for`` loop
    (:rmlink:`5.5`).
    """

    id = Field(type=T.DefiningName)
    id_type = Field(type=T.TypeExpr)
    aspects = NullField()

    defining_names = Property(Entity.id.singleton)

    defining_env = Property(Entity.expr_type.defining_env)

    @langkit_property(memoized=True, call_memoizable=True)
    def expr_type():
        return If(
            Self.id_type.is_null,

            # The type of a for loop variable does not need to be annotated, it
            # can eventually be inferred, which necessitates name resolution on
            # the loop specification. Run resolution if necessary.
            # NOTE: if we are in the context of an Ada 202x iterated component
            # association things are a bit different: `id`'s type_var should
            # already be set by the enclosing IteratedAssoc's resolution, and
            # must be retrieved directly. Calling `expression_type` now would
            # trigger an infinite loop because the enclosing ForLoopSpec is
            # not an entry point in this context.
            If(
                Self.parent.cast(ForLoopSpec).is_iterated_assoc_spec,
                Self.id.type_val.cast_or_raise(BaseTypeDecl),
                Entity.id.expression_type,
            ),

            # If there is a type annotation, just return it
            Entity.id_type.designated_type
        )

    env_spec = EnvSpec(add_to_env_kv(Self.name_symbol, Self))

    @langkit_property()
    def is_constant_object():
        # TODO: add support for Constant/Variable_Indexing
        loop_spec = Var(Entity.parent.cast(ForLoopSpec))
        return If(
            # IterType.alt_of loops are constant if the iterable object is
            # constant.
            loop_spec.loop_type.is_a(IterType.alt_of),
            loop_spec.iter_expr.cast_or_raise(Name).is_constant,
            # IterType.alt_in loops are always constant
            True
        )

    @langkit_property()
    def xref_equation():
        return Entity.id.sub_equation & If(
            Entity.id_type.is_null,
            LogicTrue(),
            Entity.id_type.sub_equation
        )


class ForLoopSpec(LoopSpec):
    """
    Specification for a ``for`` loop (:rmlink:`5.5`).
    """

    var_decl = Field(type=T.ForLoopVarDecl)
    loop_type = Field(type=IterType)
    has_reverse = Field(type=Reverse)
    iter_expr = Field(type=T.AdaNode)
    iter_filter = Field(type=T.Expr)

    @langkit_property(return_type=Bool)
    def is_iterated_assoc_spec():
        """
        Return whether this for loop spec is part of an iterated component
        association.
        """
        return Self.parent.is_a(IteratedAssoc)

    @langkit_property(memoized=True, call_memoizable=True,
                      dynamic_vars=[env, origin])
    def iter_type():
        type_var = Var(Entity.iter_expr.cast_or_raise(Expr).type_var)

        p = Var(Entity.iter_expr.resolve_names_internal_with_eq(
            # Avoid resolving to a procedure
            Predicate(AdaNode.is_not_null, type_var)

            # If there is a type annotation, use it as the expected type for
            # `iter_expr`.
            & If(
                Self.var_decl.id_type.is_null,
                LogicTrue(),
                Bind(Self.iter_expr.cast(Expr).expected_type_var,
                     Entity.var_decl.id_type.designated_type)
            )
        ))

        typ = Var(If(
            p,
            type_var.get_value.cast(T.BaseTypeDecl),
            No(BaseTypeDecl.entity)
        ))

        return origin.bind(Self.origin_node, If(
            typ.is_implicit_deref,
            typ.accessed_type,
            typ
        ))

    @langkit_property(return_type=Equation)
    def xref_equation():
        return Entity.var_decl.sub_equation & Self.loop_type.match(

            # This is a for .. in
            lambda _=IterType.alt_in:

            # Let's handle the different possibilities
            Entity.iter_expr.match(
                # Anonymous range case: for I in 1 .. 100
                lambda binop=T.BinOp:
                binop.sub_equation
                # The default type, if there is no other determined type, is
                # Integer.
                & Predicate(BaseTypeDecl.is_not_root_int_type, binop.type_var)
                & Bind(Self.var_decl.id.type_var, binop.type_var),

                # Subtype indication case: the induction variable is of the
                # type.
                lambda t=T.SubtypeIndication:
                t.sub_equation
                & Bind(Self.var_decl.id.type_var,
                       t.designated_type.canonical_type),

                lambda r=T.AttributeRef:
                r.sub_equation
                & Bind(Self.var_decl.id.type_var, r.type_var),

                # Name case: Either the name is a subtype indication, or an
                # attribute on a subtype indication, in which case the logic is
                # the same as above, either it's an expression that yields an
                # iterator.
                lambda t=T.Name: t.name_designated_type.then(
                    lambda typ:
                    t.sub_equation
                    & Bind(Self.var_decl.id.type_var, typ.canonical_type),
                    default_val=Entity.iterator_xref_equation
                ),

                lambda _: LogicTrue()  # should never happen
            ),

            # This is a for .. of
            lambda _=IterType.alt_of: Let(lambda it_typ=Entity.iter_type: If(
                it_typ.is_iterable_type,

                # Then we want the type of the induction variable to be the
                # component type of the type of the expression.
                Bind(Self.var_decl.id.type_var, it_typ.iterable_comp_type),

                LogicFalse()
            ))
        ) & If(
            Entity.iter_filter.is_null,
            LogicTrue(),
            Bind(Self.iter_filter.expected_type_var, Self.bool_type)
            & Bind(Self.iter_filter.type_var, Self.bool_type)
            & Entity.iter_filter.sub_equation
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def iterator_xref_equation():
        iter_expr = Var(Entity.iter_expr.cast_or_raise(T.Expr))

        p = Var(iter_expr.resolve_names_internal_with_eq(
            Predicate(BaseTypeDecl.is_iterator_type,
                      iter_expr.type_var)
        ))

        cursor_type = Var(iter_expr.type_val.children_env
                          .get_first('Cursor').cast_or_raise(T.BaseTypeDecl))

        return If(
            p,
            Bind(Self.var_decl.id.type_var, cursor_type),
            LogicFalse()
        )

    # This spec is not a complete resolution context when part of an iterated
    # component association: we must know the type of the enclosing aggregate
    # to determine the type of the iteration variable in case of a `for I in`.
    xref_entry_point = Property(Not(Self.is_iterated_assoc_spec))


class QuantifiedExpr(Expr):
    """
    Quantified expression (:rmlink:`4.5.8`).
    """
    quantifier = Field(type=Quantifier)
    loop_spec = Field(type=T.ForLoopSpec)
    expr = Field(type=T.Expr)

    @langkit_property(return_type=Equation)
    def xref_equation():
        # NOTE: we need to resolve the spec first so that the indexing variable
        # has a type.
        spec_success = Var(Entity.loop_spec.resolve_names)

        return If(
            spec_success,
            Bind(Self.expr.expected_type_var, Self.bool_type)
            & Entity.expr.sub_equation
            & Entity.expr.matches_expected_formal_prim_type
            & Bind(Self.type_var, Self.expr.type_var),
            LogicFalse()
        )


class Allocator(Expr):
    """
    Allocator expression (``new ...``) (:rmlink:`4.8`).
    """

    subpool = Field(type=T.Name)
    type_or_expr = Field(type=T.AdaNode)

    has_context_free_type = Property(False)

    @langkit_property(public=True)
    def get_allocated_type():
        """
        Return the allocated type for this allocator.
        """
        return origin.bind(Self.origin_node, Entity.type_or_expr.match(
            lambda t=SubtypeIndication.entity: t.designated_type,
            lambda q=QualExpr.entity: q.designated_type,
            lambda _: No(BaseTypeDecl.entity)
        ))

    @langkit_property(return_type=Equation)
    def xref_equation():
        return (
            Entity.type_or_expr.sub_equation
            & Bind(Self.expected_type_var, Self.type_var)
            & Predicate(BaseTypeDecl.matching_allocator_type,
                        Self.type_var, Entity.get_allocated_type)
        )


class QualExpr(Name):
    """
    Qualified expression (``...'(...)``) .(:rmlink:`4.7`).
    """

    prefix = Field(type=T.Name)
    suffix = Field(type=T.Expr)

    ref_var = Property(Self.prefix.ref_var)

    relative_name = Property(Entity.prefix.relative_name)

    is_constant = Property(True)

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def general_xref_equation(root=(T.Name, No(T.Name))):
        return And(
            Entity.xref_equation,
            Entity.all_args_xref_equation(root),
            Entity.parent_name(root).then(
                lambda pn:
                pn.parent_name_equation(
                    Entity.prefix.designated_type_impl,
                    root
                ),
                default_val=LogicTrue()
            )
        )

    @langkit_property(return_type=Equation)
    def xref_equation():
        typ = Var(Entity.prefix.designated_type_impl)

        return (
            Entity.suffix.sub_equation
            & Bind(Self.prefix.ref_var, typ)
            & Bind(Self.suffix.expected_type_var, typ)
            & Entity.suffix.matches_expected_type
            & Bind(Self.type_var, typ)
        )

    # TODO: once we manage to turn prefix into a subtype indication, remove
    # this property and update Allocator.get_allocated type to do:
    # q.prefix.designated_type.
    designated_type = Property(
        env.bind(Entity.node_env,
                 origin.bind(Self.origin_node,
                             Entity.prefix.designated_type_impl)),
    )

    @langkit_property()
    def designated_env():
        return Entity.designated_type.defining_env

    @langkit_property()
    def env_elements_impl():
        return Entity.prefix.env_elements_impl


class AttributeRef(Name):
    """
    Expression to reference an attribute (:rmlink:`4.1.4`).
    """

    prefix = Field(type=T.Name)
    attribute = Field(type=T.Identifier)
    args = Field(type=T.AdaNode)

    ref_var = Property(Self.r_ref_var)
    r_ref_var = UserField(type=LogicVar, public=False)

    relative_name = Property(Entity.prefix.relative_name)

    is_constant = Property(True)

    designated_type_impl = Property(Cond(
        Self.attribute.sym == 'Class',
        Entity.prefix.designated_type_impl._.classwide_type,

        Self.attribute.sym == 'Base',
        Entity.prefix.name_designated_type.scalar_base_subtype,

        No(BaseTypeDecl.entity)
    ))

    @langkit_property()
    def args_list():
        assoc_list = Var(Entity.args._.cast_or_raise(T.AssocList))
        return If(
            assoc_list.length > 0,
            assoc_list,
            No(T.AssocList.entity)
        )

    has_context_free_type = Property(Not(Self.is_access_attr))

    @langkit_property()
    def env_elements_impl():
        return Cond(
            Self.attribute.sym == 'Unrestricted_Access',

            # We need to implement env_elements_impl for this attribute to
            # handle the `X'Unrestricted_Access.all` pattern, because the
            # ExplicitDeref node expects it to return the declarations of `X`.
            Entity.prefix.env_elements_impl,

            No(T.AdaNode.entity.array),
        )

    is_access_attr = Property(
        Self.attribute.name_symbol.any_of(
            'Access', 'Unchecked_Access', 'Unrestricted_Access'
        )
    )

    @langkit_property()
    def designated_env():
        return Cond(
            Entity.is_access_attr,
            Entity.prefix.designated_env,

            Entity.attribute.name_is('Result'),
            Self.parents.find(lambda p: p.is_a(BasicSubpDecl, SubpBody))
            .as_entity.cast(T.BasicDecl).subp_spec_or_null
            .return_type.defining_env,

            EmptyEnv
        )

    @langkit_property(return_type=BasicSubpDecl.entity)
    def synthesize_attribute_subprogram(typ=BaseTypeDecl.entity):
        rel_name = Var(Entity.attribute.name_symbol)
        repo = Var(typ._.attributes_repo)
        subp = Var(Cond(
            rel_name == "succ", repo.succ,
            rel_name == "pred", repo.pred,
            rel_name == "min", repo.min,
            rel_name == "max", repo.max,

            rel_name == "rounding", repo.rounding,
            rel_name == "ceiling", repo.ceiling,
            rel_name == "floor", repo.floor,
            rel_name == "truncation", repo.truncation,
            rel_name == "machine", repo.machine,
            rel_name == "machine_rounding", repo.machine_rounding,
            rel_name == "fraction", repo.fraction,
            rel_name == "exponent", repo.exponent,

            rel_name == "copy_sign", repo.copy_sign,
            rel_name == "remainder", repo.remainder,
            rel_name == "adjacent", repo.adjacent,
            rel_name == "scaling", repo.scaling,
            rel_name == "compose", repo.compose,

            rel_name == "mod", repo.mod,

            rel_name == "image", repo.image,
            rel_name == "wide_image", repo.wide_image,
            rel_name == "wide_wide_image", repo.wide_wide_image,
            rel_name == "put_image", repo.put_image,

            rel_name == "value", repo.value,
            rel_name == "wide_value", repo.wide_value,
            rel_name == "wide_wide_value", repo.wide_wide_value,

            rel_name == "fixed_value", repo.fixed_value,
            rel_name == "integer_value", repo.integer_value,

            rel_name == "pos", repo.pos,
            rel_name == "val", repo.val,
            rel_name == "enum_rep", repo.enum_rep,
            rel_name == "enum_val", repo.enum_val,

            rel_name == "read", repo.read,
            rel_name == "write", repo.write,
            rel_name == "input", repo.input,
            rel_name == "output", repo.output,

            rel_name == "asm_input", repo.asm_input,
            rel_name == "asm_output", repo.asm_output,

            No(BasicSubpDecl)
        ))
        return BasicSubpDecl.entity.new(
            node=subp,
            info=T.entity_info.new(
                rebindings=typ.info.rebindings,
                md=No(Metadata),
                from_rebound=typ.info.from_rebound
            )
        )

    @langkit_property(return_type=BasicDecl.entity)
    def attribute_subprogram():
        rel_name = Var(Entity.attribute.name_symbol)
        typ = Var(Entity.prefix.name_designated_type)
        return Cond(
            typ.is_null,
            No(BasicDecl.entity),

            rel_name.any_of("read", "write", "input", "output"),
            typ.get_representation_clause(Entity.attribute.name_symbol).then(
                lambda x: x.expr.cast_or_raise(T.Name).referenced_decl,
                default_val=Entity.synthesize_attribute_subprogram(typ)
            ),

            rel_name == "put_image",
            typ._.get_aspect("put_image").value.cast(Name).then(
                lambda n: n.referenced_decl,
                default_val=Entity.synthesize_attribute_subprogram(typ)
            ),

            Entity.synthesize_attribute_subprogram(typ)
        )

    @langkit_property()
    def xref_equation():
        rel_name = Var(Entity.attribute.name_symbol)
        return Cond(
            # Attributes that have arguments
            rel_name.any_of('First', 'Last', 'Range', 'Length'),
            Entity.array_attr_equation,

            # Attributes that simply return subprograms
            rel_name.any_of('Succ', 'Pred', 'Min', 'Max', 'Ceiling', 'Floor',
                            'Rounding', 'Truncation', 'Exponent', 'Fraction',
                            'Copy_Sign', 'Remainder', 'Adjacent', 'Machine',
                            'Machine_Rounding', 'Scaling', 'Compose', 'Mod',
                            'Value', 'Wide_Value', 'Wide_Wide_Value',
                            'Fixed_Value', 'Integer_Value',
                            'Pos', 'Val', 'Enum_Val',
                            'Write', 'Read', 'Output', 'Input', 'Put_Image',
                            'Asm_Input', 'Asm_Output'),
            Entity.attribute_subprogram_equation,

            rel_name.any_of('Size', 'VADS_Size'), Entity.size_equation,

            rel_name.any_of('Max_Size_In_Storage_Elements', 'Aft',
                            'Object_Size', 'Value_Size', 'Storage_Size'),
            Entity.subtype_attr_equation,

            rel_name.any_of('Access',
                            'Unchecked_Access', 'Unrestricted_Access'),
            Entity.access_equation,

            rel_name == 'Image',
            Entity.image_equation(Self.std_entity('String')),

            rel_name == 'Wide_Image',
            Entity.image_equation(Self.std_entity('Wide_String')),

            rel_name == 'Wide_Wide_Image',
            Entity.image_equation(Self.std_entity('Wide_Wide_String')),

            rel_name == 'Enum_Rep',
            Entity.enum_rep_equation,

            rel_name == 'Invalid_Value',
            Entity.invalid_value_equation,

            rel_name == 'Identity', Entity.identity_equation,
            rel_name == 'Address', Entity.address_equation,

            rel_name.any_of('Small', 'Model_Small', 'Safe_Small',
                            'Epsilon', 'Model_Epsilon',
                            'Large', 'Safe_Large',
                            'Delta'),
            Entity.universal_real_equation,

            rel_name == 'Img',
            Entity.img_equation(Self.std_entity('String')),

            rel_name == 'Tag', Entity.tag_attr_equation,

            rel_name == 'Result', Entity.result_attr_equation,

            rel_name.any_of('Old', 'Loop_Entry'),
            Entity.bind_to_prefix_eq,

            rel_name.any_of('Class', 'Base'),
            Entity.prefix.sub_equation,

            rel_name.any_of('Valid', 'Machine_Overflows', 'Machine_Rounds',
                            'Has_Access_Values', 'Has_Discriminants',
                            'Has_Tagged_Values', 'Definite', 'Constrained',
                            'Initialized'),
            Entity.prefix.sub_equation
            & Bind(Self.type_var, Self.bool_type),

            rel_name.any_of('Width', 'Component_Size', 'Position',
                            'Mantissa', 'Model_Mantissa', 'Machine_Mantissa',
                            'Fore', 'Aft', 'Digits', 'Modulus',
                            'Word_Size', 'Max_Integer_Size', 'Address_Size',
                            'Maximum_Alignment', 'System_Allocator_Alignment',
                            'Finalization_Size', 'Descriptor_Size',
                            'Alignment', 'First_Bit', 'Last_Bit',
                            'Default_Bit_Order', 'Range_Length',
                            'Storage_Unit',
                            'Small_Numerator', 'Small_Denominator',
                            'Machine_Emin', 'Machine_Emax'),
            Entity.prefix.sub_equation
            & Self.universal_int_bind(Self.type_var),

            rel_name == 'Target_Name',
            Bind(Self.type_var, Self.std_entity('String')),

            rel_name == 'Storage_Pool', Entity.storage_pool_equation,

            rel_name == 'Scalar_Storage_Order',
            Entity.scalar_storage_order_equation,

            rel_name == 'Type_Class', Entity.type_class_equation,

            # Task attributes (RM 9.9)
            rel_name.any_of('Callable', 'Terminated'),
            Entity.prefix.sub_equation & Bind(Self.type_var, Self.bool_type),

            # Entry attributes (RM 9.9)
            rel_name == 'Count',
            Entity.prefix.xref_no_overloading
            & Self.universal_int_bind(Self.type_var),

            rel_name == 'Caller',
            Entity.prefix.xref_no_overloading
            & Bind(Self.type_var, Self.task_id_type),

            rel_name == 'Machine_Radix',
            Entity.universal_int_equation,

            rel_name == 'To_Address',
            Entity.to_address_equation,

            rel_name == 'Index',
            Entity.index_equation,

            rel_name == "Abort_Signal",
            Bind(Self.ref_var, Self.std_entity('abort_signal_'))
            & Bind(Self.type_var, No(BaseTypeDecl.entity)),

            PropertyError(Equation, "Unhandled attribute")
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def attribute_subprogram_equation():
        """
        Equation for type attributes that denote functions.
        """
        return (
            Entity.prefix.xref_no_overloading
            & Bind(Self.ref_var, Entity.attribute_subprogram)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def type_class_equation():
        """
        Implementation of the Type_Class attribute, provided for compatibility
        with DEC 83.
        """
        typ = Var(
            Entity
            .get_unit_root_decl(['System', 'Aux_DEC'], UnitSpecification)
            ._.children_env.get_first('Type_Class', lookup=LK.flat)
            .cast(T.BaseTypeDecl)
        )

        return Entity.prefix.xref_equation & Bind(Self.type_var, typ)

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def storage_pool_equation():
        """
        Equation for the Storage_Pool attribute.
        """
        typ = Var(
            Entity
            .get_unit_root_decl(['System', 'Storage_Pools'], UnitSpecification)
            ._.children_env.get_first('Root_Storage_Pool', lookup=LK.flat)
            .cast(T.BaseTypeDecl).classwide_type
        )

        return Entity.prefix.xref_equation & Bind(Self.type_var, typ)

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def scalar_storage_order_equation():
        """
        Equation for the Scalar_Storage_Order attribute.
        """
        typ = Var(
            Entity
            .get_unit_root_decl(['System'], UnitSpecification)
            ._.children_env.get_first('Bit_Order', lookup=LK.flat)
            .cast(T.BaseTypeDecl)
        )

        return Entity.prefix.xref_equation & Bind(Self.type_var, typ)

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def bind_to_prefix_eq():
        return And(
            Bind(Self.prefix.expected_type_var, Self.expected_type_var),
            Entity.prefix.sub_equation,
            Bind(Self.type_var, Self.prefix.type_var),
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def result_attr_equation():
        # We find the containing subprogram starting the bound env's node
        # instead of Self, as this attribute can appear in a pragma Post
        # appearing *after* the subprogram.
        containing_subp = Var(env.env_node.parents.find(
            lambda p: p.is_a(BasicSubpDecl, BaseSubpBody)
        ).as_entity.cast(T.BasicDecl))

        returns = Var(containing_subp.subp_spec_or_null.then(
            lambda ss: ss.return_type
        ))

        return And(
            Bind(Self.type_var, returns),
            Bind(Entity.prefix.ref_var, containing_subp)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def tag_attr_equation():
        tag_type = Var(
            Entity
            .get_unit_root_decl(['Ada', 'Tags'], UnitSpecification)
            ._.children_env.get_first('Tag', lookup=LK.flat)
            .cast(T.BaseTypeDecl)
        )

        return (
            # Prefix is an expression, bind prefix's ref var to it
            Entity.prefix.xref_equation

            # Type of self is String
            & Bind(Self.type_var, tag_type)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def address_equation():
        address_type = Var(
            Entity
            .get_unit_root_decl(['System'], UnitSpecification)
            ._.children_env.get_first('Address', lookup=LK.flat)
            .cast(T.BaseTypeDecl)
        )
        # Just like in access_equation, handle subprograms first, otherwise
        # paramless subprograms could match the normal path and therefore be
        # considered called.
        return Or(
            Entity.prefix.xref_no_overloading
            & Predicate(BasicDecl.is_subprogram, Self.prefix.ref_var),

            Entity.prefix.sub_equation
        ) & Bind(Self.type_var, address_type)

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def identity_equation():
        # NOTE: We don't verify that the prefix designates an exception
        # declaration, because that's legality, not name resolution.
        return And(
            Entity.prefix.sub_equation,
            Bind(Self.prefix.ref_var, Self.type_var,
                 conv_prop=BasicDecl.identity_type)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def universal_real_equation():
        return (
            Self.universal_real_bind(Self.type_var)
            & Entity.prefix.sub_equation
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def universal_int_equation():
        typ = Var(Entity.prefix.name_designated_type)

        return (
            Entity.prefix.sub_equation
            & Self.universal_int_bind(Self.type_var)
            & Entity.args_list.logic_all(
                lambda arg:
                Bind(arg.expr.expected_type_var, typ)
                & arg.expr.sub_equation
                & arg.expr.matches_expected_type
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def image_equation(str_type=T.AdaNode.entity):
        typ = Var(Entity.prefix.name_designated_type)

        return If(
            typ.is_null,

            # If prefix is not a type, then it is an expression
            Entity.prefix.sub_equation
            & Bind(Self.type_var, str_type),

            Entity.prefix.xref_no_overloading
            & Bind(Self.ref_var, Entity.attribute_subprogram)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def img_equation(str_type=T.AdaNode.entity):
        return (
            # Prefix is an expression, bind prefix's ref var to it
            Entity.prefix.xref_equation

            # Type of self is String
            & Bind(Self.type_var, str_type)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def enum_rep_equation():
        typ = Var(Entity.prefix.name_designated_type)

        return If(
            typ.is_null,

            # If prefix is not a type, then it is an expression
            Entity.prefix.sub_equation
            & Bind(Self.type_var, Self.universal_int_type),

            Entity.prefix.xref_no_overloading
            & Bind(Self.ref_var, Entity.attribute_subprogram)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def invalid_value_equation():
        typ = Var(Entity.prefix.name_designated_type)
        return And(
            Bind(Self.prefix.ref_var, typ),
            Bind(Self.type_var, typ)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def access_equation():
        return Or(
            # Access to statically known subprogram
            Entity.prefix.xref_no_overloading(all_els=True)
            & Predicate(BaseTypeDecl.is_subp_access_of,
                        Self.type_var,
                        Self.prefix.ref_var)
            & Bind(Self.type_var, Self.expected_type_var),

            # Access to object
            Entity.prefix.sub_equation
            & Or(
                # If the expected type is known, use it to infer the prefix's
                # expected type, and also use it as the actual type of the
                # access attribute which avoids synthesizing an anonymous
                # access type.

                # Note: We use the `accessed_type_no_call` conversion property
                # here in case `Self.type_var` holds an access-to-
                # subprogram type so that we don't propagate its
                # return type to the prefix of the 'Access attribute.
                Bind(Self.expected_type_var, Self.type_var)
                & Bind(Self.expected_type_var, Self.prefix.expected_type_var,
                       conv_prop=BaseTypeDecl.accessed_type_no_call)
                & Or(
                    # Either the expected type of the prefix is None, meaning
                    # the conversion property above was applied on a subprogram
                    # access (for which we cannot retrieve the dereferenced
                    # type). In that case type should be None as well.
                    Bind(Self.prefix.expected_type_var, Self.prefix.type_var)
                    & Bind(Self.prefix.type_var, No(BaseTypeDecl.entity)),

                    # Or it's an object access and so the actual type must
                    # match the expected type we inferred above.
                    Entity.prefix.matches_expected_formal_type
                ),

                # If the expected type is not known, synthesize an anonymous
                # access type for this expression.
                Bind(Self.expected_type_var, No(BaseTypeDecl.entity))
                & Bind(Self.prefix.type_var,
                       Self.type_var,
                       conv_prop=BaseTypeDecl.anonymous_access_type_or_null)
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def size_equation():
        typ = Var(Entity.prefix.name_designated_type)
        return If(
            Not(typ.is_null),

            Bind(Self.prefix.ref_var, typ)
            & Self.universal_int_bind(Self.type_var),

            Entity.prefix.sub_equation
            & Self.universal_int_bind(Self.type_var)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def array_attr_equation():
        is_length = Var(Entity.attribute.name_is('Length'))
        typ = Var(Entity.prefix.name_designated_type)

        # If the range attribute has an argument, then it's a static expression
        # representing an int that we will use as a dimension.
        dim = Var(Entity.args_list.then(lambda a: a.at(0).expr.then(
            lambda expr: Let(
                lambda _=expr.resolve_names_internal:
                expr.eval_as_int.as_int
            ), default_val=1), default_val=1
        ) - 1)

        return If(
            Not(typ.is_null),

            # Prefix is a type
            Entity.prefix.xref_no_overloading & Cond(
                typ.is_array_def_with_deref & is_length,
                Self.universal_int_bind(Self.type_var),

                # If it's an array, take the appropriate index type
                typ.is_array_def_with_deref,
                Bind(Self.type_var, typ.index_type(dim)),

                # If it's a discrete type, then bind to the discrete type
                typ.is_discrete_type | typ.is_real_type & Not(is_length),
                Bind(Self.type_var, typ),

                LogicFalse()
            ),

            # Prefix is not a type: In that case we have permission to resolve
            # prefix separately.
            Let(
                lambda
                res=Entity.prefix.resolve_names_internal_with_eq(
                    Predicate(BaseTypeDecl.is_array_def_with_deref,
                              Entity.prefix.type_var)
                ),
                pfx_typ=Entity.prefix.type_val.cast(T.BaseTypeDecl):

                If(
                    res,
                    If(
                        is_length,
                        Self.universal_int_bind(Self.type_var),
                        Bind(Self.type_var, pfx_typ.index_type(dim))
                    ),
                    LogicFalse()
                )
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def subtype_attr_equation():
        """
        Generates the xref equation for a an attribute that is defined on any
        subtype and that evaluates to an universal integer.
        """
        return (
            Bind(Self.prefix.ref_var, Entity.prefix.name_designated_type) &
            Self.universal_int_bind(Self.type_var)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def to_address_equation():
        """
        Return the xref equation for the ``To_Address`` attribute.
        """
        # TODO: this property can be completely removed once we support
        # attributes that return functions.
        to_address_subp = Var(
            Entity.get_unit_root_decl(
                ['System', 'Storage_Elements'], UnitSpecification
            )._.children_env.get_first(
                'To_Address', lookup=LK.minimal
            ).cast(BasicSubpDecl)
        )
        return And(
            Entity.prefix.sub_equation,
            Bind(Self.ref_var, to_address_subp)
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def index_equation():
        """
        Return the xref equation for the ``Index`` attribute.
        """
        typ = Var(
            env.get_first(Entity.prefix.name_symbol)
            .cast_or_raise(T.EntryDecl).family_type
        )
        return Entity.prefix.sub_equation & Bind(Self.type_var, typ)


class ValueSequence(AdaNode):
    """
    The value sequence of a reduction expression (see ``ReduceAttributeRef``).
    Ada 2022, RM 4.5.10.
    """
    # NOTE: add chunck and aspect specification fields when parallel keyword is
    # supported.
    iter_assoc = Field(type=T.IteratedAssoc)

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def xref_equation():
        """
        Return the nameres equation for this ValueSequence.
        """
        return Entity.iter_assoc.xref_equation_for_reduce


class ReduceAttributeRef(Name):
    """
    Reduction expression (``Reduce`` attribute). Ada 2022, RM 4.5.10.
    """
    prefix = Field(type=T.AdaNode)
    attribute = Field(type=T.Identifier)
    args = Field(type=T.BasicAssoc.list)

    ref_var = Property(Self.r_ref_var)
    r_ref_var = UserField(type=LogicVar, public=False)

    args_list = Property(Entity.args._.cast_or_raise(T.AssocList))

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def xref_equation():
        """
        Return the nameres equation for the Reduce attribute:
        ``Expr'Reduce (Reducer, InitVal)``
        where Expr is either a ``Name`` or a ``SequenceValue`` denoting a
        collection to reduce, ``Reducer`` is the subprogram to use to perform
        the reduction and ``InitVal`` is the initial value to be used by the
        reducer.
        """
        reducer = Var(Entity.args_list.at(0).expr)

        return Self.env_get(
            env=env,
            symbol=reducer.cast(BaseId).sym,
            from_node=Self.origin_node
        ).logic_any(
            lambda subp:
            subp.cast(BasicDecl)._.is_valid_reducer_candidate.then(
                lambda _:
                Entity.xref_equation_for_reducer_candidate(subp
                                                           .cast(BasicDecl)),
                default_val=LogicFalse()
            )
        )

    @langkit_property(return_type=Equation, dynamic_vars=[env, origin])
    def xref_equation_for_reducer_candidate(subp=T.BasicDecl.entity):
        """
        Build the equation for a reducer candidate.
        """
        reducer = Var(Entity.args_list.at(0).expr.cast(BaseId))
        initial_value_expression = Var(Entity.args_list.at(1).expr)

        # We don't need to take too many precautions here since we are sure the
        # subp parameter is a valid reducer candidate, its supb_spec isn't null
        # and it has parameters.
        param_types = Var(subp.subp_spec_or_null.param_types)
        accum_type = Var(param_types.at(0))
        value_type = Var(param_types.at(1))

        return And(
            Entity.prefix.cast(ValueSequence).then(
                lambda vs:
                Bind(vs.iter_assoc.expr.expected_type_var, value_type)
                & vs.iter_assoc.expr.matches_expected_type,
                default_val=LogicTrue()
            ),
            Entity.prefix.sub_equation,

            Bind(Self.type_var, accum_type),

            Bind(reducer.ref_var, subp),

            Bind(initial_value_expression.expected_type_var, accum_type),
            initial_value_expression.sub_equation,
            initial_value_expression.matches_expected_formal_type
        )


class UpdateAttributeRef(AttributeRef):
    """
    Reference to the ``Update`` attribute, which is a non standard GNAT
    attribute.
    """
    @langkit_property()
    def xref_equation():
        # Assign the type of the inner aggregate (Self's ``args`` field) to
        # the type of the updated value. This allows the aggregate associations
        # inside of it to be resolved independently.
        # (see AggregateAssoc.xref_equation).
        ignore(Var(Entity.prefix.resolve_names_internal))
        prefix_type = Var(Entity.prefix.type_val.cast(BaseTypeDecl))
        return And(
            Bind(Entity.args.cast_or_raise(Aggregate).type_var, prefix_type),
            Bind(Self.type_var, prefix_type)
        )


class RaiseExpr(Expr):
    """
    Expression to raise an exception (:rmlink:`4.4`).
    """

    exception_name = Field(type=T.Name)
    error_message = Field(type=T.Expr)

    has_context_free_type = Property(False)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.exception_name.sub_equation,
            Bind(Self.expected_type_var, Self.type_var),
            Entity.error_message.then(
                lambda er: And(
                    # The expected type of that error message is always String,
                    # according to RM 11.3 - 3.1/2.
                    Bind(er.expected_type_var, Self.std_entity('String')),
                    er.sub_equation
                ),
                default_val=LogicTrue()
            )
        )


class DottedName(Name):
    """
    Name to select a suffix in a prefix (:rmlink:`4.1.3`).
    """

    prefix = Field(type=T.Name)
    suffix = Field(type=T.BaseId)
    ref_var = Property(Self.suffix.ref_var)

    subp_spec_var = Property(Self.suffix.subp_spec_var)
    defines_subp_spec_var = Property(True)

    has_context_free_type = Property(Not(Self.suffix.is_a(CharLiteral)))

    @langkit_property(return_type=T.CompletionItem.array)
    def complete_items():
        return origin.bind(Self.origin_node, env.bind(
            Self.node_env,
            # In completion we always want to return everything, and flag
            # invisible things as invisible, so we set the "no_visibility" flag
            # to True.
            no_visibility.bind(
                True,
                Self.env_get_public(
                    Entity.prefix.designated_env,
                    No(Symbol),
                    LK.flat
                ).filtermap(
                    lambda n: CompletionItem.new(
                        decl=n.cast(T.BasicDecl),
                        is_dot_call=n.info.md.dottable_subp,

                        is_visible=Or(
                            # Dottable subprograms are always visible
                            n.info.md.dottable_subp,

                            # Else check visibility on the unit containing n
                            Self.has_with_visibility(n.unit),
                        )
                    ),
                    # Filter elements that are coming from a body that is not
                    # visible. This can happen with dottable subprograms
                    # defined in bodies.
                    # NOTE: We also filter `PrivatePart`s here as they are
                    # useless from the completion point of view.
                    lambda n: And(
                        # Order matters here, `has_visibility` below should not
                        # be called with n being a PrivatePart.
                        Not(n.is_a(PrivatePart)),
                        Or(
                            n.owning_unit_kind == UnitSpecification,
                            Self.has_visibility(n)
                        )
                    )
                )
            )
        ))

    @langkit_property()
    def designated_env_no_overloading():
        pfx_env = Var(Entity.prefix.designated_env_no_overloading)
        return env.bind(pfx_env,
                        Entity.suffix.designated_env_no_overloading)

    @langkit_property()
    def designated_env():
        pfx_env = Var(Entity.prefix.designated_env)
        return env.bind(pfx_env, Entity.suffix.designated_env)

    @langkit_property()
    def all_env_els_impl(
            seq=(Bool, True),
            seq_from=(AdaNode, No(T.AdaNode)),
            categories=(T.RefCategories, all_categories)
    ):
        pfx_env = Var(Entity.prefix.designated_env)
        return env.bind(
            pfx_env,
            Entity.suffix.all_env_els_impl(seq, seq_from, categories)
        )

    scope = Property(Self.suffix.then(
        lambda sfx: env.bind(Self.parent_scope, sfx.scope),
        default_val=EmptyEnv
    ))

    parent_scope = Property(Self.prefix.scope)

    relative_name = Property(Entity.suffix.relative_name)

    @langkit_property()
    def env_elements_impl():
        pfx_env = Var(origin.bind(Self.origin_node,
                                  Entity.prefix.designated_env))
        return env.bind(pfx_env, Entity.suffix.env_elements_baseid)

    @langkit_property()
    def designated_type_impl():
        return env.bind(Entity.prefix.designated_env_no_overloading,
                        Entity.suffix.designated_type_impl)

    @langkit_property()
    def xref_equation():
        base = Var(Entity.prefix.sub_equation
                   & env.bind(Entity.prefix.designated_env,
                              Entity.suffix.sub_equation))

        return If(
            Not(Entity.designated_type_impl.is_null),
            base,
            base
            & Bind(Self.expected_type_var, Self.suffix.expected_type_var)
            & Bind(Self.type_var, Self.suffix.type_var)
            & Entity.env_elements.logic_any(
                lambda e:
                Bind(Self.suffix.ref_var, e)
                & e.cast(BasicDecl.entity).constrain_prefix(Self.prefix)
            )
        )

    @langkit_property()
    def is_constant():
        # A dotted name references a constant object if the prefix or the
        # suffix does.
        return Or(
            Entity.prefix.is_constant,
            Entity.suffix.is_constant
        )


class CompilationUnit(AdaNode):
    """
    Root node for all Ada analysis units (:rmlink:`10.1.1`).
    """

    prelude = Field(doc="``with``, ``use`` or ``pragma`` statements.")
    body = Field(type=T.AdaNode)
    pragmas = Field(type=T.Pragma.list)
    no_env = UserField(type=T.LexicalEnv, public=False)

    @langkit_property(external=True, uses_entity_info=False, uses_envs=True,
                      return_type=LexicalEnv)
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
    def syntactic_fully_qualified_name():
        """
        Return the syntactic fully qualified name of this compilation unit.
        """
        return Self.as_bare_entity.body.match(
            lambda li=T.LibraryItem: li.item.defining_name._.as_symbol_array,
            lambda su=T.Subunit: su.name.as_symbol_array.concat(
                su.body.defining_name.as_symbol_array
            ),
            lambda _: PropertyError(
                Symbol.array, 'Unexpected CompilationUnit.f_body attribute'
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

    @langkit_property(return_type=T.CompilationUnit.entity.array,
                      memoized=True, public=True)
    def withed_units():
        """
        Look for all "with" clauses at the top of this compilation unit and
        return all the compilation units designated by them. For the complete
        dependencies list of compilation units, see the ``unit_dependencies``
        property.
        """
        return Self.top_level_with_package_clauses.map(
            # Try to fetch the compilation unit in a spec file first. If this
            # fails, the "with" must designate a body without spec (e.g. a
            # library-level procedure).
            lambda p: Self.withed_unit_helper(p).as_bare_entity
        )

    @langkit_property(return_type=T.CompilationUnit.entity.array,
                      memoized=True, public=True)
    def imported_units():
        """
        Return all the compilation units that are directly imported by this
        one. This includes "with"ed units as well as the direct parent unit.
        """
        return Self.withed_units.concat(
            # Library-level subprogram bodies are handled specially here,
            # as their parent environment is *not* their corresponding spec in
            # our implementation (unlike for the rest of the library-level
            # declarations).
            Self.decl.cast(BaseSubpBody).then(
                # We call subp_previous_part directly to avoid unnecessary
                # detours in which code that raises property errors could
                # be accidentally added.
                lambda subp: subp.as_bare_entity.subp_previous_part.then(
                    lambda pp:
                    pp.enclosing_compilation_unit.as_bare_entity.singleton
                )
            )._or(Self.decl._.node_env._.env_node.then(
                lambda n:
                n.enclosing_compilation_unit.as_bare_entity.singleton
            ))
        )

    @langkit_property(return_type=T.CompilationUnit.entity.array)
    def unit_dependencies_helper(visited=T.CompilationUnit.entity.array,
                                 to_visit=T.CompilationUnit.entity.array):
        """
        Helper function for "unit_dependencies" that computes transitively
        the unit dependencies of the given ``to_visit`` units. The ``visited``
        set of units is used to terminate the search once a fix-point has
        been reached, which is when all direct dependencies of ``to_visit`` are
        already included in the ``visited`` set.
        """
        now_visited = Var(visited.concat(to_visit))
        new_imports = Var(to_visit.mapcat(lambda c: c.imported_units).unique)
        to_visit_next = Var(
            new_imports.filter(lambda c: Not(now_visited.contains(c)))
        )
        return If(
            to_visit_next.length > 0,
            Self.unit_dependencies_helper(now_visited, to_visit_next),
            now_visited
        )

    @langkit_property(return_type=T.CompilationUnit.entity.array,
                      memoized=True, public=True)
    def unit_dependencies():
        """
        Return the list of all the compilation units that are (direct and
        indirect) dependencies of this one. See the
        ``withed_units``/``imported_units`` properties to only get the direct
        dependencies of this unit.
        """
        return Self.unit_dependencies_helper(
            No(T.CompilationUnit.entity.array),
            Entity.singleton
        ).unique.filter(
            # Remove Self from the list of dependencies
            lambda u: u.node != Self
        )

    @langkit_property(public=True, return_type=BasicDecl,
                      ignore_warn_on_node=True)
    def decl():
        """
        Get the root basic decl defined in this compilation unit.
        """
        return Self.body.match(
            lambda li=T.LibraryItem: li.item,
            lambda su=T.Subunit: su.body,
            lambda _: No(T.BasicDecl),
        )

    @langkit_property(return_type=Bool,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_preelaborable_impl(from_body=T.Bool):
        """
        Implementation helper for ``is_preelaborable``.

        Return whether ``Entity`` or its spec (if any) make it preelaborable.
        ``from_body`` has the same semantics as in
        ``does_aspects_make_preelaborate``.
        """
        return Entity.body.match(
            # Subunits are preelaborable iff the body they relate to is
            # preelaborable.
            lambda su=T.Subunit: (
                su.body_root.parent.parent.cast_or_raise(T.CompilationUnit)
                .is_preelaborable_impl(from_body=True)
            ),

            lambda li=T.LibraryItem: li.item.match(
                lambda subp_decl=T.SubpDecl: (
                    subp_decl.does_aspects_make_preelaborable(from_body)
                ),
                lambda gen_subp_decl=T.GenericSubpDecl: (
                    gen_subp_decl.decl
                    .does_aspects_make_preelaborable(from_body)
                ),
                lambda subp_body=T.SubpBody: Or(
                    # Subprogram bodies can have elaboration pragmas, so look
                    # for them, first.
                    subp_body.does_aspects_make_preelaborable(from_body=False),

                    # Otherwise recurse on the corresponding procedure spec (if
                    # any).
                    subp_body.decl_part.then(lambda dp: (
                        dp.parent.parent
                        .cast_or_raise(T.CompilationUnit)
                        .is_preelaborable_impl(from_body=True)
                    ))
                ),
                lambda pkg_decl=T.PackageDecl: (
                    pkg_decl.does_aspects_make_preelaborable(from_body)
                ),
                lambda gen_pkg_decl=T.GenericPackageDecl: (
                    gen_pkg_decl.package_decl
                    .does_aspects_make_preelaborable(from_body)
                ),
                lambda pkg_body=T.PackageBody: (
                    # Elaboration control pragmas cannot appear in package
                    # bodies, so recurse on the corresponding package spec.
                    pkg_body.decl_part.unit.root.as_bare_entity
                    .cast_or_raise(T.CompilationUnit)
                    .is_preelaborable_impl(from_body=True)
                ),

                lambda _: False,
            ),

            lambda _: False
        )

    @langkit_property(return_type=Bool, public=True,
                      dynamic_vars=[default_imprecise_fallback()])
    def is_preelaborable():
        """
        Whether this compilation unit is preelaborable or not.
        """
        return Entity.is_preelaborable_impl(from_body=False)

    @langkit_property(return_type=Symbol.array)
    def self_restrictions():
        """
        Return the list of restrictions pragmas that appear in the prelude of
        this particular unit. We only check the prelude as it's the only place
        they are allowed to appear in.
        """
        return Self.as_bare_entity.prelude.filtermap(
            lambda n:
            n.cast(Pragma).args.at(0)._.assoc_expr.cast(BaseId).name_symbol,

            lambda n: n.cast(Pragma).then(
                lambda p: p.id.name_symbol == "Restrictions"
            )
        )

    @langkit_property(return_type=T.CompilationUnit.entity)
    def other_part():
        """
        If this compilation unit is of kind UnitSpecification, return its
        corresponding body unit, and conversely.
        """
        other_kind = Var(If(
            Self.unit_kind == UnitSpecification,
            UnitBody,
            UnitSpecification
        ))
        return Self.designated_compilation_unit(
            Self.syntactic_fully_qualified_name,
            kind=other_kind,
            not_found_is_error=False,
        )._.as_bare_entity

    @langkit_property(return_type=Bool, public=True)
    def has_restriction(name=Symbol):
        """
        Whether this compilation unit is affected by the restriction with the
        given name.

        .. WARNING::
            This property only supports the ``No_Elaboration_Code`` restriction
            for now.
        """
        return Cond(
            name == "No_Elaboration_Code",

            Self.body.match(
                # For library items, restriction No_Elaboration_Code can appear
                # in the body or in the spec.
                lambda _=LibraryItem:
                Self.self_restrictions.contains(name)
                | Self.other_part._.self_restrictions.contains(name),

                # For subunits, restriction must appear in the root unit, so
                # we only check that.
                lambda su=Subunit: su.root_unit._.has_restriction(name),

                lambda _: PropertyError(
                    Bool, 'Unexpected CompilationUnit.f_body attribute'
                )
            ),

            PropertyError(Bool, "Unsupported restriction")
        )

    @langkit_property(return_type=Bool)
    def is_text_io_child():
        """
        Returns whether this compilation unit defines a child package of
        Ada.Text_IO.
        """
        name_parts = Var(Self.syntactic_fully_qualified_name)
        return And(
            name_parts.length == 3,
            name_parts.at(0) == "ada",
            name_parts.at(1).any_of(
                "text_io", "wide_text_io", "wide_wide_text_io"
            )
        )

    @langkit_property(
        return_type=T.Pragma.array,
        external=True,
        uses_entity_info=False,
        uses_envs=False,
    )
    def external_config_pragmas():
        """
        Return the list of pragmas from configuration pragmas files that apply
        to ``Self``'s unit.
        """
        pass

    @langkit_property(return_type=T.Pragma.array)
    def local_config_pragmas():
        """
        Return the list of configuration pragmas defined in the prelude of the
        current unit.
        """
        return Self.prelude.filtermap(
            lambda n: n.cast(Pragma),
            lambda n: n.is_a(Pragma),
        )

    @langkit_property(return_type=T.Pragma.array)
    def sources_config_pragmas():
        """
        Return the list of configuration pragmas defined in Ada sources and
        which apply to the current unit.
        """
        # First get pragmas in the prelude
        current_unit = Var(Self.local_config_pragmas)

        # Then get pragmas in related units
        related_units = Var(
            Cond(
                # If Self is a spec, we need to look at its body, and
                # conversely.
                Self.body.is_a(T.LibraryItem),
                Self.other_part._.local_config_pragmas,

                # If Self is a sub-unit, we need to look at all subunits up in
                # the chain, the root body, and the corresponding spec.
                Self.body.is_a(T.Subunit),
                Self.body.cast(Subunit).root_unit._.sources_config_pragmas,

                No(T.Pragma.array),
            )
        )

        return current_unit.concat(related_units)

    @langkit_property(return_type=T.Pragma.entity.array, public=True)
    def all_config_pragmas():
        """
        Return the list of configuration pragmas that apply to the current
        unit.

        .. note:: Using this property before creating the configuration pragmas
           files mapping using subprograms from the
           ``Libadalang.Config_Pragmas`` package will raise an error.
        """
        return (
            Self.sources_config_pragmas.concat(Self.external_config_pragmas)
            .map(lambda n: n.as_bare_entity)
        )

    @langkit_property(return_type=T.Pragma.entity.array, public=True)
    def config_pragmas(name=T.Symbol):
        """
        Return the list of configuration pragmas wih the given name that apply
        to the current unit.

        .. note:: Using this property before creating the configuration pragmas
           files mapping using subprograms from the
           ``Libadalang.Config_Pragmas`` package will raise an error.
        """
        return Self.all_config_pragmas.filter(
            lambda n: n.id.name_symbol == name
        )


@abstract
class BaseSubpBody(Body):
    """
    Base class for subprogram bodies (:rmlink:`6.3`).
    """

    overriding = Field(type=Overriding)
    subp_spec = Field(type=T.SubpSpec)

    defining_names = Property(Entity.subp_spec.name.singleton)

    is_constant_object = Property(True)

    @langkit_property()
    def defining_env():
        return If(
            Entity.in_scope,

            If(
                Entity.subp_spec_or_null
                ._.paramless(Entity.info.md.dottable_subp, can_be=True),
                Array([
                    Entity.children_env, Entity.subp_spec.defining_env
                ]).env_group(),
                Entity.children_env
            ),

            Entity.subp_spec.defining_env
        )

    type_expression = Property(Entity.subp_spec.returns)

    @langkit_property()
    def expr_type():
        return Entity.subp_spec_or_null._.return_type

    @langkit_property(return_type=T.Symbol)
    def initial_env_name(follow_private=(Bool, False)):
        return If(
            Self.is_library_item,
            Self.child_decl_initial_env_name(follow_private),
            Self.body_initial_env_name
        )

    @langkit_property(return_type=T.Symbol)
    def previous_part_env_name():
        """
        Return the name of the lexical env of the previous part of this
        subprogram body. Due to overloading, do not return anything in case
        we are not a library item or subunit.
        """
        return Cond(
            Self.is_subunit,
            Self.top_level_env_name.concat(String("__stub")).to_symbol,

            Self.is_library_item,
            Self.top_level_env_name.to_symbol,

            No(T.Symbol)
        )

    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(
            named_env(
                Self.initial_env_name(True),
                or_current=True
            )
        ),

        add_to_env(
            Entity.basic_decl_env_assocs(
                named_env(Self.initial_env_name(False), or_current=True)
            )
        ),

        add_env(transitive_parent=True),

        add_to_env_kv(
            key='__nextpart',
            value=Self,
            dest_env=Self.previous_part_env_name.then(
                lambda name: named_env(name),

                # Due to overloading, it's not possible to find the previous
                # part of a non-library item subprogram at this stage if it
                # has a top level env name (e.g. a subprogram declared in a
                # package), since its body could be defined in another unit.
                default_val=If(
                    Self.has_top_level_env_name,
                    no_env(),
                    direct_env(
                        env.bind(
                            Self.default_initial_env,
                            Entity.body_scope(
                                follow_private=False,
                                force_decl=True
                            )
                        ),
                        or_current=True
                    )
                )
            )
        ),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit),
        ),

        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        # If Self, which is assumed to be a SubpBody, is a library-level
        # subprogram, it must "inherit" the use clauses of its declaration, if
        # there is one.
        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.use_clauses_in_spec_of_subp_body,
            cond=Self.parent.is_a(T.LibraryItem)
        ),

        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.nested_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.prioritary,
            shed_corresponding_rebindings=True,
        ),

        # We must also "inherit" the use clauses from the generic formal part
        # of this body's generic declaration, if relevant.
        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.use_clauses_in_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.normal
        )
    )


class ExprFunction(BaseSubpBody):
    """
    Expression function (:rmlink:`6.8`).
    """

    expr = Field(type=T.Expr)
    aspects = Field(type=T.AspectSpec)

    xref_equation = Property(
        Bind(Entity.expr.expected_type_var, Entity.subp_spec.return_type)
        & Entity.expr.sub_equation
        & Self.expr.matches_expected_assign_type
    )

    xref_entry_point = Property(True)


class NullSubpDecl(BaseSubpBody):
    """
    Declaration for a null subprogram (:rmlink:`6.1`).
    """

    aspects = Field(type=T.AspectSpec)


class SubpRenamingDecl(BaseSubpBody):
    """
    Declaration for a subprogram renaming (:rmlink:`8.5.4`).
    """

    renames = Field(type=T.RenamingClause)
    aspects = Field(type=T.AspectSpec)

    xref_entry_point = Property(True)
    xref_equation = Property(Or(
        Cond(
            Entity.renames.renamed_object.is_a(CharLiteral),

            # If the renamed object is a char literal, simply resolves its
            # equation.
            Bind(Entity.renames.renamed_object.expected_type_var,
                 Entity.subp_spec.return_type)
            & Entity.renames.renamed_object.sub_equation,

            Entity.renames.renamed_object.is_a(AttributeRef),
            # If the renamed object is an attribute ref, do normal
            # resolution to synthesize its corresponding function.
            Entity.renames.renamed_object.sub_equation,

            Entity.renames.renamed_object.xref_no_overloading(all_els=True)
            & Predicate(BasicDecl.subp_decl_match_signature,
                        Entity.renames.renamed_object.ref_var,
                        Entity.cast(T.BasicDecl))
        ),
        # Operators might be built-in, so if we cannot find a reference, we'll
        # just abandon resolution...
        If(Entity.renames.renamed_object.is_operator_name,
           LogicTrue(), LogicFalse())
    ))


class SubpBody(BaseSubpBody):
    """
    Subprogram body(:rmlink:`6.3`) .
    """

    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    declarative_parts = Property(Entity.decls.singleton)


class HandledStmts(AdaNode):
    """
    List of statements, with optional exception handlers (:rmlink:`11.2`).
    """

    annotations = Annotations(snaps=True)

    stmts = Field(type=T.StmtList)
    exceptions = Field(type=T.AdaNode.list)


class ExceptionHandler(BasicDecl):
    """
    Exception handler (:rmlink:`11.2`).
    """

    exception_name = Field(type=T.DefiningName)
    handled_exceptions = Field(type=T.AlternativesList)
    stmts = Field(type=T.StmtList)
    aspects = NullField()

    env_spec = EnvSpec(
        add_env(),
        add_to_env(
            Entity.exception_name.then(lambda n: n.singleton.map(
                lambda n:
                new_env_assoc(key=n.name_symbol,
                              value=Self,
                              dest_env=current_env())
            ))
        )
    )

    defining_names = Property(
        Entity.exception_name.then(lambda n: n.singleton)
    )

    @langkit_property()
    def expr_type():
        return (
            Entity
            .get_unit_root_decl(['Ada', 'Exceptions'], UnitSpecification)
            ._.children_env.get_first('Exception_Occurrence', lookup=LK.flat)
            .cast(T.BaseTypeDecl)
        )

    xref_equation = Property(
        Self.handled_exceptions.logic_all(lambda he: he.as_entity.sub_equation)
    )

    xref_entry_point = Property(True)

    is_constant_object = Property(True)


@abstract
class Stmt(AdaNode):
    """
    Bass class for statements (:rmlink:`5.1`).
    """

    xref_entry_point = Property(True)

    @langkit_property(public=True)
    def is_ghost_code():
        """
        Return whether this statement is ghost code or not. See SPARK RM 6.9.
        """
        return Or(
            # Either this statement is part of a ghost declaration like a ghost
            # package or function.
            Entity.parent_basic_decl.then(
                lambda bd: bd.is_ghost_code
            ),

            # Either it's an implicitly ghost statement, because it's assigning
            # to a ghost variable, or calling a ghost procedure.
            Entity.match(
                lambda ass=T.AssignStmt: ass.dest.failsafe_referenced_def_name,
                lambda call=T.CallStmt: call.call.failsafe_referenced_def_name,
                lambda _: No(RefdDef)
            ).then(lambda res: If(
                # Sometimes name resolution errors are materialized by None
                # being returned from the queries instead of a property error.
                # But None doesn't necessarily mean there was an error, so we
                # explicitly handle the error cases by raising an exception as
                # we don't want errors to be silently ignored, and we use the
                # null coalescing operator to handle the legitimate cases.
                res.kind == RefResultKind.error,
                PropertyError(Bool, "Name resolution error"),
                res.def_name._.is_ghost_code
            ))
        )


class ErrorStmt(Stmt):
    """
    Placeholder node for syntax errors in lists of statements.
    """
    error_node = True


@abstract
class SimpleStmt(Stmt):
    """
    Base class for simple statements (:rmlink:`5.1`).
    """

    pass


@abstract
class CompositeStmt(Stmt):
    """
    Base class for composite statements (:rmlink:`5.1`).
    """

    pass


class CallStmt(SimpleStmt):
    """
    Statement for entry or procedure calls (:rmlink:`6.4`).
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
    """
    ``null;`` statement (:rmlink:`5.1`).
    """

    @langkit_property()
    def xref_equation():
        return LogicTrue()


class AssignStmt(SimpleStmt):
    """
    Statement for assignments (:rmlink:`5.2`).
    """

    dest = Field(type=T.Name)
    expr = Field(type=T.Expr)

    @langkit_property(return_type=Int, dynamic_vars=[origin])
    def complete_item_weight(item=T.BasicDecl.entity):
        # Promote declarations that returns a value
        return item.expr_type.then(
            lambda _: 100,
            default_val=0
        )

    @langkit_property()
    def xref_equation():
        return (
            Entity.dest.sub_equation
            & Bind(Self.dest.type_var, Self.expr.expected_type_var,
                   conv_prop=BaseTypeDecl.derefed_type)
            & Entity.expr.sub_equation
            & Self.expr.matches_expected_assign_type
        )


class GotoStmt(SimpleStmt):
    """
    ``goto`` statement (:rmlink:`5.8`).
    """

    label_name = Field(type=T.Name)

    @langkit_property()
    def xref_equation():
        return Entity.label_name.xref_no_overloading(sequential=False)


class ExitStmt(SimpleStmt):
    """
    ``exit`` statement (:rmlink:`5.7`).
    """

    loop_name = Field(type=T.Name)
    cond_expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return And(
            Entity.cond_expr.then(
                lambda cond:
                Bind(cond.expected_type_var, Self.bool_type)
                & cond.sub_equation
                & cond.matches_expected_formal_prim_type,
                default_val=LogicTrue()
            ),

            Entity.loop_name.then(
                lambda ln: ln.xref_no_overloading,
                default_val=LogicTrue()
            )
        )


class ReturnStmt(SimpleStmt):
    """
    ``return`` statement (:rmlink:`6.5`).
    """

    return_expr = Field(type=T.Expr)

    subp = Property(
        Self.parents.find(lambda p: p.is_a(SubpBody)).cast(SubpBody).as_entity,
        doc="Returns the subprogram this return statement belongs to"
    )

    @langkit_property()
    def xref_equation():
        return Entity.return_expr.then(
            lambda rexpr:
            Bind(rexpr.expected_type_var,
                 Entity.subp.subp_spec.returns.designated_type)
            & rexpr.sub_equation
            & rexpr.matches_expected_assign_type,
            default_val=LogicTrue()
        )


class RequeueStmt(SimpleStmt):
    """
    ``requeue`` statement (:rmlink:`9.5.4`).
    """

    call_name = Field(type=T.Name)
    has_abort = Field(type=Abort)

    innermost_entry_or_accept_stmt_params = Property(
        Entity.parents.find(
            lambda p: p.is_a(AcceptStmtWithStmts, EntryBody)
        ).match(
            lambda a=T.AcceptStmtWithStmts: a.params,
            lambda b=T.EntryBody: b.params,
            lambda _: No(T.EntryCompletionFormalParams.entity)
        ).cast(T.BaseFormalParamHolder.entity)
    )

    @langkit_property()
    def xref_equation():
        ce = Var(Entity.call_name.cast(CallExpr))
        name = Var(ce.then(lambda ce: ce.name,
                           default_val=Entity.call_name))

        entries = Var(name.all_env_elements_internal.filter(
            # RM 9.5.4: the name shall resolve to denote a procedure or entry,
            # where either:
            lambda n: n.cast(EntryDecl).then(lambda e: Or(
                # 1. The profile is empty
                e.subp_spec_or_null.then(lambda ss: And(
                    ss.nb_max_params == 0,
                    ss.returns.is_null
                )),
                # 2. The profile matches the profile of the enclosing entry
                e.spec.match_formal_params(
                    Entity.innermost_entry_or_accept_stmt_params
                )
            ))
        ))

        return And(
            # We call xref_no_overloading to make sure that sub-names are
            # bound.
            name.xref_no_overloading,

            # Then, bind the name to any entry that fits the bills
            entries.logic_any(lambda e: Let(
                # If we're binding to an entry from an entry family, resolve
                # the expression in the call expr, knowing that it can be used
                # to resolve overloads.
                lambda fam_type=e.cast(EntryDecl)._.spec.family_type
                .cast(SubtypeIndication)._.designated_type,

                # TODO: waiting for a fix to T603-061, we put `as_array` here
                # because _.at(0) does not unparse in a way that LKT can parse.
                first_param=ce._.params.as_array.at(0)._.expr:

                first_param.then(
                    lambda p: p.sub_equation & fam_type.then(
                        lambda eft:
                        Bind(p.expected_type_var, eft)
                        & p.matches_expected_type,
                        default_val=LogicTrue()
                    ),
                    default_val=LogicTrue()
                )
            ))
        )


class AbortStmt(SimpleStmt):
    """
    ``abort`` statement (:rmlink:`9.8`).
    """

    names = Field(type=T.Name.list)

    @langkit_property()
    def xref_equation():
        return Entity.names.logic_all(
            lambda name:
            name.sub_equation & Predicate(BaseTypeDecl.is_task_type,
                                          name.type_var)
        )


class DelayStmt(SimpleStmt):
    """
    ``delay`` statement (:rmlink:`9.6`).
    """

    has_until = Field(type=Until)
    expr = Field(type=T.Expr)

    @langkit_property()
    def xref_equation():
        return Entity.expr.sub_equation & If(
            Self.has_until.as_bool,
            LogicTrue(),
            Bind(Self.expr.expected_type_var, Self.std_entity('Duration'))
            & Entity.expr.matches_expected_type
        )


class RaiseStmt(SimpleStmt):
    """
    ``raise`` statement (:rmlink:`11.3`).
    """

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
                    # The expected type of that error message is always String,
                    # according to RM 11.3 - 3.1/2.
                    Bind(er.expected_type_var, Self.std_entity('String')),
                    er.sub_equation
                ),
                default_val=LogicTrue()
            )
        )


class IfStmt(CompositeStmt):
    """
    ``if`` statement block (:rmlink:`5.3`).
    """

    cond_expr = Field(type=T.Expr)
    then_stmts = Field(type=T.StmtList)
    alternatives = Field(type=T.ElsifStmtPart.list)
    else_stmts = Field(type=T.StmtList)

    @langkit_property()
    def xref_equation():
        return (
            Bind(Self.cond_expr.expected_type_var, Self.bool_type)
            & Entity.cond_expr.sub_equation
            & Self.cond_expr.matches_expected_formal_prim_type
        )


class ElsifStmtPart(AdaNode):
    """
    ``elsif`` part in an ``if`` statement block.
    """

    cond_expr = Field(type=T.Expr)
    stmts = Field(type=T.StmtList)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        return (
            Bind(Self.cond_expr.expected_type_var, Self.bool_type)
            & Entity.cond_expr.sub_equation
            & Self.cond_expr.matches_expected_formal_prim_type
        )


class LabelDecl(BasicDecl):
    """
    Declaration for a code label (:rmlink:`5.1`).
    """

    name = Field(type=T.DefiningName)
    aspects = NullField()

    defining_names = Property(Entity.name.singleton)

    env_spec = EnvSpec(add_to_env_kv(Self.name_symbol, Self))


class Label(SimpleStmt):
    """
    Statement to declare a code label (:rmlink:`5.1`).
    """

    decl = Field(type=T.LabelDecl)

    @langkit_property(return_type=Equation)
    def xref_equation():
        return LogicTrue()


class WhileLoopSpec(LoopSpec):
    """
    Specification for a ``while`` loop (:rmlink:`5.5`).
    """

    expr = Field(type=T.Expr)

    @langkit_property(return_type=Equation)
    def xref_equation():
        return And(
            Bind(Self.expr.expected_type_var, Self.bool_type),
            Entity.expr.sub_equation,
            Entity.expr.matches_expected_formal_prim_type
        )


class NamedStmtDecl(BasicDecl):
    """
    BasicDecl that is always the declaration inside a named statement.
    """
    name = Field(type=T.DefiningName)
    aspects = NullField()

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
    """
    Base class for loop statements (:rmlink:`5.5`).
    """

    spec = Field(type=T.LoopSpec)
    stmts = Field(type=T.StmtList)
    end_name = Field(type=T.EndName)

    @langkit_property(return_type=Equation)
    def xref_equation():
        return Entity.spec.then(lambda s: s.xref_equation,
                                default_val=LogicTrue())


class LoopStmt(BaseLoopStmt):
    """
    Statement for simple loops (``loop ... end loop;``) (:rmlink:`5.5`).
    """

    pass


class ForLoopStmt(BaseLoopStmt):
    """
    Statement for ``for`` loops (``for ... loop ... end loop;``)
    (:rmlink:`5.5`).
    """

    env_spec = EnvSpec(add_env())


class WhileLoopStmt(BaseLoopStmt):
    """
    Statement for ``while`` loops (``while ... loop ... end loop;``)
    (:rmlink:`5.5`).
    """

    pass


@abstract
class BlockStmt(CompositeStmt):
    """
    Base class for statement blocks (:rmlink:`5.6`).
    """

    env_spec = EnvSpec(add_env())

    xref_equation = Property(LogicTrue())


class DeclBlock(BlockStmt):
    """
    Statement block with a declarative part (:rmlink:`5.6`).
    """

    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    @langkit_property()
    def immediate_declarative_region():
        return Entity.children_env


class BeginBlock(BlockStmt):
    """
    Statement block with no declarative part (:rmlink:`5.6`).
    """

    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)


class ExtendedReturnStmt(CompositeStmt):
    """
    Extended ``return`` statement (:rmlink:`6.5`).
    """

    decl = Field(type=T.ExtendedReturnStmtObjectDecl)
    stmts = Field(type=T.HandledStmts)

    @langkit_property(return_type=Equation)
    def xref_equation():
        return LogicTrue()

    env_spec = EnvSpec(add_env())


class CaseStmt(CompositeStmt):
    """
    ``case`` statement (:rmlink:`5.4`).
    """

    expr = Field(type=T.Expr)
    pragmas = Field(type=T.Pragma.list)
    alternatives = Field(type=T.CaseStmtAlternative.list)

    @langkit_property()
    def xref_equation():
        return Entity.expr.sub_equation & (
            # First make sure null is not a possible value for the type of
            # the expression so as to avoid a null check in subsequent
            # predicates.
            Predicate(AdaNode.is_not_null, Self.expr.type_var)
            # Then make sure it is a discrete type
            & Predicate(BaseTypeDecl.is_discrete_type, Self.expr.type_var)
        )


class CaseStmtAlternative(AdaNode):
    """
    Alternative in a ``case`` statement (``when ... => ...``).
    """

    choices = Field(type=T.AlternativesList)
    stmts = Field(type=T.StmtList)

    xref_entry_point = Property(True)

    @langkit_property()
    def xref_equation():
        case_stmt = Var(Entity.parent.parent.cast_or_raise(CaseStmt))

        # Trigger name resolution on the case statement
        selected_type = Var(case_stmt.expr.expression_type)

        return Entity.choices.logic_all(lambda c: c.match(
            # Expression case
            lambda e=T.Expr:
            If(
                Not(e.cast(Name)._.name_designated_type.is_null),

                e.cast(Name).xref_no_overloading,

                Bind(e.expected_type_var, selected_type)
                & e.sub_equation
                & e.matches_expected_type
            ),

            # SubtypeIndication case (``when Color range Red .. Blue``)
            lambda t=T.SubtypeIndication: t.xref_equation,

            lambda _=T.OthersDesignator: LogicTrue(),

            lambda _: PropertyError(T.Equation, "Should not happen")
        ))


class EntryCompletionFormalParams(BaseFormalParamHolder):
    """
    Formal parameters for the completion of an ``EntryDecl`` (either an
    ``EntryBody`` or an ``AcceptStmt``).
    """
    params = Field(type=T.Params)

    abstract_formal_params = Property(
        Entity.params._.params.map(lambda p: p.cast(BaseFormalParamDecl))
    )


class AcceptStmt(CompositeStmt):
    """
    ``accept`` statement (:rmlink:`9.5.2`).
    """

    name = Field(type=T.Identifier)
    entry_index_expr = Field(type=T.Expr)
    params = Field(type=T.EntryCompletionFormalParams)

    env_spec = EnvSpec(add_env())

    @langkit_property(return_type=T.EntryDecl.entity,
                      dynamic_vars=[origin, env])
    def designated_entry():
        return Entity.name.all_env_els_impl.find(
            lambda e: e.cast(EntryDecl).then(
                lambda d: d.spec.match_formal_params(Entity.params)
            )
        ).cast(EntryDecl)

    @langkit_property(return_type=T.EntryDecl.entity, public=True,
                      dynamic_vars=[default_origin()])
    def corresponding_entry():
        """
        Return the entry which corresponds to this accept statement.
        """
        return env.bind(Entity.node_env, Entity.designated_entry)

    @langkit_property()
    def xref_equation():
        return And(
            Bind(Self.name.ref_var, Entity.designated_entry),
            Entity.entry_index_expr.then(
                lambda e: e.sub_equation,
                default_val=LogicTrue()
            )
        )


class AcceptStmtWithStmts(AcceptStmt):
    """
    Extended ``accept`` statement (:rmlink:`9.5.2`).
    """

    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)


class SelectStmt(CompositeStmt):
    """
    ``select`` statements block (:rmlink:`9.7`).
    """

    guards = Field(type=T.SelectWhenPart.list)
    else_stmts = Field(type=T.StmtList)
    abort_stmts = Field(type=T.StmtList)

    @langkit_property()
    def xref_equation():
        return Entity.guards.logic_all(lambda wp: wp.sub_equation)


class SelectWhenPart(AdaNode):
    """
    Alternative part in a ``select`` statements block (:rmlink:`9.7`).
    """

    cond_expr = Field(type=T.Expr)
    stmts = Field(type=T.StmtList)

    @langkit_property()
    def xref_equation():
        return Entity.cond_expr.then(
            lambda c: And(
                Bind(c.expected_type_var, Self.bool_type),
                c.sub_equation,
                c.matches_expected_formal_prim_type
            ),
            default_val=LogicTrue()
        )


class TerminateAlternative(SimpleStmt):
    """
    ``terminate`` alternative in a ``select`` statement (:rmlink:`9.7`).
    """

    xref_equation = Property(LogicTrue())


class PackageBody(Body):
    """
    Package body (:rmlink:`7.2`).
    """
    env_spec = EnvSpec(
        do(Self.env_hook),

        # Parent link is the package's decl, or private part if there is one
        set_initial_env(Self.body_initial_env),

        add_to_env(Entity.previous_part_link_env_assoc),

        # We make a transitive parent link only when the package is a library
        # level package.
        add_env(transitive_parent=Self.is_library_item),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.nested_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.prioritary,
            shed_corresponding_rebindings=True,
        ),

        # We must also "inherit" the use clauses from the generic formal part
        # of this body's generic declaration, if relevant.
        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.use_clauses_in_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.normal
        ),

        # Separate packages and nested packages basically need to be treated
        # the same way: we cannot use a transitive ref because of hiding
        # issues, so we'll do a prioritary ref, that groups together the
        # necessary envs.
        #
        # TODO: We need to ref use clauses, as in the regular package decl
        # case.
        reference(Self.cast(AdaNode).singleton,
                  through=T.Body.subunit_decl_env,
                  cond=Self.is_subunit,
                  kind=RefKind.prioritary),

        # If Self is not a library level package body (and hence is a nested
        # package), we need to explicitly reference its package decl, because
        # it is not in the chain of parents.
        #
        # The reference is non transitive because if it was it would cause some
        # visibility order issues.
        #
        # TODO: We can regroup this ref with the following ref, making
        # body_decl_scope return a grouped env with the use clauses in it.
        reference(Self.cast(AdaNode).singleton,
                  through=T.Body.body_decl_scope,
                  cond=Not(Self.is_compilation_unit_root),
                  kind=RefKind.prioritary),

        # Since the reference to the package decl is non transitive, we still
        # want to reference the envs that are "used" there.
        reference(Self.cast(AdaNode).singleton,
                  through=T.PackageBody.package_decl_uses_clauses_envs,
                  cond=Not(Self.is_compilation_unit_root))
    )

    package_name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.package_name.singleton)
    defining_env = Property(Entity.children_env)

    declarative_parts = Property(Entity.decls.singleton)

    @langkit_property()
    def package_decl_uses_clauses_envs():
        """
        Return the environments for the use clauses of the package decl of this
        body. Used because they need to be explicitly referenced.
        """
        pd = Var(imprecise_fallback.bind(
            False, Entity.decl_part.cast_or_raise(T.BasePackageDecl)
        ))

        return Array([pd.public_part.use_clauses_envs,
                      pd.private_part._.use_clauses_envs]).env_group()


class TaskBody(Body):
    """
    Task body (:rmlink:`9.1`).
    """

    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.name.singleton)

    declarative_parts = Property(Entity.decls.singleton)

    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(Self.body_initial_env),

        add_to_env(Entity.previous_part_link_env_assoc),

        add_env(),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit),
        ),

        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),

        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.nested_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.prioritary,
            shed_corresponding_rebindings=True,
        ),

        # We must also "inherit" the use clauses from the generic formal part
        # of this body's generic declaration, if relevant.
        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.use_clauses_in_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.normal
        ),

        reference(Self.cast(AdaNode).singleton,
                  T.TaskBody.task_type_decl_scope,
                  kind=RefKind.prioritary),

        # Reference stub's env if the body is a separate
        reference(Self.cast(AdaNode).singleton,
                  through=T.Body.subunit_decl_env,
                  cond=Self.is_subunit,
                  kind=RefKind.prioritary),
    )

    task_type_decl_scope = Property(Entity.task_type.children_env)

    @langkit_property()
    def task_type():
        return imprecise_fallback.bind(False, Entity.decl_part.match(
            lambda t=T.TaskTypeDecl: t,
            lambda t=T.SingleTaskDecl: t.task_type,
            lambda _: PropertyError(T.TaskTypeDecl.entity, "Should not happen")
        ))


class ProtectedBody(Body):
    """
    Protected object body (:rmlink:`9.4`).
    """

    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(Self.body_initial_env),

        add_to_env(Entity.previous_part_link_env_assoc),

        add_env(),

        do(Self.populate_dependent_units),

        reference(
            Self.top_level_use_package_clauses,
            through=T.Name.use_package_name_designated_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.top_level_use_type_clauses,
            through=T.Name.name_designated_type_env,
            cond=Self.parent.is_a(T.LibraryItem, T.Subunit)
        ),
        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.nested_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.prioritary,
            shed_corresponding_rebindings=True,
        ),

        # We must also "inherit" the use clauses from the generic formal part
        # of this body's generic declaration, if relevant.
        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.use_clauses_in_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.normal
        ),

        reference(Self.cast(AdaNode).singleton,
                  through=T.Body.body_decl_scope,
                  kind=RefKind.prioritary),

        # Reference stub's env if the body is a separate
        reference(Self.cast(AdaNode).singleton,
                  through=T.Body.subunit_decl_env,
                  cond=Self.is_subunit,
                  kind=RefKind.prioritary),
    )

    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)
    decls = Field(type=T.DeclarativePart)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.name.singleton)

    declarative_parts = Property(Entity.decls.singleton)


class EntryBody(Body):
    """
    Entry body (:rmlink:`9.5.2`).
    """

    entry_name = Field(type=T.DefiningName)
    index_spec = Field(type=T.EntryIndexSpec)
    params = Field(type=T.EntryCompletionFormalParams)
    aspects = Field(type=T.AspectSpec)
    barrier = Field(type=T.Expr)

    decls = Field(type=T.DeclarativePart)
    stmts = Field(type=T.HandledStmts)
    end_name = Field(type=T.EndName)

    defining_names = Property(Entity.entry_name.singleton)

    declarative_parts = Property(Entity.decls.singleton)

    env_spec = EnvSpec(
        do(Self.env_hook),

        set_initial_env(
            direct_env(
                env.bind(
                    Self.default_initial_env,
                    Entity.body_scope(False)
                )
            ),
        ),

        # Add the body to its own parent env
        add_to_env(
            new_env_assoc(Entity.name_symbol, Self).singleton,
        ),

        add_env(),
    )

    xref_entry_point = Property(True)

    @langkit_property(return_type=T.Equation)
    def xref_equation():
        return And(
            Bind(Self.barrier.expected_type_var, Self.bool_type),
            Entity.barrier.sub_equation,
            Entity.barrier.matches_expected_formal_prim_type
        )


class EntryIndexSpec(BasicDecl):
    """
    Index specification for an entry body (:rmlink:`9.5.2`).
    """

    id = Field(type=T.DefiningName)
    subtype = Field(type=T.AdaNode)
    aspects = Field(type=T.AspectSpec)

    env_spec = EnvSpec(add_to_env_kv(Entity.name_symbol, Self))

    defining_names = Property(Entity.id.singleton)
    defining_env = Property(Entity.expr_type.defining_env)
    expr_type = Property(Entity.subtype.match(
        lambda subt=T.SubtypeIndication: subt.designated_type,
        lambda e: e.cast_or_raise(T.Expr).expression_type,
    ))

    @langkit_property()
    def xref_equation():
        return Entity.subtype.sub_equation

    xref_entry_point = Property(True)


class Subunit(AdaNode):
    """
    Subunit (``separate``) (:rmlink:`10.1.3`).
    """

    name = Field(type=T.Name)
    body = Field(type=T.Body)

    @langkit_property(return_type=T.CompilationUnit.entity)
    def root_unit():
        """
        Return the compilation unit in which this subunit is rooted.
        """
        return Self.designated_compilation_unit(
            Self.name.as_symbol_array,
            UnitBody,
            load_if_needed=True,
            not_found_is_error=True
        ).as_bare_entity

    @langkit_property()
    def env_hook_subunit():
        """
        Helper for AdaNode.env_hook. Handle sub-units (separates).
        """
        # Subunit handling is very simple: we just want to fetch the containing
        # unit.
        ignore(Var(Self.root_unit))
        return False

    @langkit_property(public=True)
    def body_root():
        """
        Return the body in which this subunit is rooted.
        """
        return Self.root_unit.decl.as_bare_entity

    xref_entry_point = Property(True)
    xref_equation = Property(Bind(Self.name.ref_var, Entity.body_root))


class ProtectedBodyStub(BodyStub):
    """
    Stub for a protected object body (``is separate``) (:rmlink:`10.1.3`).
    """

    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)

    env_spec = EnvSpec(
        add_to_env(Entity.previous_part_link_env_assoc),
        add_env(names=Self.env_names)
    )


class SubpBodyStub(BodyStub):
    """
    Stub for a subprogram body (``is separate``) (:rmlink:`10.1.3`).
    """

    overriding = Field(type=Overriding)
    subp_spec = Field(type=T.SubpSpec)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.subp_spec.name.singleton)
    # Note that we don't have to override the defining_env property here since
    # what we put in lexical environment is their SubpSpec child.

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.name_symbol,
            value=Self
        ),
        add_env(names=Self.env_names),

        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.nested_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.prioritary,
            shed_corresponding_rebindings=True,
        ),

        # We must also "inherit" the use clauses from the generic formal part
        # of this body's generic declaration, if relevant.
        reference(
            Self.cast(T.AdaNode)._.singleton,
            through=T.AdaNode.use_clauses_in_generic_formal_part,
            cond=Self.should_ref_generic_formals,
            kind=RefKind.normal
        )
    )

    type_expression = Property(Entity.subp_spec.returns)


class PackageBodyStub(BodyStub):
    """
    Stub for a package body (``is separate``) (:rmlink:`10.1.3`).
    """

    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)

    env_spec = EnvSpec(
        add_to_env(Entity.previous_part_link_env_assoc),
        add_env(names=Self.env_names)
    )


class TaskBodyStub(BodyStub):
    """
    Stub for a task body (``is separate``) (:rmlink:`10.1.3`).
    """

    name = Field(type=T.DefiningName)
    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)

    env_spec = EnvSpec(
        add_to_env(Entity.previous_part_link_env_assoc),
        add_env(names=Self.env_names)
    )


class LibraryItem(AdaNode):
    """
    Library item in a compilation unit (:rmlink:`10.1.1`).
    """

    has_private = Field(type=Private)
    item = Field(type=T.BasicDecl)


class RangeSpec(AdaNode):
    """
    Range specification (:rmlink:`3.5.7`).
    """

    range = Field(type=Expr)

    xref_stop_resolution = Property(Self.parent.is_a(ComponentClause))
    xref_equation = Property(Entity.range.xref_equation & If(
        # Ada RM says that for component clauses and signed int type
        # definitions, the expected type is any integer type.
        Self.parent.is_a(ComponentClause, SignedIntTypeDef),

        Self.universal_int_bind(Self.range.expected_type_var)
        & Entity.range.matches_expected_type,

        LogicTrue()
    ))


class IncompleteTypeDecl(BaseTypeDecl):
    """
    Incomplete declaration for a type (:rmlink:`12.5`).
    """

    discriminants = Field(type=T.DiscriminantPart)
    aspects = NullField()

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def find_next_part_in(decl_part=T.DeclarativePart.entity):
        """
        Searches for the next part of Self inside the given declarative part.
        Since Self is an IncompleteTypeDecl, the next part will necessarily be
        the first type declaration of the same name that is not Self.
        """
        return decl_part.decls.find(
            lambda t: t.cast(BaseTypeDecl).then(
                lambda btd:
                btd.name.name_is(Self.name_symbol) & (btd != Entity)
            )
        ).cast(BaseTypeDecl)

    env_spec = EnvSpec(
        add_to_env_kv(Entity.name_symbol, Self),
        add_env()
    )

    defining_env = Property(
        Array([Self.children_env, Self.dottable_subps_env]).env_group(),
        type=LexicalEnv
    )

    discriminants_list = Property(Entity.discriminants.abstract_formal_params)


class IncompleteTaggedTypeDecl(IncompleteTypeDecl):
    """
    Incomplete declaration for a tagged type.
    """

    has_abstract = Field(type=Abstract)

    is_tagged_type = Property(True)


class Params(AdaNode):
    """
    List of parameter specifications.
    """

    params = Field(type=ParamSpec.list)


class ParentList(Name.list):
    """
    List of parents in a type declaration.
    """

    pass


class DiscriminantChoiceList(Identifier.list):
    """
    List of discriminant associations.
    """

    pass


class AlternativesList(AdaNode.list):
    """
    List of alternatives in a ``when ...`` clause.
    """

    @langkit_property(return_type=T.BaseTypeDecl.entity)
    def enum_type():
        """
        If this AlternativesList belongs to a case statement, return the type
        of the enum this case statement operates on. Null otherwise.
        """
        return Entity.parent.parent.parent.cast(T.CaseStmt).then(
            lambda cs: cs.expr.expression_type
        )

    @langkit_property(return_type=T.CompletionItem.array)
    def complete_items():
        """
        Return possible completions at this point in the file.
        """
        return Self.children_env.get(No(Symbol)).map(
            lambda n: CompletionItem.new(
                decl=n.cast(T.BasicDecl),
                is_dot_call=n.info.md.dottable_subp,
                is_visible=Self.has_with_visibility(n.unit),
                weight=n.match(
                    lambda eld=T.EnumLiteralDecl: If(
                        Entity.enum_type == eld.enum_type,
                        100,
                        0
                    ),
                    lambda _: 0
                )
            )
        )


class ExprAlternativesList(Expr.list):
    """
    List of alternatives in a membership test expression.
    """

    pass


class ConstraintList(AdaNode.list):
    """
    List of constraints.
    """

    pass


class UnconstrainedArrayIndex(AdaNode):
    """
    List of unconstrained array indexes.
    """

    subtype_indication = Field(type=SubtypeIndication)

    @langkit_property(dynamic_vars=[origin])
    def designated_type():
        return Entity.subtype_indication.designated_type


class AbstractStateDecl(BasicDecl):
    """
    Contained (directly or indirectly) in an AbstractStateDeclExpr, and is used
    to represent the BasicDecl associated with the abstract state introduced by
    the Abstract_State aspect. This node is necessary because all of our name
    resolution routines expect BasicDecls as environments' values.

    The only purpose of this node is to populate the env with the abstract
    state declared through this node, so it can be referred in SPARK aspects
    such as Global, Depends, Refined_State, etc.
    """
    name = Field(type=T.DefiningName)

    aspects = Field(type=T.AspectSpec)

    defining_names = Property(Entity.name.singleton)
    type_expression = Property(No(T.TypeExpr.entity))

    env_spec = EnvSpec(
        add_to_env_kv(Self.name.name_symbol, Self)
    )


class ParenAbstractStateDecl(AdaNode):
    """
    Holds an AbstractStateDecl between parentheses. Needed to support the
    syntax:

    .. code:: ada

        package Pkg
            with Abstract_State => (A, (B with Some_Aspect))
    """
    decl = Field(type=AdaNode)


class AbstractStateDeclList(AdaNode.list):
    """
    List of AbstractStateDecls.
    """
    pass


class MultiAbstractStateDecl(AdaNode):
    """
    Node that holds several AbstractStateDecl nodes, which is necessary when
    the Abstract_State aspect is associated with an aggregate in order to
    declare a list of abstract states.
    """
    decls = Field(type=AbstractStateDeclList)


class AbstractStateDeclExpr(Expr):
    """
    Directly corresponds to the right-hand side of the Abstract_State aspect.
    Only exists because the RHS of an AspectAssoc must be an expression: the
    actual logic is in AbstractStateDecl.
    """
    state_decl = Field(type=AdaNode)

    xref_equation = Property(LogicTrue())


@abstract
class PpDirective(AdaNode):
    """
    Base node for all preprocessor directives.
    """


class PpThenKw(AdaNode):
    """
    ``then`` keyword in preprocessor directives.
    """
    # Unparsers require to have a single sequence of tokens for a given node.
    # We need parsers for ``PpIfDirective`` and ``PpElseDirective`` to accept
    # both ``[els]if X then`` and ``[els]if X`` syntax forms, so we have to
    # create a (possible null) ``then_kw`` field for both.
    pass


class PpIfDirective(PpDirective):
    """
    ``if ... [then]`` preprocessor directive.
    """
    expr = Field(type=Expr)
    then_kw = Field(type=PpThenKw)


class PpElsifDirective(PpDirective):
    """
    ``elsif ... [then]`` preprocessor directive.
    """
    expr = Field(type=Expr)
    then_kw = Field(type=PpThenKw)


class PpElseDirective(PpDirective):
    """
    ``else`` preprocessor directive.
    """
    pass


class PpEndIfDirective(PpDirective):
    """
    ``end if;`` preprocessor directive.
    """
    pass
