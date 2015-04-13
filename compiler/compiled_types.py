from itertools import count

from c_api import CAPIType
from common import get_type, null_constant
from template_utils import TemplateEnvironment, common_renderer
from utils import FieldAccessor, memoized, type_check
from python_api import PythonAPIType


def decl_type(ada_type):
    res = ada_type.name()
    return res.strip() + ("*" if ada_type.is_ptr else "")


class CompiledType(object):
    """
    Base class used to describe types in the generated code.

    It is intended to be subclassed in order to create now compiled types.
    However, subclasses are not intended to be instantiated.
    """

    # Whether this type is handled through pointers only in the generated code.
    is_ptr = True

    def __init__(self):
        assert False, (
            'CompiledType subclasses are not meant to be instantiated'
        )

    @classmethod
    def add_to_context(cls, compile_ctx):
        """
        If needed, put bits into `compile_ctx` to implement this compiled type.

        Must be overriden in subclasses if needed.
        """
        pass

    @classmethod
    def needs_refcount(cls):
        raise NotImplementedError()

    @classmethod
    def name(cls):
        """
        Return a string to be used in code generation to reference this type.

        Must be overriden in subclasses.
        """
        raise NotImplementedError()

    @classmethod
    def nullexpr(cls):
        """
        Return a string to be used in code generation for "null" expressions.

        Must be overriden in subclasses.
        """
        raise NotImplementedError()

    @classmethod
    def c_type(cls, c_api_settings):
        """Return a CAPIType instance for this type

        Must be overriden in subclasses.
        """
        raise NotImplementedError()

    @classmethod
    def py_type(cls, python_api_settings):
        """Return a PythonAPIType instance for this type

        Must be overriden in subclasses.
        """
        raise NotImplementedError()


class BasicType(CompiledType):
    """
    Base class used to describe simple types that do not need declaration code
    generation.
    """
    _name = None
    _nullexpr = None
    _external = False

    @classmethod
    def needs_refcount(cls):
        return False

    @classmethod
    def name(cls):
        return cls._name

    @classmethod
    def nullexpr(cls):
        return cls._nullexpr

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, cls.name(), external=cls._external)


class BoolType(BasicType):
    is_ptr = False
    _name = get_type(bool)
    _nullexpr = "false"

    @classmethod
    def c_type(cls, c_api_settings):
        # "bool" is not a built-in in C: do not force users to pull
        # stdbool.h...
        return CAPIType(c_api_settings, 'int', external=True)

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'c_int', True)


class LongType(BasicType):
    is_ptr = False
    _name = get_type(long)
    _nullexpr = "0"
    _external = True

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'c_long', True)


class SourceLocationRangeType(BasicType):
    is_ptr = False
    _name = "SourceLocationRange"
    _nullexpr = "SourceLocationRange()"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'source_location_range')

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'SlocRange', False)


class Token(BasicType):
    is_ptr = False
    _name = "Token"
    _nullexpr = "no_token"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'token')

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'token', False)


class NoToken(Token):
    quex_token_name = "TERMINATION"


class Field(object):
    """
    Placeholder descriptors used to associate data to AST nodes (see below).
    """

    # Hack: the field declarations order in AST nodes matters.  The simple and
    # very handy syntax we use here for such declarations doesn't preserve this
    # order in Python2, however.  Waiting for the move to Python3, we use a
    # hack here: the following counter will help us to recover the declaration
    # order (assuming it is the same as the Field instantiation order).
    _counter = iter(count(0))

    def __init__(self, repr=True, indent=None):
        """Create an AST node field.

        :param bool repr: If true, the field will be displayed when
        pretty-printing the embedding AST node.

        :param indent: Property used for determining the indentation rules
        for this field. The expected type is the `:class:Indent`, but there
        are shorcuts for int and basestring that will be automatically
        converted to an indent instance according to the following rules:
          * str -> relative to ASTNode instance's own field name
          * int -> relative offset (as before)
          * others -> no shortcut

        :type indent: int|basestring|Indent
        """
        self.repr = repr
        self._name = None

        self.indent = None
        ":type: Indent"

        if indent is None:
            self.indent = indent_rel()
        elif isinstance(indent, int):
            self.indent = indent_rel(indent)
        elif isinstance(indent, basestring):
            self.indent = indent_token(indent)
        else:
            assert isinstance(indent, Indent)
            self.indent = indent

        self._index = next(self._counter)

    def _get_name(self):
        assert self._name
        return self._name

    def _set_name(self, name):
        assert isinstance(name, basestring)
        self._name = name

    name = property(_get_name, _set_name)

    def __repr__(self):
        return '<ASTNode {} Field({})>'.format(self._index, self._name)


class AstNodeMetaclass(type):
    """
    Internal metaclass for AST nodes, used to ease fields handling during code
    generation.
    """
    def __new__(mcs, name, bases, dct):
        assert len(bases) == 1, (
            "Multiple inheritance for AST nodes is not supported")

        fields = []

        # Associate a name to all fields and collect them into `field`...
        for fld_name, fld_value in dct.items():
            if isinstance(fld_value, Field):
                fld_value.name = fld_name
                fields.append(fld_value)

        # ... and then remove them as class members: we want them to be
        # stored in a single class member: the "field" one, being a list.
        for field in fields:
            dct.pop(field.name)

        # Hack to recover the order of field declarations.  See the Field class
        # definition for more details.
        dct['fields'] = sorted(fields, key=lambda f: f._index)

        # By default, ASTNode subtypes aren't abstract.
        dct['abstract'] = False

        return type.__new__(mcs, name, bases, dct)


def abstract(cls):
    """Decorator to tag an ASTNode subclass as abstract."""
    assert issubclass(cls, ASTNode)
    cls.abstract = True
    return cls


class ASTNode(CompiledType):
    """
    Base class for all user AST nodes.

    Subclasses can define new AST node types, but they also can be abstract
    themselves (to form a true tree of AST node types).  Each subclass can
    define a list of fields (see the above Field class), so that each concrete
    class' fields are the sum of all its subclass' fields plus its own.

    This base class defines utilities to emit native code for the AST node
    types: type declaration, type definition and type usage (to declare
    AST node variables).
    """

    abstract = False
    fields = []
    __metaclass__ = AstNodeMetaclass

    @classmethod
    def create_type_declaration(cls):
        """Return a forward type declaration for this AST node type."""
        return make_renderer().render('astnode_type_decl', cls=cls)

    @classmethod
    def needs_refcount(cls):
        return True

    @classmethod
    def create_type_definition(cls, compile_ctx):
        """
        Emit a type definition for this AST node type in
        `compile_ctx.types_definitions`, emit:
          - a class with all its fields and its methods;
          - a forward declaration for this AST node type's "nil" singleton;

        Also emit the implementation for the corresponding methods/singletons
        in `compile_ctx.body`.
        """
        base_class = cls.__bases__[0]

        # Some templates need all fields (inherited and not inherited) and some
        # need only not inherited ones.
        assert len(cls.get_types(compile_ctx)) == len(cls.get_fields()), (
            "{}: {} <-> {}".format(
                cls,
                cls.get_types(compile_ctx), cls.get_fields()
            )
        )
        all_field_decls = zip(cls.get_types(compile_ctx), cls.get_fields())
        astnode_field_decls = [field
                               for (field_type, field) in all_field_decls
                               if issubclass(field_type, ASTNode)]
        cls_field_decls = zip(compile_ctx.ast_fields_types[cls], cls.fields)

        t_env = TemplateEnvironment(
            cls=cls,
            all_field_decls=all_field_decls,
            astnode_field_decls=astnode_field_decls,
            cls_field_decls=cls_field_decls,
            types=compile_ctx.ast_fields_types[cls],
            base_name=base_class.name()
        )
        tdef = make_renderer(compile_ctx).render('astnode_type_def', t_env)
        if cls.is_ptr:
            compile_ctx.types_definitions.append(tdef)
        else:
            compile_ctx.val_types_definitions.append(tdef)

        compile_ctx.body.append(make_renderer(compile_ctx).render(
            'astnode_type_impl', t_env
        ))

    @classmethod
    def get_inheritance_chain(cls):
        """
        Return a list for all classes from ASTNode to `cls` in the inheritance
        chain.
        """
        return reversed([base_class for base_class in cls.mro()
                         if issubclass(base_class, ASTNode)])

    @classmethod
    def get_fields(cls):
        """
        Return the list of all the fields `cls` has, including its parents'.
        """
        fields = []
        for base_class in cls.get_inheritance_chain():
            fields.extend(base_class.fields)
        return fields

    @classmethod
    def get_types(cls, compile_ctx):
        """
        Return the list of types for all the fields `cls` has, including its
        parents'.
        """
        types = []
        for base_class in cls.get_inheritance_chain():
            types.extend(compile_ctx.ast_fields_types[base_class])
        return types

    @classmethod
    def get_public_fields(cls, compile_ctx):
        """
        Return a (field, field type) list for all the fields that are
        readable through the public API for this node type (excluding fields
        from parents types).

        This is used in the context of accessors generation. Fields from
        parent types are handled as part of parent types themselves, so there
        is no need to handle them here.
        """
        return [(field, field_type)
                for field, field_type in zip(
                cls.fields, compile_ctx.ast_fields_types[cls])]

    @classmethod
    def add_to_context(cls, compile_ctx):
        """
        Emit code to `compile_ctx` for this AST node type.  Do nothing if
        called more than once on a single class or if called on ASTNode itself.
        """
        if cls not in compile_ctx.types and cls != ASTNode:
            base_class = cls.__bases__[0]
            if issubclass(base_class, ASTNode) and base_class != ASTNode:
                base_class.add_to_context(compile_ctx)

            compile_ctx.types.add(cls)
            compile_ctx.types_declarations.append(
                cls.create_type_declaration())
            cls.create_type_definition(compile_ctx)

            # Generate field accessors (C public API) for this node kind
            primitives = []
            render = make_renderer(compile_ctx).render
            for field, field_type in cls.get_public_fields(compile_ctx):
                accessor_basename = '{}_{}'.format(cls.name(), field.name)
                accessor_fullname = compile_ctx.c_api_settings.get_name(
                    accessor_basename)

                t_env = TemplateEnvironment(
                    astnode=cls,
                    field=field,
                    field_type=field_type,
                    accessor_name=accessor_fullname,
                )
                accessor_decl = render(
                    'c_api/astnode_field_access_decl', t_env)
                accessor_impl = render(
                    'c_api/astnode_field_access_impl', t_env)

                primitives.append(FieldAccessor(
                    accessor_basename,
                    declaration=accessor_decl,
                    implementation=accessor_impl,
                    field=field,
                    field_type=field_type,
                ))
            compile_ctx.c_astnode_primitives[cls] = primitives

            # For the Python API, generate subclasses for each AST node kind
            # (for both abstract and concrete classes). Each will ship accessor
            # for the fields they define.
            if compile_ctx.python_api_settings:
                compile_ctx.py_astnode_subclasses[cls] = render(
                    'python_api/ast_subclass',
                    pyapi=compile_ctx.python_api_settings,
                    cls=cls,
                    parent_cls=list(cls.get_inheritance_chain())[-2],
                    primitives=primitives,
                )

    @classmethod
    def name(cls):
        """
        Return the name that will be used in code generation for this AST node
        type.
        """
        return cls.__name__

    @classmethod
    def repr_name(cls):
        """Return a name that will be used when serializing this AST node."""
        return getattr(cls, "_repr_name", cls.name())

    @classmethod
    def nullexpr(cls):
        """
        Return a value that can be considered as "null" for this AST node type.
        It indicates the absence of AST node.
        """
        if cls.is_ptr:
            return null_constant()
        else:
            return "nil_{0}".format(cls.name())

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, 'node')

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'node', False)


# We want structural equality on lists whose elements have the same types.
# Memoization is one way to make sure that, for each CompiledType subclass X:
#    list_type(X) == list_type(X)
@memoized
def list_type(element_type):
    """
    Return an ASTNode subclass that represent a list of `element_type`.
    """

    # List types do not need generated code for their declaration since they
    # are instantiations of a generic "ASTList" type, which inherits ASTNode
    # (so they can be considered as ASTNode).

    return type(
        '{}ListType'.format(element_type.name()), (ASTNode, ), {
            'is_ptr':   True,
            'name':     classmethod(
                lambda cls: make_renderer().render(
                    'list_type', el_type=element_type)
            ),
            'nullexpr': classmethod(lambda cls: null_constant()),
        }
    )


class EnumType(CompiledType):
    """
    Base class for compiled types that hold a single value in a set of possible
    ones.

    Subclasses must override the `alternatives` member to hold a list of
    distinct strings that represent the set of possibilities.  They represent
    the compiled type.

    Instances represent either the enum type itself in the generated code or a
    particular enum value.
    """

    is_ptr = False
    alternatives = []

    @classmethod
    def needs_refcount(cls):
        return False

    # noinspection PyMissingConstructor
    def __init__(self, alt):
        """Create a value that represent one of the enum alternatives."""
        # CompiledType are not usually supposed to be instantiated.  EnumType
        # is an exception to this rule, so do not call CompiledType.__init__.
        assert alt in self.alternatives
        self.alt = alt

    @classmethod
    def name(cls):
        return cls.__name__

    @classmethod
    def add_to_context(cls, compile_ctx):
        if cls not in compile_ctx.types:
            render = make_renderer(compile_ctx).render
            compile_ctx.types.add(cls)
            compile_ctx.types_declarations.append(
                render('enum_type_decl', cls=cls)
            )
            compile_ctx.body.append(
                render('enum_type_impl', compile_ctx=compile_ctx, cls=cls)
            )
            compile_ctx.c_astnode_field_types[cls] = render(
                'c_api/enum_type_decl', compile_ctx=compile_ctx, cls=cls,
            )
            if compile_ctx.python_api_settings:
                compile_ctx.py_astnode_field_types[cls] = render(
                    'python_api/enum_type_decl', cls=cls,
                    pyapi=compile_ctx.python_api_settings
                )

    @classmethod
    def nullexpr(cls):
        return cls.name() + "::uninitialized"

    @classmethod
    def c_type(cls, c_api_settings):
        return CAPIType(c_api_settings, cls.name())

    @classmethod
    def py_type(cls, python_api_settings):
        return PythonAPIType(python_api_settings, 'c_uint', True)

    @classmethod
    def c_alternatives(cls, c_api_settings):
        """
        Return the sequence of names to use for alternatives in the C API
        """
        # Before wrapping, names can have "_" suffixes or prefixes in order to
        # avoid clashes with keywords (for instance: "or_" instead of "or").
        # This is not needed anymore after wrapping (libfoobar_or is not a
        # reserved keyword) so remove it to have pleasant names.
        return [
            c_api_settings.get_name(
                "{}_{}".format(cls.name(), alt.strip('_'))
            )
            for alt in cls.alternatives
        ]


def make_renderer(compile_ctx=None, base_renderer=None):
    """Create a template renderer with common helpers

    If "compile_ctx" is provided, its CAPISettings instance is passed to the
    renderer and so are all commonly used C API types.
    """
    if base_renderer is None:
        base_renderer = common_renderer

    template_args = {
        'is_enum':          type_check(EnumType),
        'is_long':          type_check(LongType),
        'is_bool':          type_check(BoolType),
        'is_ast_node':      type_check(ASTNode),
        'is_sloc_range':    type_check(SourceLocationRangeType),
        'is_token_type':    type_check(Token),
        'decl_type':        decl_type,
    }
    if compile_ctx:
        capi = compile_ctx.c_api_settings
        template_args.update({
            'capi':             capi,
            'analysis_context_type': CAPIType(capi, 'analysis_context').name,
            'analysis_unit_type':    CAPIType(capi, 'analysis_unit').name,
            'node_kind_type':        CAPIType(capi, 'node_kind_enum').name,
            'node_type':             CAPIType(capi, 'node').name,
            'token_type':            CAPIType(capi, 'token').name,
            'sloc_type':             CAPIType(capi, 'source_location').name,
            'sloc_range_type':       SourceLocationRangeType.c_type(capi).name,
            'diagnostic_type':       CAPIType(capi, 'diagnostic').name,
            })
    return base_renderer.update(template_args)


class Indent(object):
    KIND_REL_POS = 1
    KIND_TOKEN_POS = 2

    def __init__(self, kind, rel_pos=0, token_field_name=""):
        self.kind = kind
        self.rel_pos = rel_pos
        self.token_field_name = token_field_name


def indent_rel(pos=0):
    return Indent(Indent.KIND_REL_POS, rel_pos=pos)


def indent_token(field_name=""):
    return Indent(Indent.KIND_TOKEN_POS, token_field_name=field_name)
