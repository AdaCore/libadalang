"""
This represents the core of the expressions parts of the DSL, that will be used
notably to define properties on AST nodes.

- Users create trees of AbstractExpression subclasses instances, and wrap them
  in Property instances in ASTNode subclasses.
- Code generation (Property.render) is done in two steps. First,
  AbstractExpression.construct returns ResolvedExpression trees which are bound
  to specific ASTNode and Field classes.
- Finally, those ResolvedExpression trees are then used to generate concrete
  code for properties in the generated library.
"""

from contextlib import contextmanager
import names
from utils import Colors, col


class Frozable(object):
    """
    Trait class that defines:

    - A frozen read-only property, False by default;
    - A freeze method that sets the property to True.

    The idea is that classes can then derive from this trait and define a
    special behavior for when the object is frozen. This is used by the
    Expression classes to make sure that the user of those classes does not
    accidentally create new expressions while trying to rely on the classes's
    non magic behavior.

    For example, for an object that implements the FieldTrait trait, you might
    want to access regular fields on the object in the implementation part:

    >>> a = FieldTraitInstance()
    >>> a.wrong_spellled_field

    If the object is not frozen, this will generate a new FieldAccess object.
    If it is frozen, this will throw an exception.
    """

    @property
    def frozen(self):
        """
        Returns wether the object is frozen.

        :rtype: bool
        """
        return self.__dict__.get('_frozen', False)

    def freeze(self):
        """
        Freeze the object and all its frozable components recursively.
        """
        self._frozen = True
        for _, val in self.__dict__.items():
            if isinstance(val, Frozable):
                val.freeze()


class FieldTrait(Frozable):
    """
    Trait class for objects on which you can use the field access notation,
    which will construct a new expression.
    """

    def __getattr__(self, attr):
        """
        Returns a FieldAccess expression object when the user uses the field
        access notation on self.

        :rtype: FieldAccess
        """
        if self.frozen:
            raise Exception("Illegal field access")
        return FieldAccess(self, attr)


class CallTrait(Frozable):
    """
    Trait class for objects on which you can use the field access notation,
    which will construct a new expression.
    """

    def __call__(self, *args, **kwargs):
        """
        Returns a OpCall expression object when the user uses the call
        notation on self.

        :rtype: OpCall
        """
        if self.frozen:
            raise Exception("Illegal call expr")
        return OpCall(self, args, kwargs)


class OrTrait(Frozable):
    """
    Trait class for objects on which you can use the binary or notation, which
    will construct a new expression.
    """

    def __or__(self, other):
        """
        Returns a OrExpr expression object when the user uses the binary or
        notation on self.

        :rtype: OrExpr
        """
        if self.frozen:
            raise Exception("Illegal or expr")
        return OrExpr(self, other)


class AndTrait(Frozable):
    """
    Trait class for objects on which you can use the binary and notation, which
    will construct a new expression.
    """

    def __and__(self, other):
        """
        Returns a AndExpr expression object when the user uses the binary and
        notation on self.

        :rtype: AndExpr
        """
        if self.frozen:
            raise Exception("Illegal and expr")
        return AndExpr(self, other)


class AbstractExpression(object):
    """
    An abstract expression is an expression that is not yet resolved. To be
    able to emulate lexical scope in expressions, the expression trees produced
    by initial python evaluation of the expressions will be a tree of
    AbstractExpression objects.

    You can then call construct on the root of the expression tree to get back
    a resolved tree of ResolvedExpression objects.
    """

    def construct(self):
        """
        Returns a resolved tree of resolved expressions.

        :rtype: ResolvedExpression
        """
        raise NotImplementedError()


class OrExpr(AbstractExpression):
    """
    Abstract expression that is the result of the evaluation of an or
    expression.

    TODO: Not implemented yet!
    """
    def __init__(self, left, right):
        self.left = left
        self.right = right


class AndExpr(AbstractExpression):
    """
    Abstract expression that is the result of the evaluation of an and
    expression.

    TODO: Not implemented yet!
    """
    def __init__(self, left, right):
        self.left = left
        self.right = right


class OpCall(AbstractExpression, FieldTrait, OrTrait, AndTrait):
    """
    Abstract expression that is the result of a call expression evaluation.

    TODO: Not implemented yet!
    """
    def __init__(self, called, args, kwargs):
        self.called = called
        self.args = args
        self.kwargs = kwargs

    def __repr__(self):
        return "<OpCall {} {} {}>".format(self.called, self.args, self.kwargs)


class FieldAccess(AbstractExpression, CallTrait, FieldTrait):
    """
    Abstract expression that is the result of a field access expression
    evaluation.
    """

    def __init__(self, receiver, field):
        """
        :param AbstractExpression receiver: Expression on which the field
               access was done.
        :param str field: The name of the field that is accessed.
        """
        self.receiver = receiver
        self.field = field

    def construct(self):
        """
        Constructs a resolved expression that is the result of:

        - Resolving the receiver;
        - Getting its corresponding field.

        :rtype: FieldAccessExpr
        """
        receiver_expr = self.receiver.construct()

        # First search for a property
        to_get = receiver_expr.type._properties.get(
            self.field,
            # If there's no property by that name, search for a field
            receiver_expr.type._fields.get(self.field, None)
        )

        # If still not found, there's a problem
        assert to_get, col("Type {} has no '{}' field or property".format(
            receiver_expr.type.__name__, self.field
        ), Colors.FAIL)

        ret = FieldAccessExpr(receiver_expr, to_get)
        return ret

    def __repr__(self):
        return "<FieldAccess {} {}>".format(self.receiver, self.field)


class PlaceHolder(AbstractExpression, FieldTrait):
    """
    Abstract expression that is an entry point into the expression DSL.

    If you have an instance of a PlaceHolder, you can use it to construct
    abstract expressions.

    You can then resolve the constructed expressions by:
    - Binding the type of the PlaceHolder instance via a call to the bind
      context manager.
    - Calling construct on the PlaceHolder.
    """

    def __init__(self, name):
        """
        :param str name: The name of the PlaceHolder variable.
        """
        self.name = name
        self._type = None

    @contextmanager
    def bind(self, type):
        """
        Bind the type of this placeholder.
        """
        self._type = type
        yield
        self._type = None

    def construct(self):
        return VarExpr(self._type, self.name)

    @property
    def type(self):
        return self._type

    def __repr__(self):
        return "<PlaceHolder {}>".format(self.name)


Self = PlaceHolder("Self")


def render(*args, **kwargs):
    from compiled_types import render
    return render(*args, property=Property.get(), Self=Self, **kwargs)


class ResolvedExpression(object):
    """
    Resolved expressions are expressions that can be readily rendered to code
    that will correspond to the initial expression, depending on the bound
    lexical scope.
    """

    def render_expr(self):
        """
        Renders the expression itself.

        :rtype: basestring
        """
        raise NotImplementedError()

    def render_pre(self):
        """
        Renders initial statements that might be needed to the expression.

        :rtype: basestring
        """
        return ""

    def render(self):
        """
        Render both the initial statements and the expression itself. This is
        basically a wrapper that calls render_pre and render_expr in turn.

        :rtype: basestring
        """
        return "{}\n{}".format(self.render_pre(), self.render_expr())

    @property
    def type(self):
        """
        Returns the type of the resolved expression.

        :rtype: CompiledType
        """
        return NotImplementedError()


class VarExpr(ResolvedExpression):
    """
    Resolved expression that represents a variable in generated code.
    """

    def __init__(self, type, name):
        self._type = type
        self.name = name

    @property
    def type(self):
        return self._type

    def render_expr(self):
        return self.name


class FieldAccessExpr(ResolvedExpression):
    """
    Resolved expression that represents a field access in generated code.
    """

    def __init__(self, receiver_expr, property):
        """
        :param ResolvedExpression receiver_expr: The receiver of the field
               access.
        :param Property|Field property: The accessed property or field.
        """
        self.receiver_expr = receiver_expr
        self.property = property

    @property
    def type(self):
        return self.property.type

    def __repr__(self):

        return "<FieldAccessExpr {} {} {}>".format(
            self.receiver_expr, self.property, self.type
        )

    def render_expr(self):
        return "{}.{}".format(self.receiver_expr.render(), self.property.name)


class LocalVars(object):
    """
    Represents the state of local variables in a property definition.
    """

    def __init__(self):
        self.local_vars = {}

    class LocalVar(object):
        """
        Represents one local variable in a property definition.
        """
        def __init__(self, name, type):
            self.name = name
            self.type = type

        def render(self):
            return "{} : {};".format(self.name, self.type)

    def __call__(self, name, type):
        """
        This getattr override allows you to declare local variables in
        templates via the syntax::

            >>> var = vars('Index', 'Integer')
        """
        ret = LocalVars.LocalVar(self, name, type)
        assert name not in self.local_vars, (
            "Already declared local variable {}".format(name)
        )
        self.local_vars[name] = ret
        return ret

    def __getattr__(self, name):
        """
        Returns existing instance of variable called name, so that you can use
        existing variables via the syntax::

            >>> ivar = var.Index
        """
        return self.local_vars[name]

    def render(self):
        return "\n".join(lv.render() for lv in self.local_vars)


class Property(object):
    """
    This is the public class via which you'll create properties in the DSL.

    You can declare your properties on your ast node subclasses directly, like
    this::

        class SubNode(ASTNode):
            my_field = Field()
            my_property = Property(Self.my_field)

    and functions will be generated in the resulting library.
    """

    __current_property__ = None

    def __init__(self, expr):
        """
        :param AbstractExpression expr: The expression for the property
        """
        self.expr = expr
        self.constructed_expr = None
        self.vars = LocalVars()

    @classmethod
    def get(cls):
        """
        Return the currently bound property. Used by the rendering context to
        get the current property.
        """
        return cls.__current_property__

    @contextmanager
    def bind(self):
        """
        Bind the current property to self, so that it is accessible in the
        expression templates.
        """
        assert self.__current_property__ is None, (
            "You cannot nest calls to Property.bind context manager"
        )
        self.__class__.__current_property__ = self
        yield
        self.__class__.__current_property__ = None

    @property
    def type(self):
        """
        Returns the type of the underlying expression after resolution.
        """
        return self.constructed_expr.type

    def render(self, owner_type):
        """
        Render the given property to generated code.

        :param CompiledType owner_type: The ast node subclass to which this
                                        property is bound.
        :rtype: basestring
        """
        with Self.bind(owner_type):
            self.expr.freeze()
            self.constructed_expr = self.expr.construct()
            with names.camel_with_underscores:
                with self.bind():
                    self.prop_decl = render('properties/decl_ada')
                    self.prop_def = render('properties/def_ada')

    @property
    def name(self):
        """
        Return the name of the property, namely P_ + the name defined by the
        user.
        :rtype: names.Name
        """
        assert self._name
        from names import Name
        return Name("P") + self._name

    @name.setter
    def name(self, name):
        assert isinstance(name, names.Name)
        self._name = name
