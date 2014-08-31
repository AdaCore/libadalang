from collections import defaultdict
import inspect
from itertools import takewhile, chain
from os import path
import sys
import subprocess

from mako.template import Template

from utils import isalambda, Colors
from quex_tokens import token_map


LANGUAGE = "cpp"

TOKEN_PREFIX = "QUEX_TKN_"

languages_extensions = {
    "ada": "adb",
    "cpp": "cpp",
}

basic_types = {
    "ada": {
        long: "Long_Integer",
        bool: "Boolean"
    },
    "cpp": {
        long: "long",
        bool: "bool"
    }
}

null_constants = {
    "ada": "null",
    "cpp": "nullptr"
}


def c_repr(string):
    return '"{0}"'.format(repr(string)[1:-1].replace('"', r'\"'))


def get_type(typ):
    return basic_types[LANGUAGE][typ]


def null_constant():
    return null_constants[LANGUAGE]


def is_row(parser):
    return isinstance(parser, Row)


def is_discard(parser):
    return isinstance(parser, Discard)

###############
# AST HELPERS #
###############


def decl_type(ada_type):
    res = ada_type.as_string()
    return res.strip() + ("*" if ada_type.is_ptr else "")


class Token(object):

    is_ptr = False

    @classmethod
    def nullexpr(cls):
        return "no_token"

    @classmethod
    def as_string(cls):
        return cls.__name__

    def __init__(self, val=None):
        self.val = val

    def __repr__(self):
        return "{0}({1})".format(
            self.__class__.__name__,
            repr(self.val),
        )

    def __or__(self, other):
        assert isinstance(other, Token)
        return Or(self, other)

    def __eq__(self, other):
        return isinstance(other, self.__class__) and self.val == other.val


class NoToken(Token):
    quex_token_name = "TERMINATION"


no_token = NoToken()


class TemplateEnvironment(object):
    """
    Environment that gathers names for template processing.

    Names are associated to values with the attribute syntax.
    """

    def __init__(self, parent_env=None, **kwargs):
        """
        Create an environment and fill it with var (a dict) and with **kwargs.
        If `parent_env` is provided, the as_dict method will return a dict
        based on the parent's.
        """
        self.parent_env = parent_env
        self.update(kwargs)

    def update(self, vars):
        for name, value in vars.iteritems():
            setattr(self, name, value)

    def as_dict(self):
        """
        Return all names in this environment and the corresponding values as a
        dict.
        """
        result = self.parent_env.as_dict() if self.parent_env else {}
        result.update(self.__dict__)
        return result

    def __setattr__(self, name, value):
        if name == 'as_dict':
            raise TypeError('This attribute is reserved')
        else:
            super(TemplateEnvironment, self).__setattr__(name, value)


def render_template(template_name, template_env=None, **kwargs):
    """
    Render the Mako template `template_name` providing it a context that
    includes the names in `template_env` plus the ones in **kwargs. Return the
    resulting string.
    """
    context = {
        'c_repr':           c_repr,
        'get_type':         get_type,
        'null_constant':    null_constant,
        'is_row':           is_row,
        'is_discard':       is_discard,

        'is_class':         inspect.isclass,
        'decl_type':        decl_type,
    }
    if template_env:
        context.update(template_env.as_dict())
    context.update(kwargs)

    # "self" is a reserved name in Mako, so our variables cannot use it.
    # TODO: don't use "_self" at all in templates. Use more specific names
    # instead.
    if context.has_key('self'):
        context['_self'] = context.pop('self')

    return mako_template(template_name).render(**context)


template_cache = {}


def mako_template(file_name):
    t_path = path.join(path.dirname(path.realpath(__file__)),
                       "templates", LANGUAGE, file_name + ".mako")
    t = template_cache.get(t_path, None)

    if not t:
        t = Template(strict_undefined=True, filename=t_path)
        template_cache[t_path] = t

    return t


__next_ids = defaultdict(int)


def gen_name(var_name):
    __next_ids[var_name] += 1
    return "{0}_{1}".format(var_name, __next_ids[var_name])


def gen_names(*var_names):
    for var_name in var_names:
        yield gen_name(var_name)


class CompiledType(object):

    is_ptr = True

    def create_type_declaration(self):
        raise NotImplementedError()

    def add_to_context(self, compile_ctx, parser):
        raise NotImplementedError()

    def create_type_definition(self, compile_ctx, source_parser):
        raise NotImplementedError()

    def create_instantiation(self, args):
        raise NotImplementedError()

    def name(self):
        raise NotImplementedError()

    def nullexpr(self):
        raise NotImplementedError()

    @classmethod
    def as_string(cls):
        return cls.__name__


class BoolType(CompiledType):
    is_ptr = False

    @classmethod
    def as_string(cls):
        return get_type(bool)

    @classmethod
    def nullexpr(cls):
        return "false"


class LongType(CompiledType):
    is_ptr = False

    @classmethod
    def as_string(cls):
        return get_type(long)

    @classmethod
    def nullexpr(cls):
        return None


class TokenType:
    pass


class Field(object):
    def __init__(self, name, type=None,
                 repr=False, kw_repr=False, opt=False, norepr_null=False):
        self.name = name
        self.kw_repr = kw_repr
        self.repr = repr
        self.type = type
        self.norepr_null = norepr_null
        self.opt = opt


class AstNodeMetaclass(type):
    def __init__(cls, name, base, dct):
        super(AstNodeMetaclass, cls).__init__(name, base, dct)
        cls.fields = dct.get("fields", [])
        cls.abstract = dct.get("abstract", False)


class ASTNode(CompiledType):
    abstract = False
    fields = []
    __metaclass__ = AstNodeMetaclass

    def __init__(self, *args):
        for field, field_val in zip(self.fields, args):
            setattr(self, field.name, field_val)

    @classmethod
    def create_type_declaration(cls):
        return render_template('astnode_type_decl', cls=cls)

    @classmethod
    def create_type_definition(cls, compile_ctx, types):
        base_class = cls.__bases__[0]

        t_env = TemplateEnvironment(
            cls=cls, types=types, base_name=base_class.name()
        )
        tdef = render_template('astnode_type_def', t_env)
        if cls.is_ptr:
            compile_ctx.types_definitions.append(tdef)
        else:
            compile_ctx.val_types_definitions.append(tdef)

        t_env.repr_m_to_fields = [
            (m, f) for m, f in zip(types, cls.fields) if f.repr
        ]
        compile_ctx.body.append(render_template('astnode_type_impl', t_env))

    @classmethod
    def get_fields(cls):
        b = cls.__bases__[0]
        bfields = b.fields if b != ASTNode else []
        return bfields + cls.fields

    @classmethod
    def add_to_context(cls, compile_ctx, parser=None):
        if not cls in compile_ctx.types:
            if not parser:
                parsers = []
            elif isinstance(parser, Row):
                parsers = [m for m in parser.parsers if not isinstance(m, _)]
                parsers = parsers[-len(cls.fields):]
            else:
                parsers = [parser]

            types = [m.get_type() for m in parsers]

            base_class = cls.__bases__[0]
            if issubclass(base_class, ASTNode) and base_class != ASTNode:
                bparser = parser if not cls.fields else None
                base_class.add_to_context(compile_ctx, bparser)

            compile_ctx.types.add(cls)
            compile_ctx.types_declarations.append(
                cls.create_type_declaration())
            cls.create_type_definition(compile_ctx, types)

    @classmethod
    def name(cls):
        return cls.__name__

    @classmethod
    def repr_name(cls):
        return getattr(cls, "_repr_name", cls.name())

    @classmethod
    def nullexpr(cls):
        if cls.is_ptr:
            return null_constant()
        else:
            return "nil_{0}".format(cls.name())


def resolve(parser):
    """
    :type parser: Parser|Token|ParserContainer
    :rtype: Parser
    """
    if isinstance(parser, Parser):
        return parser
    elif isinstance(parser, type) and issubclass(parser, Token):
        return TokClass(parser)
    elif isinstance(parser, Token):
        return Tok(parser)
    elif isinstance(parser, str):
        return Tok(Token(parser))
    elif isalambda(parser):
        return Defer(parser)
    else:
        return Defer(parser)


class CompileCtx():
    def __init__(self):
        self.body = []
        self.types_declarations = []
        self.types_definitions = []
        self.val_types_definitions = []
        self.fns_decls = []
        self.fns = set()
        self.generic_vectors = set()
        self.types = set()
        self.main_parser = ""
        self.diag_types = []
        self.test_bodies = []
        self.test_names = []
        self.rules_to_fn_names = {}

    def get_header(self):
        return render_template(
            'main_header',
            self=self,
            tdecls=self.types_declarations,
            tdefs=self.types_definitions,
            fndecls=self.fns_decls,
        )

    def get_source(self, header_name):
        return render_template(
            'main_body',
            self=self, header_name=header_name,
            bodies=self.body
        )

    def get_interactive_main(self, header_name):
        return render_template(
            'interactive_main',
            self=self, header_name=header_name
        )

    def has_type(self, typ):
        return typ in self.types


def write_cpp_file(file_path, source):
    with open(file_path, "wb") as out_file:
        p = subprocess.Popen(["clang-format"], stdin=subprocess.PIPE,
                             stdout=out_file)
        p.communicate(source)
        assert p.returncode == 0 


class Grammar(object):

    def __init__(self):
        self.resolved = False
        self.rules = {}

    def add_rules(self, **kwargs):
        for name, rule in kwargs.items():
            self.rules[name] = rule
            rule.set_name(name)
            rule.set_grammar(self)
            rule.is_root = True

    def __getattr__(self, item_name):
        if item_name in self.rules:
            r = self.rules[item_name]
            return Defer(lambda: r)

        if not self.resolved:
            return Defer(lambda: self.rules[item_name])
        else:
            raise AttributeError

    def dump_to_file(self, file_path=".", file_name="parse"):
        ctx = CompileCtx()
        for r_name, r in self.rules.items():

            r.compile(ctx)
            ctx.rules_to_fn_names[r_name] = r

        write_cpp_file(path.join(file_path, file_name + ".cpp"), 
                       ctx.get_source(header_name=file_name + ".hpp"))

        write_cpp_file(path.join(file_path, file_name + ".hpp"), 
                       ctx.get_header())

        write_cpp_file(
            path.join(file_path, file_name + "_main.cpp"),
            ctx.get_interactive_main(header_name=file_name + ".hpp")
        )


class Parser(object):

    # noinspection PyMissingConstructor
    def __init__(self):
        self._mod = None
        self.gen_fn_name = gen_name(self.__class__.__name__ + "_parse")
        self.grammar = None
        self.is_root = False
        self._name = ""
        self.res = None
        self.pos = None

    @property
    def name(self):
        return self._name

    def needs_refcount(self):
        return True

    def __or__(self, other):
        other_parser = resolve(other)
        if isinstance(other_parser, Or):
            other_parser.parsers.append(self)
            return other_parser
        elif isinstance(self, Or):
            self.parsers.append(other_parser)
            return self
        else:
            return Or(self, other_parser)

    def __xor__(self, transform_fn):
        """
        :type transform_fn: (T) => U
        :rtype: Transform
        """
        return Transform(self, transform_fn)

    def set_grammar(self, grammar):
        for c in self.children():
            c.set_grammar(grammar)
        self.grammar = grammar

    def set_name(self, name):
        for c in self.children():
            if not c._name and not isinstance(c, Defer):
                c.set_name(name)

        self._name = name
        self.gen_fn_name = gen_name("{0}_{1}_parse".format(
            name, self.__class__.__name__.lower()))

    def is_left_recursive(self):
        return self._is_left_recursive(self.name)

    def _is_left_recursive(self, rule_name):
        """
        Private function used only by is_left_recursive, will explore the
        parser tree to verify whether the named parser with name rule_name is
        left recursive or not.
        """
        raise NotImplementedError()

    def parse(self, tkz, pos):
        raise NotImplemented

    # noinspection PyMethodMayBeStatic
    def children(self):
        return []

    def compile(self, compile_ctx=None):
        """:type compile_ctx: CompileCtx"""
        t_env = TemplateEnvironment()
        t_env.self = self

        # Verify that the function hasn't been compiled yet
        if self.gen_fn_name in compile_ctx.fns:
            return
        compile_ctx.fns.add(self.gen_fn_name)

        if not compile_ctx:
            compile_ctx = CompileCtx()

        t_env.pos, t_env.res, t_env.code, t_env.defs = (
            self.generate_code(compile_ctx=compile_ctx)
        )
        t_env.code = t_env.code
        t_env.fn_profile = render_template('parser_fn_profile', t_env)
        t_env.fn_code = render_template('parser_fn_code', t_env)

        compile_ctx.body.append(t_env.fn_code)
        compile_ctx.fns_decls.append(t_env.fn_profile)

    def compile_and_exec(self, compile_ctx=None):
        raise NotImplementedError()

    def force_fn_call(self):
        return self.is_root

    def get_type(self):
        raise NotImplementedError()

    def gen_code_or_fncall(self, compile_ctx, pos_name="pos",
                           force_fncall=False):

        if self.name:
            print "Compiling rule : {0}".format(
                Colors.HEADER + self.gen_fn_name + Colors.ENDC
            )

        if self.force_fn_call() or force_fncall or \
                self.gen_fn_name in compile_ctx.fns:

            self.compile(compile_ctx)
            self.pos, self.res = gen_names("fncall_pos", "fncall_res")

            fncall_block = render_template(
                'parser_fncall',
                self=self, pos_name=pos_name
            )
            return self.pos, self.res, fncall_block, [
                (self.pos, LongType),
                (self.res, self.get_type())
            ]
        else:
            pos, res, code, decls = self.generate_code(compile_ctx, pos_name)
            self.res = res
            self.pos = pos
            return pos, res, code, decls

    def generate_code(self, compile_ctx, pos_name="pos"):
        raise NotImplemented


class Tok(Parser):

    def __repr__(self):
        return "Tok({0})".format(repr(self.tok.val))

    def needs_refcount(self):
        return False

    def _is_left_recursive(self, rule_name):
        return False

    def __init__(self, tok):
        """ :type tok: Token """
        Parser.__init__(self)
        self.tok = tok
        self._id = TOKEN_PREFIX + token_map.str_to_names[tok.val]

    def get_type(self):
        return Token

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("tk_pos", "tk_res")
        code = render_template(
            'tok_code',
            self=self, pos_name=pos_name,
            pos=pos, res=res,
        )
        return pos, res, code, [(pos, LongType), (res, Token)]


class TokClass(Parser):

    def _is_left_recursive(self, rule_name):
        return False

    def needs_refcount(self):
        return False

    def __repr__(self):
        return "TokClass({0})".format(self.tok_class.__name__)

    def __init__(self, tok_class):
        Parser.__init__(self)
        self.tok_class = tok_class

    def get_type(self):
        return Token

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("tk_class_pos", "tk_class_res")
        _id = TOKEN_PREFIX + self.tok_class.quex_token_name
        code = render_template(
            'tokclass_code',
            self=self, pos_name=pos_name,
            pos=pos, res=res, _id=_id,
        )
        return pos, res, code, [(pos, LongType), (res, Token)]


def common_ancestor(*cs):
    assert all(inspect.isclass(c) for c in cs)
    rmro = lambda k: reversed(k.mro())
    return list(takewhile(lambda a: len(set(a)) == 1,
                          zip(*map(rmro, cs))))[-1][0]


class Or(Parser):

    def _is_left_recursive(self, rule_name):
        return any(parser._is_left_recursive(rule_name)
                   for parser in self.parsers)

    def __repr__(self):
        return "Or({0})".format(", ".join(repr(m) for m in self.parsers))

    def __init__(self, *parsers):
        """ :type parsers: list[Parser|Token|type] """
        Parser.__init__(self)
        self.parsers = [resolve(m) for m in parsers]
        self.locked = False
        self.cached_type = None

    def children(self):
        return self.parsers

    def needs_refcount(self):
        assert(all(i.needs_refcount() == self.parsers[0].needs_refcount()
                   for i in self.parsers))
        return self.parsers[0].needs_refcount()

    def get_type(self):
        if self.cached_type:
            return self.cached_type
        if self.locked:
            return None
        try:
            self.locked = True
            types = set()
            for m in self.parsers:
                t = m.get_type()
                if t:
                    types.add(t)

            if all(inspect.isclass(t) for t in types):
                res = common_ancestor(*types)
            else:
                typs = list(types)
                assert all(type(t) == type(typs[0]) for t in typs)
                res = typs[0]

            self.cached_type = res
            return res
        finally:
            self.locked = False

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("or_pos", "or_res")

        t_env = TemplateEnvironment()
        t_env.self = self

        t_env.results = [
            m.gen_code_or_fncall(compile_ctx, pos_name)
            for m in self.parsers
        ]
        t_env.decls = list(chain(
            [(pos, LongType), (res, self.get_type())],
            *[r[3] for r in t_env.results]
        ))
        t_env.exit_label = gen_name("Exit_Or")
        code = render_template(
            'or_code', t_env,
            pos=pos, res=res,
            typ=decl_type(self.get_type()),
        )
        return pos, res, code, t_env.decls


class RowType(CompiledType):

    is_ptr = True

    def __init__(self, name):
        self.name = name

    def as_string(self):
        return self.name

    def nullexpr(self):
        return null_constant()


def always_make_progress(parser):
    if isinstance(parser, List):
        return not parser.empty_valid or always_make_progress(parser.parser)
    return not isinstance(parser, (Opt, Success, Null))


class Row(Parser):

    def _is_left_recursive(self, rule_name):
        for parser in self.parsers:
            res = parser._is_left_recursive(rule_name)
            if res:
                return True
            if always_make_progress(parser):
                break
        return False

    def needs_refcount(self):
        return True

    def __repr__(self):
        return "Row({0})".format(", ".join(repr(m) for m in self.parsers))

    def __init__(self, *parsers):
        """ :type parsers: list[Parser|Token|type] """
        Parser.__init__(self)
        self.parsers = [resolve(m) for m in parsers]
        self.make_tuple = True
        self.typ = RowType(gen_name("Row"))
        self.components_need_inc_ref = True
        self.args = []

    def children(self):
        return self.parsers

    def get_type(self):
        return self.typ

    def create_type(self, compile_ctx):
        t_env = TemplateEnvironment(
            parsers=[m for m in self.parsers if not isinstance(m, _)]
        )
        t_env.self = self

        compile_ctx.types_declarations.append(
            render_template('row_type_decl', t_env)
        )

        compile_ctx.types_definitions.insert(0, render_template(
            'row_type_def', t_env
        ))

        compile_ctx.body.append(render_template(
            'row_type_impl', t_env
        ))

    def generate_code(self, compile_ctx, pos_name="pos"):
        """ :type compile_ctx: CompileCtx """
        t_env = TemplateEnvironment(pos_name=pos_name)
        t_env.self = self

        t_env.pos, t_env.res, t_env.did_fail = gen_names(
            "row_pos", "row_res", "row_did_fail"
        )

        # Create decls for declarative part of the parse subprogram.
        # If make_tuple is false we don't want a variable for the result.
        decls = [(t_env.pos, LongType), (t_env.did_fail, BoolType)]

        if self.make_tuple:
            decls.append((t_env.res, self.get_type()))
            self.create_type(compile_ctx)

        t_env.subresults = list(gen_names(*[
            "row_subres_{0}".format(i)
            for i in range(len(self.parsers))
        ]))
        t_env.exit_label = gen_name("row_exit_label")

        self.args = [r for r, m in zip(t_env.subresults, self.parsers)
                     if not isinstance(m, _)]

        bodies = []
        for i, (parser, subresult) in enumerate(zip(self.parsers,
                                                    t_env.subresults)):
            t_subenv = TemplateEnvironment(
                t_env,
                parser=parser, subresult=subresult, i=i,
            )
            (t_subenv.mpos,
             t_subenv.mres,
             t_subenv.m_code,
             t_subenv.m_decls) = (
                parser.gen_code_or_fncall(compile_ctx, t_env.pos)
            )
            decls += t_subenv.m_decls
            if not is_discard(parser):
                decls.append((subresult, parser.get_type()))

            bodies.append(render_template('row_submatch', t_subenv))

        code = render_template('row_code', t_env, body='\n'.join(bodies))
        return t_env.pos, t_env.res, code, decls

    def __rshift__(self, index):
        return Extract(self, index)


class ListType(CompiledType):

    is_ptr = True

    def __init__(self, el_type):
        self.el_type = el_type

    def as_string(self):
        return render_template('list_type', el_type=self.el_type)

    def nullexpr(self):
        return null_constant()


class List(Parser):

    def _is_left_recursive(self, rule_name):
        res = self.parser._is_left_recursive(rule_name)
        assert not(
            res and (self.empty_valid
                     or not always_make_progress(self.parser))
        )
        return res

    def needs_refcount(self):
        return self.parser.needs_refcount()

    def __repr__(self):
        return "List({0})".format(
            repr(self.parser) + (", sep={0}".format(self.sep)
                                 if self.sep else "")
        )

    def __init__(self, parser, sep=None, empty_valid=False, revtree=None):
        """
        :type sep: Token|string
        :type empty_valid: bool
        """
        Parser.__init__(self)
        self.parser = resolve(parser)
        self.sep = resolve(sep) if sep else None
        self.empty_valid = empty_valid
        self.revtree_class = revtree

        if empty_valid:
            assert not self.revtree_class

    def children(self):
        return [self.parser]

    def get_type(self):
        if self.revtree_class:
            return common_ancestor(self.parser.get_type(), self.revtree_class)
        else:
            return ListType(self.parser.get_type())

    def generate_code(self, compile_ctx, pos_name="pos"):
        """:type compile_ctx: CompileCtx"""
        t_env = TemplateEnvironment(pos_name=pos_name)
        t_env.self = self

        t_env.pos, t_env.res, t_env.cpos = gen_names(
            "lst_pos", "lst_res", "lst_cpos"
        )
        t_env.ppos, t_env.pres, t_env.pcode, t_env.pdecls = (
            self.parser.gen_code_or_fncall(compile_ctx, t_env.cpos)
        )
        compile_ctx.generic_vectors.add(self.parser.get_type().as_string())
        decls = [(t_env.pos, LongType),
                 (t_env.res, self.get_type()),
                 (t_env.cpos, LongType)] + t_env.pdecls

        if self.revtree_class:
            if not self.revtree_class in compile_ctx.types:
                compile_ctx.types.add(self.revtree_class)
                self.revtree_class.create_type_definition(
                    compile_ctx, [self.get_type(), self.get_type()]
                )

        (t_env.sep_pos,
         t_env.sep_res,
         t_env.sep_code,
         t_env.sep_decls) = (
            self.sep.gen_code_or_fncall(compile_ctx, t_env.cpos)
            if self.sep else
            (None, None, None, None)
        )
        if t_env.sep_decls:
            decls += t_env.sep_decls

        code = render_template('list_code', t_env)
        return t_env.pos, t_env.res, code, decls


class Opt(Parser):

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def needs_refcount(self):
        if self._booleanize:
            return False
        return self.parser.needs_refcount()

    def __repr__(self):
        return "Opt({0})".format(self.parser)

    def __init__(self, parser, *parsers):
        Parser.__init__(self)
        self._booleanize = False
        if parsers:
            self.parser = Row(parser, *parsers)
        else:
            self.parser = resolve(parser)

    def as_bool(self):
        self._booleanize = True
        return self

    def children(self):
        return [self.parser]

    def get_type(self):
        return BoolType if self._booleanize else self.parser.get_type()

    def generate_code(self, compile_ctx, pos_name="pos"):
        t_env = TemplateEnvironment(pos_name=pos_name)
        t_env.self = self

        t_env.mpos, t_env.mres, t_env.code, decls = (
            self.parser.gen_code_or_fncall(compile_ctx, pos_name)
        )
        t_env.bool_res = gen_name("opt_bool_res")

        code = render_template('opt_code', t_env)

        if self._booleanize:
            decls.append((t_env.bool_res, BoolType))
            t_env.mres = t_env.bool_res

        return t_env.mpos, t_env.mres, code, decls

    def __rshift__(self, index):
        m = self.parser
        assert isinstance(m, Row)
        return Opt(Extract(m, index))


class Extract(Parser):

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def needs_refcount(self):
        return self.parser.needs_refcount()

    def __repr__(self):
        return "{0} >> {1}".format(self.parser, self.index)

    def __init__(self, parser, index):
        """
        :param Row parser: The parser that will serve as target for
        extract operation
        :param int index: The index you want to extract from the row
        """
        Parser.__init__(self)
        self.parser = parser
        self.index = index
        assert isinstance(self.parser, Row)
        self.parser.components_need_inc_ref = False

    def children(self):
        return [self.parser]

    def get_type(self):
        return self.parser.parsers[self.index].get_type()

    def generate_code(self, compile_ctx, pos_name="pos"):
        self.parser.make_tuple = False
        cpos, cres, code, decls = self.parser.gen_code_or_fncall(
            compile_ctx, pos_name)
        args = self.parser.args
        return cpos, args[self.index], code, decls


class Discard(Parser):

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def needs_refcount(self):
        return self.parser.needs_refcount()

    def __repr__(self):
        return "Discard({0})".format(self.parser)

    def __init__(self, parser):
        Parser.__init__(self)
        self.parser = resolve(parser)

    def children(self):
        return [self.parser]

    def get_type(self):
        return self.parser.get_type()

    def generate_code(self, compile_ctx, pos_name="pos"):
        return self.parser.gen_code_or_fncall(compile_ctx, pos_name)


_ = Discard


class Defer(Parser):

    @property
    def parser(self):
        self.resolve_parser()
        return self._parser

    @property
    def name(self):
        return self.parser.name

    def _is_left_recursive(self, rule_name):
        return self.name == rule_name

    def needs_refcount(self):
        return self.parser.needs_refcount()

    def __repr__(self):
        return "Defer({0})".format(getattr(self.parser, "_name", ".."))

    def __init__(self, parser_fn):
        Parser.__init__(self)
        self.parser_fn = parser_fn
        self._parser = None
        ":type: Parser"

    def resolve_parser(self):
        if not self._parser:
            self._parser = self.parser_fn()

    def get_type(self):
        return self.parser.get_type()

    def generate_code(self, compile_ctx, pos_name="pos"):
        return self.parser.gen_code_or_fncall(
            compile_ctx, pos_name=pos_name, force_fncall=True
        )


class Transform(Parser):

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def needs_refcount(self):
        return True

    def __repr__(self):
        return "{0} ^ {1}".format(self.parser, self.typ.name())

    def __init__(self, parser, typ):
        Parser.__init__(self)
        assert issubclass(typ, CompiledType)
        self.parser = parser
        self.typ = typ
        ":type: CompiledType"

        self._is_ptr = typ.is_ptr

    def children(self):
        return [self.parser]

    def get_type(self):
        return self.typ

    def generate_code(self, compile_ctx, pos_name="pos"):
        """:type compile_ctx: CompileCtx"""
        t_env = TemplateEnvironment()
        t_env.self = self

        self.typ.add_to_context(compile_ctx, self.parser)

        if isinstance(self.parser, Row):
            self.parser.make_tuple = False

        t_env.cpos, t_env.cres, t_env.code, decls = (
            self.parser.gen_code_or_fncall(compile_ctx, pos_name)
        )
        t_env.args = (
            self.parser.args
            if isinstance(self.parser, Row) else
            [t_env.cres]
        )

        t_env.res = gen_name("transform_res")
        code = render_template('transform_code', t_env)
        compile_ctx.diag_types.append(self.typ)

        return t_env.cpos, t_env.res, code, decls + [
            (t_env.res, self.get_type())
        ]


class Success(Parser):

    def _is_left_recursive(self, rule_name):
        return False

    def __repr__(self):
        return "Success({0})".format(self.typ.name())

    def needs_refcount(self):
        return True

    def __init__(self, result_typ):
        Parser.__init__(self)
        self.typ = result_typ

    def children(self):
        return []

    def get_type(self):
        return self.typ

    def generate_code(self, compile_ctx, pos_name="pos"):
        self.typ.add_to_context(compile_ctx, None)
        res = gen_name("success_res")
        code = render_template('success_code', self=self, res=res)

        return pos_name, res, code, [(res, self.get_type())]


class Null(Success):

    def _is_left_recursive(self, rule_name):
        return False

    def __repr__(self):
        return "Null"

    def generate_code(self, compile_ctx, pos_name="pos"):
        typ = self.get_type()
        if isinstance(typ, ASTNode):
            self.get_type().add_to_context(compile_ctx, None)
        res = gen_name("null_res")
        code = render_template('null_code', self=self, res=res)
        return pos_name, res, code, [(res, self.get_type())]

    def get_type(self):
        return (self.typ if inspect.isclass(self.typ)
                and issubclass(self.typ, CompiledType)
                else self.typ.get_type())


class EnumType(CompiledType):
    is_ptr = False
    alternatives = []

    def __init__(self, alt):
        assert alt in self.alternatives
        self.alt = alt

    @classmethod
    def name(cls):
        return cls.__name__

    @classmethod
    def add_to_context(cls, compile_ctx, parser=None):
        if not cls in compile_ctx.types:
            compile_ctx.types.add(cls)
            compile_ctx.types_declarations.append(
                render_template('enum_type_decl', cls=cls)
            )
            compile_ctx.body.append(
                render_template('enum_type_impl', cls=cls)
            )

    @classmethod
    def nullexpr(cls):
        return cls.name() + "::uninitialized"


class Enum(Parser):

    def _is_left_recursive(self, rule_name):
        if self.parser:
            return self.parser._is_left_recursive(rule_name)
        return False

    def needs_refcount(self):
        return False

    def __repr__(self):
        return "Enum({0}, {1})".format(self.parser, self.enum_type_inst)

    def __init__(self, parser, enum_type_inst):
        Parser.__init__(self)
        self.parser = resolve(parser) if parser else None
        self.enum_type_inst = enum_type_inst

    def children(self):
        return []

    def get_type(self):
        return self.enum_type_inst.__class__

    def generate_code(self, compile_ctx, pos_name="pos"):
        self.enum_type_inst.add_to_context(compile_ctx)

        res = gen_name("enum_res")

        if self.parser:
            if isinstance(self.parser, Row):
                self.parser.make_tuple = False

            cpos, _, code, decls = self.parser.gen_code_or_fncall(
                compile_ctx, pos_name
            )
        else:
            cpos, code, decls = pos_name, "", []

        body = render_template(
            'enum_code',
            self=self, res=res, cpos=cpos, code=code)

        return cpos, res, body, [(res, self.get_type())] + decls
