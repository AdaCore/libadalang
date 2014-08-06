from collections import defaultdict
import inspect
from itertools import takewhile, chain
from os import path
from copy import copy
import sys

from mako.template import Template

from tokenizer import *
from utils import isalambda, Colors, printcol
from quex_tokens import token_map

try:
    from IPython.core import ultratb

    sys.excepthook = ultratb.FormattedTB(
        mode='Verbose', color_scheme='Linux', call_pdb=1
    )
except ImportError, i:
    pass

LANGUAGE = "cpp"

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

###############
# AST HELPERS #
###############


class Decl(object):
    def __init__(self, typ_or_comb):
        self.comb = None
        self.need_free = False
        self.need_unref = False

        if typ_or_comb in (long, bool):
            self.type = typ_or_comb
            self.typestring = get_type(typ_or_comb)
        elif isinstance(typ_or_comb, Combinator):
            self.type = typ_or_comb.get_type()
            self.typestring = typ_or_comb.get_type_string()
            self.comb = typ_or_comb
            if self.comb.is_ptr():
                if self.comb.force_fn_call():
                    self.need_unref = True
                else:
                    self.need_free = True
        elif isinstance(typ_or_comb, basestring):
            self.type = None
            self.typestring = typ_or_comb

        self.default_val = None
        if inspect.isclass(self.type) and issubclass(self.type, AdaType):
            self.default_val = self.type.nullexpr()
        if self.comb and self.comb.is_ptr():
            self.default_val = null_constant()


def wrap(locs):
    d = dict(globals())
    d.update(locs)
    new_locs = dict(is_class=inspect.isclass, **d)
    s = new_locs.get("self", None)
    if s:
        del new_locs["self"]
        new_locs["_self"] = s
    return new_locs


template_cache = {}


def mako_template(file_name):
    t_path = path.join(path.dirname(path.realpath(__file__)),
                       LANGUAGE, "templates", file_name + ".mako")
    t = template_cache.get(t_path, None)
    return t or Template(strict_undefined=True, filename=t_path)


__next_ids = defaultdict(int)


def gen_name(var_name):
    __next_ids[var_name] += 1
    return "{0}_{1}".format(var_name, __next_ids[var_name])


def gen_names(*var_names):
    for var_name in var_names:
        yield gen_name(var_name)


###############
# Combinators #
###############

class AdaType(object):

    is_ptr = True

    @classmethod
    def create_type_declaration(cls):
        raise NotImplementedError()

    @classmethod
    def add_to_context(cls, compile_ctx, comb):
        raise NotImplementedError()

    @classmethod
    def create_type_definition(cls, compile_ctx, source_combinator):
        raise NotImplementedError()

    @classmethod
    def create_instantiation(cls, args):
        raise NotImplementedError()

    @classmethod
    def name(cls):
        raise NotImplementedError()

    @classmethod
    def nullexpr(cls):
        raise NotImplementedError()


class TokenType:
    pass


class Field(object):
    def __init__(self, name, tk_start=False, tk_end=False, type=None,
                 repr=False, kw_repr=False, opt=False, norepr_null=False):
        self.name = name
        self.kw_repr = kw_repr
        self.repr = repr
        self.type = type
        self.norepr_null = norepr_null
        self.tk_end = tk_end
        self.opt = opt
        self.tk_start = tk_start

    def tstring(self):
        if self.type:
            return self.type.__name__


class AstNodeMetaclass(type):
    def __init__(cls, name, base, dct):
        super(AstNodeMetaclass, cls).__init__(name, base, dct)
        cls.fields = dct.get("fields", [])
        cls.abstract = dct.get("abstract", False)


def get_tk_bound(field, field_val, start=False):
    res = field_val
    typ = field.type
    if isinstance(typ, list):
        res = field_val[0]
        typ = typ[0]
    if issubclass(ASTNode, typ):
        res = getattr(res, "tk_start" if start else "tk_end")
        res = getattr(res, "tk_start" if start else "tk_end")
    return res


class ASTNode(AdaType):
    abstract = False
    fields = []
    __metaclass__ = AstNodeMetaclass

    def __init__(self, *args):
        for field, field_val in zip(self.fields, args):
            setattr(self, field.name, field_val)
            if field.tk_start:
                self.tk_start = get_tk_bound(field, field_val, True)
            if field.tk_end:
                self.tk_end = get_tk_bound(field, field_val, False)

    @classmethod
    def create_type_declaration(cls):
        return mako_template("astnode_type_decl").render(**wrap(locals()))

    @classmethod
    def create_type_definition(cls, compile_ctx, source_combinator):

        if not source_combinator:
            matchers = []
        elif isinstance(source_combinator, Row):
            matchers = source_combinator.matchers[-len(cls.fields):]
        else:
            matchers = [source_combinator]

        base_class = cls.__bases__[0]
        base_name = base_class.name()

        tdef = mako_template("astnode_type_def").render(**wrap(locals()))
        if cls.is_ptr:
            compile_ctx.types_definitions.append(tdef)
        else:
            compile_ctx.val_types_definitions.append(tdef)

        repr_m_to_fields = [(m, f) for m, f in zip(matchers, cls.fields)
                            if f.repr]
        compile_ctx.body.append(
            mako_template("astnode_type_impl").render(**wrap(locals()))
        )

    @classmethod
    def get_fields(cls):
        b = cls.__bases__[0]
        bfields = b.fields if b != ASTNode else []
        return bfields + cls.fields

    @classmethod
    def add_to_context(cls, compile_ctx, comb):
        if not cls in compile_ctx.types:

            is_row = isinstance(comb, Row)
            new_comb = copy(comb)

            if is_row:
                new_comb.matchers = [m for m in new_comb.matchers
                                     if not isinstance(m, _)]

            base_class = cls.__bases__[0]

            if issubclass(base_class, ASTNode) and base_class != ASTNode:
                base_comb = copy(new_comb)
                if is_row and len(cls.fields):
                    base_comb.matchers = base_comb.matchers[:-len(cls.fields)]
                base_class.add_to_context(compile_ctx, base_comb)

            compile_ctx.types.add(cls)
            compile_ctx.types_declarations.append(
                cls.create_type_declaration())
            cls.create_type_definition(compile_ctx, new_comb)

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


def resolve(matcher):
    """
    :type matcher: Combinator|Token|ParserContainer
    :rtype: Combinator
    """
    if isinstance(matcher, Combinator):
        return matcher
    elif isinstance(matcher, type) and issubclass(matcher, Token):
        return TokClass(matcher)
    elif isinstance(matcher, Token):
        return Tok(matcher)
    elif isinstance(matcher, str):
        return Tok(Token(matcher))
    elif isalambda(matcher):
        return Defer(matcher)
    else:
        return Defer(matcher)


def indent(string, indent_level=3):
    return "\n".join((" " * indent_level) + s if s else ""
                     for s in string.splitlines())


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
        self.main_comb = ""
        self.diag_types = []
        self.test_bodies = []
        self.test_names = []
        self.rules_to_fn_names = {}

    def get_header(self):
        tdecls = map(indent, self.types_declarations)
        tdefs = map(indent, self.types_definitions)
        fndecls = map(indent, self.fns_decls)
        return mako_template("main_header").render(**wrap(locals()))

    def get_source(self, header_name):
        bodies = map(indent, self.body)
        return mako_template("main_body").render(**wrap(locals()))

    def get_interactive_main(self, header_name):
        return mako_template("interactive_main").render(**wrap(locals()))

    def has_type(self, typ):
        return typ in self.types


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

        with open(path.join(file_path, file_name + ".cpp"), "w") as f:
            f.write(ctx.get_source(header_name=file_name + ".hpp"))

        with open(path.join(file_path, file_name + ".hpp"), "w") as f:
            f.write(ctx.get_header())

        with open(path.join(file_path, file_name + "_main.cpp"), "w") as f:
            f.write(ctx.get_interactive_main(header_name=file_name + ".hpp"))


class Combinator(object):

    # noinspection PyMissingConstructor
    def __init__(self):
        self._mod = None
        self.gen_fn_name = gen_name(self.__class__.__name__ + "_parse")
        self.grammar = None
        self.is_root = False
        self._name = ""
        self.res = None
        self.pos = None
        self.precomputed_res = None
        self.res_needed_by_others = False

    def is_ptr(self):
        raise NotImplementedError()

    def needs_refcount(self):
        return True

    def nullexpr(self):
        raise NotImplementedError()

    def emit_repr(self, self_access_string):
        raise NotImplementedError()

    def __or__(self, other):
        other_comb = resolve(other)
        if isinstance(other_comb, Or):
            other_comb.matchers.append(self)
            return other_comb
        elif isinstance(self, Or):
            self.matchers.append(other_comb)
            return self
        else:
            return Or(self, other_comb)

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
            if c._name and not isinstance(c, Defer):
                c.set_name(name)
            self._name = name
        self.gen_fn_name = gen_name("{0}_{1}_parse".format(
            name, self.__class__.__name__.lower()))

    def parse(self, tkz, pos):
        raise NotImplemented

    # noinspection PyMethodMayBeStatic
    def children(self):
        return []

    def compile(self, compile_ctx=None):
        """:type compile_ctx: CompileCtx"""

        # Verify that the function hasn't been compiled yet
        if self.gen_fn_name in compile_ctx.fns:
            return
        compile_ctx.fns.add(self.gen_fn_name)

        if not compile_ctx:
            compile_ctx = CompileCtx()

        pos, res, code, defs = self.generate_code(compile_ctx=compile_ctx)
        code = indent(code)
        fn_profile = mako_template(
            "combinator_fn_profile").render(**wrap(locals()))

        fn_code = mako_template("combinator_fn_code").render(**wrap(locals()))

        compile_ctx.body.append(fn_code)
        compile_ctx.fns_decls.append(fn_profile)

    def compile_and_exec(self, compile_ctx=None):
        raise NotImplementedError()

    def force_fn_call(self):
        return self.is_root

    def get_type(self):
        raise NotImplementedError()

    def get_type_string(self):
        t = self.get_type()
        res = t if isinstance(t, basestring) else t.__name__
        return res.strip() + ("*" if self.is_ptr() else "")

    def gen_code_or_fncall(self, compile_ctx, pos_name="pos",
                           force_fncall=False):

        if self._name:
            print "Compiling rule : {0}".format(
                Colors.HEADER + self.gen_fn_name + Colors.ENDC
            )

        if self.precomputed_res:
            return self.precomputed_res.pos, self.precomputed_res.res, "", []

        if self.force_fn_call() or force_fncall or \
                self.gen_fn_name in compile_ctx.fns:

            self.compile(compile_ctx)
            pos, res = gen_names("fncall_pos", "fncall_res")

            fncall_block = mako_template("combinator_fncall").render(**wrap(
                locals()))

            self.res = res
            self.pos = pos
            return pos, res, fncall_block, [(pos, Decl(long)),
                                            (res, Decl(self))]
        else:
            pos, res, code, decls = self.generate_code(compile_ctx, pos_name)
            self.res = res
            self.pos = pos
            return pos, res, code, decls

    def generate_code(self, compile_ctx, pos_name="pos"):
        raise NotImplemented

    def test_parser(self, ada_string):
        tkz = make_ada_tokenizer(ada_string)
        npos, res = self.parse(tkz, 0)
        return res


class Tok(Combinator):

    def __repr__(self):
        return "Tok({0})".format(repr(self.tok.val))

    def needs_refcount(self):
        return False

    def __init__(self, tok):
        """ :type tok: Token """
        Combinator.__init__(self)
        self.tok = tok
        self._id = token_map.names_to_ids[token_map.str_to_names[tok.val]]

    def get_type(self):
        return Token

    def is_ptr(self):
        return False

    def nullexpr(self):
        return "no_token"

    def emit_repr(self, self_access_string):
        return '"{0}"'.format(self.tok.val)

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("tk_pos", "tk_res")
        repr_tok_val = repr(self.tok.val)
        code = mako_template("tok_code").render(**wrap(locals()))
        return pos, res, code, [(pos, Decl(long)), (res, Decl("Token"))]


class TokClass(Combinator):

    classes_to_identifiers = {
        Id: token_map.names_to_ids['IDENTIFIER'],
        Lbl: token_map.names_to_ids['LABEL'],
        NumLit: token_map.names_to_ids['NUMBER'],
        CharLit: token_map.names_to_ids['CHAR'],
        StringLit: token_map.names_to_ids['STRING'],
        NoToken: token_map.names_to_ids['TERMINATION'],
    }

    def needs_refcount(self):
        return False

    def __repr__(self):
        return "TokClass({0})".format(self.tok_class.__name__)

    def __init__(self, tok_class):
        Combinator.__init__(self)
        self.tok_class = tok_class

    def get_type(self):
        return Token

    def nullexpr(self):
        return "no_token"

    def emit_repr(self, self_access_string):
        return mako_template("tokclass_repr").render(**wrap(locals())).strip()

    def is_ptr(self):
        return False

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("tk_class_pos", "tk_class_res")
        _id = self.classes_to_identifiers[self.tok_class]
        code = mako_template("tokclass_code").render(**wrap(locals()))
        return pos, res, code, [(pos, Decl(long)), (res, Decl("Token"))]


def common_ancestor(*cs):
    assert all(inspect.isclass(c) for c in cs)
    rmro = lambda k: reversed(k.mro())
    return list(takewhile(lambda a: len(set(a)) == 1, zip(*map(rmro, cs))))[-1][0]


def get_combs_at_index(comb, idx):
    if isinstance(comb, Or):
        return list(chain(*(get_combs_at_index(subc, idx)
                            for subc in comb.matchers)))
    elif isinstance(comb, Row):
        return get_combs_at_index(comb.matchers[idx], 0) if len(comb.matchers) > idx else []
    elif isinstance(comb, Opt):
        return get_combs_at_index(comb.matcher, idx)
    elif isinstance(comb, Transform):
        return get_combs_at_index(comb.combinator, idx)
    else:
        if idx == 0:
            return [comb]
        else:
            return None


def group_by(lst, transform_fn=None):
    if not transform_fn:
        transform_fn = lambda x: x

    res = defaultdict(list)

    for el in lst:
        t = transform_fn(el)
        res[transform_fn(el)].append(el)

    if res.get(None):
        del res[None]

    return res.values()

def get_comb_groups(comb, idx):
    def tr(c):
        if isinstance(c, Defer):
            c.resolve_combinator()
            return c.combinator
        return None
    return group_by(get_combs_at_index(comb, idx), tr)


class Or(Combinator):

    def __repr__(self):
        return "Or({0})".format(", ".join(repr(m) for m in self.matchers))

    def __init__(self, *matchers):
        """ :type matchers: list[Combinator|Token|type] """
        Combinator.__init__(self)
        self.matchers = [resolve(m) for m in matchers]
        self.locked = False

    def children(self):
        return self.matchers

    def needs_refcount(self):
        assert(all(i.needs_refcount() == self.matchers[0].needs_refcount()
                   for i in self.matchers))
        return self.matchers[0].needs_refcount()

    def is_ptr(self):
        if not self.locked:
            self.locked = True
            assert all(i.is_ptr() == self.matchers[0].is_ptr() for i in self.matchers)
            self.locked = False

        return self.matchers[0].is_ptr()

    def emit_repr(self, self_access_string):
        return self.matchers[0].emit_repr(self_access_string)

    def nullexpr(self):
        t = self.get_type()
        if inspect.isclass(t):
            if issubclass(t, Token):
                return "no_token"
            return t.nullexpr()
        elif t == "Token":
            return "no_token"
        else:
            return null_constant()

    def get_type(self):
        if self.locked:
            return None
        try:
            self.locked = True
            types = set()

            for m in self.matchers:
                t = m.get_type()
                if t:
                    types.add(t)

            if len(types) == 1 and isinstance(list(types)[0], basestring):
                return list(types)[0]
            elif inspect.isclass(list(types)[0]):
                return common_ancestor(*types)
            else:
                raise AssertionError(
                    "Or.get_type not implemented for types {0}".format(types)
                )
        finally:
            self.locked = False

    def generate_code(self, compile_ctx, pos_name="pos"):
        # groups = get_comb_groups(self, 0)
        # for group in (g for g in groups if len(g) > 1):
        #     for comb in group[1:]:
        #         comb.precomputed_res = group[0]

        #     for comb in group[:-1]:
        #         group[0].res_needed_by_others = True

        pos, res = gen_names("or_pos", "or_res")
        typ = self.get_type_string()
        results = [m.gen_code_or_fncall(compile_ctx, pos_name)
                   for m in self.matchers]
        decls = list(chain([(pos, Decl(long)), (res, Decl(self))]
                           , *[r[3] for r in results]))
        exit_label = gen_name("Exit_Or")
        code = mako_template("or_code").render(**wrap(locals()))
        return pos, res, code, decls


class Row(Combinator):

    def needs_refcount(self):
        return True

    def __repr__(self):
        return "Row({0})".format(", ".join(repr(m) for m in self.matchers))

    def __init__(self, *matchers):
        """ :type matchers: list[Combinator|Token|type] """
        Combinator.__init__(self)
        self.matchers = [resolve(m) for m in matchers]
        self.make_tuple = True
        self.type_name = gen_name("Row")
        self.components_need_inc_ref = True

    def is_ptr(self):
        return True

    def children(self):
        return self.matchers

    def emit_repr(self, self_access_string):
        return mako_template("row_repr").render(**wrap(locals())).strip()

    def get_type(self):
        return self.type_name

    def nullexpr(self):
        return null_constant() if self.is_ptr() else "nil_{0}".format(self.type_name)

    def create_type(self, compile_ctx):
        matchers = [m for m in self.matchers if not isinstance(m, _)]
        compile_ctx.types_declarations.append(
            mako_template("row_type_decl").render(**wrap(locals()))
        )

        compile_ctx.types_definitions.insert(0, mako_template(
            'row_type_def').render(**wrap(locals()))
        )

        compile_ctx.body.append(mako_template(
            'row_type_impl').render(**wrap(locals()))
        )

    def generate_code(self, compile_ctx, pos_name="pos"):
        """ :type compile_ctx: CompileCtx """

        c_pos_name = pos_name
        pos, res, did_fail = gen_names("row_pos", "row_res", "row_did_fail")

        # Create decls for declarative part of the parse subprogram.
        # If make_tuple is false we don't want a variable for the result.
        decls = [(pos, Decl(long)), (did_fail, Decl(bool))]

        if self.make_tuple:
            decls.append((res, Decl(self)))
            self.create_type(compile_ctx)

        subresults = list(gen_names(*["row_subres_{0}".format(i)
                                      for i in range(len(self.matchers))]))
        exit_label = gen_name("row_exit_label")

        tokeep_matchers = [m for m in self.matchers if not isinstance(m, _)]
        self.args = [r for r, m in zip(subresults, self.matchers)
                     if not isinstance(m, _)]

        bodies = []
        for i, (matcher, subresult) in enumerate(zip(self.matchers,
                                                     subresults)):
            is_discard = isinstance(matcher, _)
            mpos, mres, m_code, m_decls = matcher.gen_code_or_fncall(
                compile_ctx, pos
            )
            decls += m_decls
            if not is_discard:
                decls.append((subresult, Decl(matcher)))

            bodies.append(
                mako_template('row_submatch').render(**wrap(locals()))
            )

        body = '\n'.join(bodies)
        code = mako_template("row_code").render(**wrap(locals()))

        return pos, res, code, decls

    def __rshift__(self, index):
        return Extract(self, index)


class List(Combinator):

    def needs_refcount(self):
        return self.parser.needs_refcount()

    def __repr__(self):
        return "List({0})".format(
            repr(self.parser) + (", sep={0}".format(self.sep)
                                 if self.sep else "")
        )

    def __init__(self, parser, sep=None, empty_valid=False):
        """
        :type sep: Token|string
        :type empty_valid: bool
        """
        Combinator.__init__(self)
        self.parser = resolve(parser)
        self.sep = resolve(sep) if sep else None
        self.empty_valid = empty_valid

    def children(self):
        return [self.parser]

    def is_ptr(self):
        return True

    def nullexpr(self):
        return null_constant()

    def emit_repr(self, self_access_string):
        return mako_template("list_repr").render(**wrap(locals())).strip()

    def get_type(self):
        return mako_template("list_type").render(**wrap(locals()))

    def generate_code(self, compile_ctx, pos_name="pos"):
        """:type compile_ctx: CompileCtx"""

        pos, res, cpos = gen_names("lst_pos", "lst_res", "lst_cpos")
        seps = gen_name("lst_seps")
        ppos, pres, pcode, pdecls = self.parser.gen_code_or_fncall(
            compile_ctx, cpos
        )
        pcode = indent(pcode)
        compile_ctx.generic_vectors.add(self.parser.get_type_string())
        decls = [(pos, Decl(long)),
                 (res, Decl(self)),
                 (cpos, Decl(long))] + pdecls

        sep_pos, sep_res, sep_code, sep_decls = None, None, None, None

        if self.sep:
            sep_pos, sep_res, sep_code, sep_decls = \
                self.sep.gen_code_or_fncall(compile_ctx, cpos)
            decls += sep_decls

        code = mako_template("list_code").render(**wrap(locals()))

        return pos, res, code, decls


class Opt(Combinator):

    def needs_refcount(self):
        if self._booleanize:
            return False
        return self.matcher.needs_refcount()

    def __repr__(self):
        return "Opt({0})".format(self.matcher)

    def __init__(self, matcher, *matchers):
        Combinator.__init__(self)
        self._booleanize = False
        if matchers:
            self.matcher = Row(matcher, *matchers)
        else:
            self.matcher = resolve(matcher)

    def as_bool(self):
        self._booleanize = True
        return self

    def children(self):
        return [self.matcher]

    def emit_repr(self, self_access_string):
        return mako_template("opt_repr").render(**wrap(locals())).strip()

    def is_ptr(self):
        if self._booleanize:
            return False
        return self.matcher.is_ptr()

    def nullexpr(self):
        if self._booleanize:
            return "false"
        return self.matcher.nullexpr()

    def get_type(self):
        return bool if self._booleanize else self.matcher.get_type()

    def generate_code(self, compile_ctx, pos_name="pos"):
        mpos, mres, code, decls = self.matcher.gen_code_or_fncall(
            compile_ctx, pos_name
        )
        bool_res = gen_name("opt_bool_res")

        code = mako_template("opt_code").render(**wrap(locals()))

        if self._booleanize:
            decls.append((bool_res, Decl(bool)))
            mres = bool_res

        return mpos, mres, code, decls

    def __rshift__(self, index):
        m = self.matcher
        assert isinstance(m, Row)
        return Opt(Extract(m, index))


class Extract(Combinator):

    def needs_refcount(self):
        return self.comb.needs_refcount()

    def __repr__(self):
        return "{0} >> {1}".format(self.comb, self.index)

    def __init__(self, comb, index):
        """
        :param Row comb: The combinator that will serve as target for
        extract operation
        :param int index: The index you want to extract from the row
        """
        Combinator.__init__(self)
        self.comb = comb
        self.index = index
        assert isinstance(self.comb, Row)
        self.comb.components_need_inc_ref = False

    def children(self):
        return [self.comb]

    def get_type(self):
        return self.comb.matchers[self.index].get_type()

    def nullexpr(self):
        return self.comb.matchers[self.index].nullexpr()

    def emit_repr(self, self_access_string):
        return self.comb.matchers[self.index].emit_repr(self_access_string)

    def is_ptr(self):
        return self.comb.matchers[self.index].is_ptr()

    def generate_code(self, compile_ctx, pos_name="pos"):
        self.comb.make_tuple = False
        cpos, cres, code, decls = self.comb.gen_code_or_fncall(
            compile_ctx, pos_name)
        args = self.comb.args
        return cpos, args[self.index], code, decls


class Discard(Combinator):

    def needs_refcount(self):
        return self.parser.needs_refcount()

    def __repr__(self):
        return "Discard({0})".format(self.parser)

    def __init__(self, parser):
        Combinator.__init__(self)
        self.parser = resolve(parser)

    def children(self):
        return [self.parser]

    def get_type(self):
        return self.parser.get_type()

    def is_ptr(self):
        return self.parser.is_ptr()

    def generate_code(self, compile_ctx, pos_name="pos"):
        return self.parser.gen_code_or_fncall(compile_ctx, pos_name)


_ = Discard


class Defer(Combinator):

    def needs_refcount(self):
        self.resolve_combinator()
        return self.combinator.needs_refcount()

    def __repr__(self):
        self.resolve_combinator()
        return "Defer({0})".format(getattr(self.combinator, "_name", ".."))

    def __init__(self, parser_fn):
        Combinator.__init__(self)
        self.parser_fn = parser_fn
        self.combinator = None
        ":type: Combinator"

    def resolve_combinator(self):
        if not self.combinator:
            self.combinator = self.parser_fn()

    def nullexpr(self):
        self.resolve_combinator()
        return self.combinator.nullexpr()

    def emit_repr(self, self_access_string):
        self.resolve_combinator()
        return self.combinator.emit_repr(self_access_string)

    def is_ptr(self):
        self.resolve_combinator()
        return self.combinator.is_ptr()

    def get_type(self):
        self.resolve_combinator()
        return self.combinator.get_type()

    def generate_code(self, compile_ctx, pos_name="pos"):
        self.resolve_combinator()
        return self.combinator.gen_code_or_fncall(
            compile_ctx, pos_name=pos_name, force_fncall=True
        )


class Transform(Combinator):

    def needs_refcount(self):
        return True

    def __repr__(self):
        return "{0} ^ {1}".format(self.combinator, self.typ.name())

    def __init__(self, combinator, typ):
        Combinator.__init__(self)
        assert issubclass(typ, AdaType)
        self.combinator = combinator
        self.typ = typ
        ":type: AdaType"

        self._is_ptr = typ.is_ptr

    def children(self):
        return [self.combinator]

    def get_type(self):
        return self.typ

    def emit_repr(self, self_access_string):
        return mako_template("transform_repr").render(**wrap(locals()))

    def nullexpr(self):
        return self.get_type().nullexpr()

    def is_ptr(self):
        return self._is_ptr

    def generate_code(self, compile_ctx, pos_name="pos"):
        """:type compile_ctx: CompileCtx"""

        self.typ.add_to_context(compile_ctx, self.combinator)

        if isinstance(self.combinator, Row):
            self.combinator.make_tuple = False
            cpos, cres, code, decls = self.combinator.gen_code_or_fncall(
                compile_ctx, pos_name)
            args = self.combinator.args
        else:
            cpos, cres, code, decls = self.combinator.gen_code_or_fncall(
                compile_ctx, pos_name)
            args = [cres]

        res = gen_name("transform_res")
        code = mako_template('transform_code').render(**wrap(locals()))
        compile_ctx.diag_types.append(self.typ)

        return cpos, res, code, decls + [(res, Decl(self))]


class Success(Combinator):

    def __repr__(self):
        return "Success({0})".format(self.typ.name())

    def needs_refcount(self):
        return True

    def __init__(self, result_typ):
        Combinator.__init__(self)
        self.typ = result_typ

    def is_ptr(self):
        return self.typ.is_ptr

    def children(self):
        return []

    def get_type(self):
        return self.typ

    def emit_repr(self, self_access_string):
        return mako_template("transform_repr").render(**wrap(locals()))

    def nullexpr(self):
        return self.typ.nullexpr()

    def generate_code(self, compile_ctx, pos_name="pos"):
        self.typ.add_to_context(compile_ctx, None)
        res = gen_name("success_res")
        code = mako_template("success_code").render(**wrap(locals()))

        return pos_name, res, code, [(res, Decl(self))]


class Null(Success):

    def __repr__(self):
        return "Null"

    def generate_code(self, compile_ctx, pos_name="pos"):
        typ = self.get_type()
        if isinstance(typ, AdaType):
            self.get_type().add_to_context(compile_ctx, None)
        res = gen_name("null_res")
        code = mako_template("null_code").render(**wrap(locals()))
        return pos_name, res, code, [(res, Decl(self))]

    def emit_repr(self, self_access_string):
        return '"None"'

    def get_type(self):
        return (self.typ if inspect.isclass(self.typ)
                and issubclass(self.typ, AdaType)
                else self.typ.get_type())

    def is_ptr(self):
        return (self.typ.is_ptr if inspect.isclass(self.typ)
                and issubclass(self.typ, AdaType)
                else self.typ.is_ptr())


class EnumType(object):
    alternatives = []

    def __init__(self, alt):
        assert alt in self.alternatives
        self.alt = alt

    @classmethod
    def name(cls):
        return cls.__name__

    @classmethod
    def add_to_context(cls, compile_ctx):
        if not cls in compile_ctx.types:
            compile_ctx.types.add(cls)
            compile_ctx.types_declarations.append(
                mako_template("enum_type_decl").render(**wrap(locals()))
            )
            compile_ctx.body.append(
                mako_template("enum_type_impl").render(**wrap(locals()))
            )

    @classmethod
    def nullexpr(cls):
        return cls.name() + "::uninitialized"


class Enum(Combinator):

    def needs_refcount(self):
        return False

    def __repr__(self):
        return "Enum({0}, {1})".format(self.combinator, self.enum_type_inst)

    def __init__(self, combinator, enum_type_inst):
        Combinator.__init__(self)
        self.combinator = resolve(combinator) if combinator else None
        self.enum_type_inst = enum_type_inst

    def is_ptr(self):
        return False

    def children(self):
        return []

    def get_type(self):
        return self.enum_type_inst.__class__

    def emit_repr(self, self_access_string):
        return "enum_repr({0})".format(self_access_string)

    def nullexpr(self):
        return self.enum_type_inst.nullexpr()

    def generate_code(self, compile_ctx, pos_name="pos"):
        self.enum_type_inst.add_to_context(compile_ctx)

        res = gen_name("enum_res")

        if self.combinator:
            if isinstance(self.combinator, Row):
                self.combinator.make_tuple = False

            cpos, _, code, decls = self.combinator.gen_code_or_fncall(
                compile_ctx, pos_name
            )
        else:
            cpos, code, decls = pos_name, "", []

        body = mako_template("enum_code").render(**wrap(locals()))

        return cpos, res, body, [(res, Decl(self))] + decls
