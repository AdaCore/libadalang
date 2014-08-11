import ast
import re
from collections import defaultdict
import inspect
from tokenizer import *
from utils import isalambda
import meta
import codegen
import traceback

###############
# AST HELPERS #
###############


def compile_to_module(ast):
    import imp
    mod = imp.new_module(gen_name("module"))
    code_obj = compile(ast, "<string>", "exec")
    exec code_obj in mod.__dict__
    return mod


__next_ids = defaultdict(int)

NoResult = (None, None)


def insert_list(l1, l2, idx):
    for i, el in enumerate(l1):
        l2.insert(idx + i, el)


def gen_name(var_name):
    __next_ids[var_name] += 1
    return "{0}_{1}".format(var_name, __next_ids[var_name])


def gen_names(*var_names):
    for var_name in var_names:
        yield gen_name(var_name)


def var_ref(var_name):
    return ast.Name(id=var_name, ctx=ast.Load())


def stmts(template, *args, **kwargs):
    return ast.parse(template.format(*args, **kwargs)).body


def stmts_block(template, *args, **kwargs):
    """
    :rtype: ast.Body
    """
    return ast.parse(template.format(*args, **kwargs))


def to_source(ast_inst):
    return codegen.to_source(ast_inst)


def lambda_ast(l):
    try:
        tree = ast.parse(inspect.getsource(l))
        for n in ast.walk(tree):
            if isinstance(n, ast.Lambda):
                return n
    except SyntaxError:
        _, start_line = inspect.getsourcelines(l)
        fulltree = ast.parse(open(inspect.getsourcefile(l)).read())
        for n in ast.walk(fulltree):
            if isinstance(n, ast.Lambda) and n.lineno == start_line:
                return n


def lambda_source(l):
    return codegen.to_source(lambda_ast(l))


###############
# Combinators #
###############


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


class CompileCtx():
    def __init__(self):
        self.code = stmts_block(
            "NoResult = (None, None)\n"
        )
        self.imports = {"tokenizer"}
        self.fns = set()

    def get_source(self):
        source = to_source(self.code)
        return "\n".join("from {0} import *".format(i) for i in
                         self.imports) + "\n" + source


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
            return self.rules[item_name]

        if not self.resolved:
            return Defer(lambda: self.rules[item_name])
        else:
            raise AttributeError


class Combinator(object):

    # noinspection PyMissingConstructor
    def __init__(self):
        self._statements_ast = None
        self._mod = None
        self.gen_fn_name = gen_name(self.__class__.__name__.lower() + "_parse")
        self.grammar = None
        self.is_root = False

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
            c.set_name(name)
        self.gen_fn_name = gen_name("{0}_{1}_parse".format(
            name, self.__class__.__name__.lower()))

    def parse(self, tkz, pos):
        if not self._mod:
            self.compile_and_exec()
        return self._mod.__dict__[self.gen_fn_name](tkz, pos)

    # noinspection PyMethodMayBeStatic
    def children(self):
        return []

    def compile(self, compile_ctx=None):
        """:type compile_ctx: CompileCtx"""

        if self.gen_fn_name in compile_ctx.fns:
            return

        compile_ctx.fns.add(self.gen_fn_name)

        if not compile_ctx:
            compile_ctx = CompileCtx()

        pos, res, code = self.generate_code(compile_ctx=compile_ctx)

        assert self._statements_ast

        fn_ast = stmts_block("def {0}(tkz, pos): pass", self.gen_fn_name)
        fn_ast_fn = fn_ast.body[0]
        fn_ast_fn.body.pop()

        ret_str = stmts(
            "return {0}, {1}", pos, res)

        fn_ast_fn.body = code.body + ret_str
        compile_ctx.code.body.append(fn_ast_fn)

    def compile_and_exec(self, compile_ctx=None):
        if not compile_ctx:
            compile_ctx = CompileCtx()
        self.compile(compile_ctx)
        mod = compile_to_module(compile_ctx.get_source())
        self._mod = mod


    def dump_to_file(self, file_name="compiled_parser.py"):
        def fix_code(code):
            try:
                import autopep8
                return autopep8.fix_code(code)
            except ImportError:
                return code

        ctx = CompileCtx()
        self.compile(ctx)
        with open(file_name, "w") as f:
            f.write(fix_code(ctx.get_source()))

    def force_fn_call(self):
        return self.is_root

    def gen_code_or_fncall(self, compile_ctx, pos_name="pos",
                           force_fncall=False):
        if self.force_fn_call() or force_fncall or \
                self.gen_fn_name in compile_ctx.fns:

            self.compile(compile_ctx)
            pos, res = gen_names("fncall_pos", "fncall_res")

            fncall_block = stmts_block(
                "{pos}, {res} = {self.gen_fn_name}(tkz, {pos_name})",
                **locals()
            )
            return pos, res, fncall_block
        else:
            return self.generate_code(compile_ctx, pos_name)

    def generate_code(self, compile_ctx, pos_name="pos"):
        raise NotImplemented

    def test_parser(self, ada_string):
        tkz = make_ada_tokenizer(ada_string)
        npos, res = self.parse(tkz, 0)
        return res


class Tok(Combinator):

    def __init__(self, tok):
        """ :type tok: Token|str """
        Combinator.__init__(self)

        self.tok = resolve(tok)
        ":type: Token"

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("tok_pos", "tok_res")
        repr_tok_val = repr(self.tok.val)
        self._statements_ast = stmts_block(
            "tk = tkz.get({pos_name})\n"
            '{pos}, {res} = ({pos_name} + 1, tk) '
            'if tk.val == {repr_tok_val} else NoResult',
            **locals()
        )
        return pos, res, self._statements_ast


class TokClass(Combinator):

    def __init__(self, tok_class):
        Combinator.__init__(self)
        self.tok_class = tok_class

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("tkclass_pos", "tkclass_res")
        self._statements_ast = stmts_block(
            "tk = tkz.get({2})\n"
            "{0}, {1} = ({2} + 1, tk) if isinstance(tk, {3}) else NoResult",
            pos, res, pos_name, self.tok_class.__name__
        )

        return pos, res, self._statements_ast


class Or(Combinator):

    def __init__(self, *matchers):
        """ :type matchers: list[Combinator|Token|type] """
        Combinator.__init__(self)
        self.matchers = [resolve(m) for m in matchers]

    def children(self):
        return self.matchers

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("or_pos", "or_res")
        m = stmts_block(
            "{0}, {1} = (None, None)\n"
            "while True:\n"
            "    break", pos, res
        )
        loop_body = m.body[1].body
        break_stm = loop_body.pop()

        for matcher in self.matchers:
            mpos, mres, m_code = matcher.gen_code_or_fncall(
                compile_ctx, pos_name
            )
            loop_body += m_code.body
            if_code = stmts_block(
                "if {2} is not None:\n"
                "    {0}, {1} = {2}, {3}\n"
                "    break",
                pos, res, mpos, mres
            ).body
            loop_body += if_code

        loop_body.append(break_stm)
        self._statements_ast = m
        return pos, res, m


class Row(Combinator):

    def __init__(self, *matchers):
        """ :type matchers: list[Combinator|Token|type] """
        Combinator.__init__(self)
        self.matchers = [resolve(m) for m in matchers]
        self.make_tuple = True

    def children(self):
        return self.matchers

    def generate_code(self, compile_ctx, pos_name="pos"):
        c_pos_name = pos_name
        pos, res, did_fail = gen_names("row_pos", "row_res", "row_did_fail")
        subresults = list(gen_names(*["row_subres_{0}".format(i)
                                      for i in range(len(self.matchers))]))

        self.tupl_elems = ",".join(r for r, m in zip(subresults, self.matchers)
                                   if not isinstance(m, Discard)) + ", "

        m = stmts_block(
            "{pos}, {did_fail} = None, False\n"
            "while True:\n"
            "    break\n" +
            ("{res} = ({self.tupl_elems}) if not {did_fail} else None\n"
             if self.make_tuple else ""),
            **locals()
        )
        loop_body = m.body[1].body
        break_stm = loop_body.pop()

        for matcher, subresult in zip(self.matchers, subresults):
            mpos, mres, m_code = matcher.gen_code_or_fncall(compile_ctx, c_pos_name)
            c_pos_name = mpos
            loop_body += m_code.body
            if_code = stmts_block(
                "if {mpos} is not None:\n"
                "    {pos} = {mpos}\n" +
                ("    {subresult} = {mres}\n"
                 if not isinstance(matcher, Discard) else "") +
                "else:\n"
                "    {pos} = None\n"
                "    {did_fail} = True\n"
                "    break",
                **locals()
            ).body
            loop_body += if_code

        loop_body.append(break_stm)
        self._statements_ast = m
        return pos, res, m


class List(Combinator):

    def __init__(self, parser, sep=None, empty_valid=False):
        """
        :type sep: Token|string
        :type empty_valid: bool
        """
        Combinator.__init__(self)
        self.parser = resolve(parser)
        self.sep = sep.val if isinstance(sep, Token) else sep
        self.empty_valid = empty_valid

    def children(self):
        return [self.parser]

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("lst_pos", "lst_res")
        npos, cnode = gen_names("lst_npos", "lst_cnode")
        cpos = pos if self.empty_valid else gen_name("lst_cpos")
        ppos, pres, parser_code = self.parser.gen_code_or_fncall(compile_ctx, cpos)
        code = (
            "{pos}, {res} = None, []\n"
            "{cpos} = {pos_name}\n"
            "while True:\n"
            "    {npos}, {cnode} = {ppos}, {pres}\n"
            "    if {npos} is not None:\n"
            "        {pos} = {npos}\n"
            "        {cpos} = {npos}\n"
            "        {res}.append({cnode})\n"
            "    else:\n"
            "        break\n"
        ).format(**locals())
        m = stmts_block(code)

        loop_body = m.body[2].body

        if self.sep:
            if_branch_body = loop_body[1].body
            sep_repr = repr(self.sep)
            insert_list(stmts_block(
                "if tkz.get({cpos}).val == {sep_repr}:\n"
                "    {cpos} += 1\n"
                "else:\n"
                "    break\n", **locals()
            ).body, if_branch_body, len(if_branch_body))

        insert_list(parser_code.body, loop_body, 0)

        self._statements_ast = m
        return pos, res, m


class Opt(Combinator):
    def __init__(self, matcher, *matchers):
        Combinator.__init__(self)
        if matchers:
            self.matcher = Row(matcher, *matchers)
        else:
            self.matcher = resolve(matcher)

    def children(self):
        return [self.matcher]

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res = gen_names("opt_pos", "opt_res")
        mpos, mres, code = self.matcher.gen_code_or_fncall(compile_ctx, pos_name)
        m = self.matcher

        self._statements_ast = stmts_block(
            "{pos}, {res} = ({mpos}, {mres}) if {mpos} is "
            "not None else ({pos_name}, None)\n"
            , **locals()
        )

        self._statements_ast.body = code.body + self._statements_ast.body
        return pos, res, self._statements_ast


class Discard(Combinator):
    def __init__(self, parser):
        Combinator.__init__(self)
        self.parser = resolve(parser)

    def children(self):
        return [self.parser]

    def generate_code(self, compile_ctx, pos_name="pos"):
        pos, res, self._statements_ast = self.parser.gen_code_or_fncall(
            compile_ctx, pos_name
        )
        return pos, res, self._statements_ast


class Success(Combinator):
    def __init__(self, result_fn):
        Combinator.__init__(self)
        self.result_fn = result_fn
        self.res_fn_name = gen_name("succ_result_fn")

    def generate_code(self, compile_ctx, pos_name="pos"):
        if isalambda(self.result_fn):
            lmbd_src = lambda_source(self.result_fn)
            compile_ctx.code.body.append(
                stmts("{self.res_fn_name} = {lmbd_src}\n", **locals())[0]
            )
        else:
            compile_ctx.imports.add(self.result_fn.__module__)
            self.res_fn_name = self.result_fn.__name__
        pos = pos_name
        res = gen_name("succ_res")
        self._statements_ast = stmts_block(
            "{res} = {self.res_fn_name}()\n", **locals()
        )
        return pos, res, self._statements_ast


class Defer(Combinator):
    def __init__(self, parser_fn):
        Combinator.__init__(self)
        self.parser_fn = parser_fn

    def generate_code(self, compile_ctx, pos_name="pos"):
        combinator = self.parser_fn()
        ":type: Combinator"
        pos, res, self._statements_ast = combinator.gen_code_or_fncall(
            compile_ctx, pos_name=pos_name, force_fncall=True
        )
        return pos, res, self._statements_ast


class Transform(Combinator):

    def __init__(self, combinator, transform_fn):
        Combinator.__init__(self)
        assert combinator
        self.combinator = combinator
        self.transform_fn = transform_fn
        self.fn_name = gen_name("transform_fn")

    def children(self):
        return [self.combinator]

    def generate_code(self, compile_ctx, pos_name="pos"):
        if isalambda(self.transform_fn):
            lmbd_src = lambda_source(self.transform_fn)
            compile_ctx.code.body.append(
                stmts("{self.fn_name} = {lmbd_src}\n", **locals())[0]
            )
        else:
            compile_ctx.imports.add(self.transform_fn.__module__)
            self.fn_name = self.transform_fn.__name__

        if isinstance(self.combinator, Row):
            self.combinator.make_tuple = False
            cpos, cres, code = self.combinator.gen_code_or_fncall(
                compile_ctx, pos_name)
            cres = self.combinator.tupl_elems
        else:
            cpos, cres, code = self.combinator.gen_code_or_fncall(
                compile_ctx, pos_name)

        pos = cpos
        res = gen_name("transform_res")

        self._statements_ast = stmts_block(
            "{res} = None\n"
            "if {pos} is not None:\n"
            "    {res} = {self.fn_name}({cres})\n",
            **locals()
        )
        self._statements_ast.body = code.body + self._statements_ast.body
        return pos, res, self._statements_ast

