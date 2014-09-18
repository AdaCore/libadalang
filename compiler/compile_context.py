from glob import glob
import os
import shutil
import subprocess
from os import path, environ
from distutils.spawn import find_executable


def write_cpp_file(file_path, source):
    with open(file_path, "wb") as out_file:
        if find_executable('clang-format'):
            p = subprocess.Popen(["clang-format"], stdin=subprocess.PIPE,
                                 stdout=out_file)
            p.communicate(source)
            assert p.returncode == 0
        else:
            out_file.write(source)


class CompileCtx():
    """State holder for native code emission."""

    def __init__(self):
        # TODO???  A short description for all these fields would help!
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
        self.grammar = None
        self.lexer_file = None

        # Mapping: ASTNode -> list of CompiledType instances
        self.ast_fields_types = {}

    def set_ast_fields_types(self, astnode, types):
        """
        Associate `types` (a list of CompiledType) to fields in `astnode` (an
        ASTNode sub-class). It is valid to perform this association multiple
        times as long as types are consistent.
        """
        assert len(astnode.fields) == len(types), (
            "{} has {} fields ({} types given). You probably have"
            " inconsistent grammar rules and type declarations".format(
                astnode, len(astnode.fields), len(types)
            )
        )
        # TODO: instead of expecting types to be *exactly* the same, perform
        # type unification (take the nearest common ancestor for all field
        # types).
        assert (astnode not in self.ast_fields_types or
                self.ast_fields_types[astnode] == types), (
            "Already associated types for some fields are not consistant with"
            " current ones:\n- {}\n- {}".format(
                self.ast_fields_types[astnode], types
            )
        )
        self.ast_fields_types[astnode] = types

    @property
    def render_template(self):
        # Kludge: to avoid circular dependency issues, do not import parsers
        # until needed.
        from parsers import render_template
        return render_template

    def get_header(self):
        return self.render_template(
            'main_header',
            _self=self,
            tdecls=self.types_declarations,
            tdefs=self.types_definitions,
            fndecls=self.fns_decls,
        )

    def get_source(self, header_name):
        return self.render_template(
            'main_body',
            _self=self,
            header_name=header_name,
            bodies=self.body
        )

    def get_interactive_main(self, header_name):
        return self.render_template(
            'interactive_main',
            _self=self, header_name=header_name
        )

    def set_grammar(self, grammar):
        self.grammar = grammar

    def set_lexer_file(self, lexer_file):
        """
        Set the quex file for this compile ctx. This needs to be called
        BEFORE any grammar rules are created, because Tok and TokClass rules
        needs to be aware of the quex lexer.
        """
        self.lexer_file = lexer_file
        import quex_tokens
        quex_tokens.init_token_map(self)

    def emit(self, file_root=".", file_name="parse"):
        """
        Emit native code for all the rules in this grammar as a library:
        a library specification and the corresponding implementation.  Also
        emit a tiny program that can parse starting with any parsing rule for
        testing purposes.
        """
        assert self.grammar, "Set grammar before calling emit"
        assert self.lexer_file, "Set lexer before calling emit"

        src_path = path.join(file_root, "src")

        if not path.exists(file_root):
            os.mkdir(file_root)

        for d in ["include", "obj", "src", "bin"]:
            p = path.join(file_root, d)
            if not path.exists(p):
                os.mkdir(p)

        shutil.copy("support/Makefile", path.join(file_root, "Makefile"))

        for f in glob("support/*.hpp"):
            shutil.copy(f, path.join(file_root, "include"))

        for f in glob("support/*.cpp"):
            shutil.copy(f, path.join(file_root, "src"))

        for r_name, r in self.grammar.rules.items():
            r.compute_fields_types(self)

        for r_name, r in self.grammar.rules.items():
            r.compile(self)
            self.rules_to_fn_names[r_name] = r

        write_cpp_file(path.join(src_path, file_name + ".cpp"),
                       self.get_source(header_name=file_name + ".hpp"))

        write_cpp_file(path.join(src_path, file_name + ".hpp"),
                       self.get_header())

        write_cpp_file(
            path.join(src_path, file_name + "_main.cpp"),
            self.get_interactive_main(header_name=file_name + ".hpp")
        )

        quex_py_file = path.join(environ["QUEX_PATH"], "quex-exe.py")

        subprocess.check_call(["python", quex_py_file, "-i", self.lexer_file,
                               "--engine", "quex_lexer",
                               "--token-id-offset",  "0x1000",
                               "--language", "C",
                               "--no-mode-transition-check",
                               "--single-mode-analyzer",
                               "--token-memory-management-by-user",
                               "--token-policy", "single"],
                              cwd=path.join(file_root, "src"))
