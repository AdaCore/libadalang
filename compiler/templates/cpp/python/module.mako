## vim: filetype=python

# TODO: handle Windows platforms (note that Cygwin is handled just like Linux)
import ctypes


class AnalysisContext(object):
    # TODO: document this class and its methods

    def __init__(self, c_value=None):
        self._c_value = (_create_analysis_context()
                         if c_value is None else c_value)

    def __del__(self):
        _destroy_analysis_context(self._c_value)
        super(AnalysisContext, self).__init__()

    def create_from_file(self, filename):
        value = _create_analysis_unit_from_file(self._c_value, filename)
        return AnalysisUnit(value)

    def remove(self, filename):
        _remove_analysis_unit(self._c_value, filename)


class AnalysisUnit(object):
    # TODO: document this class and its methods

    class DiagnosticsList(object):
        def __init__(self, unit):
            self.unit = unit

        def __len__(self):
            return _unit_diagnostic_count(self.unit._c_value)

        def __getitem__(self, key):
            if not isinstance(key, int):
                msg = 'list indices must be integers, not {}'.format(
                    type(key))
                raise TypeError(msg)

            result = _Diagnostic()
            success = _unit_diagnostic(self.unit._c_value, key,
                                       ctypes.byref(result))
            if not success:
                raise IndexError('child index out of range')
            else:
                return _wrap_diagnostic(result)

    def __init__(self, c_value):
        self._c_value = c_value
        _unit_incref(self._c_value)

    def __del__(self):
        _unit_decref(self._c_value)
        super(AnalysisUnit, self).__init__()

    @property
    def root(self):
        return _wrap_astnode(_unit_root(self._c_value))

    @property
    def diagnostics(self):
        return self.DiagnosticsList(self)


class Sloc(object):
    # TODO: document this class and its methods

    def __init__(self, line, column):
        self.line = line
        self.column = column

    def __nonzero__(self):
        return self.line or self.column


class SlocRange(object):
    # TODO: document this class and its methods

    def __init__(self, start, end):
        self.start = start
        self.end = end

    def __nonzero__(self):
        return self.start or self.end


class Diagnostic(object):
    # TODO: document this class and its methods

    def __init__(self, sloc_range, message):
        self.sloc_range = sloc_range
        self.message = message


class ASTNode(object):
    # TODO: document this class and its methods

    def __init__(self, c_value):
        self._c_value = c_value
        _node_incref(self._c_value)

    def __del__(self):
        _node_decref(self._c_value)
        super(ASTNode, self).__init__()

    @property
    def kind_name(self):
        kind = _node_kind(self._c_value)
        return str(_kind_name(kind))

    @property
    def sloc_range(self):
        result = _SlocRange()
        _node_sloc_range(self._c_value, ctypes.byref(result))
        return _wrap_sloc_range(result)

    @property
    def lookup(self, sloc):
        c_sloc = _unwrap_sloc(sloc)
        c_node =_lookup_in_node(self._c_value,
                                ctypes.byref(c_sloc))
        return _wrap_astnode(c_node)

    @property
    def parent(self):
        return _wrap_astnode(_node_parent(self._c_value))

    def __len__(self):
        return _node_child_count(self._c_value)

    def __getitem__(self, key):
        if not isinstance(key, int):
            msg = 'ASTNode children are integer-indexed (got {})'.format(
                type(key))
            raise TypeError(msg)

        result = _node()
        success = _node_child(self._c_value, key, ctypes.byref(result))
        if not success:
            raise IndexError('child index out of range')
        else:
            return _wrap_astnode(result)


class ASTList(ASTNode):
    # TODO: document this class
    pass


% for subclass_decl in astnode_subclass_decls:
${subclass_decl}
% endfor

UNINITIALIZED = 'uninitialized'

% for chunk in _self.py_astnode_field_types.values():
${chunk}
% endfor

#
# Low-level C binding
#

_c_lib = ctypes.cdll.LoadLibrary("libadalang.so")


def _import_func(name, argtypes, restype):
    """
    Import "name" from the C library, set its arguments/return types and return
    the binding.
    """
    func = getattr(_c_lib, name)
    func.argtypes = argtypes
    func.restype = restype
    return func


_analysis_context = ctypes.c_void_p
_analysis_unit = ctypes.c_void_p
_node = ctypes.c_void_p
_enum_node_kind = ctypes.c_uint

class _Sloc(ctypes.Structure):
    _fields_ = [("line", ctypes.c_uint32),
                ("column", ctypes.c_uint16)]

class _SlocRange(ctypes.Structure):
    _fields_ = [("start", _Sloc),
                ("end", _Sloc)]

class _Diagnostic(ctypes.Structure):
    _fields_ = [("sloc_range", _SlocRange),
                ("message", ctypes.c_char_p)]

# Analysis primitives
_create_analysis_context = _import_func(
    '${capi.get_name("create_analysis_context")}',
    [], _analysis_context
)
_destroy_analysis_context = _import_func(
    '${capi.get_name("destroy_analysis_context")}',
    [_analysis_context, ], None
)
_create_analysis_unit_from_file = _import_func(
    '${capi.get_name("create_analysis_unit_from_file")}',
    [_analysis_context, ctypes.c_char_p], _analysis_unit
)
_remove_analysis_unit = _import_func(
    '${capi.get_name("remove_analysis_unit")}',
    [_analysis_context, ctypes.c_char_p], None
)
_unit_root = _import_func(
    '${capi.get_name("unit_root")}',
    [_analysis_unit], _node
)
_unit_diagnostic_count = _import_func(
    '${capi.get_name("unit_diagnostic_count")}',
    [_analysis_unit], ctypes.c_uint
)
_unit_diagnostic = _import_func(
    '${capi.get_name("unit_diagnostic")}',
    [_analysis_unit, ctypes.c_uint, ctypes.POINTER(_Diagnostic)], ctypes.c_int
)
_unit_incref = _import_func(
    '${capi.get_name("unit_incref")}',
    [_analysis_unit], _analysis_unit
)
_unit_decref = _import_func(
    '${capi.get_name("unit_decref")}',
    [_analysis_unit], None
)

# General AST node primitives
_node_kind = _import_func(
    '${capi.get_name("node_kind")}',
    [_node], _enum_node_kind
)
_kind_name = _import_func(
    '${capi.get_name("kind_name")}',
    [_enum_node_kind], ctypes.c_char_p
)
_node_sloc_range = _import_func(
    '${capi.get_name("node_sloc_range")}',
    [_node, ctypes.POINTER(_SlocRange)], None
)
_lookup_in_node = _import_func(
    '${capi.get_name("lookup_in_node")}',
    [_node, ctypes.POINTER(_Sloc)], _node
)
_node_parent = _import_func(
    '${capi.get_name("node_parent")}',
    [_node], _node
)
_node_child_count = _import_func(
    '${capi.get_name("node_child_count")}',
    [_node], ctypes.c_uint
)
_node_child = _import_func(
    '${capi.get_name("node_child")}',
    [_node, ctypes.c_uint, ctypes.POINTER(_node)], ctypes.c_int
)
_node_incref = _import_func(
    '${capi.get_name("node_incref")}',
    [_node], _node
)
_node_decref = _import_func(
    '${capi.get_name("node_decref")}',
    [_node], None
)

% for astnode in _self.astnode_types:
    % for primitive in _self.c_astnode_primitives[astnode]:
_${primitive.name} = _import_func(
    '${capi.get_name(primitive.name)}',
    [_node,
     ctypes.POINTER(${primitive.field_type.py_type(pyapi).name_low})],
    ctypes.c_int
)
    % endfor
% endfor


#
# Layering helpers
#

def _wrap_sloc(c_value):
    return Sloc(c_value.line, c_value.column)

def _unwrap_sloc(sloc):
    return _Sloc(sloc.line, sloc.column)

def _wrap_sloc_range(c_value):
    return SlocRange(_wrap_sloc(c_value.start),
                     _wrap_sloc(c_value.end))

def _wrap_diagnostic(c_value):
    return Diagnostic(_wrap_sloc_range(c_value.sloc_range),
                      c_value.message)


_kind_to_astnode_cls = {
    0: ASTList,
    % for subclass in _self.astnode_types:
        % if not subclass.abstract:
    ${_self.node_kind_constants[subclass]}: ${subclass.name()},
        % endif
    % endfor
}

def _wrap_astnode(c_value):
    if not c_value:
        return None

    kind = _node_kind(c_value)
    return _kind_to_astnode_cls[kind](c_value)
