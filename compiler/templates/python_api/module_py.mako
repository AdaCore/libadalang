## vim: filetype=python

import ctypes
import os
import sys


class AnalysisContext(object):
    # TODO: document this class and its methods

    def __init__(self, c_value=None):
        self._c_value = (_create_analysis_context()
                         if c_value is None else c_value)

    def __del__(self):
        _destroy_analysis_context(self._c_value)
        super(AnalysisContext, self).__init__()

    def create_from_file(self, filename):
        c_value = _create_analysis_unit_from_file(self._c_value, filename)
        if not c_value.value:
            raise IOError('Could not open {}'.format(filename))
        return AnalysisUnit(c_value)

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

            diag = _Diagnostic()
            success = _unit_diagnostic(self.unit._c_value, key,
                                       ctypes.byref(diag))
            if not success:
                raise IndexError('diagnostic index out of range')
            else:
                result = _wrap_diagnostic(diag)
                _free_str(diag.message)
                return result

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


class Token(object):
    # TODO: document this class and its methods

    def __init__(self, c_value):
        text = _token_text(c_value)
        self.text = text.value
        _free_str(text)

    def __repr__(self):
        return "<Token {}>".format(self.text)


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

    # We want to let users store values as attributes for ASTNode instances.
    # However keeping always the same ASTNode wrapper around C API values
    # introduces reference counting issues. So we actually store the attributes
    # in a dedicated object called the "container". Hence ASTNode instances
    # need only two fields: the C API value itself and the corresponding
    # container.
    __slots__ = ('_c_value', '_container')
    _field_names = ()

    def __init__(self, c_value):
        super(ASTNode, self).__setattr__('_c_value', c_value)
        super(ASTNode, self).__setattr__('_container',
                                         _astnode_get_container(c_value))
        _node_incref(self._c_value)

    def __getattr__(self, name):
        return getattr(self._container, name)

    def __setattr__(self, name, value):
        setattr(self._container, name, value)

    def __del__(self):
        _node_decref(self._c_value)
        super(ASTNode, self).__init__()

    def __repr__(self):
        return '<{} wrapper around {:#x}>'.format(
            type(self).__name__,
            self._c_value.value
        )

    def __eq__(self, other):
        """Return whether "self" and "other" wrap the same ASTNode"""
        return (isinstance(other, ASTNode) and
                self._c_value.value == other._c_value.value)

    @property
    def kind_name(self):
        return self._kind_name

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
        """Return the number of ASTNode children this node has"""
        return _node_child_count(self._c_value)

    def __getitem__(self, key):
        """Return the Nth ASTNode child this node has

        Raise an IndexError if "key" is out of range.
        """
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

    def iter_fields(self):
        """Iterate through all the fields this node contains

        Return an iterator that yields (field_name, field_value) couples for
        all fields in this node.
        """
        for field_name in self._field_names:
            yield (field_name, getattr(self, 'f_{}'.format(field_name)))


class _ASTNodeContainer(object):
    pass


class ASTList(ASTNode):
    # TODO: document this class
    _kind_name = 'list'


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

so_ext = {
    'win32':  'dll',
    'darwin': 'dylib',
}.get(sys.platform, 'so')
_c_lib = ctypes.cdll.LoadLibrary("lib${c_api.lib_name}.{}".format(so_ext))


def _import_func(name, argtypes, restype):
    """
    Import "name" from the C library, set its arguments/return types and return
    the binding.
    """
    func = getattr(_c_lib, name)
    func.argtypes = argtypes
    func.restype = restype
    return func


class string_to_free(ctypes.c_char_p):
    """
    Regular c_char_p instances automatically copy the input string on struct
    field access/function return, which makes it impossible to free it
    afterwards (we have no handle to the returned buffer).  Subclassing it
    removes this automation.
    """
    pass


class _analysis_context(ctypes.c_void_p):
    pass
class _analysis_unit(ctypes.c_void_p):
    pass
class _node(ctypes.c_void_p):
    pass
_enum_node_kind = ctypes.c_uint
class _token(ctypes.c_void_p):
    pass


class _Sloc(ctypes.Structure):
    _fields_ = [("line", ctypes.c_uint32),
                ("column", ctypes.c_uint16)]


class _SlocRange(ctypes.Structure):
    _fields_ = [("start", _Sloc),
                ("end", _Sloc)]


class _Diagnostic(ctypes.Structure):
    _fields_ = [("sloc_range", _SlocRange),
                ("message", string_to_free)]


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

_free_str = _import_func(
    '${capi.get_name("free_str")}',
    [string_to_free], None
)

# General AST node primitives
_node_kind = _import_func(
    '${capi.get_name("node_kind")}',
    [_node], _enum_node_kind
)
_kind_name = _import_func(
    '${capi.get_name("kind_name")}',
    [_enum_node_kind], string_to_free
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

_token_text = _import_func(
    '${capi.get_name("token_text")}',
    [_token], string_to_free
)

% for astnode in _self.astnode_types:
    % for primitive in _self.c_astnode_primitives[astnode]:
_${primitive.name.lower} = _import_func(
    '${capi.get_name(primitive.name)}',
    [_node,
     ctypes.POINTER(${primitive.field_type.py_type(pyapi).name_low})],
    ctypes.c_int
)
    % endfor
% endfor


# Extensions handling
_register_extension = _import_func(
    '${capi.get_name("register_extension")}',
    [ctypes.c_char_p], ctypes.c_uint
)
_node_extension_destructor = ctypes.CFUNCTYPE(
    ctypes.c_void_p,
    _node, ctypes.c_void_p
)
_node_extension = _import_func(
    '${capi.get_name("node_extension")}',
    [_node, ctypes.c_uint, _node_extension_destructor],
    ctypes.POINTER(ctypes.c_void_p)
)


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
                      c_value.message.value)


_kind_to_astnode_cls = {
    1: ASTList,
    % for subclass in _self.astnode_types:
        % if not subclass.abstract:
    ${_self.node_kind_constants[subclass]}: ${subclass.name()},
        % endif
    % endfor
}

# We use the extension mechanism to keep a single container per node for all
# attributes attached to ASTNode instances. This way, users can store values in
# wrappers and expect to find these attributes back when getting the same node
# later.

_node_extension_id = _register_extension("python_api_astnode_wrapper")
def _node_ext_dtor_py(c_node, c_pyobj):
    """
    Callback for extension upon ASTNode destruction: free the reference for the
    Python container.
    """
    c_pyobj = ctypes.py_object(c_pyobj)
    ctypes.pythonapi.Py_DecRef(c_pyobj)


_node_ext_dtor_c = _node_extension_destructor(_node_ext_dtor_py)


def _astnode_get_container(c_value):
    """
    Get or create if needed a container object for the "c_value" ASTNode
    """
    # First look if we already have a container for this node so that we only
    # have one container per node.
    c_pyobj_p = _node_extension(c_value, _node_extension_id, _node_ext_dtor_c)
    c_pyobj_p = ctypes.cast(
        c_pyobj_p,
        ctypes.POINTER(ctypes.py_object)
    )
    if c_pyobj_p.contents:
        return c_pyobj_p.contents.value
    else:
        # Create a new container and store it in our extension.
        py_obj = _ASTNodeContainer()
        c_pyobj_p[0] = ctypes.py_object(py_obj)

        # We want to increment its ref count so that the wrapper will be alive
        # as long as the extension references it.
        ctypes.pythonapi.Py_IncRef(ctypes.py_object(py_obj))
        return py_obj


def _wrap_astnode(c_value):
    """
    Wrap an ASTNode from the C API with the appropriate ASTNode Python subclass
    """
    if not c_value:
        return None
    # Create a Python wrapper for this node with the appropriate ASTNode
    # subclass.
    kind = _node_kind(c_value)
    return _kind_to_astnode_cls[kind](c_value)
