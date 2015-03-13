import re

from c_api import CAPIType
import parsers


class CAPISettings(object):
    """Convenient container for C API generation settings

    The convention is to make instances for this class available to templates
    as `capi`."""

    LIB_NAME_RE = re.compile('[a-zA-Z][a-zA-Z0-9_-]+')

    def __init__(self, lib_name, symbol_prefix=None):
        """
        Create C API generation settings

        lib_name: Name of the generated library.  This will be used to build
        the name of header files, library (static and shared object) files,
        etc. It must be a valid C identifier with the exception that dash ("-")
        are allowed. Case matters (but you still choose it).

        symbol_prefix: Valid C identifier used as a prefix for all top-level
        declarations in the generated C API. None if no prefix is needed.
        """
        if not self.LIB_NAME_RE.match(lib_name):
            raise ValueError('Invalid library name: {}'.format(lib_name))

        self.lib_name = lib_name
        self.symbol_prefix = symbol_prefix

    #
    # Helpers for templates
    #

    @property
    def header_guard_id(self):
        return self.lib_name.upper().replace('-', '_')

    def get_name(self, name):
        """Wrap `name` as a top-level scope symbol."""
        return ('{}_{}'.format(self.symbol_prefix, name)
                if self.symbol_prefix else name)

    # The following types are used pervasively in templates, so we provide
    # shortcuts here.

    @property
    def analysis_context_type(self):
        return CAPIType(self, 'analysis_context')

    @property
    def analysis_unit_type(self):
        return CAPIType(self, 'analysis_unit')

    @property
    def node_kind_type(self):
        return CAPIType(self, 'node_kind', 'enum')

    @property
    def node_type(self):
        return CAPIType(self, 'node')

    @property
    def sloc_type(self):
        return CAPIType(self, 'source_location', 'struct')

    @property
    def sloc_range_type(self):
        return parsers.SourceLocationRangeType.c_type(self)
