import re


class CAPIType(object):
    """
    C API generation helper: encapsulate the logic of C types formatting
    """

    def __init__(self, c_api_settings, name, tag=None, external=False):
        """Create a stub for a C API type

        c_api_settings: A c_api_settings.CAPISettings instance.


        name: The name for the type (without any tag).

        tag: Tag for this type ("struct", "enum", ...) or None when the type is
        a typedef.

        external: Whether this type is already declared outside the C API. For
        instance: "int" is external, but "node" is not.
        """
        self.c_api_settings = c_api_settings
        self.external = external
        # Make private these two in order to avoid accidental use of these
        # instead of the properties.
        self._name = name
        self._tag = tag

    @property
    def tagged(self):
        """Return the wrapped name including any tag"""
        return ('{} {}'.format(self._tag, self.simple)
                if self._tag else self.simple)

    @property
    def simple(self):
        """Return the wrapped name without any tag"""
        # All names we define as part of the C API must be wrapped so that they
        # don't conflict with "external" names. Keep "external" ones untouched
        # since we don't control them.
        return (self._name if self.external else
                self.c_api_settings.get_name(self._name))


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
        etc. It must be a valid C identifier with the exception that dashes
        ("-") are allowed. Case matters (but you still choose it).

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
