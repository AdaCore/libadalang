import re

import names


class CAPIType(object):
    """
    C API generation helper: encapsulate the logic of C types formatting.
    """

    def __init__(self, c_api_settings, name, external=False):
        """Create a stub for a C API type.

        c_api_settings: A c_api.CAPISettings instance.

        name: The name for the type.

        external: Whether this type is already declared outside the C API. For
        instance: "int" is external, but "node" is not.
        """
        self.c_api_settings = c_api_settings
        self.external = external
        # Make private thefollowing in order to avoid accidental use of these
        # instead of the properties.
        self._name = name

    @property
    def name(self):
        """Return the C name for this type, properly wrapped if needed."""
        # All names we define as part of the C API must be wrapped so that they
        # don't conflict with "external" names. Keep "external" ones untouched
        # since we don't control them.
        return (self._name if self.external else
                self.c_api_settings.get_name(names.Name(self._name)))


class CAPISettings(object):
    """Convenient container for C API generation settings.

    The convention is to make instances for this class available to templates
    as `capi`."""

    LIB_NAME_RE = re.compile('[a-zA-Z][a-zA-Z0-9_-]+')

    def __init__(self, lib_name, symbol_prefix=''):
        """
        Create C API generation settings.

        :param str lib_name: Name of the generated library.  This will be used
        to build the name of header files, library (static and shared object)
        files, etc. It must be a valid C identifier with the exception that
        dashes ("-") are allowed. Case matters (but you still choose it).

        :param str symbol_prefix: Valid C identifier used as a prefix for all
        top-level declarations in the generated C API. Empty string (default)
        if no prefix is needed.
        """
        if not self.LIB_NAME_RE.match(lib_name):
            raise ValueError('Invalid library name: {}'.format(lib_name))

        self.lib_name = lib_name
        self.symbol_prefix = symbol_prefix

    #
    # Helpers for templates
    #

    @property
    def shared_object_basename(self):
        """
        Return the basename to use for the shared object.

        In order to get the fullname, format the following string::

            lib{shared_object_basename}.{extension}

        `extension` must be "so" on Linux, "dll" on Windows, etc.
        """
        basename = self.lib_name.lower()
        return basename[3:] if basename.startswith('lib') else basename

    @property
    def header_guard_id(self):
        return self.lib_name.upper().replace('-', '_')

    def get_name(self, name):
        """
        Wrap `name` as a top-level scope symbol.

        :type name: Name|basestring
        """
        if isinstance(name, basestring):
            name = names.Name(name)
        return names.Name('{}_{}'.format(self.symbol_prefix, name.base_name)
                          if self.symbol_prefix else name).lower

    def get_enum_alternative(self, type_name, alt_name, suffix):
        """
        Return a name that is suitable for code generation for the `alt_name`
        alternative in the `type_name` enumeration type. `suffix` should be
        used to post-process names that are invalid enumerators.
        """
        return self.get_name(
            names.Name('{}_{}'.format(type_name.base_name,
                                      alt_name.base_name)))
