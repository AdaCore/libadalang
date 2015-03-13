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
    def tagged_name(self):
        """Return the wrapped name including any tag"""
        return ('{} {}'.format(self._tag, self.name)
                if self._tag else self.name)

    @property
    def name(self):
        """Return the wrapped name without any tag"""
        return (self._name if self.external else
                self.c_api_settings.get_name(self._name))
