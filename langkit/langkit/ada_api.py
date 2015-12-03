import common
import names


class AdaAPISettings(object):
    """Convenient container for Ada API generation settings.

    The convention is to make instances for this class available to templates
    as `adaapi`.
    """

    KEYWORDS = common.keywords['ada']

    def __init__(self, lib_name):
        self.lib_name = lib_name

    @classmethod
    def escape(cls, name, suffix):
        if isinstance(suffix, basestring):
            suffix = names.Name.from_lower(suffix)
        return (name + suffix
                if name.base_name.lower() in cls.KEYWORDS else
                name)

    def get_enum_alternative(self, type_name, alt_name, suffix):
        """
        Return a name that is suitable for code generation for the `alt_name`
        alternative in the `type_name` enumeration type. `suffix` should be
        used to post-process names that are invalid enumerators.
        """
        return self.escape(alt_name, suffix)
