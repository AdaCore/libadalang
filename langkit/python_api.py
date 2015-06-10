class PythonAPIType(object):
    """
    Python API generation helper: encapsulate the logic of Python types names
    """

    def __init__(self, python_api_settings,
                 name_low, external_low):
        """Create a stub for a Python type

        python_api_settings: A python_api.PythonAPISettings instance.

        name_low: Name for the type used in the low-level binding.

        external_low: Whether this type is declared in the "ctypes" standard
        package. If not, we consider that this type is declared in the
        generated Python module.
        """
        self.python_api_settings = python_api_settings

        self._name_low = name_low
        self._external_low = external_low

    @property
    def name_low(self):
        """
        Return the name to be used for the low-level Python API for this type
        """
        return ('ctypes.{}'.format(self._name_low)
                if self._external_low else '_{}'.format(self._name_low))


class PythonAPISettings(object):
    """Container for Python API generation settings"""

    name = 'python'

    def __init__(self, module_name, c_api_settings):
        self.c_api_settings = c_api_settings
        self.module_name = module_name

    def get_enum_alternative(self, type_name, alt_name, suffix):
        """
        Return a name that is suitable for code generation for the `alt_name`
        alternative in the `type_name` enumeration type. `suffix` should be
        used to post-process names that are invalid enumerators.
        """
        return alt_name.upper
