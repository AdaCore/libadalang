default_formatting = None


class Name(object):
    """
    Code generation helpers to format names with various casing conventions
    """

    def __init__(self, mixed_with_underscores):
        """
        Create a name from a string with mixed case and underscores

        For instance: C_OOP_Extension
        """
        self.base_name = mixed_with_underscores

    def __len__(self):
        return len(self.base_name)

    def __hash__(self):
        return hash(self.base_name)

    def __eq__(self, other):
        return self.base_name == other.base_name

    @property
    def camel_with_underscores(self):
        """Format to mixed case with undercore (e.g. C_OOP_Extension)"""
        return self.base_name

    @property
    def camel(self):
        """Format to camel case (e.g. COOPExtension)"""
        return self.base_name.replace('_', '')

    @property
    def lower(self):
        """Format to lower case (e.g. c_oop_extension)"""
        return self.base_name.lower()

    @property
    def upper(self):
        """Format to upper case (e.g. C_OOP_EXTENSION)"""
        return self.base_name.upper()

    def __str__(self):
        """Format to default casing convention"""
        assert default_formatting is not None
        return getattr(self, default_formatting)

    def __add__(self, other):
        return Name('{}_{}'.format(self.base_name, other.base_name))

    @classmethod
    def from_camel_with_underscores(cls, name):
        return cls(name)

    @classmethod
    def from_camel(cls, name):
        result = list(name)
        inserted_underscores = []
        for i, c in enumerate(result):
            if i <= 1:
                continue
            c_1 = result[i - 1]
            if c_1 == c_1.upper() and c == c.lower():
                inserted_underscores.append(i - 1)
        for index in reversed(inserted_underscores):
            result.insert(index, '_')
        return cls(''.join(result))

    @classmethod
    def from_lower(cls, name):
        return cls('_'.join(word.lower().capitalize()
                            for word in name.split('_')))

    @classmethod
    def from_upper(cls, name):
        return cls('_'.join(word.lower().capitalize()
                            for word in name.split('_')))


class Convention(object):
    """Guard to set a default convention"""
    def __init__(self, name):
        self.name = name
        self.old_name = None

    def __enter__(self):
        global default_formatting
        self.old_name = default_formatting
        default_formatting = self.name

    def __exit__(self, exc, exc_type, traceback):
        default_formatting = self.old_name


camel_with_underscores = Convention('camel_with_underscores')
camel = Convention('camel')
lower = Convention('lower')
upper = Convention('upper')
