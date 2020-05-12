"""
Grab bag of python utils/examples of use of the LAL python API.
"""

import libadalang as lal


def fqdn(basic_decl, child=None):
    """
    Return the fully qualified name of basic_decl.

    :param lal.BasicDecl basic_decl: The basic declaration for which we want
        the fully qualified name.
    """

    if not basic_decl:
        return ""

    def rel_name(n):
        """
        Return the relative name of n.

        :param lal.Name n: The name.
        """
        return (n.text if n.is_a(lal.BaseId) else rel_name(n.f_suffix))

    parent_name = fqdn(basic_decl.p_semantic_parent, basic_decl)

    if child and child.is_a(lal.PackageBody):
        return parent_name
    else:
        return ".".join(
            el for el in [parent_name, rel_name(basic_decl.p_defining_name)]
            if el
        )
