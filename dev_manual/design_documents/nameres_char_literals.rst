Name Resolution of Char Literals
################################

Current problem
---------------

Currently, a char literal that is not user defined will resolve to `None`.
This is because we cannot add all existing char literals in the lexical
environments due to their number (think about `Wide_Wide_Character`).

Proposed Solution
-----------------

An idea would be to synthesize the literal on-demand: when constructing the
xref equation of a character literal, we could do something of the like
(pseudo code):

.. code-block:: python

    synth_char = SynthesizedCharEnumLit.new(
        value=c.denoted_value,
        type=c.std_char_type
    )
    synth_wide_char = SynthesizedCharEnumLit.new(
        value=c.denoted_value,
        type=c.std_wide_char_type
    )
    synth_wide_wide_char = SynthesizedCharEnumLit.new(
        value=c.denoted_value,
        type=c.std_wide_wide_char_type
    )
    ...
    return Or(
        # Current logic
        c.base_id_xref_equation,

        # Added logic
        And(
            Or(Bind(c.ref_var, synth_char),
               Bind(c.ref_var, synth_wide_char),
               Bind(c.ref_var, synth_wide_wide_char)),
            Bind(c.type_var, c.ref_var, conv_prop=BasicDecl.expr_type)
        ),
    )

