Proper handling of function attributes
######################################

Current problem
---------------

In Ada, a lot of attributes are defined as referring to built-in functions
(every attribute that "takes arguments", is actually a function).

Libadalang's parser is wrong in that regard. Historically,  it parses
``AttributeRef`` with arguments as part of the AttributeRef node, rather than
as a ``CallExpr`` where the callee is the ``AttributeRef`` node.

Recently, we made it so that it will do that only for the cases that we already
implemented using the old tree. Attributes will be parsed correctly in the
general case:

.. code:: sh

    %  parse -r expr "A'B(12, 15)"
    CallExpr[1:1-1:12]
    |name:
    |  AttributeRef[1:1-1:4]
    |  |prefix:
    |  |  Id[1:1-1:2]: A
    |  |attribute:
    |  |  Id[1:3-1:4]: B
    |  |args: <null>
    |suffix:
    |  AssocList[1:5-1:11]
    |  |  ParamAssoc[1:5-1:7]
    |  |  |designator: <null>
    |  |  |r_expr:
    |  |  |  Int[1:5-1:7]: 12
    |  |  ParamAssoc[1:9-1:11]
    |  |  |designator: <null>
    |  |  |r_expr:
    |  |  |  Int[1:9-1:11]: 15

But will be parsed incorrectly (legacy) for all attributes that should be
functions but are not handled as such by LAL:

.. code:: sh

    % parse -r expr "A'First(12)"
    AttributeRef[1:1-1:12]
    |prefix:
    |  Id[1:1-1:2]: A
    |attribute:
    |  Id[1:3-1:8]: First
    |args:
    |  AssocList[1:9-1:11]
    |  |  ParamAssoc[1:9-1:11]
    |  |  |designator: <null>
    |  |  |r_expr:
    |  |  |  Int[1:9-1:11]: 12

Those legacy attributes are handled, but the way they're handled is incorrect
and will fail when the fact that the attribute denotes functions is inevitable,
such as in the following example:

.. code:: ada

    procedure Foo is
        function Image (A : Integer) return String renames Integer'Image;
    begin
        null;
    end Foo;


Proposed Solution
-----------------

To solve this, we need to synthesize the subprogram nodes corresponding to each
attribute. If we have a unified mechanism to easily synthesize subprograms with
given profiles, we can potentially also dramatically simplify the name
resolution of said attributes, because for the moment resolution of those is
hard-coded.
