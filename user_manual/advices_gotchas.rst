Advice & gotchas
================

This section of the manual regroups together guidance that we consider vital to
develop with Libadalang, but unlike the tutorial, this is meant as a grab bag
that will be useful to new developers as well as experienced Libadalang
developers. We advise people using Libadalang to come back to this section from
time to time to see if there are new advices & gotchas.

Ada parsing: Identifiers starting with a ``$``
----------------------------------------------

Historically, Libadalang has parsed identifiers starting with a dollar (``$``)
as regular identifiers, even though it is not valid Ada. The aim for that was
to provide minimal support for GNATprep style preprocessing.

However, nowadays, full support for preprocessing has been implemented in
Libadalang, via the :ada:ref:`Langkit_Support.File_Readers` and
:ada:ref:`Libadalang.Preprocessing` APIs. If you use those,
identifiers starting with ``$`` will be interpreted as preprocessor
identifiers. Else, they will just be parsed normally.

Despite Libadalang having a real preprocessor, this behavior has been kept for
backward compatibility with some LAL projects, notably `Ada-Renaissance
<https://github.com/TNO/Renaissance-Ada>`_, and will be preserved onwards.

Ada API: Up & down casting ``Ada_Node`` instances
-------------------------------------------------

In :ada:ref:`Libadalang.Analysis`, a hierarchy of tagged types is declared,
rooting in the  :ada:ref:`Libadalang.Analysis.Ada_Node` type. It is important
to understand that those node types are equivalent to a class wide access to a
node.

They're in reality kind of a fat pointer, containing a pointer to the syntactic
node, and some more context, like generic instantiation information.

A consequence of this design choice is that you **cannot** use the regular Ada
conversion functions to up/down cast node types:

.. code-block:: ada
   :linenos:

    A : Ada_Node := Unit.Root;

    --  This code is invalid and will raise an error
    B : Compilation_Unit := Compilation_Unit (A);

    --  This code is valid and will work as you expect
    C : Compilation_Unit := A.As_Compilation_Unit;

Ada API: Equality between ``Ada_Node`` and derived types
--------------------------------------------------------

Summary
^^^^^^^

As said previously, nodes types are like fat pointers to a node, something like
``(pointer_to_node, more_entity_info)``.

For complex reasons we're exposing an ``"="`` operator on classwide nodes, and
that's the one you'll use by default, but when instantiating generic containers
with nodes, if you need an equivalent function, you cannot use ``"="``, because
that will use the default equality operator. Instead, you need to use the
``Ada_Node.Equals`` function:

.. code-block:: ada

   with Libadalang.Analysis; use Libadalang.Analysis;

   ...

   package Node_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => Ada_Node,
      Hash                => Hash,
      Equivalent_Elements => Equals,
      "="                 => Equals);


.. _standard-unit:

Standard unit peculiarities
---------------------------

In Ada, the ``Standard`` package is special: is acting such as a built-in
compilation unit and is the root of all other units: these are technically
child units of ``Standard``. It is the package that defines "native"
declarations such as ``Boolean``, ``Integer`` but also ``Character``,
``Wide_Wide_String`` and the ``Constraint_Error`` exception.

Libadalang implements this package as a special analysis unit, heavily inspired
from GNAT's own implementation of this package (see GNAT's ``-gnatS`` flag):

.. code-block:: ada

    package Standard is

      pragma Pure (Standard);

      type Boolean is (False, True);

      type Integer is range -(2 ** 31) .. +(2 ** 31 - 1);
      subtype Natural  is Integer range 0 .. +(2 ** 31 - 1);
      subtype Positive is Integer range 1 .. +(2 ** 31 - 1);
      type Short_Short_Integer is range -(2 ** 7) .. +(2 ** 7 - 1);
      type Short_Integer       is range -(2 ** 15) .. +(2 ** 15 - 1);
      type Long_Integer        is range -(2 ** 31) .. +(2 ** 31 - 1);
      type Long_Long_Integer   is range -(2 ** 63) .. +(2 ** 63 - 1);

      type Short_Float     is digits 6
        range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;
      type Float           is digits 6
        range -16#0.FFFF_FF#E+32 .. 16#0.FFFF_FF#E+32;
      type Long_Float      is digits 15
        range -16#0.FFFF_FFFF_FFFF_F8#E+256 .. 16#0.FFFF_FFFF_FFFF_F8#E+256;
      type Long_Long_Float is digits 18
        range -16#0.FFFF_FFFF_FFFF_FFFF#E+4096 ..  16#0.FFFF_FFFF_FFFF_FFFF#E+4096;

      type Character is ('A');
      type Wide_Character is ('A');
      type Wide_Wide_Character is ('A');

      package ASCII is
         NUL   : constant Character := Character'Val (16#00#);
         SOH   : constant Character := Character'Val (16#01#);
         STX   : constant Character := Character'Val (16#02#);
         ETX   : constant Character := Character'Val (16#03#);
         EOT   : constant Character := Character'Val (16#04#);
         ENQ   : constant Character := Character'Val (16#05#);
         ACK   : constant Character := Character'Val (16#06#);
         BEL   : constant Character := Character'Val (16#07#);
         BS    : constant Character := Character'Val (16#08#);
         HT    : constant Character := Character'Val (16#09#);
         LF    : constant Character := Character'Val (16#0A#);
         VT    : constant Character := Character'Val (16#0B#);
         FF    : constant Character := Character'Val (16#0C#);
         CR    : constant Character := Character'Val (16#0D#);
         SO    : constant Character := Character'Val (16#0E#);
         SI    : constant Character := Character'Val (16#0F#);
         DLE   : constant Character := Character'Val (16#10#);
         DC1   : constant Character := Character'Val (16#11#);
         DC2   : constant Character := Character'Val (16#12#);
         DC3   : constant Character := Character'Val (16#13#);
         DC4   : constant Character := Character'Val (16#14#);
         NAK   : constant Character := Character'Val (16#15#);
         SYN   : constant Character := Character'Val (16#16#);
         ETB   : constant Character := Character'Val (16#17#);
         CAN   : constant Character := Character'Val (16#18#);
         EM    : constant Character := Character'Val (16#19#);
         SUB   : constant Character := Character'Val (16#1A#);
         ESC   : constant Character := Character'Val (16#1B#);
         FS    : constant Character := Character'Val (16#1C#);
         GS    : constant Character := Character'Val (16#1D#);
         RS    : constant Character := Character'Val (16#1E#);
         US    : constant Character := Character'Val (16#1F#);
         DEL   : constant Character := Character'Val (16#7F#);
         Exclam     : constant Character := '!';
         Quotation  : constant Character := '""';
         Sharp      : constant Character := '#';
         Dollar     : constant Character := '$';
         Percent    : constant Character := '%';
         Ampersand  : constant Character := '&';
         Colon      : constant Character := ':';
         Semicolon  : constant Character := ';';
         Query      : constant Character := '?';
         At_Sign    : constant Character := '@';
         L_Bracket  : constant Character := '[';
         Back_Slash : constant Character := '\';
         R_Bracket  : constant Character := ']';
         Circumflex : constant Character := '^';
         Underline  : constant Character := '_';
         Grave      : constant Character := '`';
         L_Brace    : constant Character := '{';
         Bar        : constant Character := '|';
         R_Brace    : constant Character := '}';
         Tilde      : constant Character := '~';
         LC_A : constant Character := 'a';
         LC_B : constant Character := 'b';
         LC_C : constant Character := 'c';
         LC_D : constant Character := 'd';
         LC_E : constant Character := 'e';
         LC_F : constant Character := 'f';
         LC_G : constant Character := 'g';
         LC_H : constant Character := 'h';
         LC_I : constant Character := 'i';
         LC_J : constant Character := 'j';
         LC_K : constant Character := 'k';
         LC_L : constant Character := 'l';
         LC_M : constant Character := 'm';
         LC_N : constant Character := 'n';
         LC_O : constant Character := 'o';
         LC_P : constant Character := 'p';
         LC_Q : constant Character := 'q';
         LC_R : constant Character := 'r';
         LC_S : constant Character := 's';
         LC_T : constant Character := 't';
         LC_U : constant Character := 'u';
         LC_V : constant Character := 'v';
         LC_W : constant Character := 'w';
         LC_X : constant Character := 'x';
         LC_Y : constant Character := 'y';
         LC_Z : constant Character := 'z';
      end ASCII;

      type String is array (Positive range <>) of Character;
      pragma Pack (String);

      type Wide_String is array (Positive range <>) of Wide_Character;
      pragma Pack (Wide_String);

      type Wide_Wide_String is array (Positive range <>) of Wide_Wide_Character;

      type Duration is delta 0.000000001
        range -((2 ** 63 - 1) * 0.000000001) ..
              +((2 ** 63 - 1) * 0.000000001);
      for Duration'Small use 0.000000001;

      Constraint_Error : exception;
      Program_Error    : exception;
      Storage_Error    : exception;
      Tasking_Error    : exception;

      type Universal_Int_Type_ is range -1 .. 1;
      type Universal_Real_Type_ is digits 16;
    end Standard;

Warning: this is a stub, so don't rely on the implementation using this
specific source as it could change in future versions of Libadalang. Also be
aware that Libadalang will use these definitions on all platforms and for all
targets: do not rely on these definitions to compute memory representation of
standard entities.

Note that the ``Character``, ``Wide_Character`` and ``Wide_Wide_Character``
types are represented with partial definitions:

.. code-block:: ada

   type Character is ('A');
   type Wide_Character is ('A');
   type Wide_Wide_Character is ('A');

Defining all values for each is not realistic, as for instance
``Wide_Wide_Character`` has 4 billion values: it is not reasonable to allocate
memory for all of them. Nevertheless, undefined characters are properly
supported and synthesized on-demand, which means that any
characters from any sets can be properly handled by Libadalang.
