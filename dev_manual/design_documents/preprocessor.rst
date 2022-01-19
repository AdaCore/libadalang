Preprocessing API
=================

LAL ticket: V117-037


Problem
-------

Multiple Libadalang users need to run analysis on Ada sources using GNATprep
directives, i.e. on sources that need to be `preprocessed
<https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/the_gnat_compilation_model.html#preprocessing-with-gnatprep>`_.
There is no preprocessing support built-in Libadalang as of today, so
Libadalang users must run GNATprep themselves on the sources to analyze before
passing them to Libadalang, which is highly inconvenient.


Preliminary work
----------------

With similar needs, GNAT Studio developers have opened U310-050 to have an API
in Libadalang (file readers) which can be used to transform the content of
source files before handing it to Libadalang, which allowed them in practice to
implement a partial preprocessing pass: taking the content of ``#if`` blocks,
removing the content of ``#elsif`` and ``#else`` blocks, also not expanding
symbols.

CodePeer developpers need a GNATprep-conformant preprocessing pass, however, so
this document describes how to provide a file reader to achieve this.


Suggested API
-------------

In order to match the features in GNAT and GNATprep, we need to allow users to
provide lists of symbol/value couples that can vary from one source file to
another, and similarly to tune the preprocessing options depending on the
source file.

Note that just providing a file reader that can perform the preprocessing is
not enough: the file reader interface does not compose, as file readers read
their input from a file and return a decoded buffer, so one cannot plug a
preprocessing file reader behind another file reader.

For this reason, the suggested API first provides an independent preprocessor,
working on byte buffers, and a file reader constructor (implemented on top of
this independent preprocessor API).

.. code-block:: ada

   with Ada.Containers.Hashed_Maps;
   with Ada.Containers.Vectors;

   with Langkit_Support.File_Readers; use Langkit_Support.File_Readers;
   with Langkit_Support.Text;         use Langkit_Support.Text;

   package Libadalang.Preprocessing is

      --  The GNAT preprocessor operates on byte streams: dealing
      --  with non-ASCII symbols/values requires going through encodings such
      --  as ISO-8859-1, i.e. which have a bijection between bytes and
      --  codepoints covering all bytes.

      type Value_Kind is (Empty, String_Literal, Symbol);
      type Value_Type (Kind : Value_Kind := Empty) is record
         case Kind is
            when Empty          => null;
            when String_Literal => String_Value : Unbounded_String;
            when Symbol         => Symbol_Value : Unbounded_String;
         end case;
      end record;

      type Symbol_Assoc is record
         Symbol : Unbounded_String;
         Value  : Value_Type;
      end record;

      package Symbol_Assoc_Vectors is new Ada.Containers.Vectors
        (Positive, Symbol_Assoc);

      function Parse_Definition_File
        (Filename : String) return Symbol_Assoc_Vectors.Vector;

      type File_Config is record
         Assocs : Symbol_Assoc_Vectors.Vector;
         --  Symbol/value associations for this file

         Delete_Empty_Lines : Boolean;
         --  Whether to remove preprocessor lines and lines deleted by the
         --  preprocessing from the result (GNATprep's ``-b`` option). They are
         --  replaced by blank lines otherwise (``-c``).

         Undefined_Is_False : Boolean;
         --  Whether to treat undefined symbols as False in the context of a
         --  preprocessor test (see GNATprep's ``-u`` option).
      end record;

      Empty_File_Config : constant File_Config :=
        (Symbol_Assoc_Vectors.Empty_Vector, False, False);

      package File_Config_Maps is new Ada.Containers.Hashed_Maps
        (Unbounded_String, File_Config);

      type Preprocessor_Data is private;
      --  File-specific Symbol/value associations and options to run the
      --  preprocessor.

      function Parse_Preprocessor_Data_File
        (Filename : String) return Preprocessor_Data;
      --  Parse the preprocessor data file at ``Filename`` and return the
      --  corresponding data.
      --
      --  See GNATprep's documentation for a description of the preprocessor
      --  data file format.

      function Create_Preprocessor_Data
        (File_Configs   : File_Config_Maps.Map;
         Default_Config : File_Config := Empty_File_Config)
         return Preprocessor_Data;
      --  Create preprocessor data using the given file-specific configurations
      --  and the given default configuration (for other files).

      procedure Preprocess
        (Data            : Preprocessor_Data;
         Filename, Input : String) return String_Access;
      --  Preprocess the ``Input`` source buffer according to the corresponding
      --  source filename ``Filename`` and the given preprocesor data. Return a
      --  newly allocated string containing the preprocessed source.

      function Create_Preprocessor
        (File_Configs   : File_Config_Maps.Map;
         Default_Config : File_Config := Empty_File_Config)
         return File_Reader_Reference;
      --  Like ``Create_Preprocessor_Data``, but return a file reader
      --  implementing the preprocessing instead.

      function Create_Preprocessor_From_File
        (Filename : String) return File_Reader_Reference;
      --  Like ``Parse_Preprocessor_Data_File``, but return a file reader
      --  implementing the preprocessing instead.

   private
      --  ...
   end Libadalang.Preprocessing;


Implementation
--------------

Preprocessor data file and preprocessor files are not valid Ada sources, but
they share lexical elements with them (identifiers, string literals, comments,
etc.): it looks reasonable to use an Ada lexer to tokenize them, and from there
performing dedicated parsing.

In addition, the preprocessor needs to be aware of the Ada source code it is
processing: it must not perform symbol substitution if the symbols appear in
comments or in string literals. For instance, given the ``X := Foo``
definition:

.. code-block:: text

   $X := "$X";  --  $X

must be expanded to:

.. code-block:: ada

   Foo := "$X";  --  $X

For this reason, it will be convenient to make the preprocessor first tokenize
the sources to transform using an Ada lexer, and to extract preprocessing
directives and symbol references from there. Note that while this is possible
thanks to Libadalang's existing support for preprocessing constructs such as
``#if`` or ``$X``, running Libadalang's parser on the preprocessed sources is
not an option (at least as of today) as preprocessing directives are tokenized
as trivia, and are thus ignored by the Langkit-generated parser.

However, given the `very light grammar
<https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn/the_gnat_compilation_model.html#form-of-input-text-for-gnatprep>`_
needed to parse conditions, hand writing a parser running on top of the
previous tokenized source instead of the full Libadalang parser (which will run
the lexer one more time) looks reasonable.

The preprocessing logic to build on top of this looks trivial: replace
``#if``-disabled lines of code with blank lines (or remove them altogether
depending on the corresponding otions), replace references to defined symbols
with associated values and return back the resulting source buffer.


Limitations
-----------

Thanks to the blank line replacement strategy, line numbers between the
original (unexpanded) source and the preprocessed source can stay the same.
However column numbers will change. For instance, given the ``X := Integer``
substitution:

.. code-block:: text

   package Foo is
      I : $X := "foo";
   end Foo;

And the resulting expanded code:

.. code-block:: ada

   package Foo is
      I : Integer := "foo";
   end Foo;

The location of the ``"foo"`` string literal, originally at line 2, column 14,
becomes line 2, column 19 after preprocessing. This limitation should however
be acceptable, and is probably the best we should do anyway, as even GNAT's
integrated preprocessing has this behavior:

.. code-block:: shell

   $ gcc -c foo.ads -gnateDX=Integer
   foo.ads:2:19: error: expected type "Standard.Integer"
   foo.ads:2:19: error: found a string type
