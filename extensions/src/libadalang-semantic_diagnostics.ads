--
--  Copyright (C) 2014-2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  This package provides various helpers to format the semantic diagnostics
--  produced during name resolution.

with Ada.Containers.Vectors;

with GNATCOLL.Traces;

with Langkit_Support.Text; use Langkit_Support.Text;

with Libadalang.Analysis; use Libadalang.Analysis;

package Libadalang.Semantic_Diagnostics is

   Diags_Trace : GNATCOLL.Traces.Trace_Handle := GNATCOLL.Traces.Create
     ("LIBADALANG.SEM_DIAGS", Default => GNATCOLL.Traces.From_Config);

   type Located_Message is record
      Location : Ada_Node;
      Message  : Unbounded_Text_Type;
   end record;
   --  A message associated to a specific location

   function To_Pretty_String (Msg : Located_Message) return String;
   --  Return a formatted string showing the error message at its location

   package Located_Message_Vectors is new Ada.Containers.Vectors
     (Positive, Located_Message);

   subtype Located_Message_Vector is Located_Message_Vectors.Vector;

   type Contextual_Diagnostic is record
      Error    : Located_Message;
      Contexts : Located_Message_Vector;
   end record;
   --  A diagnostic which gather the error itself, as well as a set of
   --  informative diagnostics.

   type Node_Renderer is access function
     (Node : Ada_Node) return Text_Type;
   --  A node renderer is used during the formatting of diagnostics, to pretty
   --  print holes which actuals are nodes. Diagnostic formatters typically
   --  take a node renderer as parameter so that users can customize how nodes
   --  are renderered depending on the application.

   type Diagnostic_Aggregator is access function
     (Sem_Diags   : Solver_Diagnostic_Array;
      Render_Node : Node_Renderer) return Contextual_Diagnostic;
   --  A diagnostic aggregator takes as input the array of raw name resolution
   --  diagnostics that were produced on a single xref entry point which
   --  resolution failed, and processes/formats them to turn them into concise
   --  and human-readable errors which are ready to be emitted. Aggregators
   --  may typically choose to drop information when the raw diagnostics carry
   --  too much information which may be too difficult to render nicely or
   --  too difficult to be processed by humans.

   function Build_Contextual_Diagnostics
     (Node        : Ada_Node;
      Sem_Diags   : Solver_Diagnostic_Array;
      Aggregate   : Diagnostic_Aggregator;
      Render_Node : Node_Renderer) return Contextual_Diagnostic;
   --  Simply runs the aggregator on the given list of raw diagnostics, using
   --  the given node renderer. If the array of diagnostics is empty, this
   --  returns a generic "resolution failed" diagnostic spanning the entire
   --  node.

   function Basic_Aggregator
     (Sem_Diags   : Solver_Diagnostic_Array;
      Render_Node : Node_Renderer) return Contextual_Diagnostic;
   --  This is a basic implementation of a ``Diagnostic_Aggregator``, which
   --  tries to have a simple output:
   --   * if there is a single raw diagnostic, return it after formatting it.
   --   * if there are multiple raw diagnostics, find those which correspond
   --     to multiple overload attempts and build a nice user diagnostic which
   --     shows why each candidate was rejected, for up to 3 of them.
   --   * if there are multiple such cases or multiple unrelated errors in
   --     general, return the last one of them.

   function Basic_Node_Renderer (Node : Ada_Node) return Text_Type;
   --  Pretty print the given node to basic text without any stylization. this
   --  also renders node in generic contexts, and handles special cases such
   --  as universal types, anonymous access types, etc.

end Libadalang.Semantic_Diagnostics;
