--
--  Copyright (C) 2014-2025, AdaCore
--  SPDX-License-Identifier: Apache-2.0
--

--  Helper for ``Libadalang.Preprocessing``, implementing the preprocessing
--  itself, i.e. Ada source code transformation given a preprocessing
--  configuration.

with Langkit_Support.Diagnostics; use Langkit_Support.Diagnostics;

with Libadalang.Analysis;      use Libadalang.Analysis;
with Libadalang.Preprocessing; use Libadalang.Preprocessing;

private package Libadalang.PP_Impl is

   procedure Preprocess
     (Cfg         : File_Config;
      Context     : Analysis_Context;
      Input       : String;
      Contents    : out Preprocessed_Source;
      Diagnostics : in out Diagnostics_Vectors.Vector);
   --  Preprocess the ``Input`` source buffer using the given ``Cfg`` file
   --  configuration. Put the result in ``Contents`` and ``Diagnostics``.
   --  ``Context`` is used to parse preprocessing directives.

end Libadalang.PP_Impl;
