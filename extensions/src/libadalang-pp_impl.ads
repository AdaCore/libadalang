------------------------------------------------------------------------------
--                                                                          --
--                                Libadalang                                --
--                                                                          --
--                     Copyright (C) 2014-2022, AdaCore                     --
--                                                                          --
-- Libadalang is free software;  you can redistribute it and/or modify  it  --
-- under terms of the GNU General Public License  as published by the Free  --
-- Software Foundation;  either version 3,  or (at your option)  any later  --
-- version.   This  software  is distributed in the hope that it  will  be  --
-- useful but  WITHOUT ANY WARRANTY;  without even the implied warranty of  --
-- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.                 --
--                                                                          --
-- As a special  exception  under  Section 7  of  GPL  version 3,  you are  --
-- granted additional  permissions described in the  GCC  Runtime  Library  --
-- Exception, version 3.1, as published by the Free Software Foundation.    --
--                                                                          --
-- You should have received a copy of the GNU General Public License and a  --
-- copy of the GCC Runtime Library Exception along with this program;  see  --
-- the files COPYING3 and COPYING.RUNTIME respectively.  If not, see        --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

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
