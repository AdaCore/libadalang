pragma Warnings (Off, "referenced");
pragma Warnings (Off, "assigned but never");

with Lexer; use Lexer;
with Interfaces; use Interfaces;
with AST; use AST;
with Ada.Containers.Vectors;
with Ada.Command_Line; use Ada.Command_Line;

procedure Parse is
   Current_Pos : Long_Integer;


   ------------------------
   -- Types declarations --
   ------------------------

% for el in tdecls:
${el}

% endfor

   -------------------------------
   -- Vector types declarations --
   -------------------------------

% for vec in _self.generic_vectors:
   package ${vec}_Vectors is new Ada.Containers.Vectors
(Positive, ${vec});
   type ${vec}_Vector_Access
is access all ${vec}_Vectors.Vector;

% endfor

   ---------------------------
   -- Function declarations --
   ---------------------------

% for el in fndecls:
${el}

% endfor

   -----------------------
   -- Types definitions --
   -----------------------

% for el in tdefs:
${el}

% endfor

   ---------------------------
   -- Functions definitions --
   ---------------------------

% for el in bodies:
${el}

% endfor

begin
   if Argument_Count < 1 then
      return;
   end if;
   declare
      Lex : constant Lexer.Lexer :=
Make_Lexer_From_File (Argument (1));
      Res : ${_self.main_comb.get_type_string()};
   begin
      Res := ${_self.main_comb.gen_fn_name} (Lex, 0);
   end;
end Parse;
