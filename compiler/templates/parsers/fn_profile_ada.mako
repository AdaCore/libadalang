## vim: filetype=makoada

function ${_self.gen_fn_name} (Parser : in out Parser_Type;
                               Pos    : Integer)
                               return ${decl_type(_self.get_type())};
