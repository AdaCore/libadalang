## vim: filetype=makoada

function ${property.name} (Self : ${Self.type.name()}_Type'Class) return ${property.type.name()}
is
   ${property.vars.render()}
begin
   ${property.constructed_expr.render_pre()}
   return ${property.constructed_expr.render_expr()};
end ${property.name};
