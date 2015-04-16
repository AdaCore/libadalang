## vim: filetype=makoada

function ${accessor_name}
  (Node    : ${node_type};
   Value_P : ${field_type.c_type(capi).name}_Ptr) return int
   with Export        => True,
        Convention    => C,
        External_name => "${accessor_name}";
