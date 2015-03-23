## vim: filetype=makopython


class ${cls.name()}(${parent_cls.name()}):
    # TODO: document this class and its methods

    % if not primitives:
    pass
    % endif

    % for primitive in primitives:
    @property
    def ${primitive.field.name}(self):
        result = ${primitive.field_type.py_type(pyapi).name_low}()
        assert _${primitive.name}(self._c_value, ctypes.byref(result))

        ## Depending on the type of the field, we need to convert the value to
        ## the most appropriate Python type.
        % if is_ast_node(primitive.field_type):
        return _wrap_astnode(result)
        % elif is_sloc_range(primitive.field_type):
        return _wrap_sloc_range(result)
        % elif is_bool(primitive.field_type):
        return bool(result.value)
        % elif is_enum(primitive.field_type):
        return ${primitive.field_type.c_type(capi).simple}_to_str[result.value]
        % elif is_long(primitive.field_type):
        return result.value
        % else:
        == Purposedly wrong generated code ==
        There's a type we don't know how to wrap in Python
        % endif
    % endfor
