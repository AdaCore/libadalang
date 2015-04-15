## vim: filetype=makopython


class ${cls.name()}(${parent_cls.name()}):
    # TODO: document this class and its methods

    _field_names = ${parent_cls.name()}._field_names + (
        % for primitive in primitives:
        "${primitive.field.name.lower}",
        % endfor
    )

    % if not primitives:
    pass
    % endif

    % for primitive in primitives:
    @property
    def f_${primitive.field.name.lower}(self):
        result = ${primitive.field_type.py_type(pyapi).name_low}()
        assert _${primitive.name.lower}(self._c_value, ctypes.byref(result))

        ## Depending on the type of the field, we need to convert the value to
        ## the most appropriate Python type.
        % if is_ast_node(primitive.field_type):
        return _wrap_astnode(result)
        % elif is_sloc_range(primitive.field_type):
        return _wrap_sloc_range(result)
        % elif is_token_type(primitive.field_type):
        return Token(result)
        % elif is_bool(primitive.field_type):
        return bool(result.value)
        % elif is_enum(primitive.field_type):
        return ${primitive.field_type.c_type(capi).name}_to_str[result.value]
        % elif is_long(primitive.field_type):
        return result.value
        % else:
        <% raise Exception("Unhandled field type in the python binding: {}".format(primitive.field_type)) %>
        % endif
    % endfor
