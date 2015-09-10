## vim: filetype=makopython


class ${cls.name().camel}(${parent_cls.name().camel}):
    # TODO: document this class and its methods

    _field_names = ${parent_cls.name().camel}._field_names + (
        % for primitive in primitives:
        "${primitive.field.name.lower}",
        % endfor
    )

    % if not cls.abstract:
    _kind_name = ${repr(cls.name().camel)}
    % endif

    % for primitive in primitives:
    @property
    def f_${primitive.field.name.lower}(self):
        result = ${primitive.field.type.py_type(pyapi).name_low}()
        assert _${primitive.name.lower}(self._c_value, ctypes.byref(result))

        ## Depending on the type of the field, we need to convert the value to
        ## the most appropriate Python type.
        % if is_ast_node(primitive.field.type):
        return _wrap_astnode(result)
        % elif is_sloc_range(primitive.field.type):
        return _wrap_sloc_range(result)
        % elif is_token_type(primitive.field.type):
        return Token(result)
        % elif is_bool(primitive.field.type):
        return bool(result.value)
        % elif is_enum(primitive.field.type):
        return ${primitive.field.type.c_type(capi).name}_to_str[result.value]
        % elif is_long(primitive.field.type):
        return result.value
        % else:
        <% raise Exception("Unhandled field type in the python binding: {}".format(primitive.field.type)) %>
        % endif
    % endfor
