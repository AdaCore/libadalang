import libadalang


source_template = '''
procedure Foo {} is
begin
   null;
end Foo;
'''


ctx = libadalang.AnalysisContext('utf-8')

for i, params in enumerate([
    '',
    '(A : Integer)',
    '(A, B : Integer; C : Boolean)',
]):
    if i > 0:
        print('')

    title = 'Params: {}'.format(params or '<empty>')
    print(title)
    print('=' * len(title))

    unit = ctx.get_from_buffer('foo.adb', source_template.format(params))
    subp_spec = unit.root.f_body.f_item.f_subp_spec
    for i, (name, type_expr) in enumerate(subp_spec.p_typed_param_list):
        assert isinstance(name, libadalang.Identifier)

        if i > 0:
            print('')
        print('{}:'.format(name.f_tok.text))
        type_expr.dump()
