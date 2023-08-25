import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.adb")

for token in u.iter_tokens():
    is_83_token = u.root.p_is_keyword(token, "ada_83")
    is_12_token = u.root.p_is_keyword(token, "ada_2012")
    if is_12_token and not is_83_token:
        print(f"'{token.text}' is a keyword in Ada 2012 but not in Ada 83")

# Try checking a token with an invalid Ada version
try:
    u.root.p_is_keyword(u.first_token, "ada_2013")
except Exception as e:
    print(e)
