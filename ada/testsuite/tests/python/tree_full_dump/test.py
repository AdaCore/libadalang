from libadalang import AnalysisContext, ASTNode, Token, ASTList


ctx = AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')
unit.root.dump()
print 'Done'
