from libadalang import AnalysisContext, ASTNode, Token, ASTList


ctx = AnalysisContext()
unit = ctx.get_from_file('foo.adb')
unit.root.dump()
print 'Done'
