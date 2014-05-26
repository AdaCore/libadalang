#! /bin/bash
python -c "print '{0} %'.format(int(($(ag "^#.*?=" doc/ada_syntax.bnf| wc -l) / $(ag "^.*?=" doc/ada_syntax.bnf| wc -l).0) * 100))"
