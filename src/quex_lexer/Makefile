# PURPOSE: Makefile Demo Application of Quex
#
# ABSOLUTELY NO WARRANTY
#_______________________________________________________________________________
.PHONY: clean

ifndef QUEX_PATH
    $(error The environment variable QUEX_PATH is not defined!)
endif

include $(QUEX_PATH)/quex/code_base/core.mkd

ifdef ASSERTS_ENABLED_F
	CCFLAG_ASSERTS=# By default asserts are enabled
else 
	CCFLAG_ASSERTS=-DQUEX_OPTION_ASSERTS_DISABLED
endif

# (*) COMPILER SETTINGS ________________________________________________________
#     (change COMPILER to whatever you use as compiler on the command line,
#      e.g. "make COMPILER=icpc" will use intel's c++ compiler)
COMPILER   = gcc -ggdb -pedantic -Wconversion -Wall -W -std=c99
ADD_FLAGS := -fpic

ifneq (,$(findstring sun, $(COMPILER)))
	ADD_FLAGS := +w
endif	

CC = $(COMPILER) -c       \
	 -I./ -I$(QUEX_PATH)  \
	 $(ADD_FLAGS) $(CCFLAG_ASSERTS) 
     # -DQUEX_OPTION_ASSERTS_DISABLED
     # -DQUEX_OPTION_ASSERTS_WARNING_MESSAGE_DISABLED 
     # -DQUEX_OPTION_DEBUG_SHOW
   
LD = $(COMPILER) 

# (*) RULES ____________________________________________________________________
# -- application
_lexer.so: EasyLexer.o cffi_interface.o
	$(COMPILER) -shared -o $@ EasyLexer.o cffi_interface.o

test: test.o EasyLexer.o cffi_interface.o
	$(COMPILER) -o test_lexer test.o EasyLexer.o cffi_interface.o

test.o: test.c
	$(CC) $< -o $@ 
          
# Object files:
EasyLexer.o: EasyLexer.c 
	$(CC) $< -o $@ 

cffi_interface.o: cffi_interface.c
	$(CC) $< -o $@ 

example.o: ../example.c  EasyLexer.c
	$(CC) $< -o $@ 

# Macro expansions (only for debugging):
%.E: %.cpp    
	$(CC) -E $< -o $@ 

EasyLexer.c: ada.qx $(QUEX_CORE)
	quex -i                 ada.qx  \
	     --engine           EasyLexer  \
		 --token-id-offset  0x1000     \
		 --language         C

source-package:
	quex -i                 ada.qx  \
	     --engine           EasyLexer  \
		 --token-id-offset  0x1000     \
		 --post-categorizer            \
		 --language         C          \
		 --source-package   lexer_mine 

# (*) HELPERS __________________________________________________________________
clean:	
	rm -f EasyLexer*
	rm -f *.o
	rm -f *.bak
	rm -f *.E
	rm -f lexer
