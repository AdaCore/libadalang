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
COMPILER   = clang++ 
ADD_FLAGS := -fpic

ifneq (,$(findstring sun, $(COMPILER)))
	ADD_FLAGS := +w
endif	

OPTS= -ggdb -pedantic -Wconversion -Wall -W -Wno-deprecated-register -std=c++11 \
	  -c -g -O0 -I./ -I$(QUEX_PATH)  -ferror-limit=1\
	  $(ADD_FLAGS) $(CCFLAG_ASSERTS) \
      -DQUEX_OPTION_ASSERTS_DISABLED \
      -DQUEX_OPTION_ASSERTS_WARNING_MESSAGE_DISABLED \
	 # -DDEBUG_MODE
     # -DQUEX_OPTION_DEBUG_SHOW
	 #
CC = $(COMPILER) $(OPTS)   

LD = $(COMPILER) 

# (*) RULES ____________________________________________________________________
# -- application
#
bin/parse_main: obj/parse.o obj/parse_main.o obj/EasyLexer.o obj/cffi_interface.o
	$(COMPILER) -o bin/parse obj/parse_main.o obj/parse.o obj/EasyLexer.o obj/cffi_interface.o -lboost_program_options

bin/tests: obj/parse.o obj/parsetests.o obj/EasyLexer.o obj/cffi_interface.o
	$(COMPILER) -o bin/tests obj/parsetests.o obj/parse.o obj/EasyLexer.o obj/cffi_interface.o

_lexer.so: obj/EasyLexer.o obj/cffi_interface.o
	$(COMPILER) -shared -o obj/$@ obj/EasyLexer.o obj/cffi_interface.o

bin/test: obj/test.o obj/EasyLexer.o obj/cffi_interface.o
	$(CC) -o bin/test_lexer obj/test.o obj/EasyLexer.o obj/cffi_interface.o

obj/parse.o: src/parse.cpp
	$(CC) -std=c++11 $< -o $@ 

obj/parse_main.o: src/parse_main.cpp
	$(CC) -std=c++11 $< -o $@ 

obj/parsetests.o: src/parsetests.cpp
	$(CC) -std=c++11 $< -o $@ 

obj/test.o: src/test.c
	$(CC) $< -o $@ 
          
# Object files:
obj/EasyLexer.o: src/EasyLexer.c 
	$(CC) $< -o $@ 

obj/cffi_interface.o: src/cffi_interface.cpp
	$(CC) $< -o $@ 

# Macro expansions (only for debugging):
%.E: %.cpp    
	$(CC) -E $< -o $@ 

src/EasyLexer.c: src/ada.qx $(QUEX_CORE)
	cd src && quex -i                 ada.qx  \
	     --engine           EasyLexer  \
		 --token-id-offset  0x1000     \
		 --language         C \
		 --no-mode-transition-check \
		 --single-mode-analyzer \
		 --token-memory-management-by-user \
		 --token-policy single
	  
src/parse.cpp: src/EasyLexer.c
	bash -c "source ~/envs/libadalang_env/bin/activate && python build.py"
	clang-format-3.4 -i src/parse.cpp
	clang-format-3.4 -i src/parse.hpp
	clang-format-3.4 -i src/parse_main.cpp

source-package:
	quex -i                 ada.qx  \
	     --engine           EasyLexer  \
		 --token-id-offset  0x1000     \
		 --post-categorizer            \
		 --language         C          \
		 --source-package   lexer_mine 

# (*) HELPERS __________________________________________________________________
clean:	
	rm -f src/EasyLexer*
	rm -f src/parse.cpp
	rm -f obj/*.o
	rm -f *.bak
	rm -f *.E
	rm -f bin/*
