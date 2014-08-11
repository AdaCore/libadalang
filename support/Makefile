ifndef QUEX_PATH
    $(error The environment variable QUEX_PATH is not defined!)
endif

COMPILER   = clang++ 
ADD_FLAGS := -fpic

ifneq (,$(findstring sun, $(COMPILER)))
	ADD_FLAGS := +w
endif

OPTS= -ggdb -pedantic -Wconversion -Wall -W -Wno-deprecated-register -std=c++11 \
	  -c -g -O0 -I$(QUEX_PATH) -Iinclude -Isrc -ferror-limit=1\
	  $(ADD_FLAGS) \
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
bin/parse_main: bin obj/parse.o obj/parse_main.o obj/quex_lexer.o obj/lexer.o
	$(COMPILER) -o bin/parse obj/parse_main.o obj/parse.o obj/quex_lexer.o obj/lexer.o -lboost_program_options

obj/%.o: src/%.cpp src/quex_lexer.c src/parse.cpp
	$(CC) -std=c++11 $< -o $@ 
          
obj/%.o: src/%.c src/quex_lexer.c src/parse.cpp
	$(CC) $< -o $@ 

# Macro expansions (only for debugging):
%.E: %.cpp    
	$(CC) -E $< -o $@ 

# (*) HELPERS __________________________________________________________________
clean:	
	rm -f obj/*
	rm -f bin/*
