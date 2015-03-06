ifndef QUEX_PATH
    $(error The environment variable QUEX_PATH is not defined!)
endif

# Supported build modes: prod dev
BUILD_MODE=dev

CXX = clang++
CC = clang
CFLAGS := -fpic -Iinclude -Isrc -I$(QUEX_PATH) \
	-Wno-deprecated-register \
        -DQUEX_OPTION_ASSERTS_DISABLED \
        -DQUEX_OPTION_ASSERTS_WARNING_MESSAGE_DISABLED
CXXFLAGS = $(CFLAGS) -std=c++11
LDFLAGS := -lboost_program_options

ifeq ($(BUILD_MODE),dev)
    # Make programs debuggable
    CFLAGS += -O0 -g
    # Enable all warnings and force developpers to fix them.
    CFLAGS += -Wall -W -pedantic \
    # -DDEBUG_MODE \
    # -DQUEX_OPTION_DEBUG_SHOW
else
    CFLAGS += -Ofast -g0
endif

ifneq (,$(findstring sun, $(CXX)))
    CFLAGS += +w
endif

ifdef BOOST_LIBRARY_PATH
    LDFLAGS += -L$(BOOST_LIBRARY_PATH)
endif

LIB_OBJECTS = obj/parse.o obj/quex_lexer.o obj/lexer.o obj/ast.o obj/token.o obj/symboltable.o obj/tokendatahandler.o

# (*) RULES ____________________________________________________________________
# -- application
#
bin/parse: obj/parse_main.o $(LIB_OBJECTS)
	$(CXX) -o $@ obj/parse_main.o $(LIB_OBJECTS) $(LDFLAGS)

obj/%.o: src/%.cpp src/quex_lexer.c src/parse.cpp
	$(CXX) -c $(CXXFLAGS) -o $@ $<

obj/%.o: src/%.c src/quex_lexer.c src/parse.cpp
	$(CC) -c $(CFLAGS) -o $@ $<

# Macro expansions (only for debugging):
%.E: %.cpp
	$(CXX) $(CXXFLAGS) -E $< -o $@

# (*) HELPERS __________________________________________________________________
clean:
	rm -f obj/*
	rm -f lib/*
	rm -f bin/*
