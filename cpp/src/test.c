#include "cffi_interface.h"
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char** argv) {

    #pragma unused (argc)
    #pragma unused (argv)
    const char* string = "procedure A (B : String; C : String);";

    Lexer* lex = make_lexer_from_file("src/example.txt", nullptr);
    Token tok;
    printf("START OFFSET : %p\n", string);
    long i = 0;

    do {
        // printf("=====================\n");
        tok = get (lex, i++);
        // printf("ID : %d\n", tok._id);
        // printf("OFFSET : %p\n", tok.text);
        // printf("STRING : %s\n", tok.text);
        // printf("=====================\n\n");
    } while (tok._id != 0);
    printf("ENDED\n");
    pause();
}
