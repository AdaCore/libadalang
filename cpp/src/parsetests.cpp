#include "parse.hpp"
#define red   "\033[1;31m"        /* 0 -> normal ;  31 -> red */
#define cyan  "\033[1;36m"        /* 1 -> bold ;  36 -> cyan */
#define green "\033[1;32m"        /* 4 -> underline ;  32 -> green */
#define blue  "\033[0;34m"        /* 9 -> strike ;  34 -> blue */
 
#define black  "\033[0;30m"
#define brown  "\033[0;33m"
#define magenta  "\033[0;35m"
#define gray  "\033[0;37m"
 
#define none   "\033[0m"        /* to flush the previous property */











void SubprogramSpec_test_1 () {
    Lexer* lex = make_lexer_from_string("procedure a (b : in out integer; c, d, e, f, g, h, i : access string);", 70);
    auto res = subprogram_spec_transform_parse_2 (lex, 0);
    auto repr = res->repr();
    printf("%sResult for test SubprogramSpec_test_1 : %s%s\n", blue, none, repr.c_str());

    if (repr == "SubprogramSpec(QualifiedName([SingleTokNode(Id(a))]), [ParameterProfile([SingleTokNode(Id(b))], (True, True), TypeExpression(False, TypeRef(SingleTokNode(Id(integer))))), ParameterProfile([SingleTokNode(Id(c)), SingleTokNode(Id(d)), SingleTokNode(Id(e)), SingleTokNode(Id(f)), SingleTokNode(Id(g)), SingleTokNode(Id(h)), SingleTokNode(Id(i))], (False, False), TypeExpression(False, TypeAccessExpression(SingleTokNode(Id(string)))))], None)") {
        printf("%sTest passed%s\n", green, none);
    } else {
        printf("%sTest failed!%s\n", red, none);
    }
}











































int main(int argc, char** argv) {
    SubprogramSpec_test_1();
}
