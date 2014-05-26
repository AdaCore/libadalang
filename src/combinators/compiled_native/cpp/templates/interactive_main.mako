#include <boost/program_options/cmdline.hpp>
#include <boost/program_options/options_description.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/program_options/parsers.hpp>
#include <cstdlib>
#include "${header_name}"

namespace po = boost::program_options;
using namespace std;

void exit_and_print_usage() {
    printf("Usage : parse <rule_name> <input_string>\n");
    exit(2);
}

int main (int argc, char** argv) {

    po::options_description desc("Allowed options");
    desc.add_options()
        ("help", "produce help message")
        ("silent,s", po::value<bool>()->default_value(false)
                                       ->implicit_value(true)
                                       ->zero_tokens(), 
         "print the representation of the resulting tree")
        ("file,f", po::value<bool>()->default_value(false)
                                    ->implicit_value(true)
                                    ->zero_tokens(), 
         "trigger the file input mode")
        ("rule-name,r", po::value<string>()->default_value("compilation_unit"), "rule name to parse")
        ("input", po::value<string>(), "input file or string");

    po::variables_map vm;
    po::positional_options_description p;
    p.add("input", -1);
    po::store(po::command_line_parser(argc, argv).
              options(desc).positional(p).run(), vm);
    po::notify(vm);


    if (vm.count("help") or !(vm.count("input"))) {
        cout << "Not enough arguments !\n";
        cout << desc << "\n";
        return 1;
    }

    Lexer* lex;
    string input = vm["input"].as<string>();
    string rule_name = vm["rule-name"].as<string>();

    if (vm["file"].as<bool>()) {
        cout << "file name : " << input << endl;
        lex = make_lexer_from_file(input.c_str(), nullptr);
    } else {
         lex = make_lexer_from_string(input.c_str(), input.length());
    }

% for i, (rule_name, combinator) in enumerate(_self.rules_to_fn_names.items()):
    ${"if" if i == 0 else "else if"} (rule_name == ${c_repr(rule_name)}) {
        auto res = ${combinator.gen_fn_name}(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n", max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>())
                printf("%s\n", (${combinator.emit_repr("res")}).c_str());
        }
    }
% endfor
    else {
        printf("Unknown rule : %s\n", rule_name.c_str());
    }
    print_diagnostics();
    return 0;
}
