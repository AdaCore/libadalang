## vim: filetype=cpp

#include "utils.hpp"
#include <boost/program_options/cmdline.hpp>
#include <boost/program_options/options_description.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/program_options/parsers.hpp>
#include <cstdlib>
#include "parse.hpp"
#include <fstream>
#include <chrono>

namespace po = boost::program_options;
using namespace std;
using boost::property_tree::ptree;
using namespace std::chrono;

void parse_input(string input,
                 string rule_name,
                 bool print,
                 const vector<string> &lookups)
{

    AnalysisContext context;
    TokenDataHandler tdh(context.symbol_table);

    Parser parser = Parser(input.c_str(), input.length(), &tdh);
    % for i, (rule_name, parser) in enumerate(_self.rules_to_fn_names.items()):
        ${"if" if i == 0 else "else if"} (rule_name == ${c_repr(rule_name)}) {
            auto res = parser.${parser.gen_fn_name}(0);

            if (current_pos == -1) {
                SourceLocation sloc = parser.max_token().sloc_range.get_start();
                printf("Failed !!\n");
                printf("Last token pos : Line %d, Col %d, cat %d\n",
                       sloc.line, sloc.column, parser.max_token().id);
            } else {
                % if parser.needs_refcount():
                    res${"->" if parser.get_type().is_ptr else "."}inc_ref();
                % endif
                current_pos = 0;

                if (print)
                   % if is_ast_node(parser.get_type()):
                        res->print_node();
                   % else:
                        printf("%s\n", get_repr(res).c_str());
                   % endif

                % if is_ast_node(parser.get_type()):
                    res->validate();

                    for (auto lookup : lookups) {
                        printf("\n");
                        unsigned line, column;
                        if (sscanf(lookup.c_str(),
                                   "%u:%u", &line, &column) != 2)
                            printf("Invalid lookup request: %s\n",
                                   lookup.c_str());
                        else
                        {
                            const SourceLocation sloc((uint32_t) line,
                                                      (uint16_t) column);
                            const string lookup_str = sloc.repr();
                            auto lookup_res = res->lookup(sloc);

                            printf("Lookup %s:\n",
                                   lookup_str.c_str());
                            lookup_res->print_node();

                            lookup_res->validate();
                        }
                    }
                % else:
                    if (!lookups.empty())
                        printf("Cannot lookup non-AST nodes!\n");
                % endif

                % if parser.needs_refcount():
                    res${"->" if parser.get_type().is_ptr else "."}dec_ref();
                % endif
            }

            clean_all_memos();
        }
    % endfor
    else {
        printf("Unknown rule : %s\n", rule_name.c_str());
    }
}

int main (int argc, char** argv) {

    po::options_description desc("Allowed options");
    desc.add_options()
        ("help", "produce help message")
        ("silent,s", po::value<bool>()->default_value(false)
                                       ->implicit_value(true)
                                       ->zero_tokens(),
         "print the representation of the resulting tree")
        ("time,t", po::value<bool>()->default_value(false)
                                    ->implicit_value(true)
                                    ->zero_tokens(),
         "Time the execution of parsing")
        ("json,j", po::value<bool>()->default_value(false)
                                    ->implicit_value(true)
                                    ->zero_tokens(),
         "print the representation of the resulting tree as json")
        ("files,f", po::value<vector<string>>(),
         "trigger the file input mode")
        ("filelist,F", po::value<string>(), "list of files")
        ("rule-name,r", po::value<string>()->default_value("${_self.main_rule_name}"), "rule name to parse")
        ("input", po::value<string>()->default_value(""), "input file or string")
        ("lookup", po::value<vector<string>>(), "sloc lookups to perform");

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

    string input = vm["input"].as<string>();
    string rule_name = vm["rule-name"].as<string>();
    bool print = !vm["silent"].as<bool>();
    bool time = vm["time"].as<bool>();
    bool json = vm["json"].as<bool>();
    vector<string> lookups;

    if (vm.count("lookup")) lookups = vm["lookup"].as<vector<string>>();

    bool is_filelist = (bool)vm.count("filelist"),
        is_files = (bool)vm.count("files");

    high_resolution_clock::time_point t1;


    if (is_filelist || is_files) {
        vector<string> file_list;
        AnalysisContext context;
        if (rule_name != "${_self.main_rule_name}") {
            cout << "You can't supply a custom rule when you are parsing whole"
                "files, the main rule ${_self.main_rule_name} is necessarily"
                "used" << endl;
            return 1;
        }
        if (is_filelist) {
            ifstream fl(vm["filelist"].as<string>());
            string input_file;
            while (getline(fl, input_file)) {
                input_file = trim(input_file);
                file_list.push_back(input_file);
            }
        }

        if (is_files) {
            auto files = vm["files"].as<vector<string>>();
            file_list.insert(file_list.end(), files.begin(), files.end());
        }

        for (auto input_file : file_list) {
            if (time) t1 = high_resolution_clock::now();
            cout << "file name : " << input_file << endl;
            auto unit = context.create_from_file(input_file);
            if (print) {
                if (json) unit->print_json();
                else unit->print();
            }
            if (time) {
                auto t2 = high_resolution_clock::now();
                auto duration = duration_cast<milliseconds>( t2 - t1 ).count();
                cout << "TIME : " << duration << "ms" << endl;
            }
            cout << "removing file " << input_file << endl;
            context.remove(input_file);
        }
    } else {
        parse_input(input, rule_name, print, lookups);
    }


    print_diagnostics();
    return 0;
}
