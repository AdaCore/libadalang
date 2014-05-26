#include <boost/program_options/cmdline.hpp>
#include <boost/program_options/options_description.hpp>
#include <boost/program_options/variables_map.hpp>
#include <boost/program_options/parsers.hpp>
#include <cstdlib>
#include "parse.hpp"

namespace po = boost::program_options;
using namespace std;

void exit_and_print_usage() {
    printf("Usage : parse <rule_name> <input_string>\n");
    exit(2);
}

int main(int argc, char **argv) {

    po::options_description desc("Allowed options");
    desc.add_options()("help", "produce help message")(
        "silent,s", po::value<bool>()
                        ->default_value(false)
                        ->implicit_value(true)
                        ->zero_tokens(),
        "print the representation of the resulting tree")(
        "file,f", po::value<bool>()
                      ->default_value(false)
                      ->implicit_value(true)
                      ->zero_tokens(),
        "trigger the file input mode")(
        "rule-name,r", po::value<string>()->default_value("compilation_unit"),
        "rule name to parse")("input", po::value<string>(),
                              "input file or string");

    po::variables_map vm;
    po::positional_options_description p;
    p.add("input", -1);
    po::store(
        po::command_line_parser(argc, argv).options(desc).positional(p).run(),
        vm);
    po::notify(vm);

    if (vm.count("help") or !(vm.count("input"))) {
        cout << "Not enough arguments !\n";
        cout << desc << "\n";
        return 1;
    }

    Lexer *lex;
    string input = vm["input"].as<string>();
    string rule_name = vm["rule-name"].as<string>();

    if (vm["file"].as<bool>()) {
        cout << "file name : " << input << endl;
        lex = make_lexer_from_file(input.c_str(), nullptr);
    } else {
        lex = make_lexer_from_string(input.c_str(), input.length());
    }

    if (rule_name == "exit_statement") {
        auto res = exit_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "loop_statement") {
        auto res = loop_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "discriminant_association") {
        auto res = discriminant_association_row_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res.repr()).c_str());
        }
    } else if (rule_name == "library_unit_decl") {
        auto res = library_unit_decl_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "decimal_fixed_point_def") {
        auto res = decimal_fixed_point_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "relation") {
        auto res = relation_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "subprogram_body_stub") {
        auto res = subprogram_body_stub_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "formal_subp_decl") {
        auto res = formal_subp_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "aspect_clause") {
        auto res = aspect_clause_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "task_def") {
        auto res = task_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "factor") {
        auto res = factor_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "subprogram_access_expression") {
        auto res = subprogram_access_expression_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "aggregate_content") {
        auto res = aggregate_content_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "array_type_def") {
        auto res = array_type_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "protected_el") {
        auto res = protected_el_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "term") {
        auto res = term_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "compilation_unit") {
        auto res = compilation_unit_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "choice") {
        auto res = choice_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "signed_int_type_def") {
        auto res = signed_int_type_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "null_literal") {
        auto res = null_literal_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "requeue_statement") {
        auto res = requeue_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "range_spec") {
        auto res = range_spec_extract_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "interface_type_def") {
        auto res = interface_type_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "others_designator") {
        auto res = others_designator_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "name") {
        auto res = name_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "package_renaming_decl") {
        auto res = package_renaming_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "boolean_op") {
        auto res = boolean_op_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>())
                printf("%s\n", (enum_repr(res)).c_str());
        }
    } else if (rule_name == "protected_def") {
        auto res = protected_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "type_discriminant") {
        auto res = type_discriminant_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "renaming_clause") {
        auto res = renaming_clause_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "test_spec") {
        auto res = test_spec_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "if_expression") {
        auto res = if_expression_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "range_expression") {
        auto res = range_expression_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "identifier") {
        auto res = identifier_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "terminate_statement") {
        auto res = terminate_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "task_decl") {
        auto res = task_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "discriminant_constraint") {
        auto res = discriminant_constraint_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "subtype_decl") {
        auto res = subtype_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "delta_constraint") {
        auto res = delta_constraint_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "static_name") {
        auto res = static_name_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "char_literal") {
        auto res = char_literal_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "id_list") {
        auto res = id_list_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "overriding_indicator") {
        auto res = overriding_indicator_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>())
                printf("%s\n", (enum_repr(res)).c_str());
        }
    } else if (rule_name == "goto_statement") {
        auto res = goto_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "range_constraint") {
        auto res = range_constraint_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "discr_spec_list") {
        auto res = discr_spec_list_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "sexpr_or_diamond") {
        auto res = sexpr_or_diamond_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "record_def") {
        auto res = record_def_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "default_expr") {
        auto res = default_expr_opt_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>())
                printf("%s\n", (res != nullptr ? res->repr() : "None").c_str());
        }
    } else if (rule_name == "label") {
        auto res = label_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "delay_statement") {
        auto res = delay_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "with_decl") {
        auto res = with_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "subprogram_body") {
        auto res = subprogram_body_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "rep_component_clause") {
        auto res = rep_component_clause_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "num_literal") {
        auto res = num_literal_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "body") {
        auto res = body_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "accept_statement") {
        auto res = accept_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "string_literal") {
        auto res = string_literal_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "object_decl") {
        auto res = object_decl_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "record_type_def") {
        auto res = record_type_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "aspect_specification") {
        auto res = aspect_specification_opt_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>())
                printf("%s\n", (res != nullptr ? res->repr() : "None").c_str());
        }
    } else if (rule_name == "aggregate_content_empty_valid") {
        auto res = aggregate_content_empty_valid_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "case_statement") {
        auto res = case_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "entry_body") {
        auto res = entry_body_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "for_loop_parameter_spec") {
        auto res = for_loop_parameter_spec_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "rel_op") {
        auto res = rel_op_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>())
                printf("%s\n", (enum_repr(res)).c_str());
        }
    } else if (rule_name == "library_unit_body") {
        auto res = library_unit_body_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "constraint") {
        auto res = constraint_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "unop_term") {
        auto res = unop_term_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "expression") {
        auto res = expression_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "access_expression") {
        auto res = access_expression_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "statements") {
        auto res = statements_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "subprogram_decl") {
        auto res = subprogram_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "exception_handler") {
        auto res = exception_handler_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "primary") {
        auto res = primary_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "type_ref") {
        auto res = type_ref_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "generic_decl") {
        auto res = generic_decl_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "ext_return_statement") {
        auto res = ext_return_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "component_decl") {
        auto res = component_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "type_access_expression") {
        auto res = type_access_expression_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "mod_int_type_def") {
        auto res = mod_int_type_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "name_component") {
        auto res = name_component_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "component_def") {
        auto res = component_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "aggregate_assoc") {
        auto res = aggregate_assoc_row_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res.repr()).c_str());
        }
    } else if (rule_name == "simple_expr_2") {
        auto res = simple_expr_2_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "call_suffix") {
        auto res = call_suffix_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "aspect_assoc") {
        auto res = aspect_assoc_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "type_name") {
        auto res = type_name_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "assignment_statement") {
        auto res = assignment_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "block_statement") {
        auto res = block_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "case_expr_alt") {
        auto res = case_expr_alt_row_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res.repr()).c_str());
        }
    } else if (rule_name == "generic_renaming_decl") {
        auto res = generic_renaming_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "statement") {
        auto res = statement_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "in_out") {
        auto res = in_out_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>())
                printf("%s\n", (enum_repr(res)).c_str());
        }
    } else if (rule_name == "task_type_decl") {
        auto res = task_type_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "if_statement") {
        auto res = if_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "discriminant_spec") {
        auto res = discriminant_spec_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "context_item") {
        auto res = context_item_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "real_type_def") {
        auto res = real_type_def_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "body_stub") {
        auto res = body_stub_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "select_statement") {
        auto res = select_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "protected_type_decl") {
        auto res = protected_type_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "formal_discrete_type_def") {
        auto res = formal_discrete_type_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "discrete_range") {
        auto res = discrete_range_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "pragma") {
        auto res = pragma_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "pragma_arg") {
        auto res = pragma_arg_row_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res.repr()).c_str());
        }
    } else if (rule_name == "type_def") {
        auto res = type_def_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "iteration_scheme") {
        auto res = iteration_scheme_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "abort_statement") {
        auto res = abort_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "_object_decl") {
        auto res = _object_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "basic_decl") {
        auto res = basic_decl_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "floating_point_def") {
        auto res = floating_point_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "simple_expr") {
        auto res = simple_expr_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "case_alt") {
        auto res = case_alt_row_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res.repr()).c_str());
        }
    } else if (rule_name == "generic_formal_decl") {
        auto res = generic_formal_decl_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "use_package_decl") {
        auto res = use_package_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "parameter_profiles") {
        auto res = parameter_profiles_extract_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "digits_constraint") {
        auto res = digits_constraint_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "library_unit_renaming_decl") {
        auto res = library_unit_renaming_decl_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "qual_expr_content") {
        auto res = qual_expr_content_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "direct_name") {
        auto res = direct_name_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "discrete_subtype_definition") {
        auto res = discrete_subtype_definition_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "component_item") {
        auto res = component_item_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "_name_component") {
        auto res = _name_component_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "task_item") {
        auto res = task_item_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "protected_body") {
        auto res = protected_body_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "attribute") {
        auto res = attribute_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "positional_aggregate") {
        auto res = positional_aggregate_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "protected_decl") {
        auto res = protected_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "simple_statement") {
        auto res = simple_statement_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "generic_formal_part") {
        auto res = generic_formal_part_extract_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "task_body") {
        auto res = task_body_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "entry_decl") {
        auto res = entry_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "raise_statement") {
        auto res = raise_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "quantified_expression") {
        auto res = quantified_expression_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "exception_decl") {
        auto res = exception_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "package_decl") {
        auto res = package_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "allocator") {
        auto res = allocator_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "constrained_type_ref") {
        auto res = constrained_type_ref_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "diamond_expr") {
        auto res = diamond_expr_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "full_type_decl") {
        auto res = full_type_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "qualified_name") {
        auto res = qualified_name_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "case_expression") {
        auto res = case_expression_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "access_deref") {
        auto res = access_deref_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "return_statement") {
        auto res = return_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "enum_type_def") {
        auto res = enum_type_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "choice_list") {
        auto res = choice_list_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "type_expression") {
        auto res = type_expression_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "aggregate_field") {
        auto res = aggregate_field_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "derived_type_def") {
        auto res = derived_type_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "subunit") {
        auto res = subunit_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "generic_instantiation") {
        auto res = generic_instantiation_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "library_item") {
        auto res = library_item_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "protected_op") {
        auto res = protected_op_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "variant") {
        auto res = variant_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "index_constraint") {
        auto res = index_constraint_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "package_body") {
        auto res = package_body_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "variant_part") {
        auto res = variant_part_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "parameter_profile") {
        auto res = parameter_profile_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "aggregate") {
        auto res = aggregate_extract_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "expression_list") {
        auto res = expression_list_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "compound_statement") {
        auto res = compound_statement_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "package_body_stub") {
        auto res = package_body_stub_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "use_type_decl") {
        auto res = use_type_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "subprogram_spec") {
        auto res = subprogram_spec_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "use_decl") {
        auto res = use_decl_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "handled_statements") {
        auto res = handled_statements_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "component_list") {
        auto res = component_list_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "conditional_expression") {
        auto res = conditional_expression_or_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "access_def") {
        auto res = access_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "null_statement") {
        auto res = null_statement_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "number_decl") {
        auto res = number_decl_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "ordinary_fixed_point_def") {
        auto res = ordinary_fixed_point_def_transform_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else if (rule_name == "basic_decls") {
        auto res = basic_decls_list_parse_1(lex, 0);
        if (current_pos == -1) {
            printf("Failed !!\n");
            printf("Last token pos : Line %d, Col %d, cat %d\n",
                   max_token.line_n, max_token.column_n, max_token._id);
        } else {
            if (!vm["silent"].as<bool>()) printf("%s\n", (res->repr()).c_str());
        }
    } else {
        printf("Unknown rule : %s\n", rule_name.c_str());
    }
    print_diagnostics();
    return 0;
}
