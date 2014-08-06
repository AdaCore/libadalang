## vim: filetype=cpp

#include "${header_name}"
#include <unordered_map>

template< typename T, class Allocator > void shrink_capacity(std::vector<T,Allocator>* v)
{
   std::vector<T,Allocator>(v->begin(),v->end()).swap(*v);
}

long current_pos;
std::string indent_str("");

/*-------------------------
-- Functions definitions --
-------------------------*/

% for el in map(unicode.strip, _self.body):
${el}

% endfor

void print_diagnostics() {
% for t in _self.diag_types:
    // printf("Number of instantiations for ${t.name()}: %ld\n", ${t.name().lower()}_counter);
% endfor
}

void clean_all_memos() {
    % for fn in _self.fns:
#if DEBUG_MODE
        printf("CLEANING MEMO FOR ${fn}\n");
        fflush(stdout);
#endif
        ${fn}_memo.clear();
    % endfor
}
