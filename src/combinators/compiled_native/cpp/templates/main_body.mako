#include "${header_name}"
#include <unordered_map>

template< typename T, class Allocator > void shrink_capacity(std::vector<T,Allocator>* v)
{
   std::vector<T,Allocator>(v->begin(),v->end()).swap(*v);
}

long current_pos;
std::string indent_str("");

enum class MemoState {
    Nores, Fail, Success
};

const int memo_size=256;

/*--------------------------
-- Memoization data types --
--------------------------*/

template <typename T> struct Memo {

    struct MemoEntry {
        MemoState state;
        T instance;
        long final_pos;
    };

    long current_offset;
    MemoEntry memo_array[memo_size];

    inline MemoEntry get(long offset) {

        if (offset < this->current_offset - memo_size) {
#if DEBUG_MODE
            printf("Failure in memo get\n");
#endif
            return { MemoState::Nores };
        }

        long coffset = this->current_offset;

        while (coffset <= offset) {
            this->memo_array[coffset % memo_size].state = MemoState::Nores;
            coffset++;
        }

        this->current_offset = coffset;

        auto res = this->memo_array[offset % memo_size];
        return res;
    }

    inline void set(long offset, bool success, T instance, long final_pos) {

        if (offset < this->current_offset - memo_size) {
#if DEBUG_MODE
            printf("Failure in memo set\n");
#endif
            return;
        }

        long coffset = this->current_offset;

        while (coffset <= offset) {
            this->memo_array[coffset % memo_size].state = MemoState::Nores;
            coffset++;
        }

        this->memo_array[offset % memo_size].state = success ? MemoState::Success : MemoState::Fail;
        this->memo_array[offset % memo_size].instance = instance;
        this->memo_array[offset % memo_size].final_pos = final_pos;

        this->current_offset = coffset;
    }
};

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

% for fn in _self.fns:
    int ${fn}_sum = 0;
/*    for (auto kv : ${fn}_count) {
        ${fn}_sum += kv.second - 1;
    }
    if (${fn}_sum > 0) {
        printf("For fn ${fn}, %d redundant calls\n", ${fn}_sum);
    }
*/
% endfor
}
