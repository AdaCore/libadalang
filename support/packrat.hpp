/*--------------------------
-- Memoization data types --
--------------------------*/

const int MEMO_SIZE=256;

enum class MemoState {
    Nores, Fail, Success
};

template <typename T> struct Memo {

    struct MemoEntry {
        MemoState state;
        T instance;
        long offset, final_pos;
    };

    MemoEntry memo_array[MEMO_SIZE];

    inline void clear() {
        for (int i = 0; i < MEMO_SIZE; i++) {
            if (this->memo_array[i].state == MemoState::Success) {
#if DEBUG_MODE
                printf("dec ref at pos %d\n", i);
#endif
                dec_ref(this->memo_array[i].instance);
            }
            this->memo_array[i].state = MemoState::Nores;
        }
    }

    inline MemoEntry get(long offset) {
        auto res = this->memo_array[offset % MEMO_SIZE];
        if (res.offset == offset) {
            return res;
        } else {
            MemoEntry ret;
            ret.state = MemoState::Nores;
            return ret;
        }
    }

    inline void set(long offset, bool success, T instance, long final_pos) {
        long off = offset % MEMO_SIZE;
        // printf("IN MEMO SET, off = %d, offset = %d, state = %d\n", off, offset, memo_array[off].state);

        if (!(memo_array[off].state != MemoState::Nores && memo_array[off].offset == offset)) {
            
            // Do we need to free something ?
            if (memo_array[off].state == MemoState::Success) {
                dec_ref(memo_array[off].instance);
            }

            memo_array[off].final_pos = final_pos;
            memo_array[off].instance = instance;
            memo_array[off].offset = offset;
            memo_array[off].state = success ? MemoState::Success : MemoState::Fail;
        }
    }
};
