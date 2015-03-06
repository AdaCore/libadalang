#include "symboltable.hpp"

#undef get16bits
#if (defined(__GNUC__) && defined(__i386__)) || defined(__WATCOMC__) \
  || defined(_MSC_VER) || defined (__BORLANDC__) || defined (__TURBOC__)
#define get16bits(d) (*((const uint16_t *) (d)))
#endif

#if !defined (get16bits)
#define get16bits(d) ((((uint32_t)(((const uint8_t *)(d))[1])) << 8)\
                       +(uint32_t)(((const uint8_t *)(d))[0]) )
#endif

uint32_t CharHash::operator() (const char * const string) const {

    const char* data = string;
    size_t len = strlen(data);
    uint32_t hash = (uint32_t)len;
    uint32_t tmp;
    int rem;

    if (len <= 0 || data == NULL) return 0;

    rem = len & 3;
    len >>= 2;

    /* Main loop */
    for (;len > 0; len--) {
        hash  += get16bits (data);
        tmp    = (get16bits (data+2) << 11) ^ hash;
        hash   = (hash << 16) ^ tmp;
        data  += 2*sizeof (uint16_t);
        hash  += hash >> 11;
    }

    /* Handle end cases */
    switch (rem) {
        case 3: hash += get16bits (data);
                hash ^= hash << 16;
                hash ^= (uint32_t)((signed char)data[sizeof (uint16_t)]) << 18;
                hash += hash >> 11;
                break;
        case 2: hash += get16bits (data);
                hash ^= hash << 11;
                hash += hash >> 17;
                break;
        case 1: hash += (unsigned char)*data;
                hash ^= hash << 10;
                hash += hash >> 1;
    }

    /* Force "avalanching" of final 127 bits */
    hash ^= hash << 3;
    hash += hash >> 5;
    hash ^= hash << 4;
    hash += hash >> 17;
    hash ^= hash << 25;
    hash += hash >> 6;

    return hash;
};

const char* SymbolTable::get(const char* text, size_t len) {
    static char text_buffer[4096];
    strncpy(text_buffer, (const char*)text, len);
    text_buffer[len] = 0;
    char* ret;

    auto it = this->hmap.find (text_buffer);
    if (it == this->hmap.end()) {
        ret = strdup(text_buffer);
        this->hmap[ret] = ret;
    } else {
        ret= it->second;
    }
    return ret;
}

SymbolTable::~SymbolTable() {
    for (auto kv : this->hmap) free(kv.first);
}
