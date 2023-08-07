#include <map>
#include <any>
#include <string>
#include <vector>
#include <algorithm>
#include <cstdint>

#include "eckit/exception/Exceptions.h"


namespace fckit {
extern "C" {

using int32  = std::int32_t;
using int64  = std::int64_t;
using size_t = std::size_t;

// Generic map key:value
using FckitMap = std::map<std::string,std::any>;
using FckitMapEntry = std::pair<std::string,std::any>;


FckitMap* c_fckit_map_new() {
    return new FckitMap;
}


void c_fckit_map_delete(FckitMap* h) {
    ASSERT(h);
    delete h;
    h = nullptr;
}


void c_fckit_map_insert_int32(FckitMap* h, const char* name, int32 value) {
    ASSERT(h);
    h->insert(FckitMapEntry(name, value));
}


void c_fckit_map_insert_int64(FckitMap* h, const char* name, int64 value) {
    ASSERT(h);
    h->insert(FckitMapEntry(name, value));
}


void c_fckit_map_insert_real32(FckitMap* h, const char* name, float value) {
    ASSERT(h);
    h->insert(FckitMapEntry(name, value));
}


void c_fckit_map_insert_real64(FckitMap* h, const char* name, double value) {
    ASSERT(h);
    h->insert(FckitMapEntry(name, value));
}


void c_fckit_map_insert_c_ptr(FckitMap* h, const char* name, void* value) {
    ASSERT(h);
    h->insert(FckitMapEntry(name, value));
}


bool c_fckit_map_has(FckitMap* h, const char* name) {
    ASSERT(h);
    return find_if(
                 h->begin(),
                 h->end(),
                 [&name](const FckitMapEntry& item) { return (item.first) == name; }
                ) != h->end();
}


int c_fckit_map_size(FckitMap* h) {
    ASSERT(h);
    return h->size();
}


// ================ getters

void c_fckit_map_get_int32(FckitMap* h, const char* name, int* value) {
    ASSERT(h);
    *value = std::any_cast<int>((*h)[name]);
}


void c_fckit_map_get_int64(FckitMap* h, const char* name, int64* value) {
    ASSERT(h);
    *value = std::any_cast<int64>((*h)[name]);
}


void c_fckit_map_get_real32(FckitMap* h, const char* name, float* value) {
    ASSERT(h);
    *value = std::any_cast<float>((*h)[name]);
}


void c_fckit_map_get_real64(FckitMap* h, const char* name, double* value) {
    ASSERT(h);
    *value = std::any_cast<double>((*h)[name]);
}


void c_fckit_map_get_c_ptr(FckitMap* h, const char* name, void** value) {
    ASSERT(h);
    *value = std::any_cast<void*>((*h)[name]);
}


} // extern "C"
} // fckit namespace
