#ifndef HELPER_C
#define HELPER_C

#include "helper.h"
// converts strings representation of a type into its int representation
int stringToTyp(void* data);

// returns the byte size given integer representation of type
size_t sizeofType(int type);

// offsets a pointer n times based on type
void* offsetPointerNtimes(void* data, int type, int n);

// offsets pointer once based on type
void* offsetPointer(void* data, int type);

void errorExit(const char* message);
void* makePointerOutOfValue(int type, va_list args);

#endif