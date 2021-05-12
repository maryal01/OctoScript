#ifndef TUPLE_C
#define TUPLE_C

#include "tupleHelper.h"
#include "string.h"

char* string_of_tuple(TupleType* tup);
TupleType* copyTuple(TupleType* lt);
TupleType* tupleSet(TupleType* tup, int index, ...);
ListType* tuple_get(TupleType* tup, int index);

TupleType* tupleInsert(TupleType* tup, int index, void* val, int valType);



#endif