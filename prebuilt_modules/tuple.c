#ifndef TUPLE_C
#define TUPLE_C

#include "tuple.c"

TupleType* copyTuple(TupleType* lt);
TupleType* tupleSet(TupleType* tup, int index, ...);

TupleType* tupleSet(TupleType* tup, int index, ...) {

    if (index >= tup->len) errorExit("index too large in set");
    
    va_list args;
    va_start(args, index);

    int type = getTypeofTupleIndex(tup, index);

    void* data = makePointerOutOfValue(type, args);
    TupleType* new = copyTuple(tup);
    setValue(getTupleElement(new, index), data, type);
    
    return new;

    
}

TupleType* copyTuple(TupleType* tup)
{

    TupleType* new = createTuple((int*)(void*)tup->data, tup->len);
    

    for (int i = 0; i < tup->len; i++){
        setValue(getTupleElement(new, i), getTupleElement(tup, i), getTypeofTupleIndex(new, i));
    }

    return new;

    
} 

#endif