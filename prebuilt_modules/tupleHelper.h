#ifndef TUPLE_HELPER_H
#define TUPLE_HELPER_H



#include "dataTypes.h"

void setTupleElement(TupleType* tt, int index, void *val, int type);
TupleType* createTuple(int types[], int len);
void printTuple(TupleType* tt);
bool hasPadding(int* types, int len, int index);


// returns the tuple element at index
void* getTupleElement(TupleType* tt, int index);

// returns the size of fields of tuple until its data
size_t getTupleDataOffsetSize(int len);

// returns the size of data of tuple
size_t getTupleDataSize(int* types, int len);

// returns the size of tuple
size_t getTupleSize(int* types, int len);

// creates a tuple from the index row of the table
// tuple has size col and types as given in typenames
TupleType* createTupleFromStrings(char*** data, int row, int col, int *types);

// returns the type of tuple at index 
int getTypeofTupleIndex(TupleType* tt, int index);

// sets the type of tuple at index
void setTypeofTupleIndex(TupleType* tt, int index, int val);


#endif