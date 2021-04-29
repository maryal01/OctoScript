#ifndef TUPLE_HELPER_C
#define TUPLE_HELPER_C



#include "dataTypes.c"

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
TupleType* createTupleFromStrings(char*** data, int row, int col, ListType *typeNames);

// returns the type of tuple at index 
int getTypeofTupleIndex(TupleType* tt, int index);

// sets the type of tuple at index
void setTypeofTupleIndex(TupleType* tt, int index, int val);

void* getTupleElement(TupleType* tt, int index)
{
    
    if (index >= tt->len) errorExit("index greater than length");

    void* data = tt->data;

    data = offsetPointerNtimes(data, INT_TYPE, tt->len);

    for (int i = 0; i < index; i++) {
        
        data = offsetPointer(data, tt->data[i]);

    }
    return data;
    
}

size_t getTupleDataOffsetSize(int len)
{
    return sizeof(int) + sizeof(len) + sizeof(int) * len;
}

size_t getTupleDataSize(int* types, int len)
{
    size_t total = 0;
    for (int i = 0; i < len; i++) {
        total += sizeofType(types[i]);
    }
    return total;
    
}
size_t getTupleSize(int* types, int len)
{
    return getTupleDataOffsetSize(len) + getTupleDataSize(types, len);
}

TupleType* createTupleFromStrings(char*** data, int row, int col, ListType *typeNames)
{


    size_t size = getTupleSize((int*)(void*)typeNames->data, col);

    TupleType* tup = malloc(size);
    tup->self_type = 11;
    tup->len = col;
    
    void *datap = tup->data;
    int* types = ((int*)(void*)typeNames->data);

    for (int i = 0; i < tup->len; i++){
        setValue(datap, getListElement(typeNames, i), INT_TYPE);
        datap = offsetPointer(datap, INT_TYPE);
    }

    
    
    for (int i = 0; i < tup->len; i++) {
        int type = getTypeofTupleIndex(tup, i);
        
        void* val = convertStringtoValue(data[row][i], type);

        setValue(datap, val, type);

        datap = offsetPointer(datap, types[i]);
                
    }



    return tup;
}


int getTypeofTupleIndex( TupleType* tt, int index)
{
   if (index >= tt->len) errorExit("index greater than length");
    void* data = tt->data;
    data = offsetPointerNtimes(data, INT_TYPE, index);
    return *(int *) data;
}

void setTypeofTupleIndex(TupleType* tt, int index, int val)
{
    if (index >= tt->len) errorExit("index greater than length");
    void* data = tt->data;
    data = offsetPointerNtimes(data, INT_TYPE, index);
    *(int*)data = val;
}


#endif