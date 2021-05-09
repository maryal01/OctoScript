#ifndef TUPLE_HELPER_C
#define TUPLE_HELPER_C



#include "dataTypes.c"


TupleType* createTuple(int types[], int len);
void printTuple(TupleType* tt);


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

void* getTupleElement(TupleType* tt, int index)
{
    
    if (index >= tt->len) errorExit("index greater than length");
    //fprintf(stderr, "\n\nstart\n");
        
    void* data = tt->data;
    data = offsetPointerNtimes(data, INT_TYPE, tt->len);
    for (int i = 0; i < index; i++) {
        
        int type = getTypeofTupleIndex(tt, i);

        data = offsetPointer(data, type);


        

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
        if (i != len - 1 && types[i + 1] == STRING_TYPE) {
            
        }
    }
    return total;
    
}
size_t getTupleSize(int* types, int len)
{
    return getTupleDataOffsetSize(len) + getTupleDataSize(types, len);
}

TupleType* createTuple(int types[], int len)
{
    size_t size = getTupleSize(types, len);
    TupleType* new = malloc(size);
    new->self_type = 11;
    new->len = len;
    
    for (int i = 0; i < new->len; i++){
        setTypeofTupleIndex(new, i, types[i]);
    }

    return new;
    
}

TupleType* createTupleFromStrings(char*** data, int row, int col, int *types)
{

    
 
    size_t size = getTupleSize(types, col);

    TupleType* tup = malloc(size);
    tup->self_type = TUPLE_TYPE;
    tup->len = col;
    
    void *datap = tup->data;

    for (int i = 0; i < tup->len; i++){

        setTypeofTupleIndex(tup, i, types[i]);


    }




    
    

    for (int i = 0; i < tup->len; i++) {
        datap = getTupleElement(tup, i);
        int type = getTypeofTupleIndex(tup, i);
        
        void* val = convertStringtoValue(data[row][i], type);
        
        setValue(datap, val, type);


                
    }
        for (int i = 0; i < tup->len; i++){

        
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

void printTuple(TupleType* tt)
{
    
    fprintf(stderr, "(");
    for (int i = 0; i < tt->len; i++) {
        int type = getTypeofTupleIndex(tt, i);
        void* val = getTupleElement(tt, i);
        printVal(val, type);
        if (i != tt->len - 1) fprintf(stderr, ", ");
    }
    fprintf(stderr, ")");
}


#endif