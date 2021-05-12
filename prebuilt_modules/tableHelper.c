#ifndef TABLE_HELPER_C
#define TABLE_HELPER_C


#include "dataTypes.c"
#include "tupleHelper.c"

// creates a table from 2d array of char* given the size and a list of types
ListType* createTable(char ***data,  int row, int col, ListType* typeNames);

// prints the given table in a csv format with the delimiter
void printTable(ListType* table, FILE* file, char* delimeter);

// return the size of the table in memory
size_t getTableSize(int len);


size_t getTableSize(int len)
{
    return getListSize(11, len);
}


ListType* createTable(char ***data,  int row, int col,  ListType* typeNames)
{
    size_t size = getTableSize(col);
    ListType *table = malloc(size);
    table->len = row;
    table->self_type = LIST_TYPE;
    table->type = TUPLE_TYPE;
    void* datap = table->data;
    int* types = convertTypeListToInts(typeNames);
    for (int i = 0; i < row; i++){
        void* tup = createTupleFromStrings(data, i, col, types);

        setValue(datap, tup, TUPLE_TYPE);

        datap = offsetPointer(datap, TUPLE_TYPE);
    }
    return table;

}



void printTable(ListType* table, FILE* file, char* delimeter)
{
    for (int i = 0; i < table->len; i++) {
        TupleType* tup = *(TupleType**)getListElement(table, i);
        
        for (int j = 0; j < tup->len; j++ ) {
            int type = getTypeofTupleIndex(tup, j);
            void* data = getTupleElement(tup, j);

            char buf[BUF_SIZE];
            valToString(data, type, buf);
            fprintf(file, "%s", buf);
            if (j + 1 < tup->len ) {
                fprintf(file, "%s", delimeter);
            }
        }

        fprintf(file, "\n" );
    }
}

#endif