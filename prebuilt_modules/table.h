#ifndef TABLE_C
#define TABLE_C



#include "tableHelper.h"

// Reads a csv file and creates a table based on it
void* read(char* filename, void* typeNames, char* delimeter);

// Writes the given table to a csv file
void write(void* table, char* filename, char* delimeter);
int countRows(ListType* table);

int countCols(ListType* table);
bool empty(ListType* table);

ListType* insertRow(ListType* table, int index, TupleType* tup);
ListType* appendRow(ListType* table, TupleType* tup) ;
ListType* dropRow(ListType* table, int index);
ListType* setRow(ListType* table, int index, TupleType* tup);

ListType* insertRow(ListType* table, int index, TupleType* tup);
ListType* appendRow(ListType* table, TupleType* tup) ;
ListType* dropRow(ListType* table, int index);
ListType* setRow(ListType* table, int index, TupleType* tup);




#endif