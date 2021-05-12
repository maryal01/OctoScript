#ifndef TABLE_HELPER_H
#define TABLE_HELPER_H


#include "dataTypes.h"
#include "tupleHelper.h"

// creates a table from 2d array of char* given the size and a list of types
ListType* createTable(char ***data,  int row, int col, ListType* typeNames);

// prints the given table in a csv format with the delimiter
void printTable(ListType* table, FILE* file, char* delimeter);

// return the size of the table in memory
size_t getTableSize(int len);

#endif