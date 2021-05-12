#ifndef LIST_HELPER_H
#define LIST_HELPER_H


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "dataTypes.h"
#include "helper.h"

#define BUF_SIZE 20





// returns the size of fields of list until its data
size_t getListDataOffsetSize();

void print_list(void *data);


void printVal(void* val, int type);

// returns the size of data of list
size_t getListDataSize(int type, int len);

// returns the size of a list
size_t getListSize(int type, int len);

// returns list element at index
void* getListElement(ListType* lt, int index);

// sets the element at index to val
void setListElement(ListType* lt, int index, void* val);

// sets the given value to the given pointer location
void setValue(void* data, void* value, int type);
int* convertTypeListToInts(ListType* typeNames);
sizeStruct getCsvSize(FILE *file, char* delimeter);
void readCsv(FILE *file, char ***data, int row, int col, char* delimeter);

void printVal(void* val, int type);
void valToString(void* val, int type, char* buf);
void* convertStringtoValue(char* string, int type);

#endif