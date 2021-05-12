#ifndef LIST_H
#define LIST_H

#include <stdarg.h>
#include "listHelper.h"
#include "tupleHelper.h"
#include "tuple.h"


void printList(ListType* lt);
ListType* insertToList(ListType* lt, int index, void* val) ;
char *string_of_list(void *data);
ListType* copyList(ListType* lt);
ListType* removeFromList(ListType* lt, int index);


#endif
