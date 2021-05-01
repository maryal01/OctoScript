#ifndef LIST_C
#define LIST_C

#include <stdarg.h>
#include "listHelper.c"
#include "tupleHelper.c"
#include "tuple.c"

ListType* set(ListType* lt, int index, ...);
void printList(ListType* lt);

ListType* insertToList(ListType* lt, int index, void* val) ;

ListType* copyList(ListType* lt);
// void append(ListType* lt, ...)
// {
//     va_list args;
//     va_start(args, lt);

//     int type = lt->type;
//     switch
//     int i = va_arg(args, int);
    
//     fprintf(stderr, "hell\n");

//     fprintf(stderr,"val = %d\n", i);
   

// }

ListType* set(ListType* lt, int index, ...)
{
    if (index >= lt->len) errorExit("index too large in set");
    
    va_list args;
    va_start(args, index);

    int type = lt->type;
    
    ListType* new = copyList(lt);

    void* data = makePointerOutOfValue(type, args);

    void* listData = getListElement(new, index);
    setValue(listData, data, type);  

    return new;
}

ListType* append(ListType* lt, ...)
{
    ListType* n = copyList(lt);
    
    va_list args;
    va_start(args, lt);

    void* data = makePointerOutOfValue(n->type, args);

    return insertToList(n, n->len, data);
}

ListType* insert(ListType* lt, int index, ...)
{
    ListType* n = copyList(lt);
    
    va_list args;
    va_start(args, index);

    void* data = makePointerOutOfValue(n->type, args);

    return insertToList(n, index, data);
}

ListType* concatLists(ListType* lt, ListType* lt2)
{
    if (lt->type != lt2->type) errorExit("list types do not match");

    ListType* n = copyList(lt);
    for (int i = 0; i < lt2->len; i++) {
        n = insertToList(n, i + lt->len, getListElement(lt2, i));
    }
    return n;

}

ListType* copyList(ListType* lt)
{
    size_t size = getListSize(lt->type, lt->len);

    ListType* newList = malloc(size);
    newList->self_type = lt->self_type;
    newList->type = lt->type;
    newList->len = lt->len;

    void* newData = newList->data;

    for (int i = 0; i < lt->len; i++) {
        void* tmp = getListElement(lt, i);
        if (lt->type == TUPLE_TYPE) {
            tmp = copyTuple(tmp);
        } else if (lt->type == LIST_TYPE) {
            tmp = copyList(tmp);
        }
        setValue(getListElement(newList, i), tmp, lt->type);

    }

    return newList;
}

ListType* insertToList(ListType* lt, int index, void* val) 
{
    int type = lt->type;
    int len = lt->len;
    ListType* new = realloc(lt, getListSize(type, len + 1));

    new->len = len + 1;
    for (int i = len - 1; i >= index; i--) {
        setValue(getListElement(new, i + 1), getListElement(new, i), type);
    }
    setValue(getListElement(new, index), val, type);
    return new;
}

void printList(ListType* lt)
{
    fprintf(stderr, "len = %d\n", lt->len);
    fprintf(stderr, "self_type = %d\n", lt->self_type);
    fprintf(stderr, "type = %d\n", lt->type);
    for (int i = 0; i < lt->len; i++) {

        fprintf(stderr, "val = %d\n", *(int*)getListElement(lt, i));
    }
}




#endif