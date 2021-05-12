#ifndef LIST_C
#define LIST_C

#include <stdarg.h>
#include "listHelper.c"
#include "tupleHelper.c"
#include "tuple.c"
#include "tableHelper.c"

ListType* set(ListType* lt, int index, ...);
void printList(ListType* lt);

ListType* insertToList(ListType* lt, int index, void* val) ;

char *string_of_list(void *data){
    ListType *lp = data;

    char *list_buf = calloc(BUF_SIZE * lp->len, sizeof(char));
    
    strcpy(list_buf, "[");

    for(int i = 0; i < lp->len; i++){
        size_t offset = sizeofType(lp->type) * i;
        
        char int_buf[BUF_SIZE];
        void *data = getListElement(lp, i);
        char* string = data;


        
        valToString(data, lp->type, int_buf);
    
        strcat(list_buf, int_buf);
        if(i + 1 != lp->len){
            strcat(list_buf, ", ");

        }
    }
    strcat(list_buf, "]");
    return list_buf;
}
ListType* copyList(ListType* lt);



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
            tmp = copyTuple(*(TupleType**)tmp);
        } else if (lt->type == LIST_TYPE) {
            tmp = copyList(tmp);
        }
        setValue(getListElement(newList, i), tmp, lt->type);

    }

    return newList;
}


ListType* insertToList(ListType* lt, int index, void* val) 
{
    if (index < 0 ) errorExit("index smaller than 0");
    
    int type = lt->type;
    int len = lt->len;
    ListType* new = malloc(getListSize(type, len + 1));
    new->len = len + 1;
    new->type = type;
    new->self_type = LIST_TYPE;
    for (int i = 0; i < index; i++) {
        setValue(getListElement(new, i), *(TupleType**)getListElement(lt, i), type);
    }
    setValue(getListElement(new, index), val, type);
    for (int i = index + 1; i < new->len; i++) {
        setValue(getListElement(new, i), *(TupleType**)getListElement(lt, i - 1), type);
    }
    return new;
}

ListType* removeFromList(ListType* lt, int index) 
{
    if (index >= lt->len) errorExit("index too large when removing from list");
    if (index < 0) errorExit("index cannot be smaller than 0");
    

    int type = lt->type;
    int len = lt->len;
    ListType* new = lt;


    for (int i = index; i < new->len - 1; i++) {
        setValue(getListElement(new, i), *(TupleType**)getListElement(new, i + 1), type);
    }
    new->len = len - 1;
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