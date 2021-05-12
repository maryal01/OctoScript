


#include "list.h"

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
    if (index < 0 ) errorExit("index smaller than 0");
    
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

ListType* removeFromList(ListType* lt, int index) 
{
    if (index >= lt->len) errorExit("index too large when removing from list");
    if (index < 0) errorExit("index cannot be smaller than 0");
    
    int type = lt->type;
    int len = lt->len;
    ListType* new = lt;

    new->len = len - 1;
    for (int i = index; i < new->len; i++) {
        setValue(getListElement(new, i), getListElement(new, i + 1), type);
    }
    return new;
}




