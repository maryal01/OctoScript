#ifndef TUPLE_C
#define TUPLE_C

#include "tuple.c"

TupleType* copyTuple(TupleType* lt);
TupleType* tupleSet(TupleType* tup, int index, ...);
TupleType* tupleInsert(TupleType* tup, int index, void* val, int valType);

char* string_of_tuple(TupleType* tup)
{
fprintf(stderr, "tup len = %d\n", tup->len);
// for (int i = 0; i < tup->len; i++) {
//     fprintf(stderr, "yype = %d\n",(getTypeofTupleIndex(tup,i)));
// }
  fprintf(stderr, "yfsfdype = \n");
    char *list_buf = calloc(BUF_SIZE * tup->len, sizeof(char));
    strcpy(list_buf, "(");

    for(int i = 0; i < tup->len; i++){
        
        char int_buf[BUF_SIZE];
        void *data = getTupleElement(tup, i);
        valToString(data, getTypeofTupleIndex(tup, i), int_buf);
        fprintf("val = %s\n",int_buf);
        strcat(list_buf, int_buf);
        if(i + 1 != tup->len){
            strcat(list_buf, ", ");

        }
    }
    strcat(list_buf, ")");
    return list_buf;


}


TupleType* tupleInsert(TupleType* tup, int index, void* val, int valType)
{

     fprintf(stderr, "jelfsdffsdlo - %d\n",tup->len);
    printf("%s\n",string_of_tuple(tup));
    int len = tup->len;
     fprintf(stderr, "jello - %d\n",tup->len);

    TupleType* new = malloc(getTupleSize((int*) (void*)tup->data, tup->len) + sizeofType(INT_TYPE) + sizeofType(valType));
    new->len = len + 1;
     fprintf(stderr, "jello - %d\n",tup->len);

    new->self_type = TUPLE_TYPE;
    for (int i = 0; i < index; i++) {
        setTypeofTupleIndex(new, i, getTypeofTupleIndex(tup, i));
    } 
    setTypeofTupleIndex(new, index, valType);
    for (int i = index + 1; i < new->len; i++) {
        setTypeofTupleIndex(new, i, getTypeofTupleIndex(tup, i - 1 ));
    }
     fprintf(stderr, "jello - %d\n",tup->len);


    for (int i = 0; i < index; i++) {
        setTupleElement(new, i, getTupleElement(tup, i));
    } 
    setTupleElement(new, index, val);
    for (int i = index + 1; i < new->len; i++) {
        setTupleElement(new, i, getTupleElement(tup, i - 1));
    }
     fprintf(stderr, "jello - %d\n",tup->len);


    printf("%s\n",string_of_tuple(new));
    return new;

    
}

TupleType* tupleSet(TupleType* tup, int index, ...) {

    if (index >= tup->len) errorExit("index too large in set");  

    va_list args;
    va_start(args, index);

    int type = getTypeofTupleIndex(tup, index);

    void* data = makePointerOutOfValue(type, args);
    TupleType* new = copyTuple(tup);
    setValue(getTupleElement(new, index), data, type);
    
    return new;

    
}

TupleType* copyTuple(TupleType* tup)
{

    TupleType* new = createTuple((int*)(void*)tup->data, tup->len);
    

    for (int i = 0; i < tup->len; i++){
        setValue(getTupleElement(new, i), getTupleElement(tup, i), getTypeofTupleIndex(new, i));
    }

    return new;

    
} 


ListType* tuple_get(TupleType* tup, int index) {

    int type = getTypeofTupleIndex(tup, index);
    void* val = getTupleElement(tup, index);
    size_t s = getListSize(type, 1);
    ListType* lt = (ListType*)malloc(s);
    lt->self_type = LIST_TYPE;
    lt->len = 1;
    lt->type = type;
    void*data = getListElement(lt, 0);
    setValue(data, val, type);
    return lt;
}

#endif