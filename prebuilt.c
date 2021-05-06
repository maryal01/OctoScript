#ifndef PREBUILT_C
#define PREBUILT_C



#define INT_TYPE 0
#define BOOL_TYPE 1
#define FLOAT_TYPE 2
#define STRING_TYPE 3
#define LAMBDA_TYPE 4
#define TUPLE_TYPE 11
#define LIST_TYPE 10
#define TABLE_HEADER_TYPE 12
#define TABLE_WITHOUT_HEADER_TYPE 13

#include "prebuilt_modules/list.c"
#include "prebuilt_modules/listHelper.c"
#include "prebuilt_modules/table.c"
#include "prebuilt_modules/tableHelper.c"
#include "prebuilt_modules/tuple.c"
#include "prebuilt_modules/tupleHelper.c"
#include "prebuilt_modules/helper.c"
#include "prebuilt_modules/ioHelper.c"
#include "prebuilt_modules/string.c"




char* string_of_tuple(TupleType* tup)
{

    char *list_buf = calloc(BUF_SIZE * tup->len, sizeof(char));
    strcpy(list_buf, "[");

    for(int i = 0; i < tup->len; i++){
        
        char int_buf[BUF_SIZE];
        void *data = getTupleElement(tup, i);
        valToString(data, getTypeofTupleIndex(tup, i), int_buf);
    
        strcat(list_buf, int_buf);
        if(i + 1 != tup->len){
            strcat(list_buf, ", ");

        }
    }
    strcat(list_buf, "]");
    return list_buf;


}

#endif