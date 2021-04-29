#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "prebuilt.c"



size_t size_of_type(int type)
{
    switch (type)
    {
    case 0: //int
        return 4;
    case 1: //boolean
        return 1;
    case 2: //float
        return 8;
    case 3: //string
        return 8;
    case 4: //lambda
        return 8;
    case 10: //list
        return 8;
    case 11: //tuple
        return 8;
        // TABLEs are represented as a list of tuples, so there is no
        // actual table type under the hood
    }
}

char *string_of_list(void *data){
    ListType *lp = data;

    char *list_buf = calloc(BUF_SIZE * lp->len, sizeof(char));
    strcpy(list_buf, "[");

    for(int i = 0; i < lp->len; i++){
        size_t offset = size_of_type(lp->type) * i;
        
        char int_buf[BUF_SIZE];
        valToString(lp->data + offset, lp->type, int_buf);
        strcat(list_buf, int_buf);
        if(i + 1 != lp->len){
            strcat(list_buf, ", ");

        }
    }
    strcat(list_buf, "]");
    return list_buf;
}

void test(void *data){
    ListType *lp = data;
    
    char buf[BUF_SIZE];

    valToString(lp->data, lp->type, buf);
    printf("Data in list: self_type %d, length %d, type %d, first value %d\n",
            lp->self_type, lp->len, lp->type, *((int *)lp->data));
}


