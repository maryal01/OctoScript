#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "list.c"

#define BUF_SIZE 10



void toString(int type, void *data, char *buf){
    switch(type) {
        case 0: //int
            sprintf(buf, "%d", *(int *)data);
            break;
        case 1: //boolean
            break;
        case 2: //float
            break;
        case 3: //string
            break;
        case 4: //lambda
            break;
        case 10: //list
            break;
        case 11: //tuple
            break;
        // TABLEs are represented as a list of tuples, so there is no 
        // actual table type under the hood
    }
}

void test(void *data){
    ListType *lp = data;
    
    char buf[BUF_SIZE];

    toString(lp->type, lp->data, buf);
    printf("Data in list: self_type %d, length %d, type %d, first value %s\n",
            lp->self_type, lp->len, lp->type, buf);
}


