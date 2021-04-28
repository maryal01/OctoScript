#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 10

#include "list.c"

void toString(char sym, void *data, char *buf){
    switch(sym) {
        case 'i': //int
            printf("Data %d\n",
            ((int *)data)[1]);
            sprintf(buf, "%d", *(int *)data);
            break;
        case 'b': //boolean
            break;
        case 'f': //float
            break;
        case 's': //string
            break;
        case 'l': //lambda
            break;
        case 'T': //table
            break;
        case 'U': //tuple
            break;
        case 'L': //list
            break;
    }
}

void test(void *data){
    ListType *lp = data;
    
    char buf[BUF_SIZE];

    toString(*(lp->type), lp->data, buf);
    printf("Data in list: length %d, type %s, first value %s\n",
            lp->len, lp->type, buf);
}


