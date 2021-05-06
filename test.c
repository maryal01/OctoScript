#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 10

typedef struct ListType{
    int self_type;
    int len;
    int type;
    char data[];
} ListType;

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

void toString(int type, void *data, char *buf){
    switch(type) {
        case 0: //int
            sprintf(buf, "%d", *(int *)data);
            break;
        case 1: //boolean
            if(*(char *)data == 0){
                strcpy(buf, "false");
            }else{
                strcpy(buf, "true");
            }
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
typedef struct list_item{
    int self_type;
    int length;
    int elem_type;
    int* data;
} list_item;


char *string_of_list(list_item *data){
    // char *list_buf = calloc(BUF_SIZE * data->length, sizeof(char));
    // strcpy(list_buf, "[");

    // for(int i = 0; i < data->length; i++){
    //     size_t offset = size_of_type(data->elem_type) * i;
        
    //     char int_buf[BUF_SIZE];
    //     toString(data->elem_type, data->data + offset, int_buf);
    //     strcat(list_buf, int_buf);
    //     if(i + 1 != data->length){
    //         strcat(list_buf, ", ");

    //     }
    // }
    // strcat(list_buf, "]");
    char * a = malloc((size_t)2);
    a[0] = 's';
    a[1] = 0;
    return a;
}

void test(void *data){
    ListType *lp = data;
    
    char buf[BUF_SIZE];

    toString(lp->type, lp->data, buf);
    printf("Data in list: self_type %d, length %d, type %d, first value %d\n",
            lp->self_type, lp->len, lp->type, *((int *)lp->data));
}

void *test_return_same(void *data){
    return data;
}
