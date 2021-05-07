#ifndef LIST_HELPER_C
#define LIST_HELPER_C



#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "dataTypes.c"
#include "helper.c"
#define BUF_SIZE 20



// returns the size of fields of list until its data
size_t getListDataOffsetSize();

// returns the size of data of list
size_t getListDataSize(int type, int len);

// returns the size of a list
size_t getListSize(int type, int len);

// returns list element at index
void* getListElement(ListType* lt, int index);

// sets the element at index to val
void setListElement(ListType* lt, int index, void* val);

// sets the given value to the given pointer location
void setValue(void* data, void* value, int type);


void setValue(void* data, void* value, int type)
{
    switch(type) {
        case INT_TYPE: //int
        {
            *(int*)data = *(int*)value;
            return;
        }
        case BOOL_TYPE: //boolean
            *(bool*)data = *(bool*)value;
            return;
            
        case FLOAT_TYPE: //float
            {

            *(float*)data = *(float*)value;

            }
            return ;
            break;
        case STRING_TYPE: //string
        {
            char* string = *(char**)value;
            *(char**)data = string;
            break;
        }
        case LAMBDA_TYPE: //lambda
            errorExit("cannot have a lambda here");
            break;
        case LIST_TYPE: //list
            return ;
            break;
        case TUPLE_TYPE: //tuple
        {
            TupleType* tp = value;
            *(TupleType**)data = tp;
            return;
        }
    
    }
}







// TODO here
int* convertTypeListToInts(ListType* typeNames, int len)
{
    int *types = malloc(sizeof(int) * len);
    //char *data  = typeNames->data;
    
    
    char *datap = typeNames->data;

    
    if (typeNames->len == 1) {
        for (int i = 0; i < len; i++) {
            types[i] = stringToTyp(getListElement(typeNames, i));
            
        }
    } else {
        for (int i = 0; i < len; i++) {

            types[i] = stringToTyp(getListElement(typeNames, i));
            printf("types i = %d",types[i]);
        }
    }
    return types;
}

size_t getListDataOffsetSize()
{
    return sizeof(int) + sizeof(int) + sizeof(int);
}


size_t getListDataSize(int type, int len)
{
    size_t total = 0;
    total = sizeofType(type);
    return total * len;
}



size_t getListSize(int type, int len)
{
    return getListDataOffsetSize() + getListDataSize(type, len);
}




void* getListElement(ListType* lt, int index)
{
    if (index >= lt->len) errorExit("index greater than length");

    void* data = lt->data;
    return offsetPointerNtimes(data, lt->type, index);
    
}

void setListElement(ListType* lt, int index, void* val)
{
    if (index >= lt->len) errorExit("index greater than length");

    void* data = lt->data;
    data = offsetPointerNtimes(data, lt->type, index);
}



sizeStruct getCsvSize(FILE *file, char* delimeter)
{
    int r = 0;
    int c = 0;
    char line[4098];
	while (fgets(line, 4098, file))
    {
    	
        char* tmp = strdup(line);

	    int j = 0;
        
	    const char* tok;
        const char* ctok;
	    for (tok = strtok(line, "\n"); tok && *tok; r++, tok = strtok(NULL, "\n"))
	    {
            if (c == 0) {
                char* cline = strdup(tok);
                for (ctok = strtok(cline, delimeter); ctok && *ctok; c++, ctok = strtok(NULL, delimeter))
                {
                }
                free(cline);
            }
	    }

        free(tmp);
    }

    sizeStruct s;
    s.row = r;
    s.col = c;
    return s;

}

void readCsv(FILE *file, char ***data, int row, int col, char* delimeter){


    char line[4098];
    int i = 0;

	while (fgets(line, 4098, file))
    {
        
        // double row[ssParams->nreal + 1];
        char* tmp = strdup(line);

	    const char* tok;
        const char* ctok;
	    for (tok = strtok(line, "\n"); tok && *tok; i++, tok = strtok(NULL, "\n"))
	    {
            
        
            int j = 0;
            char* cline = strdup(tok);
            
            for (ctok = strtok(cline, delimeter) ; ctok && *ctok; j++, ctok = strtok(NULL, delimeter))
            {
                data[i][j] = (char *)ctok;
                
            }
	    }

        //free(tmp);
    }
    
}





void valToString(void* val, int type, char* buf)
{

    switch(type) {
        case INT_TYPE: //int
        {
            int i = *(int*) val;
            sprintf(buf, "%d", i);
            break;
        }

        case BOOL_TYPE: //boolean
        {
            bool b = *(bool *) val;
            if (b == false) strcpy(buf, "false");
            else strcpy(buf, "true");
            break;
        }
        case FLOAT_TYPE: //float
         {
            float f = *(float*) val;
            fprintf(stderr, "f is %.2f", f);
            sprintf(buf, "%.2f", f);
            break;
        }
        case STRING_TYPE: //string
            strcpy(buf, val);
            break;
        case LAMBDA_TYPE: //lambda // TODO
            errorExit("cannot have a lambda here");
            break;
        case LIST_TYPE: //list
            return; 
            break;
        case TUPLE_TYPE: //tuple
            return ;
            break;
    
    }
}


void* convertStringtoValue(char* string, int type)
{
    
    switch(type) {  
        case INT_TYPE: //int
        {
            int* ip = malloc(sizeof(int));
            *ip = atoi(string);

            
            return ip;  
        }
        case BOOL_TYPE: //boolean
        {   
            bool *b = malloc(sizeof(bool));
            
            if (strcmp(string, "false") == 0) {
                *b = false;
            } else if (strcmp(string, "false") == 0) {
                *b = true;
            } else errorExit("boolean not recognized as true or false");
            return b;
        }
        case FLOAT_TYPE: //float
        {
            float* fp = malloc(sizeof(float));
            *fp = strtof(string, NULL);
            
            return fp;
        }
        case STRING_TYPE: //string
            return string;
    }
    errorExit("Table type is not a primitive");
    void*sup;
    return sup;
}









// void size(void* tup){
//     TupleType* t = tup;
//     printf("total size %d\n", getTupleSize(t));
//     getDataSize(t);
// }

// int main()
// {
//     TupleType* tup = (TupleType *) malloc(sizeof(TupleType)); sizeof(int) + sizeof(char*) + (2 * sizeof(char*) + sizeof(int)
//     tup->len = 3;
//     tup->type = "sis";
//     tup->data = (char*) malloc(2 * sizeof(char*) + sizeof(int));

// }





// void get(void *data) 
// {
//     return;
//     // ListType *lp = data;
//     // char ty = (lp->type)[0];
//     // switch(ty) {
//     //     case 'i': //int
//     //     {
//     //         int i = ((int *) lp->data)[index];
//     //         int* ret = (int*) malloc(i);
//     //         *ret = i;
//     //         return ret;
//     //         break;
//     //     }
           
//     //     case 'b': //boolean
//     //     {
//     //         bool b = ((bool *) lp->data)[index];
//     //         bool* ret = (bool*) malloc(b);
//     //         *ret = b;
//     //         return ret;
//     //         break;
//     //     }

//     //     case 'f': //float
//     //     {
//     //         float f = ((float *) lp->data)[index];
//     //         float* ret = (float*) malloc(f);
//     //         *ret = f;
//     //         return ret;
//     //         break;
//     //     }

//     //     case 's': //string
//     //         break;
//     //     case 'l': //lambda
//     //         break;
//     //     case 'T': //table
//     //         break;
//     //     case 'U': //tuple
//     //         break;
//     //     case 'L': //list
//     //         break;
//     // }
//     // return malloc(1);

// }

// // list_name.set(void *data, int index, void *val)
// // {

// // }

// // list_name.insert(void *data, int index, void *val)
// // {

// // }

// // list_name.length(void *data)
// // {

// // }

// // list_name.append(void *data, void *val)
// // {

// // }
// // list_name.concat(void *val)
// // {

// // }

// // list_name.copy()

#endif