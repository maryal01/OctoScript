#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#define BUF_SIZE 10

typedef struct sizeStruct {
    int col;
    int row;
} sizeStruct;

typedef struct ListType{
    int self_type;
    int len;
    int type;
    char data[];
} ListType;


typedef struct TableType{
    int len;
    char *type;
    char data[];
} TableType;

typedef struct TupleType{
    int self_type;
    int len;
    char data[];
} TupleType;

TupleType* createTupleFromStrings(char*** data, int row, int col, ListType *typeNames);

void errorExit(const char* message)
{
    fprintf(stderr, "%s\n", message);
    exit(1);
}

void setValue(void* data, void* value, int type)
{
    switch(type) {
        case 0: //int
        {
            *(int*)data = *(int*)value;
        }
        case 1: //boolean
            return ;
            break;
        case 2: //float
            *(float*)data = *(float*)value;
            return ;
            break;
        case 3: //string
        {
            char* string = *(char**)value;
            *(char**)data = string;
            break;
        }
        case 4: //lambda
            errorExit("cannot have a lambda here");
            break;
        case 10: //list
            return ;
            break;
        case 11: //tuple
            return ;
            break;
    
    }
}


size_t getElementSizeFromType(int type)
{
    switch(type) {
        case 0: //int
            return sizeof(int);
            break;
        case 1: //boolean
            return sizeof(bool);
            break;
        case 2: //float
            return sizeof(float);
            break;
        case 3: //string
            return sizeof(char*);
            break;
        case 4: //lambda
            errorExit("cannot have a lambda here");
            break;
        case 10: //list
            return sizeof(void*);
            break;
        case 11: //tuple
            return sizeof(void*);
            break;
    
    }
}




// void *getTupleData(TupleType *t){
//     int size = t->len + 8 * t->len;
//     void *data = t + size;
//     *(int *)(data + 8)
// }


int stringToTyp(char* string)
{
    if (strcmp(string, "int") == 0) {
        fprintf(stderr, "int\n");
        return 0;
    } else if (strcmp(string, "boolean") == 0){
        fprintf(stderr, "boolt\n");
        return 1;
    } else if (strcmp(string, "float") == 0){
            fprintf(stderr, "float\n");
        return 2;
    } else if (strcmp(string, "string") == 0){
            fprintf(stderr, "string\n");
        return 3;
    }
        
    fprintf(stderr, "unrecognized string %s in type list of read", string);
    //exit(1);
}



int* convertTypeList(ListType* typeNames, int len)
{
    int *types = malloc(sizeof(int) * len);
    //char *data  = typeNames->data;
    
    
    char *datap = typeNames->data;

    // if (typeNames->len == 1) {
    //     for (int i = 0; i < len; i++) {
    //         types[i] = stringToTyp(typeNames->data[0]);
            
    //     }
    // } else {
    //     fprintf(stderr, "Now here\n");
    //     for (int i = 0; i < len; i++) {

    //         types[i] = stringToTyp(typeNames->data[0]);
    //         printf("types i = %d",types[i]);
    //     }
    // }
}

size_t getListDataOffsetSize()
{
    return sizeof(int) + sizeof(int) + sizeof(int);
}

size_t getListDataSize(int type, int len)
{
    size_t total = 0;
    total = getElementSizeFromType(type);
    return total * len;
}



size_t getListSize(int type, int len)
{
    return getListDataOffsetSize() + getListDataSize(type, len);
}

size_t getTableSize(int len)
{
    return getListSize(11, len);
}

void* offsetPointerNtimes(void* data, size_t offset, int n)
{
    bool* datab = data;
    datab += offset * n;
    return datab;
}


void* getListElement(int index, ListType* lt)
{
    if (index >= lt->len) errorExit("index greater than length");

    void* data = lt->data;
    size_t offsetSize = getElementSizeFromType(lt->type);
    return offsetPointerNtimes(data, offsetSize, index);
    
}

void* getTupleElement(int index, TupleType* tt)
{
    
    if (index >= tt->len) errorExit("index greater than length");

    void* data = tt->data;

    size_t offsetSize = tt->len * getElementSizeFromType(0);

    for (int i = 0; i < index; i++) {
        
        offsetSize += getElementSizeFromType(tt->data[i]);

    }
    return offsetPointerNtimes(data, offsetSize, 1);
    
}






size_t getTupleDataOffsetSize(int len)
{
    return sizeof(int) + sizeof(len) + sizeof(int) * len;
}

size_t getTupleDataSize(int* types, int len)
{
    size_t total = 0;
    for (int i = 0; i < len; i++) {
        total += getElementSizeFromType(types[i]);
    }
    return total;
    
}
size_t getTupleSize(int* types, int len)
{
    return getTupleDataOffsetSize(len) + getTupleDataSize(types, len);
}

// int get(void *data, int index) {
//     TupleType *lp = data;
    
//     char buf[BUF_SIZE];

//     char* tmp = ((char**)lp->data)[0];

//     fprintf(stderr, "index %s, type %s, first value %s\n",
//             tmp, lp->type, buf);

//             return 5;
// }


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



void* read(char* filename, void* typeNames, bool header, char* delimeter)
{
    FILE *file;
	file = fopen(filename, "r");

    sizeStruct s = getCsvSize(file, ",");
    fclose(file);
    file = fopen(filename, "r");

    ListType *tyname = typeNames;
    if(tyname->len != 1 && tyname->len != s.col) {
        errorExit("Error in reading table. Csv file has unequal columns to type list given");
    }

    char ***data;
    data = (char ***) malloc(s.row * sizeof(char **));
	for (int i = 0; i < s.row; i++){
		data[i] = (char **)malloc(s.col * sizeof(char*));
	}

    readCsv(file, data, s.row, s.col, delimeter);

    // int **data;
    // data = (int **) malloc(s.row * sizeof(int *));
	// for (int i = 0; i < s.row; i++){
	// 	data[i] = (int *)malloc(s.col * sizeof(int));
	// }

    

    //readCsv(file, filename, s.row, s.col, delimeter);

    // for (int i = 0; i < s.row; i++) {
    //     for (int j = 0; j < s.col; j++) {
            
    //         char* h = "hello";
    //         //data[i][j] = malloc(8);

    //         data[i][j] = h;
    //     }
    // }

    



    TupleType* tt = createTupleFromStrings(data, 0, s.col, typeNames);
    return tt;
    
}


void* convertStringtoValue(char* string, int type)
{
    switch(type) {  
        case 0: //int
        {
            int* ip = malloc(sizeof(int));
            *ip = atoi(string);

            
            return ip;  
        }
        case 1: //boolean
        {   
            bool *b = malloc(sizeof(bool));
            
            if (strcmp(string, "false") == 0) {
                *b = false;
            } else if (strcmp(string, "false") == 0) {
                *b = true;
            } else errorExit("boolean not recognized as true or false");
            return b;
        }
        case 2: //float
        {
            float* fp = malloc(sizeof(float));
            *fp = strtof(string, NULL);
            
            return fp;
        }
        case 3: //string
            return string;
    }
    errorExit("Table type is not a primitive");
}

TupleType* createTupleFromStrings(char*** data, int row, int col, ListType *typeNames)
{
    
    size_t size = getTupleSize((int*)(void*)typeNames->data, col);

    TupleType* tup = malloc(size);
    tup->self_type = 11;
    tup->len = col;
    
    void *datap = tup->data;
   int* types = ((int*)(void*)typeNames->data);
    for (int i = 0; i < tup->len; i++){
        setValue(datap, offsetPointerNtimes(types, sizeof(int), i), 0);
        datap = offsetPointerNtimes(datap, sizeof(int), 1);
    }
    
    
    
    for (int i = 0; i < tup->len; i++) {

        setValue(datap, convertStringtoValue(data[row][i], types[i]), types[i]);
        datap = offsetPointerNtimes(datap, getElementSizeFromType(types[i]), 1);
    }
    return tup;
}





ListType* createTable(int col, int row)
{
    size_t size = getTableSize(col);
    ListType *table = malloc(size);
    table->len = col;
    table->self_type = 10;
    table->type = 11;

}




void convert2dtoTable(char*** data, ListType* typeNames, sizeStruct s)
{
    TableType* tt = (TableType *) malloc(sizeof(TableType));
    tt->len = s.col;
    tt->type = "L";
    char* typeList = convertTypeList(typeNames, s.col);

    ListType* lt = (ListType *) malloc(sizeof(ListType));
    tt->len = s.row;
    tt->type = "U";
    // todo assign later tt->data = // 

    for (int i = 0; i < s.row; i++) {
        TupleType* tup = (TupleType *) malloc(sizeof(TupleType));
        tup->len = s.row;
        //tup->type = typeList;
        for (int j = 0; j < s.col; j++) {
            char sym = typeList[j];
            switch(sym) {
                case 'i': //int
                    
                    break;
                case 'b': //boolean
                    break;
                case 'f': //float
                    break;
                case 's': //string
                    break;
                case 'T': //table
                    break;
                case 'U': //tuple
                    break;
                case 'L': //list
                    break;
            }
        }
    }

} 

void tup(TupleType* tt) {

    printf("value = %d\n", ((int*)tt->data)[0]);
    printf("value = %d\n", *(int*)getTupleElement(0, tt));
    int *m = malloc(sizeof(int));
    *m = 4;
    setValue(offsetPointerNtimes(tt->data, sizeof(int), tt->len), m, 0);
    printf("value = %d\n", *(int*)getTupleElement(0, tt));
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

