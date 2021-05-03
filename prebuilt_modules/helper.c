#ifndef HELPER_C
#define HELPER_C

// converts strings representation of a type into its int representation
int stringToTyp(char* string);

// returns the byte size given integer representation of type
size_t sizeofType(int type);

// offsets a pointer n times based on type
void* offsetPointerNtimes(void* data, int type, int n);

// offsets pointer once based on type
void* offsetPointer(void* data, int type);

void errorExit(const char* message)
{
    fprintf(stderr, "%s\nThe program may continue to run after an error was detected, and can cause undefined behaviour.\n", message);
    exit(1);
}

size_t sizeofType(int type)
{
    switch(type) {
        case INT_TYPE: //int
            return sizeof(int);
        case BOOL_TYPE: //boolean
            return sizeof(bool);
        case FLOAT_TYPE: //float
            return sizeof(float);
        case STRING_TYPE: //string
            return sizeof(char*);
        case LAMBDA_TYPE: //lambda
            errorExit("cannot have a lambda here");
        case LIST_TYPE: //list
            return sizeof(void*);
        case TUPLE_TYPE: //tuple
            return sizeof(void*);    
    }
    errorExit("type not recognized");
    return 0;
}

// TODO here
int stringToTyp(char* string)
{
    if (strcmp(string, "int") == 0) {
        return INT_TYPE;
    } else if (strcmp(string, "boolean") == 0){
        return BOOL_TYPE;
    } else if (strcmp(string, "float") == 0){
        return FLOAT_TYPE;
    } else if (strcmp(string, "string") == 0){
        return STRING_TYPE;
    }
        
    errorExit("unrecognized string in type list of read");
    return 0;
}

void* offsetPointerNtimes(void* data, int type, int n)
{
    bool* datab = data;
    size_t offset = sizeofType(type);
    datab += offset * n;
    return datab;
}

void* offsetPointer(void* data, int type)
{
    return offsetPointerNtimes(data, type, 1);
}

void* makePointerOutOfValue(int type, va_list args) // TODO
{

    size_t s = sizeofType(type);
    switch(type) {
        case INT_TYPE: //int
        {
            int i = va_arg(args, int);
            int* ip = malloc(s);
            *ip = i;
            return ip;
        }
            
        case BOOL_TYPE: 
        {
            bool b = va_arg(args, int);
            bool* bp = malloc(s);
            *bp = b;
            return bp;
        }
        case FLOAT_TYPE: //float
        {
            float f = va_arg(args, double);
            float* fp = malloc(s);
            *fp = f;
            return fp;
        }
        // case STRING_TYPE: //string
        //     return sizeof(char*);
        // case LAMBDA_TYPE: //lambda
        //     errorExit("cannot have a lambda here");
        // case LIST_TYPE: //list
        //     return sizeof(void*);
        // case TUPLE_TYPE: //tuple
        //     return sizeof(void*);    
    }
    void* v = NULL;
    return v;
}

#endif