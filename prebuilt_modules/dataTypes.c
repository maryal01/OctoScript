#ifndef DATA_TYPES_C
#define DATA_TYPES_C



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

typedef struct TupleType{
    int self_type;
    int len;
    char data[];
} TupleType;

#endif
