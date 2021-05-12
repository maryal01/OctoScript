#ifndef DATA_TYPES_H
#define DATA_TYPES_H



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
