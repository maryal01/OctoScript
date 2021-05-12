#ifndef TABLE_C
#define TABLE_C



#include "tableHelper.c"
#include "listHelper.c"

#include "tupleHelper.c"
// Reads a csv file and creates a table based on it
void* read(char* filename, void* typeNames, char* delimeter)
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



    ListType* table = createTable(data, s.row, s.col, typeNames);

    return table;
    
}

// Writes the given table to a csv file
void write(void* table, char* filename, char* delimeter)
{

    FILE *file;
	file = fopen(filename, "w");
    printTable(table, file, delimeter);

}

int countRows(ListType* table)
{
    return table->len;
}

int countCols(ListType* table)
{
    TupleType** t = getListElement(table, 0);
    return (*t)->len;
}

bool empty(ListType* table){
    return countRows(table) == 0;
}


ListType* insertRow(ListType* table, int index, TupleType* tup) {
    ListType* new = copyList(table);
    if (countRows(table) != 0) {
        TupleType* t2 = *(TupleType**) getListElement(table, 0);
        for (int i = 0; i < t2->len; i++) {
            if (getTypeofTupleIndex(t2, i) != getTypeofTupleIndex(tup, i)) {
                errorExit("types of inserted tuple do not match types of the table");
            }
        }
    }
    return insertToList(new, index, tup);
}


ListType* appendRow(ListType* table, TupleType* tup) {
    
    return insertRow(table, table->len, tup);
}

ListType* dropRow(ListType* table, int index) {
     
    ListType* new = copyList(table);
    return removeFromList(new, index);
}

ListType* setRow(ListType* table, int index, TupleType* tup) {
     
    table = dropRow(table, index);
    return insertRow(table, index, tup);
}

ListType* insertCol(ListType* table, int index, ListType* lt) {
    ListType* new = copyList(table);
    for (int i = 0; i < table->len; i++) {
        TupleType* t2 = *(TupleType**) getListElement(table, i);
        t2 = tupleInsert(t2, index, getListElement(lt, i), lt->type);

        setListElement(new, i, t2);
    }
    return new;
}



ListType* appendCol(ListType* table, ListType* lt) {
    
    return insertCol(table, table->len, lt);
}

ListType* dropCol(ListType* table, int index) {
     
    ListType* new = copyList(table);
    for (int i = 0; i < table->len; i++) {
        TupleType* t2 = *(TupleType**) getListElement(table, i);
        t2 = tupleRemove(t2, index);
        setListElement(new, i, t2);
    }
    return new;
}

ListType* setCol(ListType* table, int index, ListType* lt) {
     
    table = dropCol(table, index);
    return insertCol(table, index, lt);
}






#endif