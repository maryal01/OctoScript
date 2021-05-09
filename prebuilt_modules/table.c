#ifndef TABLE_C
#define TABLE_C



#include "tableHelper.c"

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


#endif