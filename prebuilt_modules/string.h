#ifndef STRING_H
#define STRING_H

#include <ctype.h>
#include <string.h>
char* substring(char* string, int start, int end) ;

char* concat(char* s1, char* s2) ;

bool stringEquals(char* s1, char* s2);

char* toLower(char* string);

char* toUpper(char* string);

char* toString(void* val, int type);
char* intToString(void* val);
char* floatToString(void* val);

char* boolToString(void* val);
int stringCmp(char* string1, char* string2);

#endif