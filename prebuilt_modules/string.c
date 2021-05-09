#ifndef STRING_C
#define STRING_C

#include <ctype.h>
#include <string.h>
char* substring(char* string, int start, int end) {
    
    if (start >= end) errorExit(" start and end values invalidate precondition");

    int len = end - start;
    char *subbuff = malloc(len + 1);
    for (int i = 0; i < len; i++) {
        subbuff[i] = string[start + i];
    }
    subbuff[len] = '\0';
    return subbuff;
}


char* concat(char* s1, char* s2) {

    int len1 = strlen(s1);
    int len2 = strlen(s2);


    char *result = malloc(len1 + len2 + 1);
    strcpy(result, s1);
    strcat(result, s2);

    result[len1 + len2] = '\0';
    
    return result;

}

int length(char* string) {
    return strlen(string);

}


char* toLower(char* string) {

    int len = strlen(string);
    char *subbuff = malloc(len + 1);
    for (int i = 0; i < len; i++) {
        subbuff[i] = tolower(string[i]);
    }
    subbuff[len] = '\0';
    return subbuff;
}

char* toUpper(char* string) {
    
    int len = strlen(string);
    char *subbuff = malloc(len + 1);
    for (int i = 0; i < len; i++) {
        subbuff[i] = toupper(string[i]);
    }
    subbuff[len] = '\0';
    return subbuff;
    
}

char* toString(void* val, int type) {
    
    char *buf = malloc(BUF_SIZE);
    valToString(val, type, buf);
    return buf;
}

char* intToString(void* val) {
    return toString(val, INT_TYPE);
}

char* floatToString(void* val) {
    return toString(val, FLOAT_TYPE);
}

char* boolToString(void* val) {
    return toString(val, BOOL_TYPE);
}

int stringCmp(char* string1, char* string2){
    return strcmp(string1, string2);
}

#endif