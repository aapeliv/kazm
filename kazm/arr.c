#include <stdio.h>
#include <stdlib.h>

struct Arr {
    int len;
    int capacity;
    void *data;
};

void add(struct Arr *a, void *d){
    Arr *arr = (Arr *)a;
    int len = arr->len;
    int cap = arr->capacity;
    if (len != cap){
        
    }
}

int len(strucxt Arr *a){
    return a->len;
}


