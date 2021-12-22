#include <stdio.h>
#include <stdlib.h>
#include <arr.h>

struct Arr *init_arr(struct Arr *arr, int capacity)
{
    arr->len = 0; // since there are no items yet 
    arr->capacity = capacity; 
    arr->data = NULL; // since we have not yet initialized 

    return arr; // return modified arr 
}

struct Arr *init_arr_data(void *data) 
{
    struct Arr *arr = malloc(sizeof(struct Arr)); // allocate necessary space
    arr = init_arr(arr, 16); // for now the default capacity is 16
    arr->data = data; 

    return arr;
}

// what do we do when we want multiple pieces of data 

struct Thunk *makeEmptyList(int ty) {
	struct List *new = malloc(sizeof(struct List));	
	memset(new,0,sizeof(struct List));

	new->start = 0;
	new->end = 0;
	new->curr_index = -1;
	new->last_eval = NULL;
	new->type = LITLIST;
	new->content_type = ty;

	
	return init_thunk_literal(new);
}
