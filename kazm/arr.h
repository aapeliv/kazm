#ifndef ARR
#define ARR

struct Arr {
    int len;
    int capacity;
    void *data;
};

struct Arr *init_arr(struct Arr *arr, int capacity); 
struct Arr *init_arr_data(void *data);
// we do not as of now support initialization with data
// how will the data actually look like inside an Arr 
// also we should have multiple different values within a single Arr 

#endif

