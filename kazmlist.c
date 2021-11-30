#include <stdio.h>
#include <stdlib.h>

struct Node {
    void *data;
    struct Node *next;
};

struct List {
    struct Node *head;
};

static inline void initList(struct List *list)
{
    list->head = 0;
}


struct Node *addFront(struct List *list, void *data){
    if (!data)
        return NULL;
    struct Node *p = malloc(sizeof(struct Node));
    if (!p)
        return NULL;
    p->data = data;
    p->next = list->head;
    list->head = p;
    return p;
}

void traverseList(struct List *list, void (*f)(void *)){
    struct Node *orig = list->head;
    while (orig != NULL){
        f(orig->data);
        orig = orig->next;
    }
}

struct Node *findNode(struct List *list, const void *dataSought, int (*compare)(const void * const void *)){
    struct Node *orig = list->head;
    while (orig != NULL){
        if (compar(orig->data, dataSought)){
            return orig;
        }
        orig = orig->next;
    }
    return NULL;
}

void flipSignDouble(void *data){
    double *a = (double *)data;
    *a *= -1;
    data = (void *)a;
}

int compareDouble(const void *data1, const void *data2){
    return *(double *)data1 == *(double *)data2;
}

void *popFront(struct List *list){
    if (isEmptyList(list))
        return NULL;
    void *p = (list->head)->data;
    struct Node *removed = list->head;
    list->head = (list->head)->next;
    free(removed);
    return p;
}

void removeAllNodes(struct list *list){
    while (list->head != NULL)
        popFront(list);
}

struct Node *addAfter(struct List *list, struct Node *prevNode, void *data){
    struct Node *p;
    if (prevNode == NULL){
        p = addFront(list, data);
        return p;
    }
    p = malloc(sizeof(struct Node));
    p->data = data;
    struct Node *orig = list->head;
    while (orig){
        if (orig == prevNode){
            struct Node *nxt = prevNode->next;
            prevNode->next = p;
            p->next = nxt;
        }
        orig = orig->next;
    }
    return p;
}

void reverseList(struct List *list){
    struct Node *prv = NULL;
    struct Node *cur = list->head;
    struct Node *nxt;
    while (cur){
        nxt = cur->next;
        cur->next = prv;
        prv = cur;
        cur = nxt;
    }
    list->head = prv;
}
