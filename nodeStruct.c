#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

struct Node {
    struct Node *next;
    int val;
};

struct Node *createNode(int x)
{
    // stack allocation of a struct
    // struct Node p1;

    // static allocation of a struct
    // static struct Node p2;

    // heap allocation of a struct
    struct Node *node = malloc(sizeof(struct Node));
    if (!node) {
        return NULL;
    }

    node->val = x;
    node->next = NULL;
    return node;
}

int main()
{
    struct Node *node = createNode(100);
    /*
    if (!node) {
        exit(1);
    }
    */
    assert(node);
    printf("%d\n", node->val);
    free(node);
}
