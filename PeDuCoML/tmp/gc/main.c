#include "gc.h"
#include <stdio.h>

int check_arr(void *arr, size_t size)
{
    if (arr != NULL)
    {
        printf("Allocated %ld bytes successfully\n", size);
        return 1;
    }
    printf("Out of memory!\n");
    return 0;
}

void func1()
{
    char *a_arr = (char *)peducoml_alloc(6 * sizeof(char));
}

int main(void)
{
    printf("This is an implementation of a memory manager.\n");
    // peducoml_init();
    gc_init();
    print_blocks();
    func1();
    print_blocks();
    peducoml_destroy();
    return 0;
}
