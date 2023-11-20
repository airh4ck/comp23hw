#include <stdio.h>
#include <stdint.h>
#include <malloc.h>

typedef struct closure_struct
{
    int64_t (*func)();
    int64_t total_args;
    int64_t len_applied_args;
    int64_t *applied_args;
} closure;

// static closure *stored_closures[8];
// static int stored_closures_len = 0;

// extern int get_closure(int64_t ptr)
// {
//     for (int i = 0; i < stored_closures_len; i++)
//     {
//         if (ptr == (int64_t)stored_closures[i])
//         {
//             return i;
//         }
//     }
//     return -1;
// }

extern int64_t peducoml_alloc_closure(int64_t ptr, int64_t total_args)
{

    // int index = get_closure(ptr);
    // if (index == -1)
    // {
    //     closure *cl = (closure *)malloc(sizeof(closure));
    //     cl->func = (int64_t(*)(int64_t, int64_t, int64_t, int64_t))ptr;
    //     cl->total_args = total_args;
    //     cl->applied_args = NULL;
    //     cl->len_applied_args = 0;
    //     stored_closures[stored_closures_len++] = cl;
    //     return (int64_t)cl;
    // }
    // return (int64_t)stored_closures[index];
}

static int64_t peducoml_apply2(closure *cl)
{
    return cl->func(cl->applied_args[0], cl->applied_args[1], 0, 0);
}
static int64_t peducoml_apply3(closure *cl)
{
    return cl->func(cl->applied_args[0], cl->applied_args[1], cl->applied_args[2], 0);
}
static int64_t peducoml_apply4(closure *cl)
{
    return cl->func(cl->applied_args[0], cl->applied_args[1], cl->applied_args[2], cl->applied_args[3]);
}

extern int64_t peducoml_apply(int64_t c, int64_t arg)
{
    closure *cl = (closure *)c;
    if (cl->applied_args == NULL)
    {
        cl->applied_args = (int64_t *)malloc(4 * sizeof(int64_t));
    }
    cl->applied_args[cl->len_applied_args] = arg;
    cl->len_applied_args++;

    if (cl->len_applied_args == cl->total_args)
    {
        int64_t rez = INT64_MIN;
        switch (cl->len_applied_args)
        {
        case 2:
            rez = peducoml_apply2(cl);
            break;
        case 3:
            rez = peducoml_apply3(cl);
            break;
        case 4:
            rez = peducoml_apply4(cl);
            break;
        default:
            break;
        }
        free(cl->applied_args);
        free(cl);

        return rez;
    }

    return (int64_t)cl;
}

extern int print_int(int x)
{
    printf("%d", x);
    return 0;
}

extern int print_char(int c)
{
    putchar(c);
    return 0;
}

extern int print_bool(int b)
{
    if (b > 0)
    {
        puts("true");
    }
    else
    {
        puts("false");
    }
    return 0;
}

extern int print_new_line()
{
    printf("\n");
}

// List processing

typedef struct node_struct
{
    int64_t data;
    struct node_struct *next;
} node;

extern int64_t peducoml_alloc_list()
{
    node *address = (node *)malloc(sizeof(node));
    address->data = 0; // The first element of the list is its length. It is 0 when creating the list
    return (int64_t)address;
}

extern int64_t peducoml_add_to_list(int64_t list_ptr, int64_t data)
{
    node *head = (node *)list_ptr;
    node *new_elem = (node *)malloc(sizeof(node));
    new_elem->data = data;
    new_elem->next = head->next;
    head->data++; // Since we add the element, the list's length is increased by 1
    head->next = new_elem;
    return list_ptr;
}

extern int64_t peducoml_field(int64_t list_ptr, int64_t index)
{
    node *head = (node *)list_ptr;
    node *current = head->next;
    for (int i = 0; i < index; i++)
    {
        current = current->next;
    }
    return current->data;
}

extern int64_t peducoml_tail(int64_t list_ptr)
{
    node *head = (node *)list_ptr;
    node *new_elem = (node *)malloc(sizeof(node));
    new_elem->data = head->data - 1;
    new_elem->next = head->next->next;
    return (int64_t)new_elem;
}

extern int64_t peducoml_length(int64_t list_ptr)
{
    node *head = (node *)list_ptr;
    return head->data;
}

extern int64_t print_list(int64_t list_ptr)
{
    node *head = (node *)list_ptr;
    int64_t length = head->data;
    printf("[");
    if (length == 0)
    {
        printf("]");
        return 0;
    }
    node *current = head->next;
    for (int i = 0; i < head->data - 1; i++)
    {
        printf("%d; ", current->data);
        current = current->next;
    }
    printf("%d]", current->data);
    return 0;
}

// Tuple processing

extern int64_t peducoml_alloc_tuple(int64_t cardinality)
{
    int64_t *tuple_ptr = (int64_t *)malloc((cardinality + 1) * sizeof(int64_t));
    tuple_ptr[0] = 0; // The first element of the tuple is the number of elements with which it was filled.
    return (int64_t)tuple_ptr;
}

extern int64_t peducoml_fill_tuple(int64_t ptr, int64_t elem)
{
    int64_t *tuple_ptr = (int64_t *)ptr;
    tuple_ptr[0]++;
    tuple_ptr[tuple_ptr[0]] = elem;
    return ptr;
}

extern int64_t print_tuple(int64_t ptr)
{
    int64_t *tuple_ptr = (int64_t *)ptr;
    int64_t length = tuple_ptr[0];
    printf("(");
    for (int i = 1; i < length; i++)
    {
        printf("%d, ", tuple_ptr[i]);
    }
    printf("%d)", tuple_ptr[length]);
    return 0;
}