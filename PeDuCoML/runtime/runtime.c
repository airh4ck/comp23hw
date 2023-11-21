#include <stdio.h>
#include <stdint.h>
#include <malloc.h>
#include <stdlib.h>

// Application processing

typedef struct closure_struct
{
    int64_t (*func)();
    int64_t total_args;
    int64_t len_applied_args;
    int64_t *applied_args;
} closure;

static int64_t *closure_ptrs = NULL;
static int64_t stored_ptrs_len = 0;

static int64_t get_closure(int64_t ptr)
{
    for (int64_t i = 0; i < stored_ptrs_len; i++)
    {
        if (closure_ptrs[i] == ptr)
        {
            return ptr;
        }
    }

    return 0;
}

static void realloc_ptrs_storage()
{
    int64_t *new_storage = (int64_t *)malloc((stored_ptrs_len + 64) * sizeof(int64_t));
    int64_t new_storage_index = 0;
    int64_t last_stored_ptrs_len = stored_ptrs_len;
    for (int64_t i = 0; i < last_stored_ptrs_len; i++)
    {
        if (closure_ptrs[i] == 0)
        {
            stored_ptrs_len--;
            continue;
        }

        new_storage[new_storage_index] = closure_ptrs[i];
        new_storage_index++;
    }

    free(closure_ptrs);

    closure_ptrs = new_storage;
}

static void add_to_closure_ptrs(int64_t ptr)
{
    if (stored_ptrs_len == 0)
    {
        closure_ptrs = (int64_t *)malloc(64 * sizeof(int64_t));
    }

    closure_ptrs[stored_ptrs_len] = ptr;
    stored_ptrs_len++;
    if (stored_ptrs_len % 64 == 0)
    {
        realloc_ptrs_storage();
    }
}

static void remove_from_closure_ptrs(int64_t ptr)
{
    for (int64_t i = 0; i < stored_ptrs_len; i++)
    {
        if (closure_ptrs[i] == ptr)
        {
            closure_ptrs[i] = 0;
        }
    }
}

extern int64_t peducoml_alloc_closure(int64_t ptr, int64_t total_args)
{
    int64_t closure_if_exists = get_closure(ptr);
    if (closure_if_exists != 0)
    {
        return closure_if_exists;
    }

    closure *closure_ptr = (closure *)malloc(sizeof(closure));
    closure_ptr->func = (int64_t(*)())ptr;
    closure_ptr->total_args = total_args;
    closure_ptr->len_applied_args = 0;
    closure_ptr->applied_args = (int64_t *)malloc(total_args * sizeof(int64_t));
    add_to_closure_ptrs((int64_t)closure_ptr);

    return (int64_t)closure_ptr;
}

static int64_t peducoml_apply1(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0]);
}

static int64_t peducoml_apply2(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1]);
}

static int64_t peducoml_apply3(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2]);
}

static int64_t peducoml_apply4(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3]);
}

static int64_t peducoml_apply5(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4]);
}

static int64_t peducoml_apply6(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5]);
}

static int64_t peducoml_apply7(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5],
        closure_ptr->applied_args[6]);
}

static int64_t peducoml_apply8(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5],
        closure_ptr->applied_args[6],
        closure_ptr->applied_args[7]);
}

static int64_t peducoml_apply9(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5],
        closure_ptr->applied_args[6],
        closure_ptr->applied_args[7],
        closure_ptr->applied_args[8]);
}

static int64_t peducoml_apply10(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5],
        closure_ptr->applied_args[6],
        closure_ptr->applied_args[7],
        closure_ptr->applied_args[8],
        closure_ptr->applied_args[9]);
}

static int64_t peducoml_apply11(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5],
        closure_ptr->applied_args[6],
        closure_ptr->applied_args[7],
        closure_ptr->applied_args[8],
        closure_ptr->applied_args[9],
        closure_ptr->applied_args[10]);
}

static int64_t peducoml_apply12(closure *closure_ptr)
{
    return closure_ptr->func(
        closure_ptr->applied_args[0],
        closure_ptr->applied_args[1],
        closure_ptr->applied_args[2],
        closure_ptr->applied_args[3],
        closure_ptr->applied_args[4],
        closure_ptr->applied_args[5],
        closure_ptr->applied_args[6],
        closure_ptr->applied_args[7],
        closure_ptr->applied_args[8],
        closure_ptr->applied_args[9],
        closure_ptr->applied_args[10],
        closure_ptr->applied_args[11]);
}

extern int64_t peducoml_apply(int64_t ptr, int64_t arg)
{

    closure *closure_ptr = (closure *)ptr;
    closure_ptr->applied_args[closure_ptr->len_applied_args] = arg;
    closure_ptr->len_applied_args++;
    if (closure_ptr->len_applied_args == closure_ptr->total_args)
    {
        int64_t result = 0;
        switch (closure_ptr->total_args)
        {
        case 1:
            result = peducoml_apply1(closure_ptr);
            break;
        case 2:
            result = peducoml_apply2(closure_ptr);
            break;
        case 3:
            result = peducoml_apply3(closure_ptr);
            break;
        case 4:
            result = peducoml_apply4(closure_ptr);
            break;
        case 5:
            result = peducoml_apply5(closure_ptr);
            break;
        case 6:
            result = peducoml_apply6(closure_ptr);
            break;
        case 7:
            result = peducoml_apply7(closure_ptr);
            break;
        case 8:
            result = peducoml_apply8(closure_ptr);
            break;
        case 9:
            result = peducoml_apply9(closure_ptr);
            break;
        case 10:
            result = peducoml_apply10(closure_ptr);
            break;
        case 11:
            result = peducoml_apply11(closure_ptr);
            break;
        case 12:
            result = peducoml_apply12(closure_ptr);
            break;
        default:
            break;
        }

        free(closure_ptr->applied_args);
        free(closure_ptr);

        return result;
    }

    return ptr;
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
    address->next = NULL;
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
    for (int64_t i = 0; i < index; i++)
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
    for (int64_t i = 0; i < head->data - 1; i++)
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
    for (int64_t i = 1; i < length; i++)
    {
        printf("%d, ", tuple_ptr[i]);
    }
    printf("%d)", tuple_ptr[length]);
    return 0;
}

// Stdlib functions

extern int64_t print_int(int64_t x)
{
    printf("%d", x);
    return 0;
}

extern int64_t print_char(int64_t c)
{
    putchar(c);
    return 0;
}

extern int64_t print_bool(int64_t b)
{
    if (b > 0)
    {
        printf("true");
    }
    else
    {
        printf("false");
    }
    return 0;
}

extern int64_t print_new_line()
{
    printf("\n");
    return 0;
}