#include <stdio.h>
#include <stdint.h>
#include <malloc.h>

typedef struct
{
    int64_t (*func)(int64_t, int64_t, int64_t, int64_t);
    int64_t total_args;
    int64_t *applied_args;
    int64_t len_applied_args;
} closure;

static closure *stored_closures[8];
static int stored_closures_len = 0;

extern int get_closure(int64_t ptr)
{
    for (int i = 0; i < stored_closures_len; i++)
    {
        if (ptr == (int64_t)stored_closures[i])
        {
            return i;
        }
    }
    return -1;
}

extern int64_t peducoml_alloc_closure(int64_t ptr, int64_t total_args)
{
    int index = get_closure(ptr);
    if (index == -1)
    {
        closure *cl = (closure *)malloc(sizeof(closure));
        cl->func = (int64_t(*)(int64_t, int64_t, int64_t, int64_t))ptr;
        cl->total_args = total_args;
        cl->applied_args = NULL;
        cl->len_applied_args = 0;
        stored_closures[stored_closures_len++] = cl;
        return (int64_t)cl;
    }
    return (int64_t)stored_closures[index];
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
        cl->applied_args = (int64_t *)malloc(2 * sizeof(int64_t));
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
        case 4:
            rez = peducoml_apply4(cl);
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