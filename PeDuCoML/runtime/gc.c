#include <malloc.h>
#include <stdint.h>
#include "gc.h"

static mempool *pool = NULL;
static int64_t stack_bottom = 0;
const size_t INFO_SIZE = sizeof(mm_block);
const size_t INITIAL_SIZE = 4096;

typedef struct
{
    int64_t *data;
    size_t cur_size;
    size_t allocated_size;
} list;

void push_back(list *lst, int64_t elem)
{
    if (lst->data == NULL)
    {
        lst->data = (int64_t *)malloc(4 * sizeof(int64_t));
        lst->cur_size = 0;
        lst->allocated_size = 4;
    }
    else if (lst->cur_size >= lst->allocated_size)
    {
        lst->allocated_size += 4;
        lst->data = (int64_t *)realloc(lst->data, lst->allocated_size * sizeof(int64_t));
    }

    lst->data[lst->cur_size++] = elem;
}

void clear(list *lst)
{
    lst->cur_size = 0;
}

static list *marked_list;

void peducoml_init(int64_t sb)
{
    stack_bottom = sb;
    pool = (mempool *)malloc(sizeof(mempool));
    pool->current_bank = (void *)malloc(INITIAL_SIZE);
    pool->secondary_bank = (void *)malloc(INITIAL_SIZE);

    mm_block *block = pool->current_bank;
    block->is_free = 1;
    block->size = INITIAL_SIZE;

    block = pool->secondary_bank;
    block->is_free = 1;
    block->size = INITIAL_SIZE;

    marked_list = (list *)malloc(sizeof(list));
    marked_list->data = NULL;
    marked_list->cur_size = 0;
    marked_list->allocated_size = 0;
}

int gc_get_stack_bottom()
{
    fprintf(stderr, "Stack bottom: %ld\n", stack_bottom);
}

void peducoml_destroy()
{
    free(pool->current_bank);
    pool->current_bank = NULL;
    free(pool->secondary_bank);
    pool->secondary_bank = NULL;
    free(pool);
    pool = NULL;
}

void *peducoml_alloc_in_bank(size_t size, void *bank)
{
    void *cur = bank;
    mm_block *cur_block = cur;

    while (!cur_block->is_free && cur_block->size < size)
    {
        if (cur > bank + INITIAL_SIZE)
        {
            return NULL;
        }

        cur += cur_block->size + INFO_SIZE;
        cur_block = cur;
    }

    mm_block *next_block = cur + INFO_SIZE + size;
    if (size != cur_block->size)
    {
        if (INFO_SIZE > cur_block->size - size)
        {
            cur_block->size = size;
            cur_block->is_free = 0;
            return cur + INFO_SIZE;
        }
        next_block->is_free = 1;
        next_block->size = cur_block->size - size - INFO_SIZE;
    }

    cur_block->is_free = 0;
    cur_block->size = size;
    return cur + INFO_SIZE;
}

void *peducoml_alloc(size_t size)
{
    void *alloc = peducoml_alloc_in_bank(size, pool->current_bank);
    if (alloc != NULL)
    {
        return alloc;
    }

    alloc = peducoml_alloc_in_bank(size, pool->current_bank);
    if (alloc != NULL)
    {
        return alloc;
    }
    fprintf(stderr, "Heap out of memory\n");
    exit(1);
}

void swap(void *left, void *right)
{
    void *tmp = left;
    left = right;
    right = tmp;
}

void gc()
{
    mm_block *block = pool->secondary_bank;
    block->is_free = 1;
    block->size = INITIAL_SIZE;
    gc_stack_scan(stack_bottom);
    swap(pool->current_bank, pool->secondary_bank);
}

void mark(int64_t ptr)
{
    push_back(marked_list, ptr);
}

void check_pointer(int64_t ptr, int64_t cur_stack)
{
    if (pool->current_bank <= ptr && ptr < pool->current_bank + INITIAL_SIZE)
    {
        mm_block *block = (mm_block *)(ptr - INFO_SIZE);
        void *new_location = peducoml_alloc_in_bank(block->size, pool->secondary_bank);
        return (int64_t)new_location;
    }
    return ptr;
}

void print_int(int64_t s_top)
{
    fprintf(stderr, "Stack top: %ld\n", s_top);
}