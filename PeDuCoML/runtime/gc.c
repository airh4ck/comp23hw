#include <malloc.h>
#include <stdint.h>
#include "gc.h"

static mempool *pool = NULL;
static int64_t stack_bottom = 0;
const size_t INFO_SIZE = sizeof(mm_block);
const size_t INITIAL_SIZE = 4096;

void peducoml_init(int64_t sb)
{
    stack_bottom = sb;
    pool = (mempool *)malloc(sizeof(mempool));
    pool->current_bank = (void *)malloc(INITIAL_SIZE);
    pool->secondary_bank = (void *)malloc(INITIAL_SIZE);

    mm_block *block = pool->current_bank;
    block->is_free = 1;
    block->size = INITIAL_SIZE;
    // TODO: mark the second bank too
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

void *peducoml_alloc(size_t size)
{
    void *cur = pool->current_bank;
    mm_block *cur_block = cur;

    while (!cur_block->is_free && cur_block->size < size)
    {
        if (cur > pool->current_bank + INITIAL_SIZE)
        {
            // that's where the garbage collection kicks in
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

void gc()
{
    gc_stack_scan(stack_bottom);
}

void check_pointer(int64_t ptr, int64_t cur_stack)
{
    fprintf(stderr, "Current stack pointer: %ld\n", cur_stack);
    fprintf(stderr, "Found this on stack: %ld\n", ptr);
}

void print_int(int64_t s_top)
{
    fprintf(stderr, "Stack top: %ld\n", s_top);
}