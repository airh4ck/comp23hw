#include <malloc.h>
#include <stdint.h>
#include "gc.h"

static mempool *pool = NULL;
static int64_t stack_bottom = 0;
const size_t INFO_SIZE = sizeof(mm_block);
const size_t INITIAL_SIZE = 64;

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

    while (cur < bank + INITIAL_SIZE && (!cur_block->is_free || cur_block->size < size))
    {
        cur += cur_block->size + INFO_SIZE;
        cur_block = cur;
    }

    if (cur >= bank + INITIAL_SIZE)
    {
        return NULL;
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

void print_blocks()
{
    fprintf(stderr, "Printing the blocks:\n");
    void *current = pool->current_bank;
    mm_block *block = current;
    int cnt = 0;
    while (current < pool->current_bank + INITIAL_SIZE)
    {
        fprintf(stderr, "Block %ld: pointer=%ld is_free=%ld size=%ld\n", cnt++, current + INFO_SIZE, block->is_free, block->size);
        current += block->size + INFO_SIZE;
        block = current;
    }
}

void *peducoml_alloc(size_t size)
{
    void *alloc = peducoml_alloc_in_bank(size, pool->current_bank);
    if (alloc != NULL)
    {
        fprintf(stderr, "Returning allocated addr: %ld\n", alloc);
        return alloc;
    }

    gc();
    alloc = peducoml_alloc_in_bank(size, pool->current_bank);
    if (alloc != NULL)
    {
        fprintf(stderr, "Returning allocated addr: %ld\n", alloc);
        return alloc;
    }
    fprintf(stderr, "Heap out of memory\n");
    exit(1);
}

void swap(void *left, void *right)
void peducoml_free(void *ptr)
{
    int64_t tmp = left;
    left = right;
    right = tmp;
    if (ptr == NULL)
        return;

    mm_block *block_info = ptr - INFO_SIZE;
    block_info->is_free = 1;
    mm_block *next_block = ptr + block_info->size;

    if (next_block->is_free)
    {
        block_info->size = block_info->size + next_block->size + INFO_SIZE;
    }

    void *current = pool->current_bank;
    mm_block *prev_block = pool->current_bank;
    while (current < pool->current_bank + INITIAL_SIZE)
    {
        if (current + INFO_SIZE + prev_block->size == ptr - INFO_SIZE && prev_block->is_free)
        {
            prev_block->size += block_info->size + INFO_SIZE;

            return;
        }
        current += INFO_SIZE + prev_block->size;
        prev_block = current;
    }
}

void gc()
{
    fprintf(stderr, "INFO_SIZE is %ld\n", INFO_SIZE);
    fprintf(stderr, "Starting garbage collection\n");
    print_blocks();
    mm_block *block = pool->secondary_bank;
    block->is_free = 1;
    block->size = INITIAL_SIZE;
    gc_stack_scan(stack_bottom);
    fprintf(stderr, "Before swap: cur=%ld sec=%ld\n", pool->current_bank, pool->secondary_bank);
    // swap(pool->current_bank, pool->secondary_bank);
    void *tmp = pool->current_bank;
    pool->current_bank = pool->secondary_bank;
    pool->secondary_bank = tmp;
    fprintf(stderr, "After swap: cur=%ld sec=%ld\n", pool->current_bank, pool->secondary_bank);
}

void check_pointer(int64_t ptr, int64_t cur_stack)
{
    fprintf(stderr, "Current pool range is [%ld, %ld)\n", pool->current_bank, pool->current_bank + INITIAL_SIZE);
    fprintf(stderr, "Checking pointer %ld\n", ptr);
    if (pool->current_bank <= ptr && ptr < pool->current_bank + INITIAL_SIZE)
    {
        fprintf(stderr, "Pointer %ld\n is a pointer to the heap\n", ptr);
        mm_block *block = (mm_block *)(ptr - INFO_SIZE);
        void *new_location = peducoml_alloc_in_bank(block->size, pool->secondary_bank);
        return (int64_t)new_location;
    }
    return ptr;
}