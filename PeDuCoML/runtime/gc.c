#include <malloc.h>
#include <stdint.h>

typedef struct
{
    int is_free;
    size_t size;
} mm_block;

const size_t INFO_SIZE = sizeof(mm_block);

typedef struct
{
    void *current_bank;
    void *secondary_bank;
} mempool;

static mempool *pool = NULL;

void peducoml_init(size_t pool_size)
{
    pool = (mempool *)malloc(sizeof(mempool));
    pool->current_bank = (void *)malloc(pool_size);
    pool->secondary_bank = (void *)malloc(pool_size);
}

void peducoml_destroy()
{
    free(pool->current_bank);
    free(pool->secondary_bank);
    free(pool);
}

void *peducoml_alloc(size_t size)
{
    void *cur = pool->current_bank;
    mm_block *cur_block = pool->current_bank;

    size_t pool_size = sizeof(pool->current_bank);

    while (!cur_block->is_free && cur_block->size < size)
    {
        if (cur > pool->current_bank + pool_size)
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
