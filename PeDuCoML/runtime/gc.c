#include <malloc.h>
#include <stdint.h>

typedef struct
{
    int64_t *current_bank;
    int64_t *secondary_bank;
} mempool;

static mempool *pool = NULL;

void peducoml_init(size_t pool_size)
{
    pool = (mempool *)malloc(sizeof(mempool));
    pool->current_bank = (int64_t *)malloc(pool_size);
    pool->secondary_bank = (int64_t *)malloc(pool_size);
}

void peducoml_destroy()
{
    free(pool->current_bank);
    free(pool->secondary_bank);
    free(pool);
}
