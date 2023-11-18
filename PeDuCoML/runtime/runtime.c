#include <stdio.h>

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