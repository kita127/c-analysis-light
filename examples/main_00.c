#include <stdio.h>

#define CONDITION       HOGE
#define HOGE        1
#define FUGA        2


int main( void )
{
    int local_var;

    local_var = 1 + 2;

    printf("local_var ...%d\n", local_var);

    return (0);
}
