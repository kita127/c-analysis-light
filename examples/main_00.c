#include <stdio.h>
#include    "../../external_inc.h"

#define HOGE    (1)
#define FUGA    (2)
#define VARI    HOGE


#if VARI == HOGE
char hoge_globvar = 100;
  #if VARI_2 == HOGE
static int hoge_globvar_2 = xxx;
  #endif
#endif    /* VARI */

int arr_var[10];

int main( void )
{
    int local_var;
    local_var = 0;
    local_var++;

    printf("local_var...%d\n", local_var);

    return (0);
}
