/*
 * @file floor_it.c
 * @date
 * @author Chester Gillon
 * @brief Investigate the instructions generated for a floorf() function call casted to an int result
 */

#include <stdio.h>
#include <math.h>

#define NUM_ITEMS 4


int main (int argc, char *argv[])
{
    const float float_in[NUM_ITEMS] = {-1.1, -0.9, -0.1, 0.1};
    int int_out[NUM_ITEMS];
    int index;

    for (index = 0; index < NUM_ITEMS; index++)
    {
        int_out[index] = (int) floorf (float_in[index]);
    }

    printf ("Float inputs =");
    for (index = 0; index < NUM_ITEMS; index++)
    {
        printf (" %g", float_in[index]);
    }
    printf ("\n");

    printf ("(int) floorf() outputs =");
    for (index = 0; index < NUM_ITEMS; index++)
    {
        printf (" %d", int_out[index]);
    }
    printf ("\n");

    return 0;
}
