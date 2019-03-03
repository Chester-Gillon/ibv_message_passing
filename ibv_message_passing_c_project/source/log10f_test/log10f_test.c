/*
 * @file log10f_test.c
 * @date
 * @author Chester Gillon
 * @brief Investigate the instructions generated for a log10f()
 */

#include <stdio.h>
#include <math.h>

#define NUM_ITEMS 256

float float_in[NUM_ITEMS];
float float_out[NUM_ITEMS];

int main (int argc, char *argv[])
{
    int index;

    for (index = 0; index < NUM_ITEMS; index++)
    {
        float_in[index] = 1.0f + (index * 0.1f);
    }

    for (index = 0; index < NUM_ITEMS; index++)
    {
        float_out[index] = log10f (float_in[index]);
    }

    printf ("Float inputs =");
    for (index = 0; index < NUM_ITEMS; index++)
    {
        printf (" %g", float_in[index]);
    }
    printf ("\n");

    printf ("log10f() outputs =");
    for (index = 0; index < NUM_ITEMS; index++)
    {
        printf (" %g", float_out[index]);
    }
    printf ("\n");

    return 0;
}
