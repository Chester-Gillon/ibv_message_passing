/*
 * @file coverage_for_continue.c
 * @date 29 Jan 2022
 * @author Chester Gillon
 * @brief Program to try and demonstrate a coverage issue for a function which uses continue in a loop
 */

#include <stdbool.h>
#include <stdint.h>
#include <math.h>


typedef struct
{
    bool input_valid;
    float input;
    float output;
} data_t;

/* For the test data there are no elements with input_valid false, which means the coverage for the first
 * condtional continue in processing_function() should::
 * - Should not show full branch coverage
 * - Should not show the continue as covered.
 */
static data_t test_data[] =
{
    {.input_valid = true, .input = 1.2345f},
    {.input_valid = true, .input = INFINITY},
    {.input_valid = true, .input = NAN},
    {.input_valid = true, .input = -2.3455f}
};


static void processing_function (data_t *const elements, const uint32_t num_elements)
{
    for (uint32_t index = 0; index < num_elements; index++)
    {
        data_t *const element = &elements[index];

        if (!element->input_valid)
        {
            continue;
        }

        if (isnan (element->input))
        {
            continue;
        }

        if (isinf (element->input))
        {
            continue;
        }

        element->output = element->input * element->input;
    }
}


int main (int argc, char *argv[])
{
    const uint32_t num_elements = sizeof (test_data) / sizeof (test_data[0]);

    processing_function (test_data, num_elements);

    return test_data[0].input_valid;
}
