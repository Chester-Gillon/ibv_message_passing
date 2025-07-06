/*
 * @file use_all_memory.c
 * @date 6 Jul 2025
 * @author Chester Gillon
 * @brief A program which just tries to keep allocating memory
 * @details Written to investigate the Linux Out Of Memory killer behaviour
 */

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>

#include <time.h>


#define ALLOCATION_SIZE_WORDS (1024 * 1024)

#define PROGRESS_REPORT_INTERVAL_SECS 10


/**
 * @brief A 32-bit Linear congruential generator for creating a pseudo-random test pattern.
 * @details "Numerical Recipes" from https://en.wikipedia.org/wiki/Linear_congruential_generator
 * @param[in/out] seed the LCG value to advance
 */
static inline void linear_congruential_generator (uint32_t *const seed)
{
    *seed = (*seed * 1664525) + 1013904223;
}


int main (int argc, char *argv[])
{
    uint32_t *block;
    uint32_t seed = 0;
    uint64_t total_blocks = 0;
    uint64_t previous_report_blocks = 0;
    struct timespec now;
    struct timespec report_time;

    clock_gettime (CLOCK_MONOTONIC, &now);
    report_time = now;
    report_time.tv_sec += PROGRESS_REPORT_INTERVAL_SECS;

    /* Keep trying to allocate memory until get an allocation failure */
    do
    {
        block = malloc (ALLOCATION_SIZE_WORDS * sizeof (uint32_t));
        if (block != NULL)
        {
            /* Fill the allocated block with a test pattern, so the allocation is resident */
            for (uint32_t word_index = 0; word_index < ALLOCATION_SIZE_WORDS; word_index++)
            {
                block[word_index] = seed;
                linear_congruential_generator (&seed);
            }
            total_blocks++;

            /* Report regular progress on the number of blocks allocated. The change in the number of blocks since the previous
             * report is shown, to allow to determine if the rate of allocation slows down when starts using swap. */
            clock_gettime (CLOCK_MONOTONIC, &now);
            if ((now.tv_sec > report_time.tv_sec) ||
                ((now.tv_sec == report_time.tv_sec) && (now.tv_nsec > report_time.tv_nsec)))
            {
                printf ("Blocks allocated %lu (+%lu)\n", total_blocks, total_blocks - previous_report_blocks);
                previous_report_blocks = total_blocks;
                report_time.tv_sec += PROGRESS_REPORT_INTERVAL_SECS;
            }
        }
    } while (block != NULL);

    printf ("Memory allocation failure after %lu blocks\n", total_blocks);
}
