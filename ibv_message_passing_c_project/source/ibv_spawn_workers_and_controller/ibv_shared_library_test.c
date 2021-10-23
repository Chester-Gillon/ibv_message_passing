/*
 * @file ibv_shared_library_test.c
 * @date: 23 Oct 2021
 * @author Chester Gillon
 * @brief Implementation of a trivial shared library
 */

#include "ibv_shared_library_test.h"

#include <stdio.h>

void shared_library_test (const char *const calling_executable_name)
{
    printf ("Shared library called from %s\n", calling_executable_name);
}
