/*
 * @file ibv_shared_library_user.c
 * @date 23 Oct 2021
 * @author Chester Gillon
 * @brief Trivial program which can be spawned and makes use of a shared library not on the system library path.
 */

#include "ibv_shared_library_test.h"

int main (int argc, char *argv[])
{
    shared_library_test (argv[0]);
}
