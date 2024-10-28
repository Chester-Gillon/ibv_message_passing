/*
 * @file read_smi_count.c
 * @date 8 Jan 2023
 * @author Chester Gillon
 * @brief Display the current SMI count, which is the number of System Management Interrupts which have occurred
 * @details
 *   This was written for an Intel processor.
 *   Running on a AMD Ryzen 5 2400G causes the pread() to fail with EIO
 *
 *   https://stackoverflow.com/questions/51055831/is-there-an-equivalent-register-to-intels-msr-smi-count-on-amd-architecture
 *   says on AMD processors says need to use a performance counter to get a SMI count.
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>
#include <string.h>
#include <errno.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#define MSR_SMI_COUNT 0x34

int main (int argc, char *argv[])
{
    int msr_fd;
    int saved_errno;
    uint64_t msr_value;
    ssize_t num_read;

    /* Read the count from CPU 0, as all CPUs have the same MSR count */
    const char *const msr_filename = "/dev/cpu/0/msr";
    errno = 0;
    msr_fd = open (msr_filename, O_RDONLY);
    saved_errno = errno;
    if (msr_fd < 0)
    {
        if (saved_errno == ENOENT)
        {
            printf ("Error: %s doesn't exist. Try:\nsudo modprobe msr\n", msr_filename);
            return EXIT_FAILURE;
        }
        else if (saved_errno == EACCES)
        {
            printf ("Error: No permission to open %s. Try:\nsudo chmod o+r %s\n", msr_filename, msr_filename);
            return EXIT_FAILURE;
        }
        else if (saved_errno == EPERM)
        {
            printf ("Error: No permission to open %s. Try:\nsudo setcap cap_sys_rawio=ep %s\n", msr_filename, argv[0]);
            return EXIT_FAILURE;
        }
        else
        {
            printf ("Error: Unable to open %s : %s\n", msr_filename, strerror (saved_errno));
            return EXIT_FAILURE;
        }
    }

    /* Read the SMI count MSR register. All MSR reads must be 64-bits */
    errno = 0;
    num_read = pread (msr_fd, &msr_value, sizeof (msr_value), MSR_SMI_COUNT);
    saved_errno = errno;
    if (num_read != (ssize_t) sizeof (msr_value))
    {
        printf ("Error: Read of MSR_SMI_COUNT failed : %s\n", strerror (saved_errno));
    }

    /* Only the lower 32-bits of the MSI count are valid */
    printf ("SMI COUNT = %" PRIu64 "\n", msr_value & 0xffffffffU);

    close (msr_fd);

    return EXIT_SUCCESS;
}
