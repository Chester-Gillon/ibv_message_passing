/*
 * @file disk_test.c
 * @date 7 Sep 2024
 * @author Chester Gillon
 * @brief Perform a disk test at the file system level
 * @details
 *   Designed to test a disk by performing file I/O with equal write and read bandwidth.
 *   Test files are created containing a test pattern which is verified when read back.
 *   Blocking I/O calls are used with separate threads to:
 *   - Populate test buffers with a test pattern, and report regular test progress.
 *   - Write data to files.
 *   - Read data from files.
 *   - Verify the test pattern read from the files.
 *
 *   Should cause the data which is written and then read to actually occur on the disk by:
 *   a. Using O_DIRECT to bypass the Linux file system cache.
 *   b. Command line option which specifies a "write ahead" which is larger than the disk cache, so reads should be from the
 *      physical disk sectors, rather than the disk cache.
 *
 *   Statistics are reported as both:
 *   1. The file write and read data rate as seen by the program.
 *   2. Individual disk write and read data rates from the I/O statistics maintained by Linux for the block device
 *      which the directory used for test files is mounted on.
 */

#define _GNU_SOURCE /* For O_DIRECT */

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <limits.h>
#include <time.h>
#include <errno.h>

#include <getopt.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <semaphore.h>
#include <sys/vfs.h>
#include <dirent.h>
#include <pthread.h>
#include <signal.h>
#include <sys/time.h>
#include <libudev.h>


/* Format of the filenames used for disk tests */
#define TEST_FILENAME_FMT "disk_test_%06zu.dat"
#define TEST_FILENAME_SCN "disk_test_%06zu.dat"


/* Num additional blocks to allow overlapping of processing by the populate, write, read and verify threads
 * to try and maximise throughput while allowing the write-ahead blocks to bypass any cache in the disk. */
#define THREAD_OVERLAP_NUM_BLOCKS 4


/** The command line options for this program, in the format passed to getopt_long().
 *  Only long arguments are supported */
static const struct option command_line_options[] =
{
    {"test_dir", required_argument, NULL, 0},
    {"io_block_size", required_argument, NULL, 0},
    {"max_file_size_blocks", required_argument, NULL, 0},
    {"write_ahead_blocks", required_argument, NULL, 0},
    {"report_interval", required_argument, NULL, 0},
    {NULL, 0, NULL, 0}
};


/* Command line argument which specifies the directory used to create test files in */
static char arg_test_dir[PATH_MAX];


/* Command line argument which specifies the size in bytes of each block used for file I/O */
static size_t arg_io_block_size = 256 * 1024;


/* Command line argument which specifies the maximum size in blocks of each test file */
static size_t arg_max_file_size_blocks = 8192;


/* Command line argument which specifies the number of write-ahead blocks during the test */
static uint32_t arg_write_ahead_blocks = 512;


/* Command line argument which specifies the report interval in seconds, which is the interval over which statistics are
 * accumulated and then reported. */
static int64_t arg_report_interval_secs = 10;


/* File used to store a copy of the output written to the console */
static FILE *console_file;


/* Set true in a signal handler when Ctrl-C is used to request a running test stops */
static volatile bool test_stop_requested;


/* Contains the sampled disk statistics for one block device */
typedef struct
{
    /* Set true when the following statistics are valid */
    bool valid;
    /* The sysfs I/O statistics as defined by https://www.kernel.org/doc/Documentation/iostats.txt, in the order the fields
     * are read from the sysfs file */
    unsigned long num_reads_completed;
    unsigned long num_reads_merged;
    unsigned long num_sectors_read;
    unsigned long num_milliseconds_spent_reading;
    unsigned long num_writes_completed;
    unsigned long num_writes_merged;
    unsigned long num_sectors_written;
    unsigned long num_milliseconds_spent_writing;
    unsigned long num_ios_currently_in_progress;
    unsigned long num_milliseconds_spent_doing_ios;
    unsigned long weighted_num_milliseconds_spent_doing_ios;
    unsigned long num_discards_completed;
    unsigned long num_discards_merged;
    unsigned long num_sectors_discarded;
    unsigned long num_milliseconds_spent_discarding;
} sysfs_diskstats_t;


/* Defines one block used to write and then readback contents of test files */
typedef struct
{
    /* Buffer used to write to test files.
     * Buffer length is io_block_size_bytes */
    uint32_t *write_data;
    /* Buffer used to read from test files.
     * Buffer length is io_block_size_bytes */
    uint32_t *read_data;
    /* The number of the test file which contains the block */
    size_t file_num;
    /* Set when this block is for a new file, and the write and read threads need to close an existing file and open a new file.
     * This flag avoids the write and read threads having to track the overall block number during the test. */
    bool use_new_file;
    /* The test pattern at the start of the block */
    uint32_t test_pattern;
    /* Set true once the test is overwriting existing files, in that has completed the initial parse of writing total_test_blocks */
    bool overwriting_files;
    /* When true causes the write, read and verify threads to exit after then have processed the block.
     * Set when the test has been requested to stop. */
    bool final_block;
    /* Set true once this block has been processed by the verify thread. Used when the test starts so the populate thread
     * doesn't update statistics on the initial test blocks. */
    bool processed;
    /* Total number of words in the block which didn't contain the expected test pattern.
     * For simplicity the test doesn't track which files had verification errors nor the difference between the actual and
     * expected values. If verification errors are found to occur, could add additional diagnostics. */
    size_t num_verification_failures;
} file_io_block_t;


/* The test context which is specific to the populate_data_thread */
typedef struct
{
    /* Overall start and stop time for the test, used to report overall throughput */
    struct timespec start_time;
    struct timespec stop_time;
    /* The start time of the current test reporting interval */
    struct timespec report_start_time;
    /* The total number of blocks tested */
    size_t total_test_blocks;
    /* The number of blocks tested in the current report interval */
    size_t blocks_in_report_interval;
    /* Set true when have requested that the test stops */
    bool test_stopping;
    /* Index into blocks[] for the next block to populate */
    uint32_t blocks_index;
    /* Set true once the test has started to overwrite files */
    bool overwriting_files;
    /* Used to advance the test pattern for each block generated */
    uint32_t test_pattern;
    /* The overall block number of the test, over all files. This wraps. */
    size_t overall_block_number;
    /* The file_num used for the previous block populated, used to set use_new_file */
    size_t previous_file_num;
    /* The total number of words with data verification failures during the test */
    size_t total_verification_failures;
    /* The disk statistics sampled at the start of the test, used to report overall disk data rate */
    sysfs_diskstats_t disk_stats_test_start;
    /* The disk statistics sampled at the start of the current report interval, used to report disk data rate during the interval */
    sysfs_diskstats_t disk_stats_report_interval_start;
} test_populate_context_t;


/* The test context which is specific to the write_data_thread */
typedef struct
{
    /* The file descriptor currently open for writing. -1 means no file open */
    int fd;
    /* Index into blocks[] for the next block to write */
    uint32_t blocks_index;
    /* The number of blocks which have been written for which a sem_post is pending notifying the read thread.
     * As the test starts this is used to delay the reads starting to fill the write-ahead blocks. */
    uint32_t num_pending_read_blocks;
    /* Name of test file currently being written */
    char pathname[PATH_MAX];
} test_write_context_t;


/* The test context which is specific to the read_data_thread */
typedef struct
{
    /* The file descriptor currently open for reading. -1 means no file open */
    int fd;
    /* Index into blocks[] for the next block to read */
    uint32_t blocks_index;
    /* Name of test file currently being read */
    char pathname[PATH_MAX];
} test_read_context_t;


/* The test context which is specific to the verify_data_thread */
typedef struct
{
    /* Index into block[] for the next block to verify */
    uint32_t blocks_index;
} test_verify_context_t;


/* Defines the context of the disk test.
 * Some fields are shared between threads. Haven't attempted to align fields to cache line boundaries, as don't believe
 * important for throughput. */
typedef struct
{
    /* The name of the block device which contains the file system used by the disk test.
     * If NULL unable to determine the block device and therefore disk statistics can't be reported. */
    const char *block_device;
    /* The total number of blocks used for the test, based upon the amount of free space in the directory */
    size_t total_test_blocks;
    /* The size of each block written or read at once */
    size_t io_block_size_bytes;
    size_t io_block_size_words;
    /* The size of each test file in blocks */
    size_t file_size_blocks;
    /* The number of blocks which are written ahead */
    uint32_t write_ahead_blocks;
    /* Array of blocks of length [write_ahead_blocks] for writing and reading the test files */
    file_io_block_t *blocks;
    /* Semaphore given to trigger the write thread to write the next block */
    sem_t write_block_sem;
    /* Semaphore given to trigger the read thread to read the next block */
    sem_t read_block_sem;
    /* Semaphore given to trigger the verify thread to verify the next block */
    sem_t verify_block_sem;
    /* Semaphore given when the verification thread has finished with a block, and is available to be populated
     * with the next block to be written */
    sem_t block_available_sem;
    /* The identities of the threads created to perform the test */
    pthread_t populate_data_thread_id;
    pthread_t write_data_thread_id;
    pthread_t read_data_thread_id;
    pthread_t verify_data_thread_id;
    /* Set false when a file I/O error is detected which aborts the test.
     * While multiple threads can clear this, as aborts the running test shouldn't cause a race condition. */
    bool success;
    /* The thread specific parts of the context */
    test_populate_context_t populate;
    test_write_context_t write;
    test_read_context_t read;
    test_verify_context_t verify;
} test_context_t;


/**
 * @brief A 32-bit Linear congruential generator for creating a pseudo-random test pattern.
 * @details "Numerical Recipes" from https://en.wikipedia.org/wiki/Linear_congruential_generator
 * @param[in/out] seed the LCG value to advance
 */
static inline void linear_congruential_generator (uint32_t *const seed)
{
    *seed = (*seed * 1664525) + 1013904223;
}


/**
 * @brief Signal handler to request a running test stops
 * @param[in] sig Not used
 */
static void stop_test_handler (const int sig)
{
    test_stop_requested = true;
}


/**
 * @brief Write formatted output to the console and a log file
 * @param[in] format printf style format string
 * @param[in] ap The variable arguments for the formatted output
 */
static void vconsole_printf (const char *const format, va_list ap)
{
    va_list args;

    if (console_file != NULL)
    {
        va_copy (args, ap);
        vfprintf (console_file, format, args);
        va_end (args);
    }

    va_copy (args, ap);
    vprintf (format, args);
    va_end (args);
}


/**
 * @brief Write formatted output to the console and a log file
 * @param[in] format printf style format string
 * @param[in] ... printf arguments
 */
static void console_printf (const char *const format, ...) __attribute__((format(printf,1,2)));
static void console_printf (const char *const format, ...)
{
    va_list args;

    va_start (args, format);
    vconsole_printf (format, args);
    va_end (args);
}


/**
 * @brief Abort the program if an assertion fails, after displaying a message
 * @details To be used for checks which are bugs in the program, rather than a file I/O error on the disk under test.
 * @param[in] assertion Should be true to allow the program to continue.
 * @param[in] format printf style format string for error message.
 * @param[in] ... printf arguments
 */
#define CHECK_ASSERT(assertion) check_assert(assertion,#assertion)
static void check_assert (const bool assertion, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
static void check_assert (const bool assertion, const char *format, ...)
{
    if (!assertion)
    {
        va_list args;

        if (console_file != NULL)
        {
            va_start (args, format);
            fprintf (console_file, "Assertion failed : ");
            vfprintf (console_file, format, args);
            va_end (args);
            fprintf (console_file, "\n");
        }

        va_start (args, format);
        fprintf (stderr, "Assertion failed : ");
        vfprintf (stderr, format, args);
        va_end (args);
        fprintf (stderr, "\n");
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Check for a successful I/O call
 * @details If not successful display a diagnostic error and indicate failure in the test context to abort the test
 * @param[in] success Result of file I/O call
 * @param[in/out] context Test context to update upon failure.
 * @param[in] format printf style format string for error message.
 * @param[in] ... printf arguments
 */
static void check_file_io_result (const bool success, test_context_t *const context, const char *format, ...) __attribute__ ((format (printf, 3, 4)));
static void check_file_io_result (const bool success, test_context_t *const context, const char *format, ...)
{
    va_list args;

    if (!success && context->success)
    {
        va_start (args, format);
        vconsole_printf (format, args);
        va_end (args);
        context->success = false;
    }
}


/**
 * @brief Display the usage for this program, and then exit
 */
static void display_usage (void)
{
    printf ("Usage:\n");
    printf ("  disk_test <options>   Perform write/read tests of disk\n");
    printf ("\n");
    printf (" -test_dir <dir> The directory used to create test files in\n");
    printf ("\n");
    printf (" -io_block_size <bytes>\n");
    printf ("  The size in bytes of each block used for file I/O\n");
    printf ("\n");
    printf (" -max_file_size_blocks <blocks>\n");
    printf ("  The maximum size in blocks of each test file\n");
    printf ("\n");
    printf (" -write_ahead_blocks <blocks>\n");
    printf ("  The number of write-ahead blocks during the test. This is the number\n");
    printf ("  of blocks are written ahead of being read and verified. This is intended\n");
    printf ("  to be relatively small to cause the write and read bandwidth to be equal.\n");
    printf ("  This sets the amount of data buffered in memory. If should be larger than\n");
    printf ("  the write cache in the disk to force reads from the disk.\n");
    printf ("  'sdparm -al <device>' shows the state of the disk cache. E.g.:\n");
    printf ("    WCE           1  [cha: y, def:  1]  Write cache enable\n");
    printf ("    RCD           0  [cha: n, def:  0]  Read cache disable\n");
    printf ("\n");
    printf (" -report_interval <seconds>\n");
    printf ("  The number of seconds between reports of test progress\n");

    exit (EXIT_FAILURE);
}


/**
 * @brief Parse command line arguments, storing the result in global variables
 * @details Aborts the program if invalid arguments
 */
static void parse_command_line_arguments (const int argc, char *argv[])
{
    char junk;
    int opt_status;
    int option_index = 0;
    bool test_dir_specified = false;

    do
    {
        opt_status = getopt_long (argc, argv, "", command_line_options, &option_index);
        if (opt_status == '?')
        {
            display_usage ();
        }
        else if (opt_status >= 0)
        {
            const struct option *const optdef = &command_line_options[option_index];

            if (optdef->flag != NULL)
            {
                /* Argument just sets a flag */
            }
            else if (strcmp (optdef->name, "test_dir") == 0)
            {
                snprintf (arg_test_dir, sizeof (arg_test_dir), "%s", optarg);
                test_dir_specified = true;
            }
            else if (strcmp (optdef->name, "io_block_size") == 0)
            {
                if ((sscanf (optarg, "%zi%c", &arg_io_block_size, &junk) != 1) || (arg_io_block_size == 0))
                {
                    fprintf (stderr, "Invalid io_block_size %s\n", optarg);
                    exit (EXIT_FAILURE);
                }
            }
            else if (strcmp (optdef->name, "max_file_size_blocks") == 0)
            {
                if ((sscanf (optarg, "%zi%c", &arg_max_file_size_blocks, &junk) != 1) || (arg_max_file_size_blocks == 0))
                {
                    fprintf (stderr, "Invalid max_file_size_blocks %s\n", optarg);
                    exit (EXIT_FAILURE);
                }
            }
            else if (strcmp (optdef->name, "write_ahead_blocks") == 0)
            {
                if ((sscanf (optarg, "%" SCNu32 "%c", &arg_write_ahead_blocks, &junk) != 1) ||
                    (arg_write_ahead_blocks == 0))
                {
                    fprintf (stderr, "Invalid write_ahead_blocks %s\n", optarg);
                    exit (EXIT_FAILURE);
                }
            }
            else if (strcmp (optdef->name, "report_interval") == 0)
            {
                if ((sscanf (optarg, "%" SCNi64 "%c", &arg_report_interval_secs, &junk) != 1))
                {
                    fprintf (stderr, "Invalid report_interval %s\n", optarg);
                    exit (EXIT_FAILURE);
                }
            }
            else
            {
                /* This is a program error, and shouldn't be triggered by the command line options */
                fprintf (stderr, "Unexpected argument definition %s\n", optdef->name);
                exit (EXIT_FAILURE);
            }
        }
    } while (opt_status != -1);

    if (optind < argc)
    {
        fprintf (stderr, "Unexpected non-argument options\n");
        exit (EXIT_FAILURE);
    }

    if (!test_dir_specified)
    {
        fprintf (stderr, "test_dir must be specified\n");
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Read the disk statistics for one block device
 * @param[in] block_device The name of the block device to read the statistics for.
 *                         If NULL no statistics can be read.
 * @param[out] disk_stats The statistics read, and a validity status.
 */
static void read_sysfs_diskstats (const char *const block_device, sysfs_diskstats_t *const disk_stats)
{
    char stats_pathname[PATH_MAX];
    FILE *stats_file;
    int rc;

    disk_stats->valid = false;
    if (block_device != NULL)
    {
        snprintf (stats_pathname, sizeof (stats_pathname), "/sys/block/%s/stat", block_device);
        stats_file = fopen (stats_pathname, "r");
        if (stats_file != NULL)
        {
            const int num_expected_fields = 15;
            const int num_actual_fields = fscanf (stats_file, "%lu%lu%lu%lu%lu%lu%lu%lu%lu%lu%lu%lu%lu%lu%lu",
                    &disk_stats->num_reads_completed,
                    &disk_stats->num_reads_merged,
                    &disk_stats->num_sectors_read,
                    &disk_stats->num_milliseconds_spent_reading,
                    &disk_stats->num_writes_completed,
                    &disk_stats->num_writes_merged,
                    &disk_stats->num_sectors_written,
                    &disk_stats->num_milliseconds_spent_writing,
                    &disk_stats->num_ios_currently_in_progress,
                    &disk_stats->num_milliseconds_spent_doing_ios,
                    &disk_stats->weighted_num_milliseconds_spent_doing_ios,
                    &disk_stats->num_discards_completed,
                    &disk_stats->num_discards_merged,
                    &disk_stats->num_sectors_discarded,
                    &disk_stats->num_milliseconds_spent_discarding);
            disk_stats->valid = num_actual_fields == num_expected_fields;

            rc = fclose (stats_file);
            CHECK_ASSERT (rc == 0);
        }
    }
}


/**
 * @brief Calculate a floating point duration in seconds from two timestamps
 * @param[in] start_time The start time of the measurement
 * @param[in] end_time The end time of the measurement
 * @return The duration between start_time and end_time
 */
static double calculate_duration_secs (const struct timespec *const start_time, const struct timespec *const stop_time)
{
    const int64_t nsecs_per_sec = 1000000000;
    const int64_t start_nsecs = (start_time->tv_sec * nsecs_per_sec) + start_time->tv_nsec;
    const int64_t stop_nsecs = (stop_time->tv_sec * nsecs_per_sec) + stop_time->tv_nsec;
    const double duration_secs = (double) (stop_nsecs - start_nsecs) / 1E9;

    return duration_secs;
}


/**
 * @brief Calculate a file data rate in megabytes/second
 * @details This is the data rate of the file IO performed, and therefore doesn't include file system metadata overheads.
 * @param[in] duration_secs The duration in seconds of the measurement interval
 * @param[in] context Used to get the block size
 * @param[in] num_blocks The number of block transferred during the measurement interval
 * @param[out] duration_secs The duration in seconds
 * @param[out] mbytes_per_sec The data rate
 */
static void file_data_rate_mbytes_per_sec (const double duration_secs,
                                           const test_context_t *const context, const size_t num_blocks,
                                           double *const mbytes_per_sec)
{
    const double mbytes = (double) (context->io_block_size_bytes * num_blocks) / 1E6;
    *mbytes_per_sec = mbytes / duration_secs;
}


/**
 * @brief Calculate a disk data rate in megabytes/second
 * @param[in] duration_secs The duration in seconds of the measurement interval
 * @param[in] start_stats The disk statistics at the start of the measurement interval
 * @param[in] end_stats The disk statistics at the end of the measurement interval
 * @param[out] read_mbytes_per_sec The disk read data rate
 * @param[out] write_mbytes_per_sec The disk write date rate
 * @return Returns true if the data rate measurement is valid, or false if failed to read the statistics
 */
static bool disk_data_rate_mbytes_per_sec (const double duration_secs,
                                           const sysfs_diskstats_t *const start_stats, const sysfs_diskstats_t *const end_stats,
                                           double *const read_mbytes_per_sec, double *const write_mbytes_per_sec)
{
    const bool measurement_valid = start_stats->valid && end_stats->valid;

    *read_mbytes_per_sec = 0.0;
    *write_mbytes_per_sec = 0.0;
    if (measurement_valid)
    {
        /* https://stackoverflow.com/questions/37248948/how-to-get-disk-read-write-bytes-per-second-from-proc-in-programming-on-linux
         * says /sys/block/sdX/stat always reports 512 byte sectors.
         *
         * See also https://github.com/sysstat/sysstat/blob/master/iostat.c */
        const double num_bytes_per_sector = 512.0;

        const unsigned long num_read_sectors = end_stats->num_sectors_read - start_stats->num_sectors_read;
        const unsigned long num_write_sectors = end_stats->num_sectors_written - start_stats->num_sectors_written;
        const double num_read_mbytes = (num_bytes_per_sector * (double) num_read_sectors) / 1E6;
        const double num_write_mbytes = (num_bytes_per_sector * (double) num_write_sectors) / 1E6;

        *read_mbytes_per_sec = num_read_mbytes / duration_secs;
        *write_mbytes_per_sec = num_write_mbytes / duration_secs;
    }

    return measurement_valid;
}


/**
 * @brief Thread entry point to populate buffers with test data to be written to files.
 * @details This also:
 *          a. Reports regular progress of the test. Assumes the console IO and reading of disk statistics doesn't significantly
 *             slow down the populating of buffers. For additional complexity could write the regular progress to the context
 *             and post a semaphore to make the main thread display the regular progress.
 *          b. Handles a request to stop the test, and exits once the final blocks have been processed. There is no way to force
 *             the program to exit cleanly if the write or read threads get blocked in a file I/O call.
 * @param[in/out] arg Test context
 * @return Not used
 */
static void *populate_data_thread (void *const arg)
{
    test_context_t *const context = arg;
    int rc;
    int saved_errno;
    struct timespec now;
    struct timespec report_progress_abs_time;
    char time_str[80];
    struct tm broken_down_time;
    struct timeval tod;
    double reporting_interval_duration_secs;
    double overall_duration_secs;
    double reporting_interval_file_data_rate;
    double overall_file_data_rate;
    double disk_read_mbytes_per_sec;
    double disk_write_mbytes_per_sec;
    sysfs_diskstats_t current_disk_stats;
    bool test_complete = false;

    context->populate.test_stopping = false;
    context->populate.blocks_index = 0;
    context->populate.test_pattern = 0;
    context->populate.overwriting_files = false;
    context->populate.overall_block_number = 0;
    context->populate.previous_file_num = 0;
    context->populate.total_test_blocks = 0;
    context->populate.blocks_in_report_interval = 0;
    context->populate.total_verification_failures = 0;

    console_printf ("Press Ctrl-C to stop test\n");
    read_sysfs_diskstats (context->block_device, &current_disk_stats);
    context->populate.disk_stats_test_start = current_disk_stats;
    context->populate.disk_stats_report_interval_start = current_disk_stats;
    rc = clock_gettime (CLOCK_REALTIME, &context->populate.start_time);
    context->populate.report_start_time = context->populate.start_time;
    CHECK_ASSERT (rc == 0);
    report_progress_abs_time = context->populate.start_time;
    report_progress_abs_time.tv_sec += arg_report_interval_secs;

    /* Run test until complete */
    do
    {
        /* Wait for a block to be available, using a timeout for the next report time.
         * The timeout ensure will wake to report if the file I/O performed by the other threads stalls for some reason. */
        errno = 0;
        rc = sem_timedwait (&context->block_available_sem, &report_progress_abs_time);
        saved_errno = errno;

        if (rc == 0)
        {
            file_io_block_t *const block = &context->blocks[context->populate.blocks_index];

            /* For block processed handle test completion and updating of test statistics */
            if (block->processed)
            {
                /* Determine when a test stop has been requested */
                if (block->final_block)
                {
                    /* Detect test complete when the final block has been processed */
                    test_complete = true;
                    block->final_block = false;
                    context->populate.total_test_blocks += context->populate.blocks_in_report_interval;
                }
                else if (!context->populate.test_stopping && test_stop_requested)
                {
                    context->populate.test_stopping = true;
                    block->final_block = true;
                }
                else
                {
                    block->final_block = false;
                }

                block->processed = false;
                context->populate.blocks_in_report_interval++;
                context->populate.total_verification_failures += block->num_verification_failures;
            }

            if (block->final_block || (!context->populate.test_stopping))
            {
                /* Populate the test contents of the block and notify the write thread */
                block->test_pattern = context->populate.test_pattern;
                block->overwriting_files = context->populate.overwriting_files;
                block->file_num = (context->populate.overall_block_number / context->file_size_blocks) + 1;
                block->use_new_file = block->file_num != context->populate.previous_file_num;
                for (uint32_t word_index = 0; word_index < context->io_block_size_words; word_index++)
                {
                    block->write_data[word_index] = context->populate.test_pattern;
                    linear_congruential_generator (&context->populate.test_pattern);
                }
                context->populate.previous_file_num = block->file_num;

                rc = sem_post (&context->write_block_sem);
                CHECK_ASSERT (rc == 0);

                context->populate.overall_block_number++;
                if (context->populate.overall_block_number == context->total_test_blocks)
                {
                    context->populate.overall_block_number = 0;
                    context->populate.overwriting_files = true;
                }
            }

            context->populate.blocks_index = (context->populate.blocks_index + 1) % context->write_ahead_blocks;
        }
        else
        {
            CHECK_ASSERT ((saved_errno == ETIMEDOUT) || (saved_errno == EINTR));
        }

        /* Report progress at regular intervals */
        rc = clock_gettime (CLOCK_REALTIME, &now);
        CHECK_ASSERT (rc == 0);
        if (((now.tv_sec == report_progress_abs_time.tv_sec) && (now.tv_nsec >= report_progress_abs_time.tv_nsec)) ||
             (now.tv_sec > report_progress_abs_time.tv_sec))
        {
            read_sysfs_diskstats (context->block_device, &current_disk_stats);
            context->populate.total_test_blocks += context->populate.blocks_in_report_interval;

            reporting_interval_duration_secs = calculate_duration_secs (&context->populate.report_start_time, &now);
            file_data_rate_mbytes_per_sec (reporting_interval_duration_secs, context, context->populate.blocks_in_report_interval,
                    &reporting_interval_file_data_rate);
            overall_duration_secs = calculate_duration_secs (&context->populate.start_time, &now);
            file_data_rate_mbytes_per_sec (overall_duration_secs, context, context->populate.total_test_blocks,
                    &overall_file_data_rate);

            /* Display time when these statistics are reported */
            gettimeofday (&tod, NULL);
            const time_t tod_sec = tod.tv_sec;
            const int64_t tod_msec = tod.tv_usec / 1000;
            localtime_r (&tod_sec, &broken_down_time);
            strftime (time_str, sizeof (time_str), "%H:%M:%S", &broken_down_time);
            size_t str_len = strlen (time_str);
            snprintf (&time_str[str_len], sizeof (time_str) - str_len, ".%03" PRIi64, tod_msec);

            console_printf ("\n%s\n", time_str);
            console_printf ("  Currently %s block %zu in file %zu\n",
                    context->populate.overwriting_files ? "overwriting" : "writing",
                    context->populate.overall_block_number,
                    (context->populate.overall_block_number / context->file_size_blocks) + 1);
            console_printf ("  File data rates (MB/s) interval: %.2f over %.3f secs  overall: %.2f over %.3f secs\n",
                    reporting_interval_file_data_rate, reporting_interval_duration_secs,
                    overall_file_data_rate, overall_duration_secs);
            if (context->populate.total_verification_failures > 0)
            {
                console_printf ("  total_verification_failures: %zu\n", context->populate.total_verification_failures);
            }
            if (disk_data_rate_mbytes_per_sec (reporting_interval_duration_secs,
                    &context->populate.disk_stats_report_interval_start, &current_disk_stats,
                    &disk_read_mbytes_per_sec, &disk_write_mbytes_per_sec))
            {
                console_printf ("  Disk data rates (MB/s) interval: Read %.2f  Write %.2f\n",
                        disk_read_mbytes_per_sec, disk_write_mbytes_per_sec);
            }
            if (disk_data_rate_mbytes_per_sec (overall_duration_secs,
                    &context->populate.disk_stats_test_start, &current_disk_stats,
                    &disk_read_mbytes_per_sec, &disk_write_mbytes_per_sec))
            {
                console_printf ("  Disk data rates (MB/s) overall: Read %.2f  Write %.2f\n",
                        disk_read_mbytes_per_sec, disk_write_mbytes_per_sec);
            }

            report_progress_abs_time.tv_sec += arg_report_interval_secs;
            context->populate.report_start_time = now;
            context->populate.blocks_in_report_interval = 0;
            context->populate.disk_stats_report_interval_start = current_disk_stats;
        }
    } while (!test_complete && context->success);

    rc = clock_gettime (CLOCK_REALTIME, &context->populate.stop_time);
    CHECK_ASSERT (rc == 0);

    /* Report test overall data rate */
    read_sysfs_diskstats (context->block_device, &current_disk_stats);
    overall_duration_secs = calculate_duration_secs (&context->populate.start_time, &context->populate.stop_time);
    file_data_rate_mbytes_per_sec (overall_duration_secs, context, context->populate.total_test_blocks, &overall_file_data_rate);
    console_printf ("\nOverall file data rate (MB/s) %.2f over %.3f secs\n",
            overall_file_data_rate, overall_duration_secs);
    console_printf ("total_verification_failures: %zu\n", context->populate.total_verification_failures);
    if (disk_data_rate_mbytes_per_sec (overall_duration_secs,
            &context->populate.disk_stats_test_start, &current_disk_stats,
            &disk_read_mbytes_per_sec, &disk_write_mbytes_per_sec))
    {
        console_printf ("Disk data rates (MB/s) overall: Read %.2f  Write %.2f\n",
                disk_read_mbytes_per_sec, disk_write_mbytes_per_sec);
    }

    return NULL;
}


/**
 * @brief Thread entry point to write populate buffers to test files.
 * @param[in/out] arg Test context
 * @return Not used
 */
static void *write_data_thread (void *const arg)
{
    test_context_t *const context = arg;
    int rc;
    int saved_errno;
    ssize_t bytes_written;
    volatile size_t num_chars;
    bool test_complete = false;

    /* Only the owner needs permission */
    const mode_t file_mode = S_IRUSR | S_IWUSR;

    /* Initialise to no file open */
    context->write.fd = -1;
    context->write.blocks_index = 0;
    context->write.num_pending_read_blocks = 0;

    /* Run until test complete */
    do
    {
        /* Wait for a populated block to write */
        rc = sem_wait (&context->write_block_sem);
        CHECK_ASSERT (rc == 0);

        const file_io_block_t *const block = &context->blocks[context->write.blocks_index];

        if (context->success && block->use_new_file)
        {
            if (context->write.fd != -1)
            {
                /* Close previous file */
                errno = 0;
                rc = close (context->write.fd);
                saved_errno = errno;
                check_file_io_result (rc == 0, context, "%s close(%d) failed : %s\n",
                        context->write.pathname, context->write.fd, strerror (saved_errno));
            }

            /* Bypass the file system cache */
            int flags = O_DIRECT | O_WRONLY;

            if (!block->overwriting_files)
            {
                /* When should be creating a file on the first parse, check the file doesn't already exist */
                flags |= O_CREAT | O_EXCL;
            }

            /* Open the file for writing */
            if (context->success)
            {
                num_chars = sizeof (context->write.pathname);
                snprintf (context->write.pathname, num_chars, "%s/" TEST_FILENAME_FMT, arg_test_dir, block->file_num);
                errno = 0;
                context->write.fd = open (context->write.pathname, flags, file_mode);
                saved_errno = errno;
                check_file_io_result (context->write.fd != -1, context, "open(%s,0x%x,0x%x) for writing failed : %s\n",
                        context->write.pathname, flags, file_mode, strerror (saved_errno));
            }
        }

        /* Write the file test pattern to the block */
        if (context->success)
        {
            errno = 0;
            bytes_written = write (context->write.fd, block->write_data, context->io_block_size_bytes);
            saved_errno = errno;
            check_file_io_result (bytes_written == (ssize_t) context->io_block_size_bytes, context,
                    "%s fd %d only wrote %zd out of %zu bytes : %s\n", context->write.pathname, context->write.fd,
                    bytes_written, context->io_block_size_bytes, strerror (saved_errno));
        }

        test_complete = block->final_block;
        if ((context->write.num_pending_read_blocks + THREAD_OVERLAP_NUM_BLOCKS) >= context->write_ahead_blocks)
        {
            /* Tell the read thread to process the oldest write-ahead block. */
            rc = sem_post (&context->read_block_sem);
            CHECK_ASSERT (rc == 0);
        }
        else
        {
            /* Filling the write-ahead blocks at the start of the test, so hold off starting the read */
            context->write.num_pending_read_blocks++;
        }

        context->write.blocks_index = (context->write.blocks_index + 1) % context->write_ahead_blocks;
    } while (!test_complete && context->success);

    /* Close current file */
    if (context->success)
    {
        errno = 0;
        rc = close (context->write.fd);
        saved_errno = errno;
        check_file_io_result (rc == 0, context, "%s close(%d) failed : %s\n",
                context->write.pathname, context->write.fd, strerror (saved_errno));
    }

    /* Flush the write-ahead blocks at the end of the test */
    while (context->success && (context->write.num_pending_read_blocks > 0))
    {
        rc = sem_post (&context->read_block_sem);
        CHECK_ASSERT (rc == 0);
        context->write.num_pending_read_blocks--;
    }

    /* If the test has failed, wake up the read thread if currently waiting for a block to allow the read thread to exit */
    if (context->success)
    {
        context->write.fd = -1;
    }
    else
    {
        rc = sem_post (&context->read_block_sem);
        CHECK_ASSERT (rc == 0);
    }

    return NULL;
}


/**
 * @brief Thread entry point to read buffers from test files.
 * @param[in/out] arg Test context
 * @return Not used
 */
static void *read_data_thread (void *const arg)
{
    test_context_t *const context = arg;
    int rc;
    int saved_errno;
    volatile size_t num_chars;
    ssize_t bytes_read;
    bool test_complete = false;

    /* Initialise to no file open */
    context->read.fd = -1;
    context->read.blocks_index = 0;

    /* Run test until complete */
    do
    {
        /* Wait for a block to read */
        rc = sem_wait (&context->read_block_sem);
        CHECK_ASSERT (rc == 0);

        const file_io_block_t *const block = &context->blocks[context->read.blocks_index];

        if (context->success && block->use_new_file)
        {
            if (context->read.fd != -1)
            {
                /* Close previous file */
                errno = 0;
                rc = close (context->read.fd);
                saved_errno = errno;
                check_file_io_result (rc == 0, context, "%s close(%d) failed : %s\n",
                        context->read.pathname, context->read.fd, strerror (saved_errno));
            }

            /* Open the file for reading access bypassing the file system cache */
            if (context->success)
            {
                num_chars = sizeof (context->read.pathname);
                snprintf (context->read.pathname, num_chars, "%s/" TEST_FILENAME_FMT, arg_test_dir, block->file_num);
                errno = 0;
                context->read.fd = open (context->read.pathname, O_DIRECT | O_RDONLY, 0);
                saved_errno = errno;
                check_file_io_result (context->read.fd != -1, context, "open(%s) for reading failed : %s\n",
                        context->read.pathname, strerror (saved_errno));
            }
        }

        /* Read block from the test file */
        if (context->success)
        {
            errno = 0;
            bytes_read = read (context->read.fd, block->read_data, context->io_block_size_bytes);
            saved_errno = errno;
            check_file_io_result (bytes_read == (ssize_t) context->io_block_size_bytes, context,
                    "%s fd %d only read %zd out of %zu bytes : %s\n", context->read.pathname, context->read.fd,
                    bytes_read, context->io_block_size_bytes, strerror (saved_errno));
        }

        /* Tell the verify thread the block is available */
        test_complete = block->final_block;
        rc = sem_post (&context->verify_block_sem);
        CHECK_ASSERT (rc == 0);

        context->read.blocks_index = (context->read.blocks_index + 1) % context->write_ahead_blocks;
    } while (!test_complete && context->success);

    /* If the test has failed, wake up the verify thread if currently waiting for a block to allow the verify thread to exit */
    if (context->success)
    {
        context->read.fd = -1;
    }
    else
    {
        rc = sem_post (&context->verify_block_sem);
        CHECK_ASSERT (rc == 0);
    }

    return NULL;
}


/**
 * @brief Thread entry point to verify the test data read from files.
 * @param[in/out] arg Test context
 * @return Not used
 */
static void *verify_data_thread (void *const arg)
{
    test_context_t *const context = arg;
    int rc;
    bool test_complete = false;

    context->verify.blocks_index = 0;

    /* Run test until complete */
    do
    {
        /* Wait for a block to verify */
        rc = sem_wait (&context->verify_block_sem);
        CHECK_ASSERT (rc == 0);

        file_io_block_t *const block = &context->blocks[context->verify.blocks_index];

        /* Verify the contents of the block.
         * Performs a comparison of the entire block, and only checks the individual words if the overall comparison
         * indicates a difference. */
        block->num_verification_failures = 0;
        if (context->success)
        {
            if (memcmp (block->read_data, block->write_data, context->io_block_size_bytes) != 0)
            {
                uint32_t expected_pattern = block->test_pattern;

                for (size_t word_index = 0; word_index < context->io_block_size_words; word_index++)
                {
                    if (block->read_data[word_index] != expected_pattern)
                    {
                        block->num_verification_failures++;
                    }

                    linear_congruential_generator (&expected_pattern);
                }
            }
        }
        block->processed = true;

        /* Tell the populate thread the block is available for reuse */
        test_complete = block->final_block;
        rc = sem_post (&context->block_available_sem);
        CHECK_ASSERT (rc == 0);

        context->verify.blocks_index = (context->verify.blocks_index + 1) % context->write_ahead_blocks;
    } while (!test_complete && context->success);

    return NULL;
}


/**
 * @brief Perform the initialisation for the disk test
 * @param[out] context The initialised test context
 */
static void initialise_disk_test (test_context_t *const context)
{
    const size_t page_size = (size_t) sysconf (_SC_PAGE_SIZE);
    struct stat test_dir_stat;
    struct statfs test_dir_statfs;
    int rc;
    int saved_errno;
    void *buffer;

    /* Initialise the test context for the test options in use */
    memset (context, 0, sizeof (*context));
    context->io_block_size_bytes = arg_io_block_size;
    context->io_block_size_words = context->io_block_size_bytes / sizeof (uint32_t);
    context->file_size_blocks = arg_max_file_size_blocks;
    context->write_ahead_blocks = arg_write_ahead_blocks + THREAD_OVERLAP_NUM_BLOCKS;

    /* Report the test options in use, which may be set from the command line arguments or defaults */
    console_printf ("Disk test using:\n");
    console_printf ("  test_dir %s\n", arg_test_dir);
    console_printf ("  io_block_size %zu\n", context->io_block_size_bytes);
    console_printf ("  max_file_size_blocks %zu\n", arg_max_file_size_blocks);
    console_printf ("  write_ahead_blocks %u\n", arg_write_ahead_blocks);

    /* Validate the test directory argument */
    errno = 0;
    rc = stat (arg_test_dir, &test_dir_stat);
    saved_errno = errno;
    if (rc != 0)
    {
        console_printf ("Error: Failed to stat() %s : %s\n", arg_test_dir, strerror (saved_errno));
        exit (EXIT_FAILURE);
    }
    if ((test_dir_stat.st_mode & S_IFMT) != S_IFDIR)
    {
        console_printf ("Error: %s is not a directory\n", arg_test_dir);
        exit (EXIT_FAILURE);
    }

    /* Check the block size used for the test is a multiple of the file system optimal block size, which
     * is required to use O_DIRECT. */
    console_printf ("File system Optimal block size for I/O %zu\n", test_dir_stat.st_blksize);
    if ((arg_io_block_size % (size_t) test_dir_stat.st_blksize) != 0)
    {
        console_printf ("Error: io_block_size %zu is not a multiple of the file system Optimal block size for I/O %zu\n",
                arg_io_block_size, test_dir_stat.st_blksize);
        exit (EXIT_FAILURE);
    }

    /* Delete the test files from a previous run, to free up disk space */
    DIR *const test_dir = opendir (arg_test_dir);
    if (test_dir == NULL)
    {
        console_printf ("Error: opendir(%s) failed\n", arg_test_dir);
        exit (EXIT_FAILURE);
    }

    /* Obtain the name of the block device which contains the test directory.
     * The sysname returned for arg_test_dir might be for a  partition, e.g. of the form sdb2.
     * Try and locate the partent device for which sysfs disk statstics can be obtained. */
    sysfs_diskstats_t probe_diskstats;
    struct udev *const udev = udev_new();
    CHECK_ASSERT (udev != NULL);
    const char *block_device = NULL;
    struct udev_device *dev = udev_device_new_from_devnum (udev, 'b', test_dir_stat.st_dev);
    probe_diskstats.valid = false;
    while ((dev != NULL) && (!probe_diskstats.valid))
    {
        block_device = udev_device_get_sysname (dev);
        if (block_device != NULL)
        {
            read_sysfs_diskstats (block_device, &probe_diskstats);
            if (!probe_diskstats.valid)
            {
                dev = udev_device_get_parent (dev);
            }
        }
        else
        {
            dev = NULL;
        }
    }

    if (block_device != NULL)
    {
        context->block_device = strdup (block_device);
        console_printf ("Block device %s\n", context->block_device);
    }
    else
    {
        context->block_device = NULL;
        console_printf ("Unable to determine block device for %s, no disk statistics will be reported\n", arg_test_dir);
    }
    udev_unref (udev);

    volatile size_t num_chars;
    char existing_test_file_pathname[PATH_MAX];
    size_t test_file_number;
    uint32_t num_removed_test_files = 0;
    struct dirent *test_dir_entry = readdir (test_dir);
    while (test_dir_entry != NULL)
    {
        if (test_dir_entry->d_type == DT_REG)
        {
            if (sscanf (test_dir_entry->d_name, TEST_FILENAME_SCN, &test_file_number) == 1)
            {
                /* use of num_chars suppresses -Wformat-truncation, as suggested by
                 * https://stackoverflow.com/a/70938456/4207678 */
                num_chars = sizeof (existing_test_file_pathname);
                snprintf (existing_test_file_pathname, num_chars, "%s/%s",
                        arg_test_dir, test_dir_entry->d_name);
                errno = 0;
                rc = remove (existing_test_file_pathname);
                saved_errno = errno;

                if (rc == 0)
                {
                    num_removed_test_files++;
                }
                else
                {
                    console_printf ("Error: Failed to remove existing test file %s : %s\n",
                            existing_test_file_pathname, strerror (saved_errno));
                }
            }
        }
        test_dir_entry = readdir (test_dir);
    }

    rc = closedir (test_dir);
    CHECK_ASSERT (rc == 0);
    if (num_removed_test_files > 0)
    {
        console_printf ("Removed %" PRIu32 " test files from previous run\n", num_removed_test_files);
    }

    /* Get the file system information to determine the total number of blocks to write during the test.
     * The assumption is that the file system used for the test is only being written to by this test program,
     * and can therefore simply determine the total number blocks for the test by the amount of free space
     * at the start of the test. This doesn't account for file system meta data overhead. */
    errno = 0;
    rc = statfs (arg_test_dir, &test_dir_statfs);
    saved_errno = errno;
    if (rc != 0)
    {
        console_printf ("Error: Failed to statfs() %s : %s\n", arg_test_dir, strerror (saved_errno));
        exit (EXIT_FAILURE);
    }
    const size_t available_space_bytes = (size_t) test_dir_statfs.f_bavail * (size_t) test_dir_statfs.f_bsize;
    context->total_test_blocks = available_space_bytes / context->io_block_size_bytes;
    if (context->total_test_blocks < context->write_ahead_blocks)
    {
        console_printf ("Error: Number of free blocks on file system is %zu, which isn't sufficient for the write_ahead_blocks of %u\n",
                context->total_test_blocks, context->write_ahead_blocks);
        exit (EXIT_FAILURE);
    }

    /* Allocate buffers for file I/O */
    context->blocks = calloc (context->write_ahead_blocks, sizeof (context->blocks[0]));
    CHECK_ASSERT (context->blocks != NULL);
    for (uint32_t block_index = 0; block_index < context->write_ahead_blocks; block_index++)
    {
        file_io_block_t *const block = &context->blocks[block_index];

        buffer = NULL;
        rc = posix_memalign (&buffer, page_size, context->io_block_size_bytes);
        check_assert ((rc == 0) && (buffer != NULL), "Buffer allocation failed");
        block->write_data = buffer;

        buffer = NULL;
        rc = posix_memalign (&buffer, page_size, context->io_block_size_bytes);
        check_assert ((rc == 0) && (buffer != NULL), "Buffer allocation failed");
        block->read_data = buffer;

        /* Ensure the populate thread doesn't report any failures on the initial write-ahead blocks */
        block->processed = false;
        block->final_block = false;
        block->num_verification_failures = 0;
    }

    /* Install signal handler, used to request test is stopped */
    struct sigaction action;

    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_test_handler;
    action.sa_flags = SA_RESTART;
    rc = sigaction (SIGINT, &action, NULL);
    CHECK_ASSERT (rc == 0);

    /* Initialise inter-thread semaphores.
     * write_ahead_blocks is initialised to all available, with other semaphores empty */
    rc = sem_init (&context->write_block_sem, 0, 0);
    CHECK_ASSERT (rc == 0);
    rc = sem_init (&context->read_block_sem, 0, 0);
    CHECK_ASSERT (rc == 0);
    rc = sem_init (&context->verify_block_sem, 0, 0);
    CHECK_ASSERT (rc == 0);
    rc = sem_init (&context->block_available_sem, 0, context->write_ahead_blocks);

    context->success = true;

    /* Create test threads */
    rc = pthread_create (&context->populate_data_thread_id, NULL, populate_data_thread, context);
    CHECK_ASSERT (rc == 0);
    rc = pthread_create (&context->write_data_thread_id, NULL, write_data_thread, context);
    CHECK_ASSERT (rc == 0);
    rc = pthread_create (&context->read_data_thread_id, NULL, read_data_thread, context);
    CHECK_ASSERT (rc == 0);
    rc = pthread_create (&context->verify_data_thread_id, NULL, verify_data_thread, context);
    CHECK_ASSERT (rc == 0);
}


/**
 * @brief Perform the finalisation for the disk test, freeing resources
 * @details The test files which were created are left, they will be removed during the initialisation of the next run
 * @param[out] context The test context to finalise
 */
static void finalise_disk_test (test_context_t *const context)
{
    int rc;

    /* Wait for all test threads to exit */
    rc = pthread_join (context->populate_data_thread_id, NULL);
    CHECK_ASSERT (rc == 0);
    rc = pthread_join (context->write_data_thread_id, NULL);
    CHECK_ASSERT (rc == 0);
    rc = pthread_join (context->read_data_thread_id, NULL);
    CHECK_ASSERT (rc == 0);
    rc = pthread_join (context->verify_data_thread_id, NULL);
    CHECK_ASSERT (rc == 0);
}


int main (int argc, char *argv[])
{
    test_context_t test_context;

    parse_command_line_arguments (argc, argv);

    /* Set filenames which contain the console output containing the date/time of the run */
    const time_t tod_now = time (NULL);
    struct tm broken_down_time;
    char date_time_str[80];
    char console_filename[160];

    localtime_r (&tod_now, &broken_down_time);
    strftime (date_time_str, sizeof (date_time_str), "%Y%m%dT%H%M%S", &broken_down_time);
    snprintf (console_filename, sizeof (console_filename), "%s_console.txt", date_time_str);

    console_file = fopen (console_filename, "wt");
    if (console_file == NULL)
    {
        fprintf (stderr, "Failed to create %s\n", console_filename);
        exit (EXIT_FAILURE);
    }

    initialise_disk_test (&test_context);

    finalise_disk_test (&test_context);

    return EXIT_SUCCESS;
}
