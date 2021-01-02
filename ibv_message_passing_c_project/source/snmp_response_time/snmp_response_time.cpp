/*
 * @file snmp_response_time.cpp
 * @date 28 Dec 2020
 * @author Chester Gillon
 * @brief Program to time SNMP responses to GET requests.
 * @details Performs snmp_synch_response() calls at regular intervals, collecting the time taken for the response.
 *          During the run reports summary information for the min/median/max response times for both a STATUS and TIMEOUT status.
 *          The local time of the min/max response times are reported, so can be correlated against Wireshark times if want to
 *          investigate the packets exchanged when the maximum response time occurs.
 *
 *          Can optionally write a detailed CSV file wit all response times, to allow more analysis of the spread of
 *          response times.
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <map>
#include <vector>
#include <cinttypes>

#include <time.h>
#include <signal.h>

#include <net-snmp/net-snmp-config.h>
#include <net-snmp/net-snmp-includes.h>


/** Used to collect results for one type of status by result time, to be able to report statistics during a test.
 *  Key is the response time. Value is the CLOCK_REALTIME at which made the request */
typedef std::multimap<double, struct timespec> snmp_results_by_response_time_map_t;


/** The results for one snmp_synch_response() call */
typedef struct
{
    /* The CLOCK_REALTIME at which the request was made */
    struct timespec start_time;
    /* The status from the snmp_synch_response(), either STAT_SUCCESS or STAT_TIMEOUT */
    int status;
    /* The elapsed time the snmp_synch_response() call took */
    double response_time_secs;
} snmp_response_time_sample_t;


typedef std::vector<snmp_response_time_sample_t> snmp_response_time_sample_vector_t;


/** The results for the SNMP response time test */
typedef struct
{
    /* The CLOCK_REALTIME at which to make the next request */
    struct timespec next_request_time;
    /* Timing for SNMP requests which had a SUCCESS status */
    snmp_results_by_response_time_map_t success;
    /* Timing for SNMP requests which had a TIMEOUT status */
    snmp_results_by_response_time_map_t timeout;
    /* Results for all requests made during the test */
    snmp_response_time_sample_vector_t samples;
} snmp_response_time_results_t;


/** Interval between SNMP requests */
#define SNMP_REQUEST_INTERVAL_SECS 0.2


/*
 * a list of variables to query for.
 * @todo The names have been fully-qualified to allow them to be found.
 */
struct oid_s
{
    const char *name;
    oid oid_numeric[MAX_OID_LEN];
    size_t oid_len;
} oids[] =
{
    { .name = "SNMPv2-MIB::system.sysDescr.0" },
    { .name = "DISMAN-EVENT-MIB::sysUpTimeInstance" },
    { NULL }
};


/** Set from a signal handler to request that the test is stopped */
static volatile bool exit_requested;


/**
 * @brief Signal handler to request the test is stopped
 */
static void stop_test_handler (const int sig)
{
    exit_requested = true;
}


/**
 * @brief Abort the program if an assertion fails, after displaying a message
 * @param[in] assertion Should be true to allow the program to continue.
 * @param[in] format printf style format string for error message.
 * @param[in] ... printf arguments
 */
static void check_assert (const bool assertion, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
static void check_assert (const bool assertion, const char *format, ...)
{
    if (!assertion)
    {
        va_list args;

        va_start (args, format);
        vfprintf (stderr, format, args);
        va_end (args);
        fprintf (stderr, "\n");
        exit (EXIT_FAILURE);
    }
}


/**
 * @brief Convert a struct timespec (seconds and nanoseconds components) into an integer number of nanoseconds
 */
static int64_t convert (const struct timespec &time)
{
    const int64_t nsecs_per_sec = 1000000000;

    return (time.tv_sec * nsecs_per_sec) + time.tv_nsec;
}


/**
 * @brief += operator to add a floating point number of seconds onto a struct timespec
 */
static struct timespec& operator+=(struct timespec &start_time, const double increment_float_secs)
{
    const int64_t nsecs_per_sec = 1000000000;
    const int64_t increment_ns = static_cast<int64_t> (increment_float_secs * 1E9);

    int64_t time_ns = convert (start_time);
    time_ns += increment_ns;
    start_time.tv_sec  = time_ns / nsecs_per_sec;
    start_time.tv_nsec = time_ns % nsecs_per_sec;

    return start_time;
}



/**
 * @brief >= operator to compare two struct timespec values
 */
static bool operator>=(const struct timespec &left, const struct timespec &right)
{
    const int64_t left_ns = convert (left);
    const int64_t right_ns = convert (right);

    return left_ns >= right_ns;
}


/**
 * @brief Perform the host independent SNMP initialisation to find the OID values used by the test.
 */
static void find_oids (void)
{
    struct oid_s *op = oids;

    /* Win32: init winsock */
    SOCK_STARTUP;

    /* initialize library */
    init_snmp ("snmp_response_time");

    /* parse the oids */
    while (op->name)
    {
        op->oid_len = sizeof (op->oid_numeric) / sizeof (op->oid_numeric[0]);
        if (!read_objid(op->name, op->oid_numeric, &op->oid_len))
        {
            snmp_perror ("read_objid");
            exit (EXIT_FAILURE);
        }
        op++;
    }
}


/**
 * @brief Return the elapsed time in nanoseconds between two time stamps
 * @param[in] start_time The start time for the elapsed duration
 * @param[in] end_time The end time for the elapsed duration
 * @return Returns the elapsed time in nanoseconds
 */
static int64_t get_elapsed_ns (const struct timespec &start_time, const struct timespec &end_time)
{
    const int64_t start_time_ns = convert (start_time);
    const int64_t end_time_ns = convert (end_time);

    return end_time_ns - start_time_ns;
}


/**
 * @brief Return the elapsed time in seconds between two time stamps
 * @param[in] start_time The start time for the elapsed duration
 * @param[in] end_time The end time for the elapsed duration
 * @return Returns the elapsed time in nanoseconds
 */
static double get_elapsed_secs (const struct timespec &start_time, const struct timespec &end_time)
{
    return get_elapsed_ns (start_time, end_time) / 1E9;
}


/**
 * @brief Perform one SNMP get, updating the results with the time for either a successful get or a timeout
 * @param[in/out] session SNMP session to the host to perform the get for
 * @param[in] op Which OID to get.
 * @param[in/out] results Updated with the test results
 */
static void time_snmp_get (struct snmp_session *const session, const struct oid_s *const op,
                           snmp_response_time_results_t &results)
{
    int rc;
    struct timespec start_time;
    struct timespec end_time;
    struct snmp_pdu *response;
    snmp_response_time_sample_t sample;
    struct snmp_pdu *const request = snmp_pdu_create (SNMP_MSG_GET);
    snmp_add_null_var (request, op->oid_numeric, op->oid_len);

    rc = clock_gettime (CLOCK_REALTIME, &start_time);
    check_assert (rc == 0, "clock_gettime");
    const int status = snmp_synch_response (session, request, &response);
    rc = clock_gettime (CLOCK_REALTIME, &end_time);
    check_assert (rc == 0, "clock_gettime");
    const double response_time_secs = get_elapsed_secs (start_time, end_time);

    sample.start_time = start_time;
    sample.response_time_secs = response_time_secs;
    sample.status = status;
    results.samples.push_back (sample);

    switch (status)
    {
    case STAT_SUCCESS:
        /* For this test the actual variable retrieved, or even if the variable doesn't exist on the host, doesn't matter */
        results.success.emplace (response_time_secs, start_time);
        break;

    case STAT_TIMEOUT:
        results.timeout.emplace (response_time_secs, start_time);
        break;

    case STAT_ERROR:
        /* Assume that errors are local, and not caused by the host we are performing SNMP queries on */
    default:
        snmp_perror ("snmp_synch_response");
        exit (EXIT_FAILURE);
        break;
    }
    snmp_free_pdu (response);

    if (end_time >= results.next_request_time)
    {
        /* Delayed response past the nominal request interval */
        results.next_request_time = end_time;
    }

    /* Advance to the next request time */
    results.next_request_time += SNMP_REQUEST_INTERVAL_SECS;
}


/**
 * @brief Format a local time as hours/minutes/seconds/nanoseconds
 * @param[in] time_value The local time to format
 * @param[out] time_text Textual version of time_value
 */
#define TIME_STRLEN 40
static void format_local_time (const struct timespec &time_value, char time_text[TIME_STRLEN])
{
    struct tm broken_down_time;

    localtime_r (&time_value.tv_sec, &broken_down_time);
    const size_t len = strftime (time_text, TIME_STRLEN, " %T.", &broken_down_time);
    snprintf (&time_text[len], TIME_STRLEN - len, "%09" PRIi64, time_value.tv_nsec);
}


/**
 * @brief Report to standard out a one line summary for the SNMP response timing for one type of status.
 * @param[in] results The current test results
 * @param[in] status_to_report Which status to report the result for (STAT_SUCCESS or STAT_TIMEOUT)
 */
static void report_test_summary (const snmp_response_time_results_t &results, const int status_to_report)
{
    const snmp_results_by_response_time_map_t &by_response_time = (status_to_report == STAT_SUCCESS) ?
            results.success : results.timeout;
    const char *const status_name = (status_to_report == STAT_SUCCESS) ? "SUCCESS" : "TIMEOUT";
    snmp_results_by_response_time_map_t::const_iterator it;
    char time_text[TIME_STRLEN];

    printf ("%s : %zu results", status_name, by_response_time.size());

    if (!by_response_time.empty())
    {
        it = by_response_time.begin();
        format_local_time (it->second, time_text);
        printf (" min response time = %.6f secs(%s)", it->first, time_text);

        /* Report the median response time as the middle value in the multimap which is keyed by the response times */
        size_t num_steps_to_median = by_response_time.size() / 2;
        while (num_steps_to_median > 0)
        {
            ++it;
            num_steps_to_median--;
        }
        printf (" median response time = %.6f secs", it->first);

        it = by_response_time.end();
        --it;
        format_local_time (it->second, time_text);
        printf (" max response time = %.6f secs(%s)", it->first, time_text);
    }

    printf ("\n");
}


/**
 * @brief Create a CSV file containing the details of all SNMP response times during the test
 * @param[in] csv_details_filename CSV filename to write
 * @param[in]results Contains the results to write
 */
static void write_csv_details (const char *const csv_details_filename, const snmp_response_time_results_t &results)
{
    snmp_response_time_sample_vector_t::const_iterator it;
    char time_text[TIME_STRLEN];

    FILE *const csv_file = fopen (csv_details_filename, "w");
    if (csv_file == NULL)
    {
        fprintf (stderr, "Failed to create %s\n", csv_details_filename);
        exit (EXIT_FAILURE);
    }

    fprintf (csv_file, "Local time,Status,SNMP response time (secs)\n");
    for (it = results.samples.begin(); it < results.samples.end(); ++it)
    {
        format_local_time (it->start_time, time_text);
        fprintf (csv_file, "%s,%s,%.9f\n", time_text, (it->status == STAT_SUCCESS) ? "SUCCESS" : "TIMEOUT", it->response_time_secs);
    }

    fclose (csv_file);
}


int main (int argc, char *argv[])
{
    struct snmp_session ss;
    struct snmp_session *sp;
    struct oid_s *op;
    struct sigaction action;
    int rc;

    if ((argc < 2) || (argc > 3))
    {
        fprintf (stderr, "Usage: %s <hostname> [<csv_details_filename>]\n", argv[0]);
        exit (EXIT_FAILURE);
    }

    char *const hostname = argv[1];
    char *const csv_details_filename = (argc >= 3) ? argv[2] : NULL;

    find_oids ();

    /* Open a SNMP session for v2c using public community so no security required */
    snmp_sess_init (&ss);
    ss.version = SNMP_VERSION_2c;
    ss.peername = strdup (hostname);
    ss.community = reinterpret_cast <u_char *> (strdup ("public"));
    ss.community_len = strlen (reinterpret_cast <const char *> (ss.community));
    sp = snmp_open(&ss);
    if (sp == NULL)
    {
        snmp_perror ("snmp_open");
        exit (EXIT_FAILURE);
    }

    /* Install a signal handler to allow a request to stop of the test */
    printf ("Press Ctrl-C to stop the test\n");
    memset (&action, 0, sizeof (action));
    action.sa_handler = stop_test_handler;
    action.sa_flags = SA_RESTART;
    rc = sigaction (SIGINT, &action, NULL);
    check_assert (rc == 0, "sigaction");

    /* Run the test until requested to stop */
    const double summary_interval_secs = 20.0;
    struct timespec next_summary_time;
    struct timespec now;
    snmp_response_time_results_t results;
    rc = clock_gettime (CLOCK_REALTIME, &results.next_request_time);
    check_assert (rc == 0, "clock_gettime");
    next_summary_time = results.next_request_time;
    next_summary_time += summary_interval_secs;
    while (!exit_requested)
    {
        for (op = oids; !exit_requested && (op->name != NULL); op++)
        {
            clock_nanosleep (CLOCK_REALTIME, TIMER_ABSTIME, &results.next_request_time, NULL);

            time_snmp_get (sp, op, results);

            rc = clock_gettime (CLOCK_REALTIME, &now);
            check_assert (rc == 0, "clock_gettime");
            if (now >= next_summary_time)
            {
                report_test_summary (results, STAT_SUCCESS);
                report_test_summary (results, STAT_TIMEOUT);
                next_summary_time += summary_interval_secs;
            }
        }
    }

    snmp_close(sp);

    /* Display final status */
    printf ("\n");
    report_test_summary (results, STAT_SUCCESS);
    report_test_summary (results, STAT_TIMEOUT);

    if (csv_details_filename != NULL)
    {
        write_csv_details (csv_details_filename, results);
    }

    return EXIT_SUCCESS;
}
