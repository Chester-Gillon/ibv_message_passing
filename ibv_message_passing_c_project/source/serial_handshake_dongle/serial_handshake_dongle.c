/*
 * @file serial_handshake_dongle.c
 * @date 16 Oct 2021
 * @author Chester Gillon
 * @brief Created to investigate using serial port handshake signals under Linux to create an identity dongle.
 * @details This program doesn't verify the identity, but cycles around the combinations of two handshake output signals
 *          reporting the values readback from the four handshake input signals, available on a 9-pin serial port.
 *
 *          The idea is to be able to create an identity by connecting the handshake output signals to one or more
 *          of the handshake signals. By driving the output signals as complementary outputs should be able to
 *          read back an identity, and filter out invalid combinations due to a missing wire. If there is a missing
 *          external wire then the affected input(s) will read back as a fixed value for both output combinations.
 *
 *          The possible identities which can be read are:
 *
 *          Output pins connected to input pins   Output values    Expected input values
 *           DCD  DSR  CTS  RI                      DTR  RTS         DCD  DSR  CTS  RI
 *           DTR  RTS  RTS  RTS                      0    1           0    1    1    1
 *                                                   1    0           1    0    0    0
 *
 *           RTS  DTR  RTS  RTS                      0    1           1    0    1    1
 *                                                   1    0           0    1    0    0
 *
 *           DTR  DTR  RTS  RTS                      0    1           0    0    1    1
 *                                                   1    0           1    1    0    0
 *
 *           RTS  RTS  DTR  RTS                      0    1           1    1    0    1
 *                                                   1    0           0    0    1    0
 *
 *           DTR  RTS  DTR  RTS                      0    1           0    1    0    1
 *                                                   1    0           1    0    1    0
 *
 *           RTS  DTR  DTR  RTS                      0    1           1    0    0    1
 *                                                   1    0           0    1    1    0
 *
 *           DTR  DTR  DTR  RTS                      0    1           0    0    0    1
 *                                                   1    0           1    1    1    0
 *
 *           RTS  RTS  RTS  DTR                      0    1           1    1    1    0
 *                                                   1    0           0    0    0    1
 *
 *           DTR  RTS  RTS  DTR                      0    1           0    1    1    0
 *                                                   1    0           1    0    0    1
 *
 *           RTS  DTR  RTS  DTR                      0    1           1    0    1    0
 *                                                   1    0           0    1    0    1
 *
 *           DTR  DTR  RTS  DTR                      0    1           0    0    1    0
 *                                                   1    0           1    1    0    1
 *
 *           RTS  RTS  DTR  DTR                      0    1           1    1    0    0
 *                                                   1    0           0    0    1    1
 *
 *           DTR  RTS  DTR  DTR                      0    1           0    1    0    0
 *                                                   1    0           1    0    1    1
 *
 *           RTS  DTR  DTR  DTR                      0    1           1    0    0    0
 *                                                   1    0           0    1    1    1
 */

#include <stdlib.h>
#include <stdio.h>

#include <time.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>


typedef struct
{
    const char *name;
    int bit_mask;
} handshake_bit_t;


/* Defines the output handshake pins on a standard 9 way D-Type serial port */
#define NUM_OUTPUT_HANDSHAKE_PINS 2
static const handshake_bit_t handshake_outputs[NUM_OUTPUT_HANDSHAKE_PINS] =
{
    {.name = "DTR", .bit_mask = TIOCM_DTR}, /* 9 way D-Type pin 4 */
    {.name = "RTS", .bit_mask = TIOCM_RTS}  /* 9 way D-Type pin 7 */
};


/* Defines the input handshake pins on a standard 9 way D-Type serial port */
#define NUM_INPUT_HANDSHAKE_PINS 4
static const handshake_bit_t handshake_inputs[NUM_INPUT_HANDSHAKE_PINS] =
{
    {.name = "DCD", .bit_mask = TIOCM_CD},  /* 9 way D-Type pin 1 */
    {.name = "DSR", .bit_mask = TIOCM_DSR}, /* 9 way D-Type pin 6 */
    {.name = "CTS", .bit_mask = TIOCM_CTS}, /* 9 way D-Type pin 8 */
    {.name = "RI ", .bit_mask = TIOCM_RI}   /* 9 way D-Type pin 9 */
};


int main (int argc, char *argv[])
{
    int fd;
    int modem_bits;
    int rc;
    int bit;

    if (argc != 2)
    {
        fprintf (stderr, "Usage: %s <serial_device>\n", argv[0]);
        exit (EXIT_FAILURE);
    }
    const char *const serial_device = argv[1];

    fd = open (serial_device, O_RDWR);
    if (fd == -1)
    {
        perror ("open() failed");
        exit (EXIT_FAILURE);
    }

    /* Gives a 10 millisecond delay after setting the handshake output pins before sampling the handshake input pins,
     * to give time for the signals to settle. */
    const struct timespec settle_delay =
    {
        .tv_sec = 0,
        .tv_nsec = 10000000
    };

    /* Write headers */
    printf ("Outputs   Inputs\n");
    for (bit = 0; bit < NUM_OUTPUT_HANDSHAKE_PINS; bit++)
    {
        printf ("%s ", handshake_outputs[bit].name);
    }
    printf ("  ");
    for (bit = 0; bit < NUM_INPUT_HANDSHAKE_PINS; bit++)
    {
        printf ("%s ", handshake_inputs[bit].name);
    }
    printf ("\n");

    /* Step through the combinations of output handshake pins */
    for (int output_combo = 0; output_combo < (1 << NUM_OUTPUT_HANDSHAKE_PINS); output_combo++)
    {
        /* Perform a read-modify-write to set the output handshake pins */
        rc = ioctl (fd, TIOCMGET, &modem_bits);
        if (rc != 0)
        {
            perror ("ioctl(TIOCMGET) failed");
            exit (EXIT_FAILURE);
        }
        for (bit = 0; bit < NUM_OUTPUT_HANDSHAKE_PINS; bit++)
        {
            if ((output_combo & (1 << bit)) != 0)
            {
                modem_bits |= handshake_outputs[bit].bit_mask;
            }
            else
            {
                modem_bits &= ~handshake_outputs[bit].bit_mask;
            }
        }
        rc = ioctl (fd, TIOCMSET, &modem_bits);
        if (rc != 0)
        {
            perror ("ioctl(TIOCMSET) failed");
            exit (EXIT_FAILURE);
        }

        /* Sample the handshake pins after a settle delay */
        nanosleep (&settle_delay, NULL);
        rc = ioctl (fd, TIOCMGET, &modem_bits);
        if (rc != 0)
        {
            perror ("ioctl(TIOCMGET) failed");
            exit (EXIT_FAILURE);
        }

        /* Display the handshake output and input pins */
        for (bit = 0; bit < NUM_OUTPUT_HANDSHAKE_PINS; bit++)
        {
            printf (" %s  ", ((modem_bits & handshake_outputs[bit].bit_mask) != 0) ? "1" : "0");
        }
        printf ("  ");
        for (bit = 0; bit < NUM_INPUT_HANDSHAKE_PINS; bit++)
        {
            printf (" %s  ", ((modem_bits & handshake_inputs[bit].bit_mask) != 0) ? "1" : "0");
        }
        printf ("\n");
    }

    close (fd);

    return EXIT_SUCCESS;
}
