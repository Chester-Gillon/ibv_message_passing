/*
 * @file read_usb_id_dongle.c
 * @date 12 Dec 2020
 * @author Chester Gillon
 * @brief Read the CBUS[3:0] GPIO input pins on a FTDI FT201XQ USB device
 * @details Written to investigate creating a USB dongle which can read identity bits.
 *          Requires that the FT201XQ EEPROM has been configured to set CBUS[3:0] as GPIO.
 *
 *          Works with ftdi_sio loaded as the driver.
 *
 *          Tested by connecting one CBUS pin in turn to ground, using the weak pull-ups in the device.
 *
 *          To allow a a non-root user to access the device:
 *          a. The user had to in the dialout group.
 *          b. Added a /etc/udev/rules.d/99-libftdi.rules with:
 *
 *          SUBSYSTEMS=="usb", ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6015", GROUP="dialout", MODE="0660"
 *
 * @todo Attempted to use ftdi_read_eeprom() followed by ftdi_eeprom_decode() to check the configuration of the CBUS pins, but
 *       ftdi_eeprom_decode() reported an invalid checksum.
 *
 *       UMFT201XB_delivered_configuration.xml contains the configuration read by the Windows FT_prog reports that the
 *       delivered configuration for a UMTF201XB-01 was CBUS[3:0] set as GPIO.
 */

#include <stdlib.h>
#include <stdio.h>

#include <ftdi.h>


int main (int argc, char *argv[])
{
    const int ftdi_vendor = 0x0403;
    const int FT201X_product_id = 0x6015;
    struct ftdi_context ftdic;
    int rc;

    rc = ftdi_init (&ftdic);
    if (rc < 0)
    {
        fprintf (stderr, "ftdi_init failed\n");
        exit (EXIT_FAILURE);
    }

    rc = ftdi_usb_open (&ftdic, ftdi_vendor, FT201X_product_id);
    if (rc < 0)
    {
        fprintf(stderr, "unable to open ftdi device: %d (%s)\n", rc, ftdi_get_error_string(&ftdic));
        exit(-1);
    }

    const int all_input_bitmask = 0;
    rc = ftdi_set_bitmode(&ftdic, all_input_bitmask, BITMODE_CBUS);
    if (rc < 0)
    {
        fprintf(stderr, "unable to set BITMODE_CBUS: %d (%s)\n", rc, ftdi_get_error_string(&ftdic));
        exit(-1);
    }

    unsigned char pins;
    rc = ftdi_read_pins(&ftdic, &pins);
    if (rc < 0)
    {
        fprintf(stderr, "unable to read CBUS pins: %d (%s)\n", rc, ftdi_get_error_string(&ftdic));
        exit(-1);
    }

    /* The ID is the lower four bits, read from the CBUS[3:0] pins */
    unsigned int id = pins & 0xf;
    printf ("id=0x%x\n", id);

    ftdi_deinit (&ftdic);

    return EXIT_SUCCESS;
}
