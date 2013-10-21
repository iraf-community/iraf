#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "fitsio.h"


int voc_imgQParams (char *name, double *ra, double *dec, double *rad);


int main (int argc, char *argv[])
{
    double  ra, dec, rad;
    int     status = 0;

    status = voc_imgQParams (argv[1], &ra, &dec, &rad);
    printf ("RA=%f  DEC=%f  SZ=%f\n", ra, dec, rad);
}


int voc_imgQParams (char *name, double *ra, double *dec, double *rad)
{
    fitsfile *fptr;

    double  xpos=0.0, ypos=0.0, xpix=0.0, ypix=0.0;
    double  xrval=0.0, yrval=0.0, xrpix=0.0, yrpix=0.0;
    double  xinc=0.0, yinc=0.0, rot=0.0;
    double  ll_x=0.0, ll_y=0.0, ur_x=0.0, ur_y=0.0, c_x=0.0, c_y=0.0;
    long    naxes[3], pcount, gcount;
    int     status=0, extend, simple, naxis, bitpix;
    char    ctype[5];


    /*  Open the FITS file.
     */
    if (ffopen (&fptr, name, READWRITE, &status) > 0) {
	fprintf (stderr, "Error: open status = %d\n", status);
	exit (0);
    }

    /*  Get the primary header keywords.
     */
    ffghpr (fptr, 99, &simple, &bitpix, &naxis, naxes, &pcount,
           &gcount, &extend, &status);

fprintf (stderr, "gcount = %d  pcount = %d  extend = %d \n", gcount, pcount, extend);

    /*  Get the header WCS keywords.
     */
    ffgics (fptr, &xrval, &yrval, &xrpix,
               &yrpix, &xinc, &yinc, &rot, ctype, &status);
    if (status != 506)
        fprintf (stderr, "Read WCS keywords with ffgics status = %d\n",status);


    xpix = 0.5;
    ypix = 0.5;
    status = 0;
    if (ffwldp (xpix, ypix, xrval, yrval, xrpix, yrpix, xinc, yinc, rot, ctype,
               &ll_x, &ll_y, &status) > 0) {
        fprintf (stderr, "Calculated sky coordinate with ffwldp status = %d\n",
	    status);

    } else {
        status = 0;
        xpix = (double) naxes[0];
        ypix = (double) naxes[1];
        ffwldp (xpix, ypix, xrval, yrval, xrpix, yrpix, xinc, yinc, rot, ctype,
           &ur_x, &ur_y, &status);

        status = 0;
        xpix = (double) naxes[0] / 2.;
        ypix = (double) naxes[1] / 2.;
        ffwldp (xpix, ypix, xrval, yrval, xrpix, yrpix, xinc, yinc, rot, ctype,
           &c_x, &c_y, &status);
    }

    *ra   = c_x;
    *dec  = c_y;
    *rad  = sqrt ((c_x - ll_x)*(c_x - ll_x) + (c_y - ll_y)*(c_y - ll_y));

#ifdef DBG_WCS
    printf ("  CRVAL1, CRVAL2 = %16.12f, %16.12f\n", xrval, yrval);
    printf ("  CRPIX1, CRPIX2 = %16.12f, %16.12f\n", xrpix, yrpix);
    printf ("  CDELT1, CDELT2 = %16.12f, %16.12f\n", xinc, yinc);
    printf ("  Rotation = %10.3f, CTYPE = %s\n", rot, ctype);
    printf ("  Pixels (%8.4f,%8.4f) --> (%11.6f, %11.6f) Sky\n",
        xpix, ypix, ll_x, ll_y);

    fprintf (stderr, "RA=%f  DEC=%f  SZ=%f\n", c_x, c_y,
	sqrt ((c_x - ll_x)*(c_x - ll_x) + (c_y - ll_y)*(c_y - ll_y)));

    ffxypx (xpos, ypos, xrval, yrval, xrpix, yrpix, xinc, yinc, rot, ctype,
           &xpix, &ypix, &status);
    printf ("Calculated pixel coordinate with ffxypx status = %d\n", status);
    printf ("  Sky (%11.6f, %11.6f) --> (%8.4f,%8.4f) Pixels\n",
            xpos, ypos, xpix, ypix);
#endif


    if (ffclos (fptr, &status) > 0) {
	fprintf (stderr, "Error: close status = %d\n", status);
	return (-1);
    }

    return (0);
}
