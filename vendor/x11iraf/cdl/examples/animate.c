#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>
#include "cdl.h"

/* ANIMATE -- Example task to demonstrate rapid display of rasters such as
 * would be done in an animation sequence, or in an acquisition system
 * which needs to display guider images repeatedly (the actual inspiration
 * for this demo.
 *
 * Examples:
 *	Display a scrolling test pattern:
 *	    % animate 
 *
 * Usage:
 *	animate [-nx N] [-ny N] [-fbconfig N]
 *
 *	Note that your choice of frame buffer is important since if it's
 * larger than the image size you're spending a lot of overhead in writing
 * blank pixels around the actual image.  When writing a true subraster of 
 * the image/frame buffer, the routine first reads back the pixels in the 
 * affected rows, edits the pixels in the subraster columns and then writes 
 * back the entire row.  If the subraster width is the same size as the 
 * frame buffer width it does a straight write which will speed up the dis-
 * play since you avoid the readback overhead.  It may be worth defining a 
 * custom frame buffer the size of your image to speed things up.
 */

#define	DIAGONAL	0			/* test pattern flags 	*/
#define	VERTICAL	1
#define	HORIZONTAL	2

#define max(a,b)        (a > b ? a : b) 	/* Utility macros  	*/
#define min(a,b)        (a < b ? a : b)

	
CDLPtr	cdl;
uchar	*pix = NULL;

main (argc, argv)
int	argc;
char	*argv[];
{
	register int i, frame=1;
	int	nx=256, ny=256, fbconfig=0;
	int	status=0, fb_w, fb_h, nf, lx, ly;
	uchar	*line;

	/* Handle command line args. */
	if (argc > 1) {
	    for (i=1; i < argc; i++) {
		if (strcmp (argv[i], "-nx") == 0)
		    nx = atoi (argv[++i]);
		else if (strcmp (argv[i], "-ny") == 0)
		    ny = atoi (argv[++i]);
		else if (strcmp (argv[i], "-fbconfig") == 0)
		    fbconfig = atoi (argv[++i]);
	    }
	}

	/* Open the package and a connection to the server. */
	if (!(cdl = cdl_open ((char *)getenv("IMTDEV"))) )
	   exit (-1);

	/* Make a test pattern. */
	pix = (uchar *) malloc (nx * ny);
	makeTestPattern (pix, nx, ny, DIAGONAL);

	/* Now select a frame buffer large enough for the image. The 
	 * fbconfig number is passed in the WCS packet, but the display
	 * calls below will compute the correct WCS for the image and
	 * transmit that prior to display, all we're doing here is
	 * setting up the FB to be used.
	 */
	if (fbconfig == 0)
            cdl_selectFB (cdl, nx, ny, &fbconfig, &fb_w, &fb_h, &nf, 1);
	else
	    cdl_lookupFBSize (cdl, fbconfig, &fb_w, &fb_h, &nf);


	/* For the WCS we assume a simple linear transform where the image is
	 * Y-flipped, the (x,y) translation is computed so it is correct
	 * for an frame buffer >= than the image size.  The Z-transform is
	 * fixed since we're using a test pattern with known values.
	 */
        cdl_setWCS (cdl, "test pattern", "", 1., 0., 0., -1., 
            (float) (nx / 2) - (fb_w / 2) + 1,	    /* X translation */
            (float) (fb_h / 2) + (ny / 2),	    /* Y translation */
	    0.0, 200.0, CDL_LINEAR);		    /* Z transform   */


	/* Select and clear the initial frame prior to display. */
	cdl_setFrame (cdl, frame);
	cdl_clearFrame (cdl);

	/* Now display the pixels.  We'll compute the image placement
	 * ourselves and write the image as a raw subraster of the frame
	 * buffer in the center of the display.
	 */
        lx = (fb_w / 2) - (nx / 2);
        ly = fb_h - ((fb_h / 2) + (ny / 2));
        status = cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix);

	/* Now animate the image. */
	line = (uchar *) malloc (nx);
	while (1) {
	    /* Shift the image down one line, roll the botton line to the
	     * top and redisplay the subraster/
	     */
	    memcpy (pix, line, nx);
	    memmove (pix+nx, pix, (ny-1)*nx);
	    memcpy (line, pix+((ny-1)*nx), nx);

            status = cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix);
	}


	cdl_close (cdl);			/* close the package 	*/
	free ((unsigned char *) pix);		/* free the raster	*/
	exit (status);
}


/* Make a test pattern. */

makeTestPattern (raster, nx, ny, pattern)
uchar   *raster;
int	nx, ny, pattern;
{
	register uchar pix;
	register int i, j;
	register float scale;

    	for (i = 0; i < nx; i++) {
            for (j = 0; j < ny; j++) {
		switch (pattern) {
		case DIAGONAL: 				/* Diagonal ramp */
          	    scale =  200. / (float)(ny) / 2.;
            	    pix =  (uchar) max(2, (min(200,(scale*i + scale*j))));
		    break;
		case VERTICAL: 				/* Vertical ramp */
            	    scale =  200. / (float)(ny);
            	    pix =  (uchar) max(2, (min(200,(scale * i))));
		    break;
		case HORIZONTAL:			/* Horizontal ramp */
            	    scale =  200. / (float)(nx);
            	    pix =  (uchar) max(2, (min(200,(scale * j))));
		    break;
                }
            	raster[i * nx + j] = pix;
            }
	}
}
