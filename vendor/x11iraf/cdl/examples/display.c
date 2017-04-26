#include <stdio.h>
#ifdef ULTRIX
#include <sys/types.h>
#endif
#include <sys/types.h>
#include <unistd.h>
#include "cdl.h"

/* 
 *  DISPLAY -- Example task to display an image as a command-line task.
 *  This task is meant to show three ways the CDL can be used to display
 *  and image, it is functional but perhaps not highly efficient for this
 *  reason.  See the code comments for a description of each method.
 *
 *  Examples:
 *      To display a simple IRAF or FITS file:
 *	    % ./display -frame 2 image.imh
 *          % ./display image.fits
 *
 *      To display a FITS file as a raw image:
 *          % ./display -nx 512 -ny 512 -depth 16 -hskip 5760 -raw dpix.fits
 *
 *  Usage:
 * 	display [-depth N] [-fits] [-frame N] [-fbconfig N] [-hskip N]
 *	    [-iraf] [-nozscale] [-nx N] [-ny N] [-raw] [-zscale] 
 *	    [-ns N] [-nl N] [-log] file
 */

#define NONE	-1
#define	IRAF	 0
#define	FITS	 1
#define	RAW	 2

#define ABS(x)	(x > 0 ? x : -x)

#ifdef SOLARIS
char    *getcwd();
#else
char    *getwd();
#endif


main (argc, argv)
int	argc;
char	*argv[];
{
	CDLPtr	cdl;
	char 	*fname;
	int	i, log = 0, status = 0, frame = 1, fbconfig = 0, zscale = 1;
	int	format = NONE, nx = 0, ny = 0, depth = 8, hskip = 0;
	int	ns = -1, nl = -1;
	float 	z1, z2;
	int	fb_w, fb_h, nf;
	unsigned char *pix = NULL;
        char    *path_prefix = (char *) calloc (1025, sizeof(char));
        char    *path = (char *) calloc (512, sizeof(char));
        char    *node = (char *) calloc (512, sizeof(char));

	
	/* Process the command line options. */
	if (argc > 1) {
	    for (i=1; i < argc; i++) {
                if (strcmp (argv[i], "-depth") == 0)
		    depth = atoi (argv[++i]);
                else if (strcmp (argv[i], "-fits") == 0)
		    format = FITS;
                else if (strcmp (argv[i], "-frame") == 0)
		    frame = atoi (argv[++i]);
                else if (strcmp (argv[i], "-fbconfig") == 0)
		    fbconfig = atoi (argv[++i]);
                else if (strcmp (argv[i], "-hskip") == 0)
		    hskip = atoi (argv[++i]);
                else if (strcmp (argv[i], "-iraf") == 0)
		    format = IRAF;
                else if (strcmp (argv[i], "-log") == 0)
		    log++;
                else if (strcmp (argv[i], "-nozscale") == 0)
		    zscale = 0;
                else if (strcmp (argv[i], "-ns") == 0)
		    ns = atoi (argv[++i]);
                else if (strcmp (argv[i], "-nl") == 0)
		    nl = atoi (argv[++i]);
                else if (strcmp (argv[i], "-nx") == 0)
		    nx = atoi (argv[++i]);
                else if (strcmp (argv[i], "-ny") == 0)
		    ny = atoi (argv[++i]);
                else if (strcmp (argv[i], "-raw") == 0)
		    format = RAW;
                else if (strcmp (argv[i], "-zscale") == 0)
		    zscale = 1;
	    }
	}

	/* Open the package and a connection to the server. */
	if (!(cdl = cdl_open ((char *)getenv("IMTDEV"))) )
	   exit (-1);

	fname = argv[argc-1];

	if (ns >  0)  cdl_setSample (cdl, ns);
	if (nl >  0)  cdl_setSampleLines (cdl, nl);
	if (log > 0)  cdl_setZTrans (cdl, CDL_LOG);

	/* METHOD 1:  Displays the image using the high-level format display
 	 * call.  Display as an IRAF image if the option was set indicating
	 * this is the format, otherwise test the file to see if it is anyway.
	 */
	if (format == IRAF || (format == NONE && cdl_isIRAF (fname))) {
	    status = cdl_displayIRAF (cdl, fname, 1, frame,
		(fbconfig==0 ? FB_AUTO : fbconfig), zscale);


	/* METHOD 2:  Uses the CDL procedure for getting image pixels from
	 * a known format, minimal work required to display an image.  The
 	 * point here is that you can use this mthod to process the image
	 * yourself prior to display, e.g. subsample the pixels, apply a user
	 * LUT, etc but still use the CDL to get the raw image and do the
	 * display.
	 */
	} else if (format == FITS || (format == NONE && cdl_isFITS (fname))) {
	    float *bscale, *bzero;
	    char  title[80];

	    /* Get the FITS image pixels, exit w/ an error status if something
	     * went wrong, the procedure will print what that was.
	     */
            if (cdl_readFITS (fname, &pix, &nx, &ny, &depth, title)) {
		cdl_close (cdl);		/* close the package  */
		exit (1);			/* exit w/ error code */
	    }

	    /* Now select a frame buffer large enough for the image. The 
	     * fbconfig number is passed in the WCS packet, but the display
	     * call below will compute the correct WCS for the image and
	     * transmit that prior to display, all we're doing here is
	     * setting up the FB to be used.
	     */
	    if (fbconfig == 0)
                cdl_selectFB (cdl, nx, ny, &fbconfig, &fb_w, &fb_h, &nf, 0);

	    /* Lastly, display the pixels to the requested frame, do any
	     * zscaling requested using the CDL procedure.  
	     */
	    (void) cdl_setTitle (cdl, title);
            (void) cdl_setName (cdl, fname);
            if (cdl_displayPix(cdl, pix, nx, ny, depth, frame, fbconfig,zscale))
                status = 1;
 
	    /* Now just free the pixel pointer to clean up.
	     */
	    free ((unsigned char *) pix);


	/* METHOD 3:  Displays an image of raw pixels.  The client code is
	 * responsible for reading the image and calling all the procedures
	 * needed for image display, initialize the frame, zscaling pix, etc.
	 * While we assume a simple raster format in this program, the user
	 * code can read a compressed image format such as GIF, mosaic multiple
	 * images for display as a single image, or just about anything that
	 * produces a raster for display. The intent here is to show all the
	 * lowest level calls needed for displaying the image.
	 */
	} else if (format == RAW) {
	    FILE	*fd;
	    int		lx, ly;

	    if (nx == 0 || ny == 0) {
	        fprintf (stderr, "No size given for raw data.\n");
	        exit (1);
	    }

	    /* Open the image file if we can. */
	    if (fd = fopen (fname, "r")) {

		/* Seek to the offset specified. */
		lseek (fileno(fd), (off_t) hskip, SEEK_SET);

		/* Allocate the pixel pointer and read the data. */
		pix = (unsigned char *) malloc (nx * ny * (ABS(depth) / 8));
		fread (pix, ABS(depth)/8, nx * ny, fd);

		/* If we're zscaling and depth is more than 8-bits, do that. */
		if (zscale && ABS(depth) > 8) {
		    cdl_computeZscale (cdl, pix, nx, ny, depth, &z1, &z2);
		    cdl_zscaleImage (cdl, &pix, nx, ny, depth, z1, z2);
		}

		/* Select and clear the requested frame prior to display. */
	        cdl_setFrame (cdl, frame);
		cdl_clearFrame (cdl);

		/* Now select a frame buffer large enough for the image.  
		 * We'll ask that this be reset but the change won't go to
		 * the server until we send in the WCS below.  
		 */
                cdl_selectFB (cdl, nx, ny, &fbconfig, &fb_w, &fb_h, &nf, 1);

		/* Compute the image placement so it's centered in the frame,
		 * but note the cdl_writeSubRaster() routine can place an
		 * arbitrary raster anywhere in the frame buffer.  
        	lx = (fb_w / 2) - (nx / 2);
        	ly = fb_h - ((fb_h / 2) + (ny / 2));

		/* Set the mapping we'll send with the WCS which must be
		 * called before the cdl_setWCS() call since the data is sent
		 * with the WCS and not as a separate call.
		 */

                /* First we must compose a node!path prefix for the image */
                gethostname (node, 512);
#ifdef SOLARIS
                (void) getcwd (path, 512);
#else
                (void) getwd (path);
#endif
                if (*fname == '/')
                    (void) sprintf (path_prefix, "%s!%s", node, fname);
                else
                    (void) sprintf (path_prefix, "%s!%s/%s", node, path, fname);

		cdl_setMapping (cdl, "image", 0., 0., nx, ny, lx, ly, nx, ny,
		    path_prefix);

		/* For the WCS we assume a simple linear transform where the
		 * image is Y-flipped, the (x,y) translation is computed so
		 * it is correct for an frame buffer >= than the image size.
		 */
                cdl_setWCS (cdl, fname, "", 1., 0., 0., -1., 
                    (float) (nx / 2) - (fb_w / 2) + 1,	    /* X trans.     */
                    (float) (fb_h / 2) + (ny / 2),	    /* Y trans.     */
		    z1, z2, CDL_LINEAR);		    /* Z transform  */


		/* Now display the pixels.  
		 */
                if (cdl_writeSubRaster (cdl, lx, ly, nx, ny, pix))
                    status = 1;

		/* Now just free the pixel pointer to clean up.
		 */
		free ((unsigned char *) pix);
	        fclose (fd);

	    } else
		status = 1;

	} else {
            if (access (fname, F_OK) == 0)
	        fprintf (stderr, "'%s': unknown image format.\n", fname);
	    else
                fprintf (stderr, "'%s': image does not exist.\n", fname);
	    status = 1;
	}
	
	cdl_close (cdl);			/* close the package */
	exit (status);
}
