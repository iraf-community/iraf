#include <stdio.h>
#include <unistd.h>
#include "cdl.h"

/*  MOSAIC --  Example task to demostrate a possible mosaic display
 *  application.  Image names may optionally be drawn on each tile of
 *  the mosaic by specifying the -label and -color flags.  The -pad flag
 *  can be used to put a space between images, -sample is used to sub-
 *  sample an image before display.  Assumes all images are the same size.
 *
 *  Examples:
 *      To display four images with a gap in a 1600x1600 frame buffer
 *	    % mosaic -nx 2 -ny 2 -fbconfig 4 -pad 20 foo[1234].imh
 *	To display a set of images, subsampling each by 4 pixels
 *	    % mosaic -nx 2 -ny 2 -pad -10 -sample 4 foo[1234].imh
 *
 *  Usage:
 *	mosaic [-fbconfig N] [-frame N] [-color N] [-label] [-nozscale]
 *	    [-raw -xdim N -ydim N -bitpix N [-hskip N] ]
 *	    [-nx N] [-ny N] [-pad N] [-sample N]image1 image2 .....
 */

#define ABS(x)  (x > 0 ? x : -x)


main (argc, argv)
int	argc;
char	**argv;
{
	CDLPtr	cdl;
	char 	*fname = NULL, title[128];
	int	i, j, k, status=0, label=0, frame=1, fb=FB_AUTO, zscale=1;
	int	sample=1, pad=0, col=204, imx=0, imy=0, nimages, nim;
	int	ii, xinit, rowx, rowy, nnx, nny, fb_w, fb_h, nf, mx, my;
 	int	raw = 0, bitpix=0, hskip=0;
	int	nx = 1, ny = 1;
	float 	z1, z2;
	unsigned char *pix = NULL;
	

	/* Process the command line options. */
	if (argc > 1) {
	    for (i=1; i < argc; i++) {
                if (strncmp (argv[i], "-fbconfig",3) == 0)
		    fb = atoi (argv[++i]);
                else if (strncmp (argv[i], "-frame",3) == 0)
		    frame = atoi (argv[++i]);
                else if (strncmp (argv[i], "-color",3) == 0)
		    col = atoi (argv[++i]);
                else if (strncmp (argv[i], "-label",4) == 0)
		    label = 1;
                else if (strncmp (argv[i], "-nozscale",4) == 0)
		    zscale = 0;
                else if (strncmp (argv[i], "-nx",3) == 0)
		    nx = atoi (argv[++i]);
                else if (strncmp (argv[i], "-ny",3) == 0)
		    ny = atoi (argv[++i]);
                else if (strncmp (argv[i], "-pad",4) == 0)
		    pad = atoi (argv[++i]);
                else if (strncmp (argv[i], "-raw",4) == 0)
		    raw++;
                else if (strncmp (argv[i], "-xdim",4) == 0)
		    imx = atoi (argv[++i]);
                else if (strncmp (argv[i], "-ydim",4) == 0)
		    imy = atoi (argv[++i]);
                else if (strncmp (argv[i], "-bitpix",4) == 0)
		    bitpix = atoi (argv[++i]);
                else if (strncmp (argv[i], "-hskip",4) == 0)
		    hskip = atoi (argv[++i]);
                else if (strncmp (argv[i], "-sample",4) == 0) {
		    sample = atoi (argv[++i]);
		    if (sample % 2) {
			fprintf (stderr, "ERROR: Sample size must be even\n");
			exit (-1);
		    }
		} else 
		    break;
	    }
	} else {
   	    printf("Usage:\n\tmosaic ");
 	    printf("[-fbconfig N] [-frame N] [-color N] [-label] [-nozscale]");
 	    printf("\n\t[-raw -xdim N -ydim N -bitpix N [-hskip N] ]");
 	    printf("\n\t[-nx N] [-ny N] [-pad N] [-sample N] image1 ....\n");
	    exit (1);
	}
	nimages = argc - i;


	/* Open the package and a connection to the server. */
	if (!(cdl = cdl_open ((char *)getenv("IMTDEV"))) )
	   exit (-1);

	/* Clear the frame to begin. */
        (void) cdl_clearFrame (cdl);

	/* Loop over each of the images in the list. */
	nim = 0;
	rowx = rowy = 0;
	nnx = nny = 0;
	for (k=0; k < ny && nim < nimages; k++) {
	    rowy += nny + pad; 
	    for (rowx = xinit, j=0; j < nx && nim < nimages; j++) {

		/* Get the image name for display. */
		fname = argv[i++];

		/* Figure out what kind of image it is and get the pixels. */
            	if (!raw && cdl_isIRAF (fname))
		    status = cdl_readIRAF (fname, 1, &pix, &imx, &imy, &bitpix,
			title);
            	else if (!raw && cdl_isFITS (fname))
		    status = cdl_readFITS (fname, &pix, &imx, &imy, &bitpix, 
			title);
            	else {
		    if (raw) {
			/* It's a raw data array, be sure we have everyting
			 * we need in order to read it.
			 */
			if (imx == 0 || imy == 0 || bitpix == 0) {
                            fprintf(stderr, "ERROR: Use of '-raw' requires ");
                            fprintf(stderr, "'-xdim', '-ydim' and '-bitpix'\n");
                            status = 1;
			} else {
			    FILE *fd;

		            /* Open the image file if we can. */
		            if (fd = fopen (fname, "r")) {

                	        /* Seek to the offset specified. */
                	        lseek (fileno(fd), (off_t) hskip, SEEK_SET);

                	        /* Allocate the pixel pointer and read data. */
                	        pix = (pix ? pix : (uchar *) malloc (
				    imx*imy*(ABS(bitpix) / 8)) );
                	        fread (pix, ABS(bitpix)/8, imx*imy, fd);
				fclose (fd);
			    } else
                                status = 1;
			}
		    } else {
			/* Didn't set the raw flag, and we don't know what
			 * it is....punt.
			 */
                        if (access (fname, F_OK) == 0)
                            fprintf (stderr, "'%s': unknown image format.\n",
				fname);
                        else
                            fprintf (stderr, "'%s': image doesn't exist.\n",					fname);
                        status = 1;
		    }
            	}
            	if (status)  goto err_; 

		/* Compute subsampled image size. */
		if (sample > 1)
		    nnx = imx / sample, nny = imy / sample;
		else
		    nnx = imx, nny = imy;

		/* Unless we asked for a specific FB size find one large enough
		 * to handle the mosaic.  We don't check to be sure what's
		 * returned is really large enough.
		 */
		if (nim == 0 && fb == FB_AUTO) {
                    cdl_selectFB (cdl, nx*nnx+(pad*(nx-1)), ny*nny+(pad*(ny-1)),
		        &fb, &fb_w, &fb_h, &nf, 1);
		} else {
		    cdl_setFBConfig (cdl, fb);
		    cdl_lookupFBSize (cdl, fb, &fb_w, &fb_h, &nf);
		}

		/* Define a WCS for the frame. */
                cdl_setWCS (cdl, "image mosaic", "", 1., 0., 0., -1., 0.,
                    (float) ny*imy+(pad*(ny+1)), 1., 255., 1);

		/* The first time through figure out the placement so the
	   	 * entire mosaic is centered in the frame.
		 */
		if  (nim == 0) {
		    mx = (nx * nnx) + pad * (nx-1);
		    my = (ny * nny) + pad * (ny-1);
		    rowy = (fb_h - my) / 2;
		    xinit = rowx = (fb_w - mx) / 2;
		}

		/* Compute the zscaled imaged pixels. */
		if (zscale) {
                    cdl_computeZscale (cdl, pix, imx ,imy, bitpix, &z1, &z2);
		    printf ("%s: z1=%g  z2=%g\n", fname, z1, z2);
                    cdl_zscaleImage (cdl, &pix, imx ,imy, bitpix, z1, z2);
		}

		/* Subsample the image if requested. */
		if (sample > 1) {
		    int l, m, n;

		    for (l=0, n=0; l < imy; l+=sample)
		        for (m=0; m < imx; m+=sample)
		            pix[n++] = pix[(l*imx)+m];
		}

		/* Write the image to the frame buffer. */
            	if (cdl_writeSubRaster (cdl, rowx, rowy, nnx, nny, pix))
		    goto err_; 

		/* Draw the image name as a label. */
		if (label) 
		    cdl_markText (cdl, rowx+10, rowy+10, fname, 1., 0., col);

		nim++;
	        rowx += nnx + pad; 
	    }
	}
	
	/* Close the package and clean up. */
err_:	cdl_close (cdl);			
	free ( (char *) pix);
	exit (status);
}
