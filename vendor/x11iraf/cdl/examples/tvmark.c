#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "cdl.h"

/* 
 *  TVMARK -- Example task for displaying an marking images.  This program
 *  can be used to either display an image and overlay points defined in
 *  a coordinate file, map an existing display frame for marking, or option-
 *  ally enter a cursor command loop after either of these providing other
 *  marking capability.  All options support minimum match.
 *
 *  Examples:
 *	% tvmark dpix.fits
 * 	% tvmark -coords coords -color 205 dpix.fits
 *	% tvmark -frame 2
 *	% tvmark -coords coords -interactive dpix.fits
 *
 *  Usage:
 * 	tvmark [-frame N] [-fbconfig N] [-coords <file>] [-size N] [-color N]
 *	     [-nolabel] [-fill] [-interactive] [image]
 */


static void	tvmInteractive(), tvmPrintHelp();

#define	SZ_NAME		128


/* typedef	unsigned char	uchar; */	/* Defined in 'cdl.h'	*/


main (argc, argv)
int	argc;
char	*argv[];
{
	CDLPtr	cdl;
	char 	*fname = NULL, *cfname = NULL;
	int	i, status = 0, fill = 0, frame = 1, fb = FB_AUTO, zscale = 1;
	int	color = 201, label = 1, size = 9, interactive = 0;
	float 	z1, z2;
	int	fb_w, fb_h, nf;
	unsigned char *pix = NULL;
	
	/* Process the command line options. */
	if (argc > 1) {
	    for (i=1; i < argc; i++) {
                if (strncmp (argv[i], "-color",4) == 0)
		    color = atoi (argv[++i]);
                else if (strncmp (argv[i], "-coords",4) == 0)
		    cfname = argv[++i];
                else if (strncmp (argv[i], "-fbconfig",3) == 0)
		    fb = atoi (argv[++i]);
                else if (strncmp (argv[i], "-fill",4) == 0)
		    fill = 1;
                else if (strncmp (argv[i], "-frame",3) == 0)
		    frame = atoi (argv[++i]);
                else if (strncmp (argv[i], "-interactive",2) == 0)
		    interactive = 1;
                else if (strncmp (argv[i], "-nolabel",4) == 0)
		    label = 0;
                else if (strncmp (argv[i], "-nozscale",4) == 0)
		    zscale = 0;
                else if (strncmp (argv[i], "-size",2) == 0)
		    size = atoi (argv[++i]);
		else 
		    fname = argv[i];
	    }
	}

	/* Open the package and a connection to the server.  */
	if (!(cdl = cdl_open ((char *)getenv("IMTDEV"))) )
	   exit (-1);

	/* If an image was specified display it first, otherwise assume the
	 * image has already been loaded in the frame and mark that.
	 */
	if (fname) {
	    if (cdl_isIRAF (fname))
	        status = cdl_displayIRAF (cdl, fname, 1, frame, fb, zscale);
	    else if (cdl_isFITS (fname))
	        status = cdl_displayFITS (cdl, fname, frame, fb, zscale);
	    else {
	        if (access (cfname, F_OK) == 0)
	            fprintf (stderr, "'%s': unknown image format.\n", fname);
	 	else
	            fprintf (stderr, "'%s': image doesn't exist.\n", fname);
	        status = 1;
	    }
	    if (status)  goto err_;
	} else {
	
	    /* If we've requested a special frame buffer, set it now. */
	    if (fb > 0)
		cdl_setFBConfig (cdl, fb);

	    /* Map the current display frame for use as an image. */
	    cdl_mapFrame (cdl, frame);
	}

	/* If a coordinate file was specified read the file and mark those
	 * coords with points.
	 */
	if (cfname)
	    (void) cdl_markCoordsFile (cdl, cfname, M_STAR, size, color, label);

	/* Lastly, start up an interactive cursor loop if needed. */
	if (interactive)
	    tvmInteractive (cdl, label, fill, color, size);
	
	/* Close the package and clean up. */
err_:	cdl_close (cdl);			
	exit (status);
}


/*  TVMINTERACTIVE -- Process commands interactively.  */

static void
tvmInteractive (cdl, label, fill, color, size)
CDLPtr	cdl;
int	label, fill, color, size;
{
	float	angle = 0.0, rx, ry, txsize = 1.5, off = 0.5;
	int	nx, ny, i, x, y, x2, y2, font = F_ROMAN, mag = 3;
	int	number=1, radius=11, xrad=11, yrad=6, nannuli=3, sep=5;
	int	linewidth = 1, textwidth = 1, linestyle =  0, wcs = 0;
	char	key, *cp, cmd[SZ_NAME], str[SZ_NAME];
	unsigned char *pix;


	/* Process commands until a 'q' keystrke is hit.
	 */
	while (cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key) != 'q') {
	    x = (int) (rx + off);		/* convert to int pixels */
	    y = (int) (ry + off);

	    switch (key) {
	    case ':':				/* process a colon command */
		putchar (':');
		fgets (str, 64, stdin);
		for (i=0; str[i] != ' ' && str[i]; i++)
		    cmd[i] = str[i];
		cmd[i++] = '\0';

		if (strcmp (cmd, "angle") == 0)
		    angle = atof (&str[i]);
		else if (strcmp (cmd, "color") == 0)
		    color = atoi (&str[i]);
		else if (strcmp (cmd, "fill") == 0)
		    fill = atoi (&str[i]);
		else if (strcmp (cmd, "font") == 0) {
		    switch (str[i]) {
		    case 'r':  font = F_ROMAN;   break;
		    case 'g':  font = F_GREEK;   break;
		    case 't':  font = F_TIMES;   break;
		    case 'f':  font = F_FUTURA;  break;
		    }
		    cdl_setFont (cdl, font);
		} else if (strcmp (cmd, "number") == 0)
		    number = atoi (&str[i]);
		else if (strcmp (cmd, "nannuli") == 0)
		    nannuli = atoi (&str[i]);
		else if (strcmp (cmd, "label") == 0)
		    label = atoi (&str[i]);
		else if (strcmp (cmd, "mag") == 0)
		    mag = atoi (&str[i]);
		else if (strcmp (cmd, "sep") == 0)
		    sep = atoi (&str[i]);
		else if (strcmp (cmd, "size") == 0)
		    size = atoi (&str[i]);
		else if (strcmp (cmd, "txsize") == 0)
		    txsize = atof (&str[i]);
		else if (strcmp (cmd, "radius") == 0)
		    radius = atoi (&str[i]);
		else if (strcmp (cmd, "xrad") == 0)
		    xrad = atoi (&str[i]);
		else if (strcmp (cmd, "yrad") == 0)
		    yrad = atoi (&str[i]);
		else if (strcmp (cmd, "linewidth") == 0 ||
		    strcmp (cmd, "width") == 0) {
		        linewidth = atoi (&str[i]);
		        cdl_setLineWidth (cdl, linewidth);
		} else if (strcmp (cmd, "linestyle") == 0 || 
		    strcmp (cmd, "style") == 0) {
		        linestyle = atoi (&str[i]);
		        cdl_setLineStyle (cdl, linestyle);
		} else if (strcmp (cmd, "textwidth") == 0) {
		    textwidth = atoi (&str[i]);
		    cdl_setTextWidth (cdl, textwidth);
		} else if (strcmp (cmd, "print") == 0) {
                    cdl_readFrameBuffer (cdl, &pix, &nx, &ny);
                    cdl_printPix (cdl, NULL, pix, nx, ny, 1);
		} else if (strcmp (cmd, "snap") == 0) {
                    cdl_readFrameBuffer (cdl, &pix, &nx, &ny);
                    cdl_printPixToFile (cdl, &str[i], pix, nx, ny, 1);
		} else if (strcmp (cmd, "status") == 0) {
		    printf ("angle	= %-5.3g\tcolor	= %d\t",angle, color);
		    printf ("fill	= %-5d\tnumber	= %d\n",fill, number);
		    printf ("nannuli	= %-5d\tsep	= %d\t",nannuli, sep);
		    printf ("size	= %-5d\ttxsize	= %g\n",size, txsize);
		    printf ("xrad	= %-5d\tyrad	= %d\t",xrad, yrad);
		    printf ("label	= %-5d\tfont    = %d\n",label, font);
		    printf ("linewidth  = %-5d\tmag     = %d\n",linewidth, mag);
		    printf ("textwidth  = %-5d\tstyle	= %d\n",
			textwidth, linestyle);
		}
		break;

	    case '?':
		tvmPrintHelp ();
		break;

	    case 'p':				/* plus mark		*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size,
		    M_PLUS, color);
		break;
	    case 'x':				/* cross mark		*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size, 
		    M_CROSS, color);
		break;
	    case '.':				/* point mark		*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size, 
		    M_POINT, color);
		break;
	    case '*':				/* star mark		*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size, 
		    M_STAR, color);
		break;
	    case '_':				/* horiz dash mark	*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size, 
		    M_HBLINE, color);
		break;
	    case '|':				/* vert dash mark	*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size, 
		    M_VBLINE, color);
		break;
	    case 'o':				/* circle mark		*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size, 
		    M_CIRCLE|fill, color);
		break;
	    case 's':				/* square mark		*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size, 
		    M_BOX|fill, color);
		break;
	    case 'v':				/* diamond mark		*/
		cdl_markPoint (cdl, x, y, (label ? number++ : 0), size, 
		    M_DIAMOND|fill, color);
		break;

	    case 'b':				/* Box			*/
		printf ("Hit another key to define the box...\n");
	  	(void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
	    	x2 = (int) (rx + off); 		y2 = (int) (ry + off);
		cdl_markBox (cdl, x, y, x2, y2, fill, color);
		break;
	    case 'c':				/* Circle		*/
		/*
		printf ("Hit another key to set radius ...\n");
	  	(void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
	    	x2 = (int) (rx + off); 		y2 = (int) (ry + off);
		radius = (int) sqrt ((double) ((x2-x)*(x2-x) + (y2-y)*(y2-y)));
		*/
		cdl_markCircle (cdl, x, y, radius, fill, color);
		break;
	    case 'd':				/* Delete marker	*/
		cdl_deleteMark (cdl, x, y);
		break;
	    case 'e':				/* Ellipse		*/
		cdl_markEllipse (cdl, x, y, xrad, yrad, angle, fill, color);
		break;
	    case 'l':				/* Line			*/
		printf ("Hit another key to set line endpoint...\n");
	  	(void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
	    	x2 = (int) (rx + off); 		y2 = (int) (ry + off);
		cdl_markLine (cdl, x, y, x2, y2, color);
		break;
	    case 't':				/* Text string		*/
		printf ("Text string: ");
		fgets (str, 64, stdin);
		for (cp = (char *)str; *cp != NULL && (char)*cp != '\n'; cp++)
		    ;
		*cp = '\0';
		cdl_markText (cdl, x, y, str, txsize, angle, color);
		break;
	    case 'C':				/* Circular annuli	*/
		cdl_markCircAnnuli (cdl, x, y, radius, nannuli, sep, color);
		break;
	    case 'D':				/* Delete all markers	*/
		cdl_clearOverlay (cdl);
		break;
	    case 'E':				/* Elliptical annuli	*/
		cdl_markEllipAnnuli (cdl, x, y, xrad, yrad, angle, nannuli,
		    sep, color);
		break;
	    case 'P':				/* Polygon		*/
		break;


	    case 'z':
		/* A more complex example to display a rectangular region
		 * 'size' pixels wide to an area 'mag' times bigger on each
	 	 * side.  The first keystroke in the center of the zoomed
		 * region, the second is the center of the magnified area.
		 * Corners are connected and boxes drawn around each area.
		 * An expensive oepration but you can a cute result.
		 */
		printf ("Hit another key to position zoomed raster...\n");
	  	(void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
	    	x2 = (int) (rx + off); 		y2 = (int) (ry + off);

		{   register int i, j, ii, jj;
		    int	llx = x - size / 2, lly = y - size / 2,
		        urx = x + size / 2, ury = y + size / 2;
		    int	zllx = x2 - (mag*size) / 2, zlly = y2 - (mag*size) / 2,
		        zurx = x2 + (mag*size) / 2, zury = y2 + (mag*size) / 2;
		    uchar *ras = (uchar *) malloc (size*size);
		    uchar *zoom = (uchar *) malloc ((mag*size)*(mag*size));
		    uchar *rp, *zp, *line;

		    /* Read the image pixels and magnify it.  */
		    cdl_readSubRaster (cdl, llx, lly, size, size, &ras);
		    rp = ras, zp = zoom;
		    for (i=0; i < size; i++) {
			line = ras + i * size;
			for (ii=0; ii < mag; ii++) {
			    for (j=0; j < size; j++) {
			        for (jj=0; jj < mag; jj++)
			            *zp++ = *rp;
			        rp++;
			    }
			    rp = line;
			}
		    }

		    /* Draw a box around the zoom area and connect the corners
		     * to the zoomed subraster. */
		    cdl_markBox (cdl, llx, lly, urx, ury, 0, color);
		    cdl_markLine (cdl, llx, lly, zllx, zlly, color);
		    cdl_markLine (cdl, llx, ury, zllx, zury, color);
		    cdl_markLine (cdl, urx, lly, zurx, zlly, color);
		    cdl_markLine (cdl, urx, ury, zurx, zury, color);

		    /* Now draw the magnified raster and put a box around it.*/
		    cdl_writeSubRaster (cdl,zllx,zlly,mag*size,mag*size,zoom);
		    cdl_markBox (cdl, zllx, zlly, zurx, zury, 0, color);
		}

	    default:
		break;
	    }
	}
}


/*  TVMPRINTHELP -- Print a help summary.  */

static void
tvmPrintHelp ()
{
    printf ("\t\t\tCommand Summary\n");
    printf ("\n");
    printf ("    :angle <real>    - set ellipse of text rotation angle\n");
    printf ("    :color <int>     - set marker color\n");
    printf ("    :fill  <0|1>     - set fill option (zero or one)\n");
    printf ("    :font  <font>    - set text font (roman, greek, times)\n");
    printf ("    :number <int>    - set point number\n");
    printf ("    :nannuli <int>   - set number of annuli\n");
    printf ("    :label <0|1>     - set point label option\n");
    printf ("    :linewidth <int> - set line width\n");
    printf ("    :linestyle <int> - set line style\n");
    printf ("    :textwidth <int> - set text width\n");
    printf ("    :mag <val>       - set magnify value for 'z' keystroke\n");
    printf ("    :sep <int>       - set annuli separation (pixels)\n");
    printf ("    :size <int>      - set point marker size\n");
    printf ("    :txsize <real>   - set relative text size\n");
    printf ("    :xrad <int>      - set ellipse x radius\n");
    printf ("    :yrad <int>      - set ellipse y radius\n");
    printf ("    :status          - print current settings\n");
    printf ("    :snap <file>     - snap frame buffer as EPS to file\n");
    printf ("    :print           - print frame buffer to default printer\n");
    printf ("\n");
    printf ("Point Markers:\n");
    printf ("    v  - diamond mark \tp  - plus mark \t\tx  - cross mark\n");
    printf ("    .  - point mark \t*  - star mark \t\t_  - horiz dash mark\n");
    printf ("    |  - vert dash mark o  - circle mark \ts  - square mark\n");
    printf ("\n");
    printf ("Misc. Commands\n");
    printf ("    ?  - Print Help \tq  - Quit\n");
    printf ("    b  - Box\t\tc  - Circle\n");
    printf ("    d  - Delete marker\te  - Ellipse marker\n");
    printf ("    l  - Line\t\tt  - Text string\n");
    printf ("    C  - Circular annuli D  - Delete all markers\n");
    printf ("    E  - Elliptical annuli\n");
    printf ("    z  - zoom in on region\n");
}
