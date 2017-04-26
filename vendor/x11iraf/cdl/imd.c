#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <netdb.h>
#include <fcntl.h>
#include <stdio.h>
#define  CDL_LIBRARY_SOURCE
#include "cdl.h"


/*
 *  IMAGE DISPLAY -- The image display interface is responsible for actually
 *  displaying an image to the server, for reading back a raster from the
 *  server, and cursor positioning.  This is a mid-level interface for 
 *  handling the steps necessary for image display operations without dealing
 *  directly with the details of communicating with the server.
 *
 *           imd = imd_open  (imtdev)
 *         imd_displayImage  (imd, pix, nx, ny, frame, fbconfig, comp_wcs)
 *           imd_readCursor  (imd, sample, &x, &y, &wcs, &key)
 *         imd_[set|get]WCS  (imd, name, title, a, b, c, d, tx, ty, z1, z2, zt)
 *     imd_[set|get]Mapping  (imd, region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)
 *                imd_close  (imd)
 *
 *     Low Level Procedures
 *     --------------------
 *           imd_writeImage  (imd, pix, nx, ny, lx, ly)
 *            imd_readImage  (imd, &pix, &nx, &ny)
 *      imd_readFrameBuffer  (imd, &pix, &nx, &ny)
 *             imd_setFrame  (imd, frame)
 *          imd_setFBConfig  (imd, configno)
 *          imd_getFBConfig  (imd, &configno, &width, &height, &nframes))
 *              imd_setName  (imd, name)
 *             imd_setTitle  (imd, title)
 *            imd_setCursor  (imd, x, y, wcs)
 *           imd_clearFrame  (imd)
 *       imd_writeSubRaster  (imd, lx, ly, nx, ny, pix)
 *        imd_readSubRaster  (imd, lx, ly, nx, ny, &pix)
 *
 *      We leave it to the higher level procedures to handle Z-scale trans-
 *  formations, spatial scaling, and high level image I/O.  All display pixels
 *  are assumed to be scaled to 8-bits already.
 */

/* Function prototypes */
#ifdef __STDC__
#include <stddef.h>
#include <stdlib.h>
#endif

#define	SZ_BLOCK	16384

/* Types of coordinate and greyscale transformations. */
#define W_UNITARY       0               /* values map without change	*/
#define W_LINEAR        1               /* linear mapping		*/
#define W_LOG           2               /* logarithmic mapping		*/
#define W_USER          3               /* user transformation 		*/

/* Connection types. */
#define	UNIX		10
#define	INET		11
#define	FIFO		12

/* Default Values. */
#define	DEF_FBCONFIG	1		/* default frame buffer config 	*/
#define DEF_OSDEV_1     "unix:/tmp/.IMT%d"
#define DEF_OSDEV_2     "fifo:/dev/imt1i:/dev/imt1o"

/* Frame buffer configuration file definitions. */
#define FBCONFIG_1      ".imtoolrc"
#define FBCONFIG_2      "/usr/local/lib/imtoolrc"
#define FBCONFIG_ENV1   "imtoolrc"
#define FBCONFIG_ENV2   "IMTOOLRC"      
#define DEF_FRAME_WIDTH  512
#define DEF_FRAME_HEIGHT 512


int	imd_debug = 0;			/* deug flag	*/
char	buf[SZ_LINE];			/* temp buffer	*/


#ifdef ANSI_FUNC

static int imd_writeLine(IMDPtr imd, uchar *pix, int nbytes, int x, int y);
static int imd_readLine(IMDPtr imd, uchar *pix, int nbytes, int x, int y);
static int imd_writeRawBuf(IMDPtr imd, uchar *pix, int nbytes, int x, int y, int nx, int ny);
static int imd_readRawBuf(IMDPtr imd, uchar *pix, int nbytes, int x, int y, int nx, int ny);
static IMDPtr imd_initialize(int fdin, int fdout, int domain);
static int imd_parseImtdev(char *imtdev, char *unixaddr, unsigned short *host_port, unsigned long *host_addr, char *ififo, char *ofifo);
static int imd_loadImtoolrc(IMDPtr imd);
static int imd_getstr(char **ipp, char *obuf, int maxch);
static void imd_minmax(uchar *pix, int nbytes, int *pmin, int *pmax);
static int imd_wcsVersion (IMDPtr imd);

#else

static  IMDPtr  imd_initialize();
static  int 	imd_writeLine(), imd_readLine();
static  int     imd_writeRawBuf(), imd_readRawBuf();
static  int     imd_parseImtdev(), imd_loadImtoolrc(), imd_getstr();
static  int     imd_writeLine(), imd_readLine(), imd_wcsVersion();
static  void    imd_minmax();

#endif

#ifndef __STDC__
	char	*getenv();
#endif



/*  IMD_OPEN -- Open a connection to the display server.  The caller may 
 *  either specify a connection at device open time, or the procedure will
 *  attempt to first connect on a unix socket or fifo pipe if that fails.
 *  The syntax for the imtdev argument is as follows:
 *
 *      	<domain> : <address>
 *
 *  where <domain> is one of "inet" (internet tcp/ip socket), "unix" (unix
 *  domain socket) or "fifo" (named pipe).  The form of the address depends
 *  upon the domain, as illustrated in the examples below.
 *
 *  inet:5137                   Server connection to port 5137 on the local
 *                              host.  For a client, a connection to the
 *                              given port on the local host.
 *
 *  inet:5137:foo.bar.edu       Client connection to port 5137 on internet
 *                              host foo.bar.edu.  The dotted form of address
 *                              may also be used.
 *
 *  unix:/tmp/.IMT212           Unix domain socket with the given pathname
 *                              IPC method, local host only.
 *
 *  fifo:/dev/imt1i:/dev/imt1o  FIFO or named pipe with the given pathname.
 *                              IPC method, local host only.  Two pathnames
 *                              are required, one for input and one for
 *                              output, since FIFOs are not bidirectional.
 *                              For a client the first fifo listed will be
 *                              the client's input fifo; for a server the
 *                              first fifo will be the server's output fifo.
 *                              This allows the same address to be used for
 *                              both the client and the server, as for the
 *                              other domains.
 *
 *  The address field may contain up to two "%d" fields.  If present, the
 *  user's UID will be substituted (e.g. "unix:/tmp/.IMT%d"). The default
 *  connection if no imtdev is specified is "unix:/tmp/.IMT%d", failing that,
 *  a connection is attempted on the /dev/imt1[io] named fifo pipes.
 */

#ifdef ANSI_FUNC

IMDPtr 
imd_open (
    char *imtdev		/* connection type 	*/
)
#else

IMDPtr
imd_open (imtdev)
char 	*imtdev;		/* connection type 	*/
#endif
{
	IMDPtr	imd;
	int	domain, fd, fdin, fdout, free_imtdev=0, model;
	unsigned short host_port;
	unsigned long  host_addr;
	char 	unixaddr[SZ_NAME];
	char 	input_fifo[SZ_NAME], output_fifo[SZ_NAME];


	if (imtdev == NULL) {
	    struct  sockaddr_un sockaddr;

	    /* Try first to connect on a unix socket. */
	    if ((fd = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
		imtdev = DEF_OSDEV_2;
		if (imd_debug)
		    printf ("Can't get unix socket...\n");
		goto retry;
	    }

	    /* Compose network address. */
            bzero ((char *)&sockaddr, sizeof(sockaddr));
            sockaddr.sun_family = AF_UNIX;
            sprintf (sockaddr.sun_path, "/tmp/.IMT%d", (int)getuid());

	    if (connect(fd, (struct sockaddr *)&sockaddr, sizeof(sockaddr))<0){
                close (fd);
		imtdev = DEF_OSDEV_2;
		if (imd_debug)
		    printf ("Can't connect to socket '%s'\n",sockaddr.sun_path);
	        goto retry;		/* no connection */
            } else {
		imtdev = (char *) calloc (SZ_IMTDEV, sizeof(char));
		free_imtdev++;
		strcpy (imtdev, sockaddr.sun_path);
                fdin = fdout = fd;
	    }
	    model = DEF_MODEL;

	} else {
retry: 	    domain =  imd_parseImtdev (imtdev, unixaddr, &host_port, &host_addr,
		input_fifo, output_fifo, &model);

	    switch (domain) {
	    case FIFO:
               /* Open the fifos. */
                if ((fdin = open (input_fifo, O_RDONLY|O_NDELAY)) != ERR)
                    fcntl (fdin, F_SETFL, O_RDONLY);
                if ((fdout = open (output_fifo, O_WRONLY|O_NDELAY)) != ERR)
                    fcntl (fdout, F_SETFL, O_WRONLY);

                /* Clean up if there is an error. */
                if (fdin < 0 || fdout < 0) {
		    (void) close (fdin);
		    (void) close (fdout);
		    if (imd_debug)
		        printf ("Can't connect to fifo '%s'\n",input_fifo);
                    goto err;
		}
                break;     

	    case UNIX:
		{   struct  sockaddr_un sockaddr;

	            /* Try first to connect on a unix socket. */
	            if ((fd = socket (AF_UNIX, SOCK_STREAM, 0)) < 0) {
			if (imd_debug)
		    	    printf ("Can't get unix socket...\n");
		        goto err;
		    }

	            /* Compose network address. */
                    bzero ((char *)&sockaddr, sizeof(sockaddr));
                    sockaddr.sun_family = AF_UNIX;
                    strcpy (sockaddr.sun_path, unixaddr);

	            if (connect(fd, (struct sockaddr *)&sockaddr, 
		        sizeof(sockaddr))<0){
                            close (fd);
			    if (imd_debug)
		    	        printf ("Can't connect to socket '%s'\n",
				    sockaddr.sun_path);
	                    goto err;		/* no connection */
                    } else
                        fdin = fdout = fd;
		}
		break;

	    case INET:
		{   struct  sockaddr_in sockaddr;

                    /* Get socket. */
                    if ((fd = socket (AF_INET, SOCK_STREAM, 0)) < 0) {
			if (imd_debug)
		    	    printf ("Can't get inet socket...\n");
                        goto err;
		    }

                    /* Compose network address. */
                    bzero ((char *)&sockaddr, sizeof(sockaddr));
                    sockaddr.sin_family = AF_INET;
                    sockaddr.sin_port = host_port;
                    bcopy ((char *)&host_addr, (char *)&sockaddr.sin_addr,
                        sizeof(host_addr));

                    /* Connect to server. */
                    if (connect (fd, (struct sockaddr *)&sockaddr, 
		        sizeof(sockaddr)) < 0) {
                            close (fd);
			    if (imd_debug)
		    	        printf ("Can't connect to socket '%s'\n",
				    (char *)sockaddr.sin_addr.s_addr);
                            goto err;
                    } else
                        fdin = fdout = fd;
		}
		break;

	    default:
		goto err;
	    } 
	}

	/* Allocate and initialize imd structure.  */
	imd = imd_initialize (fdin, fdout, domain, model);


	if (imd_debug)
	    fprintf (stderr, "Connection established on '%s'\n", imtdev);
	if (free_imtdev)
	    free ((char *)imtdev);
	return (imd);

err:
	fprintf (stderr, "Cannot open server connection on '%s'.\n", imtdev);
	if (free_imtdev)
	    free ((char *)imtdev);
	return NULL;
}


/* IMD_DISPLAYIMAGE --  Display an image to the server, setting the WCS and
 * frame as needed.  This is a high-level procedure used to make image
 * display easy.  It is assumed that the pixel array has already been scaled
 * to 8-bits.
 */

int
imd_displayImage (imd, pix, nx, ny, frame, fbconfig, comp_wcs)
IMDPtr	imd;			/* package pointer	  */
uchar 	*pix;			/* pixels to display	  */
int	nx, ny;			/* dimensions		  */
int	frame;			/* frame to display	  */
int	fbconfig;		/* fb config number	  */
int	comp_wcs;		/* compute a WCS	  */
{
	register uchar	*ip = pix;
	register uchar	*bp, *pp;
	register int i, nnx = nx, nny = ny;
	int	status, use_subras = 0;
	int	pmin, pmax, x_off = 0, y_off = 0, lx, ly;
	int	fbwidth  = imd->fbtab[fbconfig-1]->width;
	int	fbheight = imd->fbtab[fbconfig-1]->height;
	int	imd_writeImage();

	if (imd_debug)
	    printf ("[imd_displayImage] frame=%d fb=%d->[%d,%d] %dx%d bytes\n",
		frame, fbconfig, fbwidth, fbheight, nx, ny);

	imd_setFrame (imd, frame);			/* select frame	*/
	imd_clearFrame (imd);				/* erase frame	*/
	imd_setFBConfig (imd, fbconfig);		/* set fbconfig	*/

	/* Check to see if the image to be displayed is larger than the
	 * current frame buffer.  If so we'll need to pull out a subraster
	 * the size of the FB for display.
	 */
	if (nx > fbwidth || ny > fbheight) {
	    nnx = min(nx, fbwidth);		/* get new dimensions */
	    nny = min(ny, fbheight);
	    bp = ip = (uchar *) calloc (nnx * nny, sizeof (uchar));
	    use_subras++;

	    /* Pull out the subraster for display */
	    x_off =  (nx>fbwidth ? ((nx - fbwidth) / 2 - 1) : 0);
	    y_off =  (ny>fbheight ? ((ny - fbheight) / 2 - 1) : 0);
	    pp = pix + (y_off * nx) + x_off;
	    for (i=0; i < nny; i++) {
		bcopy (pp, bp, nnx);
		pp += nx;
		bp += nnx;
	    }
	}

	if (imd_debug)
	    printf("[imd_displayImage] nnx=%d nny=%d xo=%d yo=%d\n",
		nnx, nny, x_off, y_off);

	/* Center the placement in user coords.
	 */
	lx = (fbwidth / 2) - (nnx / 2);
	ly = fbheight - ((fbheight / 2) + (nny / 2));

	/* Compute a WCS for this image if it's not already defined. */
	if (comp_wcs) {
	    imd->a  =  1.0;
	    imd->b  =  0.0;
	    imd->c  =  0.0;
	    imd->d  = -1.0;
	    imd->tx = (float) (nnx / 2) - (fbwidth / 2) + 1 + (float) x_off;
	    imd->ty = (float) (fbheight / 2) + (nny / 2) + (float) y_off;

	    if (imd->z1 == INDEF || imd->z2 == INDEF) {
	        imd_minmax (pix, nnx*nny, &pmin, &pmax);
		imd->z1 = (imd->z1 == INDEF) ? (float) pmin : imd->z1;
		imd->z2 = (imd->z2 == INDEF) ? (float) pmax : imd->z2;
	    }
	    imd->ztrans = W_LINEAR;

            /* Initialize the mapping. */
            imd->sx        = 1.0;
            imd->sy        = 1.0;
            imd->snx       = nnx;
            imd->sny       = nny;
            imd->dx        = lx;
            imd->dy        = ly;
            imd->dnx       = nnx;
            imd->dny       = nny;
            imd->iis_valid = 1;

	    if (imd_setWCS (imd, imd->name, imd->title, imd->a, imd->b, imd->c,
		imd->d, imd->tx, imd->ty, imd->z1, imd->z2, imd->ztrans))
	            return (ERR);
	}

	/* Finally, display the image.
	 */
	status = imd_writeImage (imd, ip, nnx, nny, lx, ly);

	if (use_subras)
	    free ((char *)ip);
	return (status);
}


/* IMD_READCURSOR -- Read the current cursor position.  If sample is defined
 * logical cursor position will be sampled and returned immediately, otherwise
 * the server will block until a key is hit and we return that value as well.
 */

#ifdef ANSI_FUNC

int 
imd_readCursor (
    IMDPtr imd,			/* package pointer	*/
    int sample,			/* wait for keystroke?	*/
    float *x,
    float *y,			/* position		*/
    int *wcs,			/* WCS			*/
    char *key			/* keystroke		*/
)
#else

int
imd_readCursor (imd, sample, x, y, wcs, key)
IMDPtr	imd;			/* package pointer	*/
int	sample;			/* wait for keystroke?	*/
float	*x, *y;			/* position		*/
int 	*wcs;			/* WCS			*/
char	*key;			/* keystroke		*/
#endif
{
	if (imd_debug)
	    printf ("[imd_readCursor]\n");

	return (com_readCursor(imd->datain, imd->dataout, 
	    sample, x, y, wcs, key));
}


/* IMD_SETWCS -- Set the WCS of the screen.  The WCS is passed in a string
 * defined as:
 *      	Image_Name_String\n a b c d tx ty z1 z2 zt
 * where:
 *              X' = a*X + c*Y + tx
 *              Y' = b*X + d*Y + ty
 *
 * z1 is the minimum pixel value, z2 is the maximum pixel value,  zt
 * defines the type of transformation to use.
 */

#ifdef ANSI_FUNC

int 
imd_setWCS (
    IMDPtr imd,			/* package pointer	*/
    char *name,			/* name string		*/
    char *title,		/* title string		*/
    float a,
    float b,
    float c,
    float d,			/* WCS values		*/
    float tx,
    float ty,			/* translation		*/
    float z1,
    float z2,			/* zscale values	*/
    int zt			/* transformation type	*/
)
#else

int
imd_setWCS (imd, name, title, a, b, c, d, tx, ty, z1, z2, zt)
IMDPtr	imd;			/* package pointer	*/
char	*name;			/* name string		*/
char	*title;			/* title string		*/
float	a, b, c, d;		/* WCS values		*/
float	tx, ty;			/* translation		*/
float	z1, z2;			/* zscale values	*/
int	zt;			/* transformation type	*/
#endif
{
	int   nbytes = 0, status = 0, version = imd->iis_version;
        char  old_wcs[SZ_OLD_WCSBUF], mapping[SZ_OLD_WCSBUF];
        char  wcstext[SZ_WCSBUF];


        /* Format the string to set world coordinate parameters.  */
        (void) sprintf (old_wcs, "%s%s%s\n%g %g %g %g %g %g %g %g %d\n",
	    (*name ? name : " "), (title ? " - " : " "), (*title ? title : " "),
            a, b, c, d, tx, ty, z1, z2, zt);

	if (imd->iis_version > 0 && imd->iis_valid) {
            char  *path_prefix, *node, *path;
#ifdef SOLARIS
            char    *getcwd();
#else
            char    *getwd();
#endif

	    path_prefix = (char *) calloc (1025, sizeof(char));
	    path = (char *) calloc (512, sizeof(char));
	    node = (char *) calloc (512, sizeof(char));


	    /* Get a node!path prefix for the data file. */
            gethostname (node, SZ_FNAME);

#ifdef SOLARIS
            (void) getcwd (path, 511);
#else
            (void) getwd (path);
#endif
	    if (*name == '/')
                (void) sprintf (path_prefix, "%s!%s", node, name);
	    else
                (void) sprintf (path_prefix, "%s!%s/%s", node, path, name);

            (void) sprintf (mapping, "%s %g %g %d %d %d %d %d %d\n%s\n",
                (*imd->region ?  imd->region : "image"),
                imd->sx, imd->sy, imd->snx, imd->sny,
                imd->dx, imd->dy, imd->dnx, imd->dny,
                (*imd->ref ? imd->ref : path_prefix));

	    (void) sprintf (wcstext, "%s%s", old_wcs, mapping);

	    free ((char *)path_prefix);
	    free ((char *)path);
	    free ((char *)node);

	} else
	    (void) strcpy (wcstext, old_wcs);


	if (imd_debug) { printf ("[imd_setWCS] wcs='%s'\n", wcstext); }

	/* Send the WCS buffer. */
        nbytes = strlen (wcstext) + 1;
	status = com_writeWCS (imd->dataout, wcstext, nbytes, version);

	/* Invalidate the mapping once it's been sent. */
	imd->iis_valid = 0;

	return (status);
}


#ifdef USE_OLD_GETWCS

/* IMD_OLDGETWCS -- Get the current display frame WCS information.
 */

#ifdef ANSI_FUNC

int 
imd_getWCS (
    IMDPtr imd,			/* package pointer	*/
    int wcs,			/* WCS number to get	*/
    char *name,			/* name string		*/
    char *title,		/* title string		*/
    float *a,
    float *b,
    float *c,
    float *d,			/* WCS values		*/
    float *tx,
    float *ty,			/* translation		*/
    float *z1,
    float *z2,			/* zscale values	*/
    int *zt			/* transformation type	*/
)
#else

int
imd_getWCS (imd, wcs, name, title, a, b, c, d, tx, ty, z1, z2, zt)
IMDPtr	imd;			/* package pointer	*/
int 	wcs;			/* WCS number to get	*/
char	*name;			/* name string		*/
char	*title;			/* title string		*/
float	*a, *b, *c, *d;		/* WCS values		*/
float	*tx, *ty;		/* translation		*/
float	*z1, *z2;		/* zscale values	*/
int	*zt;			/* transformation type	*/
#endif
{
	int   datain   = imd->datain;
	int   dataout  = imd->dataout;
	int   nbytes = 0, version = imd->iis_version;
	int   tokens, status = 0;
	char  wcs_info[SZ_WCSBUF];


	/* Read the WCS from the server. */
	if (com_readWCS (datain, dataout, wcs_info, &nbytes, wcs, version))
	    return (ERR);

	/* Parse the return buffer. */
	name[0] = title[0] = '\0';
	if (version == 0) {
            tokens = sscanf (wcs_info, "%[^\n]\n%g%g%g%g%g%g%g%g%d",
                buf, a, b, c, d, tx, ty, z1, z2, zt);
            if (tokens == EOF) {
                /* Had no WCS info, can't even get name, but no error.  */
                *a = *b = *c = *d = *tx = *ty = *z1 = *z2 = *zt = 0;

            } else if (tokens < 10) {
                /* partial read, something must be wrong.  */
                return (ERR);
            }
	    sscanf (buf, "%s - %s", name, title);
	}

	if (imd_debug) { 
	    printf ("[imd_getWCS] wcs=%d wcsbuf='%s'\n", wcs, wcsbuf);
	}
 
	return (OK);
}
#endif


/* IMD_GETWCS -- Get the current display frame WCS information.
 */

#ifdef ANSI_FUNC

int 
imd_getWCS (
    IMDPtr imd,			/* package pointer	*/
    int wcs,			/* WCS number to get	*/
    char *name,			/* name string		*/
    char *title,		/* title string		*/
    float *a,
    float *b,
    float *c,
    float *d,			/* WCS values		*/
    float *tx,
    float *ty,			/* translation		*/
    float *z1,
    float *z2,			/* zscale values	*/
    int *zt			/* transformation type	*/
)
#else

int
imd_getWCS (imd, wcs, name, title, a, b, c, d, tx, ty, z1, z2, zt)
IMDPtr	imd;			/* package pointer	*/
int 	wcs;			/* WCS number to get	*/
char	*name;			/* name string		*/
char	*title;			/* title string		*/
float	*a, *b, *c, *d;		/* WCS values		*/
float	*tx, *ty;		/* translation		*/
float	*z1, *z2;		/* zscale values	*/
int	*zt;			/* transformation type	*/
#endif
{
	int   fbwidth  = imd->fbtab[imd->fbconfig-1]->width;
	int   fbheight = imd->fbtab[imd->fbconfig-1]->height;
	int   datain   = imd->datain;
	int   dataout  = imd->dataout;
	int   nbytes = 0, version = imd->iis_version;
	int   tokens, status = 0;
	char  wcsbuf[SZ_WCSBUF];


	/* Read the WCS from the server. */
	if (com_readWCS (datain, dataout, wcsbuf, &nbytes, wcs, version))
	    return (ERR);

	if (imd_debug) { 
	    printf ("[imd_getWCS] wcs=%d wcsbuf='%s'\n", wcs, wcsbuf);
	}
 
        /* Attempt to read the WCS and set up a unitary transformation
         * if the information cannot be read.
         */
        if (sscanf (wcsbuf, "%[^\n]\n%f%f%f%f%f%f%f%f%d",
            buf, a, b, c, d, tx, ty, z1, z2, zt) < 7) {

            strcpy (title, "[NO WCS]\n");
            *a  = *d  = 1;
            *b  = *c  = 0;
            *tx = *ty = 0;
            *zt = 0;
        } else {
	    if (name == NULL) 
		name = imd->name;
	    if (title == NULL) 
		title = imd->title;
	    sscanf (buf, "%s - %s", name, title);
        }

	/* Now try to read the mapping if we're expecting one, otherwise
	 * set up a default mapping of the entire frame buffer.
	 */
	if (imd->iis_version > 0) {
	    register int i, j;

	    /* Skip over the first two lines of WCS data. */
	    strcpy (buf, wcsbuf);
	    for (i=0, j=0; j < 2 && buf[i]; i++)
	        if (buf[i] == '\n') j++;

	    /* Attempt to read the mapping. */
	    imd->ref[0] = '\0';
	    imd->region[0] = '\0';
	    if (sscanf (&buf[i], "%s%f%f%d%d%d%d%d%d\n%s\n",
	        imd->region, &imd->sx, &imd->sy, &imd->snx, &imd->sny, 
	        &imd->dx, &imd->dy, &imd->dnx, &imd->dny, imd->ref) < 10) {

		    /* Error in the read, set a default. */
	            strcpy (imd->region, "none");
	            strcpy (imd->ref, "none");
	            imd->sx  = 1.0;
	            imd->sy  = 1.0;
	            imd->snx = fbwidth;
	            imd->sny = fbheight;
	            imd->dx  = 1;
	            imd->dy  = 1;
	            imd->dnx = fbwidth;
	            imd->dny = fbheight;

        	    imd->iis_valid = 0;
		    return (ERR);
	    } else
        	imd->iis_valid = 1;
	}

	return (OK);
}


/*  IMD_SETMAPPING -- Set the mapping information to be sent with the next
 *  imd_setWcs() call.
 */
#ifdef ANSI_FUNC

int 
imd_setMapping  (
    IMDPtr  imd,                /* package ptr          */
    char    *region,            /* region name 		*/
    float   sx,			/* source rect		*/
    float   sy,
    int	    snx, 		/* source extent	*/
    int	    sny,
    int	    dx, 		/* dest rect		*/
    int	    dy,
    int	    dnx, 		/* dest extent		*/
    int	    dny,
    char    *ref                /* reference name	*/
)
#else

int 
imd_setMapping  (imd, region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)
IMDPtr  imd;                    /* package ptr          */
char    *region;                /* region name 		*/
float	sx, sy;			/* source rect		*/
int	snx, sny;		/* source extent	*/
int	dx, dy;			/* dest rect		*/
int	dnx, dny;		/* dest extent		*/
char    *ref;                  	/* reference name	*/
#endif
{
        if (imd_debug) {
            printf ("[imd_setMapping] region='%s' ref='%s'\n",
		( region ? region : ""), (ref ? ref : ""));
            printf ("\tsrc = %g,%g%d,%d   dest = %d,%d,%d,%d\n",
               sx, sy, snx, sny, dx, dy, dnx, dny);
        }

        strcpy (imd->region, (region ? region : ""));
        strcpy (imd->ref, (ref ? ref : ""));
        imd->sx   = sx;
        imd->sy   = sy;
        imd->snx  = snx;
        imd->sny  = sny;
        imd->dx   = dx;
        imd->dy   = dy;
        imd->dnx  = dnx;
        imd->dny  = dny;

        return ((imd->iis_valid = 1));
}


/*  IMD_GETMAPPING --  Get the mapping information returned with the last
 *  imd_getWcs() call.
 */
#ifdef ANSI_FUNC
int 
imd_getMapping  (
    IMDPtr  imd,                /* package ptr          */
    char    *region,            /* region name 		*/
    float   *sx,		/* source rect		*/
    float   *sy,
    int	    *snx, 		/* source extent	*/
    int	    *sny,
    int	    *dx, 		/* dest rect		*/
    int	    *dy,
    int	    *dnx, 		/* dest extent		*/
    int	    *dny,
    char    *ref                /* reference name	*/
)

#else
int 
imd_getMapping  (imd, region, sx,sy,snx,sny, dx,dy,dnx,dny, ref)
IMDPtr  imd;                    /* package ptr          */
char    *region;                /* region name 		*/
float	*sx, *sy;		/* source rect		*/
int	*snx, *sny;		/* source extent	*/
int	*dx, *dy;		/* dest rect		*/
int	*dnx, *dny;		/* dest extent		*/
char    *ref;                  	/* reference name	*/
#endif
{
        if (imd->iis_valid) {
            strcpy (region, imd->region);
            *sx  = imd->sx;
            *sy  = imd->sy;
            *snx = imd->snx;
            *sny = imd->sny;
            *dx  = imd->dx;
            *dy  = imd->dy;
            *dnx = imd->dnx;
            *dny = imd->dny;
            strcpy (ref, imd->ref);
	}

        if (imd_debug) {
            printf ("[imd_getMapping] valid=%d region='%s' ref='%s'\n",
		imd->iis_valid, (region ? region : ""), (ref ? ref : ""));
            printf ("\tsrc = %g,%g,%d,%d   dest = %d,%d,%d,%d\n",
               *sx, *sy, *snx, *sny, *dx, *dy, *dnx, *dny);
        }

        return (imd->iis_valid);
}


/* IMD_CLOSE -- Close the connection to the display server.
 */

#ifdef ANSI_FUNC

int 
imd_close (
    IMDPtr imd			/* package pointer	*/
)
#else

int
imd_close (imd)
IMDPtr	imd;			/* package pointer	*/
#endif
{
	if (imd_debug)
	    printf ("[imd_close]\n");

	if (imd) {
	    register int i;

	    /* Close the connection. */
	    if (imd->datain)
		close (imd->datain);
	    if (imd->dataout)
		close (imd->dataout);

	    /* Free the frame buffer configuration table. */
            for (i=0;  i < MAX_FBCONFIG;  i++)
	        free ((char *)imd->fbtab[i]);

	    /* Free the pointers in the imd structure. */
	    free (imd->title);
	    free (imd->name);
	    free (imd->region);
	    free (imd->ref);
	    free (imd);
	}

	return (OK);
}


/* --------------------
 * Low-Level Procedures
 * --------------------*/

/* IMD_WRITEIMAGE -- Display a raw pixel array to the server given the array
 * dimensions, and a location.  We use this instead of imd_writeSubRaster()
 * since we can assume the frame is blank and we wish to avoid the overhead
 * having to read back the frame buffer.  The image corner position is given
 * in user coords where the [0,0] origin is in the LL of the display window,
 * we convert this to frame buffer coords where the origin is in the UL.
 */

int
imd_writeImage (imd, pix, nx, ny, llx, lly)
IMDPtr	imd;			/* package pointer	 */
uchar 	*pix;			/* pixels to display	 */
int	nx, ny;			/* dimensions		 */
int	llx, lly;		/* LL corner of image    */
{
	register int i, j, k, y, nbytes, nl = ny, imline;
	register int nnx = nx, nny = ny, block_has_data = 0;
	register uchar *ip = pix;
	register uchar *bp, *block, *ep;
	register int lx, ly, nblocks, lines_per_block, fbline;
	int	 x_off = 0, y_off = 0;
	int	 fbwidth  = imd->fbtab[imd->fbconfig-1]->width;
	int	 fbheight = imd->fbtab[imd->fbconfig-1]->height;


        /* Check to see if the image to be displayed is larger than the
         * current frame buffer.
         */
        if (nx > fbwidth || ny > fbheight) {
            nnx = min(nx, fbwidth);             /* get new dimensions */
            nny = min(ny, fbheight);
            x_off =  (nx>fbwidth ? ((nx - fbwidth) / 2 - 1) : 0);
            y_off =  (ny>fbheight ? ((ny - fbheight) / 2 - 1) : 0);
        }

	/* Now see whether the image extends over the boundaries of the
  	 * frame buffer in any direction.  If so we'll set up to write 
	 * only those pixels on the frame buffer.
	 */
	if (llx < 0) {			/* image is left of frame 	  */
            x_off = -llx;
	    nnx = nx + llx;
	}
	if (lly < 0) {			/* image is below frame 	  */
            y_off = -lly;
	    nny = ny + lly;
	}
	if ((llx + nx) > fbwidth) 	/* image overflows frame to right */
	    nnx = fbwidth - llx;
	if ((lly + ny) > fbheight) 	/* image overflows frame at top   */
	    nny = fbheight - lly;
	ip = pix + (y_off * nx) + x_off;

	/* Convert corner position to frame buffer coords. */
	lx = max (0, llx);
	ly = min (fbheight - 1, fbheight - lly - 1);

	/* Compute the corner points in user units. */
	imd->xs = llx;
	imd->xe = llx + nnx - 1;
	imd->ys = lly;
	imd->ye = lly + nny - 1;

	if (imd_debug) {
	    printf("[imd_writeImage] %dx%d bytes at [%d,%d] of [%d,%d]\n", 
		nx, ny, lx, ly, fbwidth, fbheight);
	    printf("[imd_writeImage] Xends = [%d,%d]  Yends = [%d,%d]\n",
		imd->xs, imd->xe, imd->ys, imd->ye);
	    printf("[imd_writeImage] nnx=%d nny=%d llx=%d lly=%d xo=%d yo=%d\n",
                nnx, nny, llx, lly, x_off, y_off);
	    imd_debug = 1;
	}

	/* Display image. */
	lines_per_block = (int) (SZ_BLOCK / fbwidth);
	nblocks = (int) (fbheight / lines_per_block);
	nbytes = fbwidth * lines_per_block;
	block = (uchar *) calloc (nbytes, sizeof (uchar));

	if (imd_debug) {
	    printf ("height=%d nblocks=%d lines/block=%d\n", 
	        fbheight, nblocks, lines_per_block);
	}

	/* For each of the blocks we're sending.... */
	fbline = fbheight - 1;
	imline = ly;
	ep = ip + nx * ny;
	for (i=0, y = fbheight - lines_per_block; i < nblocks; i++) {
	    bp = block + (lines_per_block - 1) * fbwidth;
	    block_has_data = 0;

	    if (nnx != fbwidth || nny != fbheight)
	        /* Clear the block array */
		for (k=0; k < nbytes; k++)
		    block[k] = 0;

	    /* Map image pixels to the data block. */
	    for (j=0; j < lines_per_block && nl >= 0; j++) {
	        if (fbline == imline && ip < ep) {
		    block_has_data = 1;
		    bcopy (ip, bp+lx, nnx);
		    ip += nx;
		    imline--;
		    nl--;
		} 
		bp -= fbwidth;
	        fbline--;
	    }

	    if (block_has_data)
	        if (imd_writeLine(imd, block, nbytes, 0, y))
	            return (ERR);
	    y -= lines_per_block;
	}

	/* Now take care of any remaining pixels in the frame buffer. */
	nbytes = (fbwidth * fbheight) - (nblocks * nbytes);
        if (nbytes && nl >= 0) {
	    lines_per_block = nbytes / fbwidth;
            bp = block + (lines_per_block - 1) * fbwidth;

            if (nnx != fbwidth || nny != fbheight) {
                /* Clear the block array */
                for (k=0; k < nbytes; k++)
                    block[k] = 0;
            }

            /* Map image pixels to the data block. */
	    if (nl >= 0) {
                for (j=0; j < lines_per_block && nl >= 0; j++) {
                    if (fbline == imline) {
                        bcopy (ip, bp+lx, nnx);
                        ip += nx;
                        imline--;
                        nl--;
                    }
                    bp -= fbwidth;
                    fbline--;
                }
                if (imd_writeLine(imd, block, nbytes, 0, 0))
                    return (ERR);  
            }
        }

	free ((char *)block);
	return (OK);
}


/* IMD_READIMAGE -- Read the currently displayed image and return a pointer to
 * the array and it's dimensions.  Since we know where the image was written
 * in the frame buffer this is really just a large subregion read.
 */

int
imd_readImage (imd, pix, nx, ny)
IMDPtr	imd;			/* package pointer	*/
uchar	*pix;			/* image pixels (output)*/
int	*nx, *ny;		/* dimensions (output) 	*/
{
	*nx = (imd->xe - imd->xs + 1);
	*ny = (imd->ye - imd->ys + 1);

	if (imd_debug)
	    printf ("[imd_readImage]  nx=%d ny=%d at [%d,%d]\n",
		*nx, *ny, imd->xs, imd->ys);

        /* Read the image region buffer. */
	if (!pix)
	    pix = (uchar *) malloc ((*nx) * (*ny));
	return (imd_readSubRaster(imd, imd->xs, imd->ys, *nx, *ny, pix));
}


/* IMD_READFRAMEBUFFER -- Read the contents of the entire frame buffer and
 * return a pointer to the array and it's dimensions. 
 */

int
imd_readFrameBuffer (imd, pix, nx, ny)
IMDPtr	imd;			/* package pointer	*/
uchar	*pix;			/* image pixels (output)*/
int	*nx, *ny;		/* dimensions (output) 	*/
{
	if (imd_debug)
	    printf ("[imd_readFrameBuffer]\n");

	*nx = imd->fbtab[imd->fbconfig-1]->width;
	*ny = imd->fbtab[imd->fbconfig-1]->height;

	if (imd_debug)
	    printf ("[imd_readFrameBuffer]  nx=%d ny=%d at [%d,%d]\n",
		*nx, *ny, 0, 0);

        /* Read the frame buffer. */
	if (!pix)
	    pix = (uchar *) malloc ((*nx) * (*ny));
	return (imd_readSubRaster(imd, 0, 0, *nx, *ny, pix));
}


/* IMD_SETFRAME -- Set the current display frame.
 */

#ifdef ANSI_FUNC

int 
imd_setFrame (
    IMDPtr imd,			/* package pointer	*/
    int frame			/* frame number		*/
)
#else

int
imd_setFrame (imd, frame)
IMDPtr	imd;			/* package pointer	*/
int	frame;			/* frame number		*/
#endif
{
	if (imd_debug)
	    printf ("[imd_setFrame] frame = %d\n", frame);

	imd->frame = frame;
	return (com_setFrame (imd->dataout, imd->frame));
}


/* IMD_SETFBCONFIG -- Select the frame buffer configuration.
 */

#ifdef ANSI_FUNC

int 
imd_setFBConfig (
    IMDPtr imd,			/* package pointer	*/
    int configno		/* frame config number	*/
)
#else

int
imd_setFBConfig (imd, configno)
IMDPtr	imd;			/* package pointer	*/
int	configno;		/* frame config number	*/
#endif
{
	if (imd_debug)
	    printf ("[imd_setFBConfig] config = %d\n", configno);

	imd->fbconfig  = configno;
	return (com_setFBConfig (imd->dataout, imd->fbconfig));
}


/* IMD_GETFBCONFIG -- Get the current frame buffer config info used by the
 * the interface.
 */

#ifdef ANSI_FUNC

int 
imd_getFBConfig (
    IMDPtr imd,			/* package pointer	*/
    int *configno,		/* frame config number	*/
    int *width,
    int *height,		/* frame buffer size	*/
    int *nframes		/* number of frames	*/
)
#else

int
imd_getFBConfig (imd, configno, width, height, nframes)
IMDPtr	imd;			/* package pointer	*/
int	*configno;		/* frame config number	*/
int	*width, *height;	/* frame buffer size	*/
int	*nframes;		/* number of frames	*/
#endif
{
	*configno = imd->fbconfig;
	*width    = imd->fbtab[imd->fbconfig-1]->width;
	*height   = imd->fbtab[imd->fbconfig-1]->height;
	*nframes  = imd->fbtab[imd->fbconfig-1]->nframes;
	if (imd_debug)
	    printf ("[imd_getFBConfig] config=%d w=%d h=%d nf=%d\n",
		*configno, *width, *height, *nframes);

	return (com_setFBConfig (imd->dataout, imd->fbconfig));
}


/* IMD_SETNAME -- Set the current image name (for the WCS string);
 */

#ifdef ANSI_FUNC

int 
imd_setName (
    IMDPtr imd,			/* package pointer	*/
    char *name			/* image name		*/
)
#else

int
imd_setName (imd, name)
IMDPtr	imd;			/* package pointer	*/
char	*name;			/* image name		*/
#endif
{
	if (imd_debug)
	    printf ("[imd_setName] Name = %s\n", name);

	strcpy (imd->name, name);
	return (OK); 
}


/* IMD_SETTITLE -- Set the current image title (for the WCS string)
 */

#ifdef ANSI_FUNC

int 
imd_setTitle (
    IMDPtr imd,			/* package pointer	*/
    char *title			/* image title		*/
)
#else

int
imd_setTitle (imd, title)
IMDPtr	imd;			/* package pointer	*/
char	*title;			/* image title		*/
#endif
{
	if (imd_debug)
	    printf ("[imd_setTitle] title = %s\n", title);

	strcpy (imd->title, title);
	return (OK); 
}


/* IMD_SETCURSOR -- Set the image cursor position.
 */

#ifdef ANSI_FUNC

int 
imd_setCursor (
    IMDPtr imd,			/* package pointer	*/
    int x,
    int y,			/* position		*/
    int wcs			/* cursor wcs		*/
)
#else

int
imd_setCursor (imd, x, y, wcs)
IMDPtr	imd;			/* package pointer	*/
int	x, y;			/* position		*/
int	wcs;			/* cursor wcs		*/
#endif
{
	if (imd_debug)
	    printf ("[imd_setCursor] position = [%d,%d]\n", x, y);

	/* need to convert to frame coords? */

	return (com_setCursor(imd->dataout, x, y, wcs));
}


/* IMD_CLEARFRAME -- Clear the current display frame.
 */

#ifdef ANSI_FUNC

int 
imd_clearFrame (
    IMDPtr imd			/* package pointer	*/
)
#else

int
imd_clearFrame (imd)
IMDPtr	imd;			/* package pointer	*/
#endif
{
	if (imd_debug)
	    printf ("[imd_eraseFrame]\n");

	return (com_eraseFrame(imd->dataout));
}


/* IMD_READSUBRASTER -- Read a rectangular region of the frame buffer.
 */

int
imd_readSubRaster (imd, llx, lly, nx, ny, pix)
IMDPtr	imd;			/* package pointer	*/
int	llx, lly;		/* region corner  	*/
int	nx, ny;			/* dimensions  		*/
uchar	*pix;			/* image pixels (output)*/
{
	register int i, j, nl, y, nbytes, lx, ly;
	register uchar *ip = NULL, *bp = NULL, *block = NULL;
	register int nblocks, lines_per_block;
	register int nnx = nx, nny = ny, x_off = 0, y_off = 0;
	int	fbwidth  = imd->fbtab[imd->fbconfig-1]->width;
	int	fbheight = imd->fbtab[imd->fbconfig-1]->height;


	/* Make sure we've got a reasonable request. */
	if (nx > fbwidth || ny > fbheight) {
	    fprintf (stderr,
		"Error: attempt to read raster larger than display.\n");
	    return (ERR);
	}

        /* Now see whether the raster extends over the boundaries of the
         * frame buffer in any direction.  If so we'll clip the raster to
         * write only those pixels on the frame buffer.
         */

        if (-llx > imd->xs) {		/* raster overflows bottom */
            x_off = -llx + imd->xs;
            nnx = nx - x_off;
        }
        if (-lly > imd->ys) {		/* raster overflows left   */
            y_off = -lly + imd->ys;
            nny = ny - y_off;
        }
        if ((llx + nx) > fbwidth) {      /* raster overflows right */
            nnx = fbwidth - llx;
	}
        if ((lly + ny) > fbheight) {     /* raster overflows top   */
            nny = fbheight - lly;
	    /*y_off = ny - nny;*/
	}

	/* Allocate the pointer if needed.  Clear the output array if we're
	 * clipping to guarantee zero values.
	 */
	if (!pix)
	    pix = (uchar *) calloc (nx * ny, sizeof (uchar));

        /* Convert corner position to frame buffer coords. */
	lx = max (0, llx + imd->xs);
	ly = min (fbheight - 1, fbheight - (lly + imd->ys) - 0);

	if (imd_debug) {
	    printf ("[imd_readSubRas] %d bytes at [%d,%d] orig [%d,%d]\n", 
		nx*ny, lx, ly, llx, lly);
	    printf ("[imd_readSubRas] img corner at [%d,%d] -> [%d,%d]\n",
		imd->xs, imd->ys, imd->xe, imd->ye);
	}

	/* Figure out how many reads we'll need. */
	if (imd->model == MOD_NORMAL) {
	    lines_per_block = min (nny, (int)(SZ_BLOCK / fbwidth));
	    nbytes = fbwidth * lines_per_block;
	} else {
	    lines_per_block = min (nny, (int)(SZ_BLOCK / nx));
	    nbytes = nx * lines_per_block;
	}
	nblocks = (int) (nny / lines_per_block);
	block = (uchar *) calloc (nbytes, sizeof (uchar));

	if (imd_debug) {
	    printf("[imd_readSubRas] ny=%d nblks=%d lin/bl=%d nbytes=%d\n", 
	        ny, nblocks, lines_per_block, nbytes);
            printf("[imd_readSubRas] nnx=%d nny=%d llx=%d lly=%d xo=%d yo=%d\n",
                nnx, nny, llx, lly, x_off, y_off);
	}


	/* Loop over each of the blocks needed to get the region.  */
	nl = nny;
	ip = pix + (y_off * nx) + x_off;
	y = ly - lines_per_block + 1;
	for (i=0; i < nblocks; i++, y -= lines_per_block) {
	    if (imd_readLine(imd, block, nbytes, 0, y))
		return (ERR);

	    /* Pull the requested pixels from the block.  */
	    bp = block + (lines_per_block - 1) * fbwidth;
	    for (j=0; j < lines_per_block && nl; j++) {
		/*bcopy (bp+lx+x_off-1, ip, nnx);*/
		bcopy (bp+lx, ip, nnx);
		bp -= fbwidth;
		ip += nx;
		nl--;
	    }
	}

	/* Take care of the remaining pixels. */
	if (nl) {
	    nbytes = nl * fbwidth;
	    y += lines_per_block - nl + 1;

	    /* Read the last block. */
	    if (imd_readLine(imd, block, nbytes, 0, y))
	        return (ERR);

	    /* Pull the requested pixels from the block.  */
	    bp = block + (nl - 1) * fbwidth;
	    for (j=0; j < nl; j++) {
		/*bcopy (bp+lx+x_off-1, ip, nx);*/
		bcopy (bp+lx, ip, nnx);
		bp -= fbwidth;
		ip += nx;
	    }
	}

	free ((char *)block);
	return (OK);
}


/* IMD_WRITESUBRASTER -- Write a rectangular region of the frame buffer.
 */

int
imd_writeSubRaster (imd, llx, lly, nx, ny, pix)
IMDPtr	imd;			/* package pointer	*/
int	llx, lly;		/* region corner  	*/
int	nx, ny;			/* dimensions  		*/
uchar	*pix;			/* subraster pixels 	*/
{
        register int i, j, nl, y, nbytes, lx, ly;
        register uchar *ip = NULL, *bp = NULL, *block = NULL;
        register int nblocks, lines_per_block;
	register int nnx = nx, nny = ny, x_off = 0, y_off = 0;
        int     fbwidth  = imd->fbtab[imd->fbconfig-1]->width;
        int     fbheight = imd->fbtab[imd->fbconfig-1]->height;


	/* Make sure at least part of the raster is on the frame buffer,
	 * otherwise it's an error.
	 */
	if (llx > fbwidth || lly > fbheight || (llx+nx) < 0 || (lly+ny) < 0) {
	    fprintf (stderr, "Error: attempt to write raster out of bounds.\n");
	    return (ERR);
	}

        /* Now see whether the raster extends over the boundaries of the
         * frame buffer in any direction.  If so we'll clip the raster to
	 * write only those pixels on the frame buffer.
         */
        if (-llx > imd->xs) {		/* raster overflows bottom */
            x_off = -llx + imd->xs;
            nnx = nx - x_off;
        }
        if (-lly > imd->ys) {		/* raster overflows left   */
            y_off = -lly + imd->ys;
            nny = ny - y_off;
        }
        if ((llx + nx) > fbwidth)       /* raster overflows right  */
            nnx = fbwidth - llx;
        if ((lly + ny) > fbheight)      /* raster overflows top    */
            nny = fbheight - lly;

        /* Check to see if the image to be displayed is larger than the
         * current frame buffer.
         */
        if (nx > fbwidth || ny > fbheight) {
            nnx = min(nx, fbwidth);             /* get new dimensions */
            nny = min(ny, fbheight);
            x_off =  (nx>fbwidth ? ((nx - fbwidth) / 2 - 1) : 0);
            y_off =  (ny>fbheight ? ((ny - fbheight) / 2 - 1) : 0);
        }

        ip = pix + (y_off * nx) + x_off;

        /* Convert corner position to frame buffer coords. */
        lx = max (0, llx + imd->xs);
        ly = min (fbheight - 1, fbheight - (lly + imd->ys) - 0);

        if (imd_debug)
            printf ("[imd_writeSubRaster] %d bytes at [%d,%d] of [%d,%d]\n",
                nx*ny, lx, ly, fbwidth, fbheight);

        /* Figure out how many reads we'll need. */
	if (imd->model == MOD_NORMAL) {
            lines_per_block = min (nny, (int)(SZ_BLOCK / fbwidth));
            nbytes = fbwidth * lines_per_block;
	} else {
            lines_per_block = min (nny, (int)(SZ_BLOCK / nx));
            nbytes = nx * lines_per_block;
	}
        nblocks = (int) (nny / lines_per_block);
        block = (uchar *) calloc (nbytes, sizeof (uchar));

        if (imd_debug) {
	    printf ("\tnnx=%d nny=%d llx=%d lly=%d xo=%d yo=%d xs=%d ys=%d\n",
		nnx, nny, llx, lly, x_off, y_off, imd->xs, imd->ys);
            printf ("\tny=%d nblocks=%d lines/block=%d nbytes=%d\n",
                ny, nblocks, lines_per_block, nbytes);
        }

        /* Loop over each of the blocks needed to get the region. */
        nl = nny;
        y = ly - lines_per_block + 1;
        for (i=0; i < nblocks; i++, y -= lines_per_block) {

	    if (imd->model == MOD_NORMAL) {

                /* Read a block of data containing the subraster but only if 
	         * we need to.
	         */
	        if (nnx != fbwidth)
                    if (imd_readLine(imd, block, nbytes, 0, y))
                        return (ERR);

                /* Copy the subraster pixels to the block just read. */
                bp = block + (lines_per_block - 1) * fbwidth;
                for (j=0; j < lines_per_block && nl; j++) {
                    bcopy (ip, bp+lx, nnx);
                    bp -= fbwidth;
                    ip += nx;
                    nl--;
                }

                /* Write the edited block back to the server. */
                if (imd_writeLine(imd, block, nbytes, 0, y))
                    return (ERR);

	    } else {
                /* Copy the subraster pixels to the block just read. */
                bp = block + (lines_per_block - 1) * nx;
                for (j=0; j < lines_per_block && nl; j++) {
                    bcopy (ip, bp, nnx);
                    bp -= nx;
                    ip += nx;
                    nl--;
                }

                /* Write the edited block back to the server. */
		nbytes = lines_per_block * ny;
                if (imd_writeRawBuf(imd, block, nbytes, lx, y,
		    nx, lines_per_block))
                        return (ERR);
	    }
        }

        /* Take care of the remaining pixels. */
        if (nl) {
            nbytes = nl * fbwidth;

            /* Read the last block. */
	    y += lines_per_block - nl;


	    if (imd->model == MOD_NORMAL) {
	        if (nnx != fbwidth)
                    if (imd_readLine(imd, block, nbytes, 0, y))
                        return (ERR);

                /* Copy the subraster pixels to the block just read. */
                bp = block + (nl - 1) * fbwidth;
                for (j=0; j < nl; j++) {
                    bcopy (ip, bp+lx, nnx);
                    bp -= fbwidth;
                    ip += nx;
                }

                /* Write the edited block back to the server. */
                if (imd_writeLine(imd, block, nbytes, 0, y))
                    return (ERR);

	    } else {
                /* Copy the subraster pixels to the block just read. */
                bp = block + (nl - 1) * nx;
                for (j=0; j < nl; j++) {
                    bcopy (ip, bp, nnx);
                    bp -= nx;
                    ip += nx;
                }

                /* Write the edited block back to the server. */
		nbytes = nl * ny;
                if (imd_writeRawBuf(imd, block, nbytes, lx, y, nx, nl))
                    return (ERR);
	    }
        }

        free ((char *)block);
	return (OK);
}


/* IMD_SETDEBUG -- Set the state of the debug flag.
 */

#ifdef ANSI_FUNC

int 
imd_setDebug (int state)
#else

int
imd_setDebug (state)
int	state;
#endif
{
	imd_debug = state;
	return (OK);
}



/* ------------------
 * PRIVATE PROCEDURES
 * ------------------*/

/* IMD_WRITELINE --  Send the command to write a block of pixels to the
 * server.  This is a low-level routine called to either write a single
 * line of data, or when the number of bytes exceeds the frame buffer width
 * it can be used to send a complete "block" in the image display.
 */

static int
imd_writeLine (imd, pix, nbytes, x, y)
IMDPtr	imd;				/* package pointer	*/
uchar	*pix;				/* pixel array		*/
int	nbytes;				/* npix to write	*/
int	x, y;				/* coords for start	*/
{
	register short sx=x, sy=y;

	if (imd_debug > 1)
	    printf ("[imd_writeLine] %d bytes at [%d,%d]\n", nbytes, x, y);

	return (com_writeData(imd->dataout, sx, sy, pix, nbytes));
}


/* IMD_READLINE --  Send the command to read a sequential block of pixels
 * from the server. 
 */

static int
imd_readLine (imd, pix, nbytes, x, y)
IMDPtr	imd;				/* package pointer	*/
uchar	*pix;				/* pixel array		*/
int	nbytes;				/* npix to write	*/
int	x, y;				/* coords for start	*/
{
	register short sx=x, sy=y;

	if (imd_debug)
	    printf ("[imd_readLine] %d bytes at [%d,%d]\n", nbytes, x, y);

	return (com_readData(imd->datain, imd->dataout, sx, sy, pix, &nbytes));
}


/* IMD_WRITERAWBUF --  Send the command to write a block of pixels to the
 * server.  This is a low-level routine called to either write a single
 * line of data, or when the number of bytes exceeds the frame buffer width
 * it can be used to send a complete "block" in the image display.
 */

static int
imd_writeRawBuf (imd, pix, nbytes, x, y, nx, ny)
IMDPtr	imd;				/* package pointer	*/
uchar	*pix;				/* pixel array		*/
int	nbytes;				/* npix to write	*/
int	x, y;				/* coords for start	*/
int	nx, ny;				/* dimensions of buf 	*/
{
	register short sx=x, sy=y;

	if (imd_debug > 1)
	    printf ("[imd_writeRawBuf] %d bytes at [%d,%d][%d,%d]\n", 
		nbytes, x, y, nx, ny);

	return (com_writeSubraster(imd->dataout, sx, sy, pix, nx, ny));
}


/* IMD_READRAWBUF --  Send the command to write a block of pixels to the
 * server.  This is a low-level routine called to either write a single
 * line of data, or when the number of bytes exceeds the frame buffer width
 * it can be used to send a complete "block" in the image display.
 */

static int
imd_readRawBuf (imd, pix, nbytes, x, y, nx, ny)
IMDPtr	imd;				/* package pointer	*/
uchar	*pix;				/* pixel array		*/
int	nbytes;				/* npix to read		*/
int	x, y;				/* coords for start	*/
int	nx, ny;				/* dimensions of buf 	*/
{
	register short sx=x, sy=y;

	if (imd_debug > 1)
	    printf ("[imd_readRawBuf] %d bytes at [%d,%d][%d,%d]\n", 
		nbytes, x, y, nx, ny);

	return (com_readSubraster(imd->dataout, sx, sy, pix, nx, ny));
}


/* IMD_INITIALIZE -- Allocate and initialize the imd package structure.
 */

#ifdef ANSI_FUNC

static IMDPtr 
imd_initialize (
    int fdin,
    int fdout,				/* device descriptors  	*/
    int domain,				/* connection type	*/
    int model				/* subraster model	*/
)
#else

static IMDPtr
imd_initialize (fdin, fdout, domain, model)
int	fdin, fdout;			/* device descriptors  	*/
int	domain;				/* connection type	*/
int 	model;				/* subraster model	*/
#endif
{
	IMDPtr	imd;

        /* Allocate and initialize imd structure. */
        imd = (struct IMD *) calloc (1, sizeof (struct IMD));

        imd->datain    = fdin;
        imd->dataout   = fdout;
        imd->domain    = domain;
        imd->ztrans    = W_LINEAR;
        imd->frame     = 1;
        imd->fbconfig  = 1;

        /* Initialize a WCS, set the defaults for 512x512 framebuffer. */
        imd->a         = 1.0;                  
        imd->b         = 0.0;
        imd->c         = 0.0;
        imd->d         = -1.0;
        imd->tx        = 1.0;
        imd->ty        = 512.0;
        imd->z1        = 0.0;
        imd->z2        = 255.0;

        /* Initialize the mapping. */
        imd->sx        = 1.0;
        imd->sy        = 1.0;
        imd->snx       = 512;
        imd->sny       = 512;
        imd->dx        = 0;
        imd->dy        = 0;
        imd->dnx       = 511;
        imd->dny       = 511;
        imd->iis_valid = 0;

	/* Allocate the structure pointers. */
        imd->name      = (char *) calloc (SZ_NAME, sizeof(char));
        imd->title     = (char *) calloc (SZ_NAME, sizeof(char));
        imd->region    = (char *) calloc (SZ_NAME, sizeof(char));
        imd->ref       = (char *) calloc (SZ_NAME, sizeof(char));

	/* Get the server IIS version number. */
	imd->iis_version = imd_wcsVersion (imd);

printf ("init:  model = %d\n", model);
	if (imd->iis_version > 10) {
	    /* Server supports fast subraster writes.
	    */
	    if (model == MOD_NORMAL)
                imd->model     = MOD_FAST;
	} else {
	    if (model == MOD_FAST)
                imd->model     = MOD_NORMAL;
	}
printf ("\n\niis_version=%d  model=%d -> %d\n", imd->iis_version, model,
imd->model);

        /* Load the frame buffer configuration file. */
        imd_loadImtoolrc (imd);

	return (imd);
}


/* IMD_PARSEIMTDEV -- Parse an IMTDEV device string, returning the domain
 * type as the function value and loading the path/host information as
 * needed.
 */

#ifdef ANSI_FUNC

static int 
imd_parseImtdev (
    char *imtdev,			/* device string     		*/
    char *unixaddr,			/* unix socket path  		*/
    unsigned short *host_port,		/* inet port number  		*/
    unsigned long *host_addr,		/* inet host address 		*/
    char *ififo,			/* fifo paths	     		*/
    char *ofifo,
    int  *model				/* subraster display model 	*/
)
#else

static int
imd_parseImtdev (imtdev, unixaddr, host_port, host_addr, ififo, ofifo, model)
char		*imtdev;		/* device string     		*/
char		*unixaddr;		/* unix socket path  		*/
unsigned short	*host_port;		/* inet port number  		*/
unsigned long	*host_addr;		/* inet host address 		*/
char		*ififo, *ofifo;		/* fifo paths	     		*/
int  	        *model;			/* subraster display model 	*/
#endif
{
	char	*ip, *dp;
	char	osfn[SZ_LINE*2];


	if (imtdev == NULL)
	    return ERR;
	else {
            /* Extract any subraster display model option from the IMTDEV
	    ** string.  This will be the word "fast:" or "normal:" prefixed
	    ** to the device string.  A missing option will use the default
	    ** model, either way we'll update the device string before parsing
	    ** for the connection.
	    */
	    if (strncmp (imtdev, "fast:", 5) == 0) {
		*model = MOD_FAST;
	        dp = &imtdev[5];
	    } else if (strncmp (imtdev, "normal:", 5) == 0) {
		*model = MOD_NORMAL;
	        dp = &imtdev[7];
	    } else {
		*model = DEF_MODEL;
	        dp = imtdev;
	    }
printf ("parse:  model = %d   (def=%d)\n", *model, DEF_MODEL);


            /* Expand any %d fields in the network address to the UID. */
            sprintf (osfn, (char *)dp, getuid(), getuid());
 
	    if (strncmp (osfn, "fifo:", 5) == 0) {
                /* FIFO (named pipe) connection.  */
                ip = osfn + 5;
                if (!imd_getstr (&ip, ififo, SZ_NAME))
                    return ERR;
                if (!imd_getstr (&ip, ofifo, SZ_NAME))
                    return ERR;

		return FIFO;

	    } else if (strncmp (osfn, "inet:", 5) == 0) {

                /* Internet connection.  */
                char port_str[SZ_NAME], host_str[SZ_NAME];
                unsigned short port;
                struct servent *sv;
                struct hostent *hp;

                /* Get port number.  This may be specified either as a service
                 * name or as a decimal port number.
                 */
                ip = osfn + 5;
                if (imd_getstr (&ip, port_str, SZ_NAME) <= 0)
                    return ERR;
                if (isdigit (port_str[0])) {
                    port = atoi (port_str);
                    *host_port = htons (port);
                } else if ((sv = getservbyname(port_str,"tcp"))) {
                    *host_port = sv->s_port;
                } else
                    return ERR;

                /* Get host address.  This may be specified either has a host
                 * name or as an Internet address in dot notation.  If no host
                 * name is specified default to the local host.
                 */
                if (imd_getstr (&ip, host_str, SZ_NAME) <= 0)
                    strcpy (host_str, "localhost");
                if (isdigit (host_str[0])) {
                    *host_addr = inet_addr (host_str);
                    if ((int)*host_addr == -1)
                        return ERR;
                } else if ((hp = gethostbyname(host_str))) {
                    bcopy (hp->h_addr, (char *)host_addr, sizeof(*host_addr));
                } else
                    return ERR;

		return INET;

	    } else if (strncmp (osfn, "unix:", 5) == 0) {
                /* Unix domain socket connection.  */
                ip = osfn + 5;
                if (!imd_getstr (&ip, unixaddr, SZ_NAME))
                    return ERR;

		return UNIX;
	    }
	}


	return (ERR);
}


/* IMD_WCSVERSION -- Query the server for the support IIS WCS version.
 */

#ifdef ANSI_FUNC

static int
imd_wcsVersion (
    IMDPtr imd				/* package pointer	*/
)
#else

static int
imd_wcsVersion (imd)
IMDPtr 	imd;				/* package pointer	*/
#endif
{
	/* Allow the user to disable the use of maps. */
	if (getenv ("CDL_NOMAPS"))
	    return (0);
	else
	    return (com_wcsVersion (imd->datain, imd->dataout));
}


/* IMD_LOADIMTOOLRC -- Load the frame buffer configuration table into a
 * runtime table.  An error is returned if the table cannot be found and
 * a default frame buffer size of 512x512 with 2 frames is available (this
 * is all the server will have abvailable anyway).
 */

#ifdef ANSI_FUNC

static int 
imd_loadImtoolrc (
    IMDPtr imd				/* package pointer	*/
)
#else

static int
imd_loadImtoolrc (imd)
IMDPtr	imd;				/* package pointer	*/
#endif
{
        register char   *ip;
        register FILE   *fp = NULL;
        int     config, nframes, width, height, i;
        char    *fname;


        /* Initialize the config table. */
        for (i=0;  i < MAX_FBCONFIG;  i++) {
	    imd->fbtab[i] = (FBTab *) calloc (sizeof(FBTab), sizeof(char));
            imd->fbtab[i]->config  = i;
            imd->fbtab[i]->nframes = 2;
            imd->fbtab[i]->width   = DEF_FRAME_WIDTH;
            imd->fbtab[i]->height  = DEF_FRAME_HEIGHT;
        }

        /* Attempt to open the config file. */
        if ((fname=getenv(FBCONFIG_ENV1)) || (fname=getenv(FBCONFIG_ENV2))) {
            fp = fopen (fname, "r");
	    if (imd_debug)
	        printf ("Using IMTOOLRC='%s'...\n", fname);
	}
        if (!fp && (fname = getenv ("HOME"))) {
	    if (imd_debug)
	        printf ("$IMTOOLRC not found...\n");
            sprintf (buf, "%s/%s", fname, FBCONFIG_1);
            fp = fopen (fname = buf, "r");
	    if (fp && imd_debug)
	        printf ("Using $HOME/.imtoolrc...\n");
	}
        if (!fp) {
	    if (imd_debug)
	        printf ("$HOME/.imtoolrc not found...\n");
            fp = fopen ((fname = FBCONFIG_2), "r");
	    if (fp && imd_debug)
	        printf ("Using /usr/local/lib/imtoolrc...\n");
	}
        if (!fp) {
            fprintf (stderr, 
		"Warning: cannot find frame buffer configuration table.\n");
            return (ERR);
	}
   
        /* Scan the frame buffer configuration file.
         */
        while (fgets (buf, SZ_LINE, fp) != NULL) {
            /* Skip comment lines and blank lines. */
            for (ip=buf;  *ip == ' ' || *ip == '\t';  ip++)
                ;
            if (*ip == '\n' || *ip == '#')
                continue;
            if (!isdigit (*ip))
                continue;
            switch (sscanf (ip, "%d%d%d%d",&config,&nframes,&width,&height)) {
            case 4:
                break;                  /* normal case */
            case 3:
                height = width;         /* default to square format */
                break;
            default:
                fprintf (stderr, "Warning: bad config `%s'\n", ip);
                continue;
            }

            nframes = max (1, nframes);
            width   = max (1, width);
            height  = max (1, height);

            /* Since the frame buffer is stored in a memory pixrect
             * (effectively), the line length should be an integral number
             * of 16 bit words.
             */
            if (width & 1) {
                fprintf (stderr, "Warning: fb config %d [%d-%dx%d] - ",
                    config, nframes, width, height);
                fprintf (stderr, "frame width should be even, reset to %d\n",
                    --width);
            }

            imd->fbtab[config-1]->config  = max(1,min(MAX_FBCONFIG, config));
            imd->fbtab[config-1]->nframes = nframes;
            imd->fbtab[config-1]->width   = width;
            imd->fbtab[config-1]->height  = height;

	    if (imd_debug > 1)
		printf ("%3d %2d %6d %6d\n", config, nframes, width, height);
        }

        fclose (fp); 
	return (OK);
}


/* IMD_GETSTR -- Internal routine to extract a colon delimited string from a
 * network filename.
 */
#ifdef ANSI_FUNC

static int 
imd_getstr (char **ipp, char *obuf, int maxch)
#else

static int
imd_getstr (ipp, obuf, maxch)
char **ipp;
char *obuf;
int maxch;
#endif
{
        register char *ip = *ipp, *op = obuf;
        register char *otop = obuf + maxch;
        char *start;

        while (isspace(*ip))
            ip++;
        for (start=ip;  *ip;  ip++) {
            if (*ip == ':') {
                ip++;
                break;
            } else if (op && op < otop)
                *op++ = *ip;
        }

        if (op)
            *op = '\0';
        *ipp = ip;

        return (ip - start);
}


/* IMD_MINMAX -- Compute the min/max values of an array.
 */

static void
imd_minmax (pix, nbytes, pmin, pmax)
uchar 	*pix;
int	nbytes;
int	*pmin, *pmax;
{
	register int i;

	*pmin = *pmax = pix[0];
	for (i=1; i<nbytes; i++) {
	    *pmin = min (*pmin, pix[i]);
	    *pmax = max (*pmax, pix[i]);
	}
}
