/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */
 
#include <sys/times.h>
#include <sys/types.h>
#include <sys/socket.h>
#if defined(AIX) || defined(AIXV3) || defined (AIXV4)
#include <sys/select.h>
#endif
#include <netinet/in.h>
#include <sys/un.h> 
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>


#ifdef	HAVE_CDL 	/* If we have the CDL we can build a proxy server. */
#include "cdl.h"
#endif


/*
 *  VXIMTOOL.C -- Virtual image display server.  This is a server process much
 *  like XIMTOOL, except that all the process does is respond to datastream
 *  requests to read and write to internal frame buffers maintained as arrays
 *  in memory.  Multiple frame buffers and frame buffer configurations are
 *  supported.   A log is kept to the stderr of all datastream requests.  The
 *  process is terminated with an EOF on the stdin.
 *
 *
 *  To build:  cc vximtool.c -o vximtool
 *             cc vximtool.c -o vximtool -lsocket # For Solaris systems
 *             cc -DANSI_FUNC vximtool.c          # Use ANSI function prototypes
 *
 *  Usage:  vximtool -verbose >& spool            # run server, log output
 *	    vximtool -i		                  # run interactively
 *	    vximtool -noraster	                  # don't store images in memory
 *	    vximtool -i < cursor_file             # take cursor input from file
 *
 *  Options     
 * 
 *    vximtool [-background] [-config <num>] [-fifo <pipe>] [-fifo_only] [-help]
 *        [-i] [-imtoolrc <file>] [-inet_only | -port_only] [-noraster] 
 *        [-nframes <num>] [-port <num>] [-proxy] [-verbose] [-unix <name>] 
 *        [-unix_only] 
 *
 *
 *  Minimal match for command line options may be used.  In interactive mode,
 *  cursor value strings may be typed in on the server stdin in response to
 *  cursor read requests from the client.  Otherwise, a constant cursor value
 *  "1.0 1.0 101 q" is returned.
 */


/* Default values, size limiting values.
 */
#define	MAX_FBCONFIG		128	/* max possible frame buf sizes	*/
#ifndef	HAVE_CDL
#define	MAX_FRAMES		16	/* max number of frames		*/
#endif
#define	MAX_MAPPINGS		32	/* max number mappings/frame	*/
#define	MAX_CLIENTS		8	/* max display server clients	*/
#define	DEF_NFRAMES		1	/* save memory; only one frame	*/
#define	DEF_FRAME_WIDTH		512	/* 512 square frame		*/
#define	DEF_FRAME_HEIGHT	512	/* 512 square frame		*/

#define	SZ_LABEL	256		/* main frame label string	*/
#define	SZ_IMTITLE	128		/* image title string		*/
#define	SZ_FIFOBUF	4000		/* transfer size for FIFO i/o	*/
#define	SZ_FNAME	256
#define	SZ_LINE		256

/* Magic numbers. */
#define DEF_PORT	5137	        /* default tcp/ip socket  	*/
#define DEF_NPORTS	1	        /* default no. sockets to open 	*/
#define DEF_PROXY_PORT	5136	        /* default proxy socket  	*/
#define	I_DEVNAME	"/dev/imt1o"    /* pseudo device names    	*/
#define	O_DEVNAME	"/dev/imt1i"    /* our IN is client's OUT 	*/
#define	DEF_UNIXADDR	"/tmp/.IMT%d"   /* default unix socket    	*/
#define	FBCONFIG_1	".imtoolrc"
#define	FBCONFIG_2	"/usr/local/lib/imtoolrc"
#define	FBCONFIG_ENV1	"imtoolrc"
#define	FBCONFIG_ENV2	"IMTOOLRC"

/* IIS definitions. */
#define IO_TIMEOUT      30
#define MAXCONN         5
#define SZ_IOBUF        65536           /* max size data transfer       */
#define SZ_FIFOBUF      4000
#define SZ_OLD_WCSBUF   320             /* old WCS text buffer size     */
#define SZ_WCSBUF       1024            /* WCS text buffer size         */
#define SZ_FNAME        256
#define SZ_IMCURVAL     160

#define IIS_VERSION     10              /* version 10 -> 1.0            */

#define MEMORY          01              /* frame buffer i/o             */
#define LUT             02              /* lut i/o                      */
#define FEEDBACK        05              /* used for frame clears        */
#define IMCURSOR        020             /* logical image cursor         */
#define WCS             021             /* used to set WCS              */

#define PACKED          0040000
#define COMMAND         0100000
#define IIS_READ        0100000
#define IMC_SAMPLE      0040000
#define IMT_FBCONFIG    077
#define XYMASK          077777

struct  iism70 {			/* DO NOT change the order of   */
        short   tid;			/* this structure.		*/
        short   thingct;
        short   subunit;
        short   checksum;
        short   x, y, z;
        short   t;
};

/* IIS data pixel values. */
#define CMS_DATASTART   1
#define CMS_DATAEND     200
#define CMS_DATARANGE   200

/* WCS definitions. */
#define	W_UNITARY	0
#define	W_LINEAR	1
#define	W_LOG		2
#define	W_DEFFORMAT	" %7.2f %7.2f %7.1f%c"

/* Rotation matrix defining world coordinate system (WCS) of a frame.  */
typedef struct {
	int valid;			/* has WCS been set?		   */
	float a, b;			/* x, y scale factors		   */
	float c, d;			/* x, y cross factors		   */
	float tx, ty;			/* x, y translation		   */
	float z1, z2;			/* greyscale range		   */
	int zt;				/* greyscale mapping		   */
	char format[32];		/* wcs output format		   */
	char imtitle[SZ_IMTITLE+1];	/* image title from WCS	*/
} Ctran, *CtranPtr;

/* Coordinate mappings on each frame buffer. */
typedef struct {
        int   id;                       /* object id                    */
        Ctran ctran;                    /* world coordinate system      */
        char  ref[SZ_FNAME+1];          /* image reference from WCS     */
        int   regid;                    /* region id                    */
        char  region[SZ_FNAME+1];       /* region name from WCS         */
        float sx, sy;                   /* source rect                  */
        int   snx, sny;                 
        int   dx, dy;                   /* destination rect             */
        int   dnx, dny;                 
} Mapping, *MappingPtr;

/* The frame buffers. */
typedef struct {
	int frameno;			/* frame number			   */
	char *framebuf;			/* frame buffer raster		   */
	char label[SZ_LABEL+1];		/* frame label string		   */
	Ctran ctran;			/* world coordinate system	   */
	char wcsbuf[SZ_WCSBUF];		/* wcs info string		   */
        Mapping mapping[MAX_MAPPINGS];  /* coordinate mappings          */
        int     nmaps;                  /* number of defined mappings   */
} FrameBuf, *FrameBufPtr;

/* Possible frame buffer sizes. */
typedef struct {
	int nframes;			/* number of frames		   */
	int width;			/* frame buffer width		   */
	int height;			/* frame buffer height		   */
} FbConfig, *FbConfigPtr;

/* Client I/O channel. */
typedef struct {
	void *vxim;			/* backpointer to vxim descriptor   */
	int type;			/* channel type 		   */
	int listen_fd;			/* socket server fd		   */
	int datain;			/* input channel 		   */
	int dataout;			/* output channel 		   */
	int keepalive;			/* used to keep input fifo ready   */
	int connected;			/* channel is connected to client  */
	int port;			/* inet port number 		   */
	char path[SZ_FNAME+1];		/* for unix sockets 		   */
	int reference_frame;		/* reference (cmd i/o) frame 	   */
        int version;                    /* flags capability of client    */
	FrameBufPtr rf_p;		/* reference frame descriptor 	   */
} IoChan, *IoChanPtr;

#define	IO_FIFO		1
#define	IO_INET		2
#define	IO_UNIX		3


/* Application runtime descriptor.
 * --------------------------------
 */
typedef struct {
	int  def_config;		   /* default FB config            */
	int  def_nframes;		   /* default number of frames     */
	char *imtoolrc;			   /* imtoolrc file name           */
	char *input_fifo;		   /* client's output              */
	char *output_fifo;		   /* client's input               */
	char *unixaddr;			   /* format for unix socket path  */
	int  port;			   /* port for INET socket         */
	int  nports;			   /* nports to open		   */

	/* Internal state. */
	int display_frame;		   /* currently displayed frame    */
	int fb_configno;		   /* current config number        */
	int nframes;			   /* current number of frame bufs */
	int width, height;		   /* current width, height        */
	IoChan chan[MAX_CLIENTS];	   /* client i/o descriptors       */
	FrameBufPtr df_p;		   /* display frame descriptor     */
	FrameBuf frames[MAX_FRAMES];	   /* array of frame descriptors   */
	FbConfig fb_config[MAX_FBCONFIG];  /* fb config table              */

} VXimData, *VXimDataPtr;


/* Initialize the structure with out starting values.  These can be reset
 * with command line options.
 */
VXimData server_data = {
	1,				    /* def_config 		   */
	DEF_NFRAMES,			    /* def_nframes 		   */
	FBCONFIG_2,			    /* def_imtoolrc 		   */
	O_DEVNAME,			    /* input_fifo		   */
	I_DEVNAME,			    /* output_fifo	 	   */
	DEF_UNIXADDR,			    /* unixaddr	 		   */
	DEF_PORT,			    /* port		 	   */
	DEF_NPORTS,			    /* nports		 	   */
	1,				    /* display_frame 		   */
	1,				    /* fb_configno	 	   */
	2,				    /* nframes	 		   */
	512, 512			    /* width, height 		   */
};

/* Functions.
 */
#ifndef abs
#define	abs(a)		(((a)<0)?(-(a)):(a))
#endif
#ifndef min
#define min(a,b)	((a)<(b)?(a):(b))
#endif
#ifndef max
#define max(a,b)	((a)<(b)?(b):(a))
#endif

#ifdef SOLARIS
#define bzero(a,n)      memset(a,0,n)
#define bcopy(a,b,n)    memmove(b,a,n)
#endif

#define	SELWIDTH	32

#ifdef HAVE_CDL
static 	int	proxy = 0;
static  int	nclients = 0;
CDLPtr	cdl[MAX_CLIENTS];
#endif
static	int	keep_raster = 1;
extern	int	errno;
static	int	background = 0;
static	int	objid = 0;
static	int	verbose = 0;
static	int	interactive = 0;
static	float	cursor_x = 1.0, cursor_y = 1.0;
static	fd_set	fds, allset;


#ifdef  ANSI_FUNC

int main(int argc, char **argv);
static int 	 vx_iisopen(register VXimDataPtr vxim);
static void 	 vx_iisclose(register VXimDataPtr vxim);
static IoChanPtr open_fifo(register VXimDataPtr vxim);
static IoChanPtr open_inet(register VXimDataPtr vxim);
static IoChanPtr open_unix(register VXimDataPtr vxim);
static void 	 vx_connectClient(IoChanPtr chan, int *source);
static void 	 vx_disconnectClient(register IoChanPtr chan);
static IoChanPtr get_iochan(register VXimDataPtr vxim);
static void 	 vx_iisio(IoChanPtr chan, int *fd_addr, int source);
static void 	 set_fbconfig(IoChanPtr chan, int config, int frame);
static int	 decode_frameno(register int z);
static void	 bswap2(char *a, char *b, int nbytes);
static void 	 vx_retCursorVal(register int dataout, float sx, float sy,
		     int wcs, int key, char *strval);
static CtranPtr  wcs_update(register VXimDataPtr vxim, FrameBufPtr fr);
static void 	 vx_initialize(register VXimDataPtr vxim, int config,
		     int nframes, int reset);
static void 	 vx_initFrame(register VXimDataPtr vxim, int frame,
		     int nframes, FbConfigPtr config);
static void 	 vx_eraseFrame(register VXimDataPtr vxim, int frame);
static void 	 get_fbconfig(register VXimDataPtr vxim);
static void 	 Usage(void);
static void 	 printoption(char *st);
static void	 add_mapping(register VXimDataPtr vxim, CtranPtr ctran, 
		     char *wcsbuf, FrameBufPtr fr);
#ifdef	HAVE_CDL
static void 	 vx_flip(char *buffer, int nx, int ny);
#endif
static int 	 iis_read (int fd, void *vptr, int nbytes);
static int 	 iis_write (int fd, void *vptr, int nbytes);

#else

static void 	 vx_iisclose(), vx_connectClient(), vx_disconnectClient();
static void	 vx_iisio(), set_fbconfig(), vx_retCursorVal();
static void 	 vx_initialize(), vx_initFrame(), vx_eraseFrame();
static void 	 get_fbconfig(), Usage(), printoption();
static void	 add_mapping();
#ifdef	HAVE_CDL
static void 	 vx_flip();
#endif
static int 	 vx_iisopen(), decode_frameno(), iis_read(), iis_write();
static void	 bswap2();
static IoChanPtr open_fifo(), open_inet(), open_unix(), get_iochan();
static CtranPtr  wcs_update();

#endif


/* 
 * VXIMTOOL -- Virtual display server process.  This task is an image display
 * server like XImtool, responding to datastream requests on fifo pipes,
 * inet sockets, or unix sockets.  Up to 16 frames are supported, frame
 * buffers may be any of the defined frames in the imtoolrc file.  Images
 * are stored in memory, allowing readback by the client.  Cursor input can
 * come from stdin (or a redirected file) allowing the user to respond to
 * client cursor requests.  The task is terminated with an EOF on stdin.
 */

#ifdef ANSI_FUNC

int 
main (int argc, char **argv)
#else

main (argc, argv)
int	argc;
char	**argv;
#endif
{
        register VXimDataPtr vxim = &server_data;
	register IoChanPtr chan;
	register int i, nopen, n;
	char 	 buf[SZ_FNAME];
	int	 fd;

	/* Process the command line arguments. */
	for (i=1; i < argc; i++) {

#ifdef	HAVE_CDL
            /* Anything without a '-' is a client device to add to the proxy
             * list.  Format of the arg must be a valid IMTDEV string.
             */
            if (proxy && argv[i][0] != '-') {
		if ((cdl[nclients++] = cdl_open (argv[i])) == (CDLPtr) NULL)
		    nclients--;
		else if (verbose)
		    printf ("Connected to server on %s\n", argv[i]);
		continue;
 	    }
#endif
	    if (strncmp (argv[i], "-background", 2) == 0) {
                background = 1;
	    } else if (strncmp (argv[i], "-config", 2) == 0) {
                vxim->def_config = atoi (argv[++i]);
	    } else if (strncmp (argv[i], "-fifo_only", 6) == 0) {
                vxim->unixaddr = "none";
                vxim->port = 0;
	    } else if (strncmp (argv[i], "-fifo", 5) == 0) {
                vxim->input_fifo = (char *) calloc (SZ_FNAME, sizeof(char));
                vxim->output_fifo = (char *) calloc (SZ_FNAME, sizeof(char));
                sprintf (vxim->input_fifo, "%si", argv[++i]);
                sprintf (vxim->output_fifo, "%so", argv[i]);
	    } else if (strncmp (argv[i], "-help", 2) == 0) {
		Usage ();
		exit (0);
	    } else if (strncmp (argv[i], "-imtoolrc", 3) == 0) {
                vxim->imtoolrc = argv[++i];
	    } else if (strncmp (argv[i], "-inet_only", 3) == 0) {
                vxim->input_fifo = "";
                vxim->unixaddr = "none";
	    } else if (strcmp (argv[i], "-i") == 0) {
		interactive++;
	    } else if (strncmp (argv[i], "-noraster", 3) == 0) {
		keep_raster = 0;
	    } else if (strncmp (argv[i], "-nframes", 3) == 0) {
                vxim->def_nframes = min (MAX_FRAMES, atoi (argv[++i]));
	    } else if (strncmp (argv[i], "-nports", 3) == 0) {
		vxim->nports = atoi (argv[++i]);
	    } else if (strncmp (argv[i], "-port_only", 6) == 0) {
                vxim->input_fifo = "";
                vxim->unixaddr = "none";
	    } else if (strncmp (argv[i], "-port", 5) == 0) {
                vxim->port = atoi (argv[++i]);
#ifdef	HAVE_CDL
	    } else if (strncmp (argv[i], "-proxy", 5) == 0) {
                proxy = 1;
                vxim->port = DEF_PROXY_PORT;	/* re-assign port	      */
                vxim->input_fifo = "";		/* shut off other connections */
                vxim->unixaddr = "none";
#endif
	    } else if (strncmp (argv[i], "-verbose", 2) == 0) {
                verbose = 1;
	    } else if (strncmp (argv[i], "-unix_only", 6) == 0) {
                vxim->input_fifo = "";
                vxim->port = 0;
	    } else if (strncmp (argv[i], "-unix", 5) == 0) {
                vxim->unixaddr = argv[++i];
	    }
	}

#ifdef	HAVE_CDL
	/* If we're acting as a proxy server, but can't connect to anything,
	 * exit.  In this case it is required that the servers be running
	 * before starting the proxy program so we have a connection waiting.
	 */
	if (!nclients && proxy) {
	    fprintf (stderr, "Error: No servers available for display.\007\n");
	    exit (-1);
	}
#endif

        /* Initialize the frame buffers */
        vx_initialize (vxim, vxim->def_config, vxim->def_nframes, 1);

        /* Listen for a client connection and initialize the fdset. */
	if (!(nopen = vx_iisopen (vxim))) {
	    fprintf (stderr, "Error: Cannot open client communications.\007\n");
	    exit (-1);
	}
	FD_ZERO (&allset);
 	for (i=0; i < nopen; i++) {
            chan = &vxim->chan[i];
 	    FD_SET (chan->datain, &allset); 
	}
 	if (!background || interactive)
	    FD_SET (fileno(stdin), &allset); 

	/* Sit in a loop waiting on input, processing the events. */
	while (1) {
   	    fds = allset;		/* reset the FD set on each pass */

	    if ((n = select (SELWIDTH, &fds, NULL, NULL, NULL)) > 0) {

		/* Loop over each of the open connections, checking for and
		 * processing input on any that are ready.
		 */
 		for (i=0; i < nopen; i++) {
             	    chan = &vxim->chan[i]; 
             	    fd = chan->datain;
		    if (FD_ISSET(fd, &fds)) {

		  	/* Connect the client if not already connected. */
		        if (!chan->connected) {
			    if (verbose)  {
				if (chan->type == IO_UNIX)
				    fprintf (stderr,
					"connecting client on %s\n",
					chan->path);
				else if (chan->type == IO_INET)
				    fprintf (stderr,
					"connecting client on port %d\n",
					vxim->port);
			    }
		            vx_connectClient (chan, &chan->datain);
		        }

			/* Process any waiting input. */
 			vx_iisio (chan, &chan->datain, chan->type);  
			fflush (stdout); fflush (stderr);
		    }
		}

		/* Check the stdin for an EOF so we can quit gracefully. */
 	        if (!background) {
		    if (FD_ISSET(fileno(stdin), &fds)) {
		        if ((n = read (fileno(stdin), buf, SZ_FNAME)) <= 0) {
        		    /* Shut it down. */
 			    vx_iisclose (vxim); 
			    exit (0);
		        }
		    }
		}

	    } else if (n < 0) {
		fprintf (stderr, "Error: select error\007\n");
		exit (-1);
	    }
	}
}



/* VX_IISOPEN -- Initialize the IIS protocol module and ready the module to
 * accept client connections and begin processing client requests.  Clients
 * may connect to the server using a fifo connection or an internet or
 * UNIX domain socket connection.  All three types of server ports are
 * simultaneously ready to receive client connections.
 */
#ifdef ANSI_FUNC

static int 
vx_iisopen (register VXimDataPtr vxim)
#else

static int
vx_iisopen (vxim)
register VXimDataPtr vxim;
#endif
{
    int i, port, last_port = (vxim->port + vxim->nports - 1);
    int nopen = 0;

    if (open_unix (vxim))
        nopen++;
    if (open_fifo (vxim))
        nopen++;

    for (port=vxim->port; port > 0 && port <= last_port; port++) {
        if (open_inet (vxim, port))
            nopen++;
    }

    return (nopen);
}


/* VX_IISCLOSE -- Close down the IIS protocol module.
 */
#ifdef ANSI_FUNC

static void 
vx_iisclose (register VXimDataPtr vxim)
#else

static void
vx_iisclose (vxim)
register VXimDataPtr vxim;
#endif
{
	register IoChanPtr chan = NULL;
	register FrameBufPtr fb;
	register int i, j;

	for (i=0;  i < (sizeof(vxim->chan) / sizeof(vxim->chan[0]));  i++) {
	    chan = &vxim->chan[i];

	    /* Free the in-memory frame buffer rasters. */
	    for (j=0; j < vxim->nframes; j++) {
	        fb = &vxim->frames[j];
	        if (keep_raster && fb->framebuf)
		    free (fb->framebuf);
	    }

	    /* Close the I/O channels. */
	    switch (chan->type) {
	    case IO_FIFO:
		if (chan->keepalive >= 0)
		    close (chan->keepalive);
		if (chan->datain >= 0)
		    close (chan->datain);
		if (chan->dataout >= 0)
		    close (chan->dataout);
		chan->type = 0;
	        if (verbose)
	            fprintf (stderr, "vximtool: closing fifo connection\n");
		break;

	    case IO_INET:
		close (chan->datain);
		chan->type = 0;
	        if (verbose)
	            fprintf (stderr, "vximtool: closing inet socket\n");
		break;

	    case IO_UNIX:
		close (chan->datain);
		unlink (chan->path);
		chan->type = 0;
	        if (verbose)
	            fprintf (stderr, "vximtool: closing unix socket\n");
		break;
	    }
	}
}


/* OPEN_FIFO -- Open the server fifo port and make ready to accept client
 * connections and begin processing client requests.  There is no client
 * yet at this stage.
 */
#ifdef ANSI_FUNC

static IoChanPtr 
open_fifo (register VXimDataPtr vxim)
#else

static IoChanPtr
open_fifo (vxim)
register VXimDataPtr vxim;
#endif
{
	register IoChanPtr chan;
	int datain, dataout;
	int keepalive;


#if defined(__DARWIN__) || defined (__CYGWIN__)
        /* On OS X and Cygwin we don't use fifos. */
        xim->input_fifo = "none";
        return (NULL);
#endif

	/* Setting the input fifo to "none" or the null string disables
	 * fifo support.
	 */
	if (!vxim->input_fifo[0] || strcmp(vxim->input_fifo,"none")==0)
	    return (NULL);

	datain = dataout = -1;

	/* Open the output fifo (which is the client's input fifo).  We have
	 * to open it ourselves first as a client to get around the fifo
	 * open-no-client error.  
	 */
	if ((datain = open (vxim->input_fifo, O_RDONLY|O_NDELAY)) != -1) {
	    if ((dataout = open (vxim->input_fifo, O_WRONLY|O_NDELAY)) != -1)
		fcntl (dataout, F_SETFL, O_WRONLY);
	    else
		goto done;
	    close (datain);
	} else
	    goto done;

	/* Open the input stream, a FIFO pseudodevice file used by
	 * applications to send us commands and data.
	 */
	if ((datain = open (vxim->output_fifo, O_RDONLY|O_NDELAY)) == -1)
	    goto done;
	else {
	    /* Clear O_NDELAY for reading. */
	    fcntl (datain, F_SETFL, O_RDONLY);

	    /* Open the client's output fifo as a pseudo-client to make it
	     * appear that a client is connected.
	     */
	    keepalive = open (vxim->output_fifo, O_WRONLY);
	}
done:
	/* Allocate and fill in i/o channel descriptor. */
	if (datain > 0 && dataout > 0 && (chan = get_iochan(vxim))) {
	    chan->vxim = (void *) vxim;
	    chan->type = IO_FIFO;
	    chan->datain = datain;
	    chan->dataout = dataout;
	    chan->keepalive = keepalive;
	    chan->connected = 1;
	    chan->reference_frame = 1;
	    chan->rf_p = &vxim->frames[0];
	} else {
	    fprintf (stderr, "Warning: cannot open %s\n", vxim->output_fifo);
	    chan = NULL;
	}

	/* Register input callback. */
	if (!chan) {
	    if (datain > 0)
		close (datain);
	    if (dataout > 0)
		close (dataout);
	} else if (verbose) {
	    fprintf (stderr,
		"vximtool: Open to accept input on fifo: %s\n", 
		    vxim->input_fifo);
	}

	return (chan);
}


/* OPEN_INET -- Set up a port to be used for incoming client connections
 * using internet domain sockets.
 */
#ifdef ANSI_FUNC

static IoChanPtr 
open_inet (register VXimDataPtr vxim, int port)
#else

static IoChanPtr
open_inet (vxim, port)
register VXimDataPtr vxim;
int      port;
#endif
{
	register int s = 0;
	register IoChanPtr chan;
	struct sockaddr_in sockaddr;

	/* Setting the port to zero disables inet socket support. */
	if (port <= 0)
	    return (NULL);

	if ((s = socket (AF_INET, SOCK_STREAM, 0)) < 0)
	    goto err;

	memset ((void *)&sockaddr, 0, sizeof(sockaddr));
	sockaddr.sin_family = AF_INET;
	sockaddr.sin_port = htons((short)port);
	sockaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	if (bind (s, (struct sockaddr *)&sockaddr, sizeof(sockaddr)) < 0)
	    goto err;

	if (listen (s, MAXCONN) < 0)
	    goto err;

	/* Allocate and fill in i/o channel descriptor. */
	if ((chan = get_iochan(vxim))) {
	    chan->vxim = (void *) vxim;
	    chan->type = IO_INET;
	    chan->port = port;
	    chan->datain = s;
	    chan->dataout = s;
	    chan->listen_fd = s;
	    chan->connected = 0;
	    chan->reference_frame = 1;
	    chan->rf_p = &vxim->frames[0];
	    if (verbose)
	        fprintf (stderr,
		    "vximtool: Open to accept input on inet: port %d\n", port);
	    return (chan);
	}
err:
	fprintf (stderr, "vximtool: cannot open socket on port %d, errno=%d\n",
	    port, errno);
	if (s)
	    close (s);
	return (NULL);
}


/* OPEN_UNIX -- Set up a port to be used for incoming client connections
 * using unix domain sockets.
 */
#ifdef ANSI_FUNC

static IoChanPtr 
open_unix (register VXimDataPtr vxim)
#else

static IoChanPtr
open_unix (vxim)
register VXimDataPtr vxim;
#endif
{
	register int s = 0;
	register IoChanPtr chan;
	struct sockaddr_un sockaddr;
	char path[256];

	/* Setting the addr to "none" or the null string disables unix
	 * socket support.
	 */
	if (!vxim->unixaddr[0] || strcmp(vxim->unixaddr,"none")==0)
	    return (NULL);

	/* Get path to be used for the unix domain socket. */
	sprintf (path, vxim->unixaddr, getuid());
	unlink (path);

	if ((s = socket (AF_UNIX, SOCK_STREAM, 0)) < 0)
	    goto err;

	memset ((void *)&sockaddr, 0, sizeof(sockaddr));
	sockaddr.sun_family = AF_UNIX;
	strcpy (sockaddr.sun_path, path);
	if (bind (s, (struct sockaddr *)&sockaddr, sizeof(sockaddr)) < 0)
	    goto err;

	if (listen (s, MAXCONN) < 0)
	    goto err;

	/* Allocate and fill in i/o channel descriptor. */
	if ((chan = get_iochan(vxim))) {
	    chan->vxim = (void *) vxim;
	    chan->type = IO_UNIX;
	    chan->datain = s;
	    chan->dataout = s;
	    chan->listen_fd = s;
	    chan->connected = 0;
	    chan->reference_frame = 1;
	    chan->rf_p = &vxim->frames[0];
	    strncpy (chan->path, path, SZ_FNAME);
	    if (verbose)
	        fprintf (stderr,
		    "vximtool: Open to accept input on unix: %s\n", path);
	    return (chan);
	}
err:
	fprintf (stderr, "vximtool: cannot open socket on port %s, errno=%d\n",
	    path, errno);
	if (s)
	    close (s);
	return (NULL);
}


/* VX_CONNECTCLIENT -- Called when a client has attempted a connection on
 * a socket port.  Accept the connection and set up a new i/o channel to
 * communicate with the new client.
 */
#ifdef ANSI_FUNC

static void 
vx_connectClient (IoChanPtr chan, int *source)
#else

static void
vx_connectClient (chan, source)
IoChanPtr chan;
int *source;
#endif
{
	register VXimDataPtr vxim = (VXimDataPtr) chan->vxim;
	register int s;

	/* Accept connection. */
	if ((s = accept ((int)*source, (struct sockaddr *)0, (int *)0)) < 0)
	    return;
/*	
	if (fcntl (s, F_SETFD, O_RDWR|O_NDELAY) < 0) {
	    close (s);
	    return;
	}
*/

	/* Allocate and fill in i/o channel descriptor. */
	FD_SET(s, &allset);
	chan->datain = s;
	chan->dataout = s;   
	chan->connected = 1;
	chan->reference_frame = 1;
	chan->rf_p = &vxim->frames[0];

	switch (chan->type) {
	case IO_INET:
	    if (verbose) 
		fprintf (stderr,
		    "connecting client on port %d\n", chan->port);
	case IO_UNIX:
	    if (verbose && chan->type == IO_UNIX) 
		fprintf (stderr,
		    "connecting client on %s\n", chan->path);
	}
}


/* VX_DISCONNECTCLIENT -- Called to close a client connection when EOF is
 * seen on the input port.  Close the connection and free the channel
 * descriptor.
 */
#ifdef ANSI_FUNC

static void 
vx_disconnectClient (register IoChanPtr chan)
#else

static void
vx_disconnectClient (chan)
register IoChanPtr chan;
#endif
{
	switch (chan->type) {
	case IO_INET:
	    if (verbose) 
		fprintf (stderr,
		    "disconnecting client on port %d\n", chan->port);
	case IO_UNIX:
	    if (verbose && chan->type == IO_UNIX) 
		fprintf (stderr,
		    "disconnecting client on %s\n", chan->path);
 	    FD_CLR(chan->datain, &allset); 
	    close (chan->datain);
	    chan->datain = chan->dataout = chan->listen_fd;
	    chan->connected = 0;
	    break;
	default:
	    break;
	}
}


/* GET_IOCHAN --- Get an i/o channel descriptor.
 */
#ifdef ANSI_FUNC

static IoChanPtr 
get_iochan (register VXimDataPtr vxim)
#else

static IoChanPtr
get_iochan (vxim)
register VXimDataPtr vxim;
#endif
{
	register int i;

	for (i=0;  i < MAX_CLIENTS;  i++)
	    if (!vxim->chan[i].type)
		return (&vxim->chan[i]);

	return (NULL);
}


/* VX_IISIO -- File i/o callback procedure, called when there is input
 * pending on the data stream to the vximtool client.
 */
#ifdef ANSI_FUNC

static void 
vx_iisio (IoChanPtr chan, int *fd_addr, int source)
#else

static void
vx_iisio (chan, fd_addr, source)
IoChanPtr chan;
int *fd_addr;
int source;
#endif
{
	register VXimDataPtr vxim = (VXimDataPtr) chan->vxim;
	register int sum, i;
	register short *p;
	int	datain = *fd_addr;
	int	dataout = chan->dataout;
	int	ndatabytes, nbytes, n, ntrys=0;
	struct	iism70 iis;
	char	buf[SZ_FIFOBUF];
	static	int errmsg=0, bswap=0;


	/* Get the IIS header. */
	if ((n = iis_read (datain, (char *)&iis, sizeof(iis))) < sizeof(iis)) {
	    if (n != 0)
		fprintf (stderr, 
		   "vximtool: command input read error, n=%d of %d, errno=%d\n",
			n, sizeof(iis), errno);
	    if (n <= 0)
		vx_disconnectClient (chan);
	    return;
	} else if (bswap)
	    bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));

	/* Verify the checksum.  If it fails swap the bytes and try again.
	 */
	for (;;) {
	    for (i=0, sum=0, p=(short *)&iis;  i < 8;  i++)
		sum += *p++;
	    if ((sum & 0177777) == 0177777)
		break;

	    if (ntrys++) {
		if (!errmsg++) {
		    fprintf (stderr, "vximtool: bad data header checksum\n");
		    if (bswap)
			bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		    fprintf (stderr, "noswap:");
		    for (i=0, p=(short *)&iis;  i < 8;  i++)
			fprintf (stderr, " %6o", p[i]);
		    fprintf (stderr, "\n");

		    bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		    fprintf (stderr, "  swap:");
		    for (i=0, p=(short *)&iis;  i < 8;  i++)
			fprintf (stderr, " %6o", p[i]);
		    fprintf (stderr, "\n");
		}
		break;

	    } else {
		bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		bswap = !bswap;
	    }
	}

	ndatabytes = -iis.thingct;
	if (!(iis.tid & PACKED))
	    ndatabytes *= 2;

	if (verbose) {
            fprintf (stderr, "%s: ", (source == IO_FIFO ? "fifo" : 
				(source == IO_INET ? "inet" : "unix")));
            fprintf (stderr, 
               "subunit=%03o tid=%06o nbytes=%6d x=%06o y=%06o z=%06o t=%06o\n",
               iis.subunit & 077,
               iis.tid,
	       ndatabytes,
               iis.x & 0177777,
               iis.y & 0177777,
               iis.z & 0177777,
               iis.t & 0177777);
            fflush (stdout);
	}


	switch (iis.subunit & 077) {
	case FEEDBACK:
	    /* The feedback unit is used only to clear a frame.
	     */
	    chan->reference_frame = decode_frameno (iis.z & 07777);
	    vx_eraseFrame (vxim, chan->reference_frame);
#ifdef	HAVE_CDL
	    if (proxy) {
	        for (i=0; i < nclients; i++)
	    	    cdl_clearFrame (cdl[i]);
	    }
#endif
	    if (verbose)
  	        fprintf (stderr, "erase frame %d - ref = %d\n", 
		    decode_frameno(iis.z & 0177777), chan->reference_frame);
	    break;

	case LUT:
	    /* Data mode writes to the frame lookup tables are not implemented.
	     * A command mode write to the LUT subunit is used to connect
	     * image memories up to the RGB channels, i.e., to select the frame
	     * to be displayed.  We ignore any attempt to assign multiple
	     * frames to multiple color channels, and just do a simple frame
	     * select.
	     */
	    if (iis.subunit & COMMAND) {
		int	frame, z, n;
		short	x[14];

		if (iis_read (datain, (char *)x, ndatabytes) == ndatabytes) {
		    if (bswap)
			bswap2 ((char *)x, (char *)x, ndatabytes);

		    z = x[0];
		    if (!z) z = 1;
		    for (n=0;  !(z & 1);  z >>= 1)
			n++;

		    frame = max (1, n + 1);
		    if (frame > vxim->nframes) {
			if (frame <= MAX_FRAMES) {
#ifdef	HAVE_CDL
	    		    if (proxy) {
	    		        for (i=0; i < nclients; i++) {
	    			    cdl_setFBConfig (cdl[i], vxim->fb_configno);
	    			    cdl_setFrame (cdl[i], frame);
	    		        }
	    		    }
#endif
			    set_fbconfig (chan, vxim->fb_configno, frame);
	    		    if (verbose)
                                fprintf (stderr, "set_fbconfig (%d, %d)\n",
                                    vxim->fb_configno, frame);
			} else {
			    fprintf (stderr, "imtool warning: ");
			    fprintf (stderr, 
				"attempt to display nonexistent frame %d\n",
				frame);
			    return;
			}
		    }

		    vxim->display_frame = frame;
                    if (verbose) 
		 	fprintf (stderr, "set_frame (%d)\n", frame);
		    return;
		}
	    }

	case MEMORY:
	    /* Load data into the frame buffer.  Data is assumed to be byte
	     * packed.
	     */
	    if (iis.tid & IIS_READ) {
		/* Read from the display.
		 */
        	register FrameBufPtr fb;
		unsigned char *ip, iobuf[SZ_IOBUF];
		int     nbytes, nleft, n, x, y;
		long    starttime;

		/* Get the frame to be read from. */
	        chan->reference_frame = decode_frameno (iis.z & 0177777);
        	fb = &vxim->frames[chan->reference_frame-1];

		nbytes = ndatabytes;
		x = iis.x & XYMASK;
		y = iis.y & XYMASK;

		if (x < 0 || x >= vxim->width || y < 0 || y >= vxim->height) {
		    fprintf (stderr,
			"vximtool: attempted read out of bounds on framebuf\n");
		    fprintf (stderr,
			"read %d bytes at [%d,%d]\n", nbytes, x, y);
		    memset ((void *)iobuf, 0, min(SZ_IOBUF,nbytes));
		} else {
                    if (verbose) 
			fprintf (stderr, "read %d bytes at [%d,%d]\n",
			    nbytes, x, y);
		    if (keep_raster)
 		        bcopy(&fb->framebuf[(y * vxim->width)+x], iobuf,nbytes);
		    else
 		        bzero (iobuf, nbytes);
#ifdef	HAVE_CDL
		    if (proxy) {
		 	unsigned char *pix = (unsigned char *)malloc(SZ_IOBUF);
			cdl_readSubRaster (cdl[0], x,
			    (vxim->height-y-max(1, nbytes/vxim->width)),
			    min(vxim->width, nbytes), 
			    max(1, nbytes/vxim->width),
			    &pix);
 		        bcopy(pix, iobuf, nbytes);
			vx_flip ((char *)iobuf, min(vxim->width, nbytes), 
			    max(1, nbytes/vxim->width));
 		        free ((char *)pix);
		    }
#endif
		}

		/* Return the data from the frame buffer. */
		starttime = time(0);
		for (nleft=nbytes, ip=iobuf;  nleft > 0;  nleft -= n) {
		    n = (nleft < SZ_FIFOBUF) ? nleft : SZ_FIFOBUF;
		    if ((n = iis_write (dataout, ip, n)) <= 0) {
			if (n < 0 || (time(0) - starttime > IO_TIMEOUT)) {
			    fprintf (stderr, "IMTOOL: timeout on write\n");
			    break;
			}
		    } else
			ip += n;
		}

		return;

	    } else {
		/* Write to the display.
		 */
        	register FrameBufPtr fb;
		unsigned char *op, iobuf[SZ_IOBUF];
		int     nbytes, nleft, n, x, y;
		long    starttime;

		/* Get the frame to be written into (encoded with a bit for
		 * each frame, 01 is frame 1, 02 is frame 2, 04 is frame 3,
		 * and so on).
		 */
	        chan->reference_frame = decode_frameno (iis.z & 0177777);
        	fb = &vxim->frames[chan->reference_frame-1];

		nbytes = ndatabytes;
		x = iis.x & XYMASK;
		y = iis.y & XYMASK;

		/* Read the data into the frame buffer.
		 */
		starttime = time(0);
		for (nleft=nbytes, op=iobuf;  nleft > 0;  nleft -= n) {
		    n = (nleft < SZ_FIFOBUF) ? nleft : SZ_FIFOBUF;
		    if ((n = iis_read (datain, op, n)) <= 0) {
			if (n < 0 || (time(0) - starttime > IO_TIMEOUT))
			    break;
		    } else
			op += n;
		}

		if (x < 0 || x >= vxim->width || y < 0 || y >= vxim->height) {
		    fprintf (stderr,
		       "vximtool: attempted write out of bounds on framebuf\n");
		    fprintf (stderr,
			"write %d bytes at [%d,%d]\n", nbytes, x, y);
		    bzero ((void *)iobuf, nbytes);
		} else {
                    if (verbose) 
			fprintf (stderr, "write %d bytes at x=%d, y=%d\n",
			    nbytes, x, y);
		    if (keep_raster)
 		        bcopy(iobuf, &fb->framebuf[(y * vxim->width)+x],nbytes);
#ifdef	HAVE_CDL
		    if (proxy) {
			vx_flip ((char *)iobuf, min(vxim->width, nbytes), 
			    max(1, nbytes/vxim->width));
			for (i=0; i < nclients; i++) 
			    cdl_writeSubRaster (cdl[i], x, 
			        (vxim->height-y-max(1, nbytes/vxim->width)),
				min(vxim->width, nbytes), 
				max(1, nbytes/vxim->width),
				iobuf);
		    }
#endif
		}

		return;
	    }
	    break;

	case WCS:
	    /* Read or write the WCS for a frame.  The frame number to
	     * which the WCS applies is passed in Z and the frame buffer
	     * configuration in T.  The client changes the frame buffer
	     * configuration in a WCS set.  The WCS text follows the header
	     * as byte packed ASCII data.
	     */
	    if (iis.tid & IIS_READ) {
		/* Return the WCS for the referenced frame.
		 */
		char emsg[SZ_FNAME];
		char *text;
		int frame;

		if ((iis.y & 0177777)) {
		    /* This is a check by the client on our capabilities.
		     * Return with a version number which can be used by the
		     * client.  However we write back using the old WCS 
		     * buffer size for compatability.
		     */
		    sprintf (text=emsg, "version=%d", IIS_VERSION);
		    chan->version = IIS_VERSION;

		    iis_write (dataout, text, SZ_OLD_WCSBUF);
		    if (verbose) 
			fprintf (stderr, "version query wcs: %s\n",text);

		} else {
		    frame = decode_frameno (iis.z & 0177777);
	            chan->reference_frame = frame;

		    if (chan->rf_p->frameno <= 0)
		        strcpy (text=emsg, "[NOSUCHFRAME]\n");
		    else
		        text = chan->rf_p->wcsbuf;

		    if ((iis.x & 0177777))
		        iis_write (dataout, text, SZ_WCSBUF);
		    else
		        iis_write (dataout, text, SZ_OLD_WCSBUF);

		    if (verbose) {
                        fprintf (stderr, "query wcs:\n");
                        write (2, text, SZ_WCSBUF);
		    }
		}

	    } else {
		/* Set the WCS for the referenced frame.
		 */
		register CtranPtr ct;
		int fb_config, frame, new_wcs = 0;

		frame = decode_frameno (iis.z & 0177777);
		fb_config = (iis.t & 0777) + 1;
		new_wcs   = (iis.t & 0777);

		/* See if we need to change the frame buffer configuration,
		 * or allocate a new frame.
		 */
		if (fb_config != vxim->fb_configno) {
		    set_fbconfig (chan, fb_config, frame);
#ifdef	HAVE_CDL
		    if (proxy) {
			for (i=0; i < nclients; i++) {
	    		    cdl_setFBConfig (cdl[i], fb_config);
	    		    cdl_setFrame (cdl[i], frame);
			}
		    }
#endif
		} else if (frame > vxim->nframes && frame < MAX_FRAMES) {
		    set_fbconfig (chan, vxim->fb_configno, frame);
#ifdef	HAVE_CDL
		    if (proxy) {
			for (i=0; i < nclients; i++) {
	    		    cdl_setFBConfig (cdl[i], vxim->fb_configno);
	    		    cdl_setFrame (cdl[i], frame);
			}
		    }
#endif
		}

		/* Read in and set up the WCS. */
		chan->reference_frame = frame;
		memset ((char *)buf, 0, SZ_WCSBUF);
		if (iis_read (datain, buf, ndatabytes) == ndatabytes)
		    strncpy (chan->rf_p->wcsbuf, buf,
			(new_wcs ? SZ_WCSBUF : SZ_OLD_WCSBUF));

		if (verbose) {
                    fprintf (stderr, "set wcs:  nbytes=%d\n", ndatabytes);
                    write (2, buf, ndatabytes);
		}

		strcpy (chan->rf_p->ctran.format, W_DEFFORMAT);
		chan->rf_p->ctran.imtitle[0] = '\0';
		chan->rf_p->ctran.valid = 0;
		ct = wcs_update (vxim, chan->rf_p);

#ifdef	HAVE_CDL
		if (proxy) {
		    for (i=0; i < nclients; i++)
			cdl_setWCS (cdl[i], " ", ct->imtitle, 
			    ct->a, ct->b, ct->c, ct->d, 
			    ct->tx, ct->ty, ct->z1, ct->z2, ct->zt);
		}
#endif

		/* Add the mapping information.  */
                add_mapping (vxim, ct, chan->rf_p->wcsbuf,
                    &vxim->frames[chan->reference_frame-1]);
	    }
	    return;

	case IMCURSOR:
	    /* Read or write the logical image cursor.  This is an extension
	     * added to provide a high level cursor read facility; this is
	     * not the same as a low level access to the IIS cursor subunit.
	     * Cursor reads may be either nonblocking (immediate) or blocking,
	     * using the keyboard or mouse to terminate the read, and
	     * coordinates may be returned in either image (world) or frame
	     * buffer pixel coordinates.
	     */
	    if (iis.tid & IIS_READ) {
		/* Read the logical image cursor.  In the case of a blocking
		 * read all we do is initiate a cursor read; completion occurs
		 * when the user hits a key or button.
		 */
		float sx, sy;

		if (verbose)
		    fprintf (stderr, "read cursor position\n");
		if (iis.tid & IMC_SAMPLE) {
		    /* Sample the cursor position and return the cursor value
		     * on the output datastream encoded in a fixed size
		     * ascii buffer.
		     */
		    int   wcs = iis.z;

		    sx = cursor_x;
        	    sy = cursor_y;
#ifdef	HAVE_CDL
		    if (proxy) {
			char	key = ' ';
			char	curval[SZ_IMCURVAL], keystr[20];

			cdl_readCursor (cdl[0], 1, &sx, &sy, &key);

        		/* Encode the cursor value. */
        		if (key == EOF)
        		    sprintf (curval, "EOF\n");
        		else {
        		    if (isprint (key) && !isspace(key)) {
           		        keystr[0] = key;
                		keystr[1] = '\0';
            		    } else
                		sprintf (keystr, "\\%03o", key);

            		    sprintf (curval, "%10.3f %10.3f %d %s %s\n",
                	        sx, sy, 1, keystr, "");
			}
			iis_write (dataout, curval, sizeof(curval));
		    } else
#endif
		        vx_retCursorVal (chan->dataout, sx, sy, wcs, 0, "");

		} else {
		    /* Initiate a user triggered cursor read. */
		    char key = 'q';
#ifdef	HAVE_CDL
		    if (proxy) {
			char	curval[SZ_IMCURVAL], keystr[20];

			cdl_readCursor (cdl[0], 0, &sx, &sy, &key);

        		/* Encode the cursor value. */
        		if (key == EOF)
        		    sprintf (curval, "EOF\n");
        		else {
        		    if (isprint (key) && !isspace(key)) {
           		        keystr[0] = key;
                		keystr[1] = '\0';
            		    } else
                		sprintf (keystr, "\\%03o", key);

            		    sprintf (curval, "%10.3f %10.3f %d %s %s\n\0",
                	        sx, sy, 1, keystr, "");
			}
			iis_write (chan->dataout, curval, sizeof(curval));
		    } else
#endif
		        vx_retCursorVal (chan->dataout, 1., 1., 101, key, "");
		}

	    } else {
		/* Write (set) the logical image cursor position. */
		register CtranPtr ct;
		int sx = iis.x, sy = iis.y;
		float wx = sx, wy = sy;
		int wcs = iis.z;

		if (verbose)
		    fprintf (stderr, "write cursor position: [%d,%d]\n", sx,sy);
		if (wcs) {
		    ct = wcs_update (vxim, vxim->df_p);
		    if (ct->valid) {
			if (abs(ct->a) > .001)
			    sx = (wx - ct->tx) / ct->a;
			if (abs(ct->d) > .001)
			    sy = (wy - ct->ty) / ct->d;
		    }
		}

        	cursor_x = sx;
        	cursor_y = sy;
#ifdef	HAVE_CDL
		if (proxy)
		    cdl_setCursor (cdl[0], sx, sy, wcs);
#endif
	    }
	    return;

	default:
	    /* Ignore unsupported command input.
	     */
	    fprintf (stderr, "unsupported input: subunit=%03o\n", 
		iis.subunit & 077);
	    break;
	}

	/* Discard any data following the header. */
	if (!(iis.tid & IIS_READ))
	    for (nbytes = ndatabytes;  nbytes > 0;  nbytes -= n) {
		n = (nbytes < SZ_FIFOBUF) ? nbytes : SZ_FIFOBUF;
		if ((n = iis_read (datain, buf, n)) <= 0)
		    break;
	    }
}


/* SET_FBCONFIG -- Set the frame buffer configuration, or add additional
 * frames to the current configuration.
 */
#ifdef ANSI_FUNC

static void 
set_fbconfig (IoChanPtr chan, int config, int frame)
#else

static void
set_fbconfig (chan, config, frame)
IoChanPtr chan;
int config;
int frame;
#endif
{
	register VXimDataPtr vxim = (VXimDataPtr) chan->vxim;
	register FrameBufPtr fb = &vxim->frames[frame-1];
	register int i;

	if (config != vxim->fb_configno) {
	    /* Change the frame buffer configuration. */
	    vx_initialize (vxim, config,
		max (vxim->fb_config[config-1].nframes, frame), 1);

	} else if (frame > vxim->nframes) {
	    /* Add additional frames.  */
	    for (i=1;  i <= frame;  i++) {
		fb = &vxim->frames[i-1];
		if (fb->frameno != i)
		    vx_initFrame (vxim, i, frame, &vxim->fb_config[config-1]);
	    }
	}

	chan->reference_frame = frame;
}


/* DECODE_FRAMENO -- Decode encoded IIS register frame number.
 */
#ifdef ANSI_FUNC

static int
decode_frameno (register int z)
#else

static int
decode_frameno (z)
register int	z;
#endif
{
	register int	n;

	/* Get the frame number, encoded with a bit for each frame, 01 is
	 * frame 1, 02 is frame 2, 04 is frame 3, and so on.
	 */
	if (!z) z = 1;
	for (n=0;  !(z & 1);  z >>= 1)
	    n++;

	return (max (1, n + 1));
}


/* BSWAP2 - Move bytes from array "a" to array "b", swapping successive
 * pairs of bytes.  The two arrays may be the same but may not be offset
 * and overlapping.
 */
#ifdef ANSI_FUNC

static void
bswap2 (
    char *a,
    char *b,		/* input array			*/
    int nbytes		/* number of bytes to swap	*/
)
#else

static void
bswap2 (a, b, nbytes)
char 	*a, *b;		/* input array			*/
int	nbytes;		/* number of bytes to swap	*/
#endif
{
	register char *ip=a, *op=b, *otop;
	register unsigned temp;

	/* Swap successive pairs of bytes.
	 */
	for (otop = op + (nbytes & ~1);  op < otop;  ) {
	    temp  = *ip++;
	    *op++ = *ip++;
	    *op++ = temp;
	}

	/* If there is an odd byte left, move it to the output array.
	 */
	if (nbytes & 1)
	    *op = *ip;
}


/* VX_RETCURSORVAL -- Return the cursor value on the output datastream to
 * the client which requested the cursor read.
 */
#ifdef ANSI_FUNC

static void 
vx_retCursorVal (
    register int dataout,
    float sx,
    float sy,			/* cursor screen coordinates */
    int wcs,			/* nonzero if WCS coords desired */
    int key,			/* keystroke used as trigger */
    char *strval		/* optional string value */
)
#else

static void
vx_retCursorVal (dataout, sx, sy, wcs, key, strval)
register int dataout;
float	sx, sy;			/* cursor screen coordinates */
int	wcs;			/* nonzero if WCS coords desired */
int	key;			/* keystroke used as trigger */
char	*strval;		/* optional string value */
#endif
{
	char curval[SZ_IMCURVAL];
	char keystr[20];

        /* If running SERVER in interactive mode, allow the user to type
         * in the cursor value on the standard input.
         */
        if (interactive) {
            printf ("enter cursor value string (x y wcs key str): ");
            fflush (stdout);
            if (fgets (curval, SZ_IMCURVAL, stdin) != NULL)
                goto ret;
        }

        /* Encode the cursor value. */
        if (key == EOF)
            sprintf (curval, "EOF\n");
        else {
            if (isprint (key) && !isspace(key)) {
                keystr[0] = key;
                keystr[1] = '\0';
            } else
                sprintf (keystr, "\\%03o", key);

            sprintf (curval, "%10.3f %10.3f %d %s %s\n",
                sx, sy, wcs, keystr, strval);
        }
ret:
	fprintf (stderr, "%s", curval);

	/* Send it to the client program and terminate cursor mode. */
	write (dataout, curval, sizeof(curval));
}


/* WCS_UPDATE -- Load the screen WCS, if not yet validated, from the user
 * wcs file, if any.
 *
 * File format (two lines):
 *
 *	image title (imtool header label string)\n
 *	a b c d tx ty z1 z2 zt
 *
 * The WCS text is passed in via the data stream as a write to the subunit
 * WCS and left in the buffer "wcsbuf".
 */
#ifdef ANSI_FUNC

static CtranPtr 
wcs_update (register VXimDataPtr vxim, FrameBufPtr fr)
#else

static CtranPtr 
wcs_update (vxim, fr)
register VXimDataPtr vxim;
FrameBufPtr fr;
#endif
{
	register CtranPtr ct = &fr->ctran;
	char buf[1024], *format;

	/* Get the new WCS. */
	if (!ct->valid) {
	    fr->label[0] = '\0';
	    ct->zt = W_UNITARY;

	    /* Attempt to read the WCS and set up a unitary transformation
	     * if the information cannot be read.
	     */
	    if (sscanf (fr->wcsbuf, "%[^\n]\n%f%f%f%f%f%f%f%f%d",
		buf, &ct->a, &ct->b, &ct->c, &ct->d, &ct->tx, &ct->ty,
		&ct->z1, &ct->z2, &ct->zt) < 7) {

		if (fr->wcsbuf[0])
		    fprintf (stderr, "vximtool: error decoding WCS\n");

		strncpy (ct->imtitle, "[NO WCS]\n", SZ_IMTITLE);
		ct->a  = ct->d  = 1;
		ct->b  = ct->c  = 0;
		ct->tx = ct->ty = 0;
		ct->zt = W_UNITARY;

	    } else
		strncpy (ct->imtitle, buf, SZ_IMTITLE);

	    ct->valid++;
	}

	/* Determine best format for wcs output. */
	if (ct->valid && ct->zt == W_LINEAR) {
	    float   z1, z2, zrange;
	    z1 = ct->z1;
	    z2 = ct->z2;
	    zrange = (z1 > z2) ? z1 - z2 : z2 - z1;
	    if (zrange < 100.0 && (abs(z1) + abs(z2)) / 2.0 < 200.0)
		format = " %7.2f %7.2f %7.3f%c";
	    else if (zrange > 99999.0 || (abs(z1) + abs(z2)) / 2.0 > 99999.0)
		format = " %7.2f %7.2f %7.3g%c";
	    else
		format = W_DEFFORMAT;
	} else
	    format = " %7.2f %7.2f %7.0f%c";

	strcpy (ct->format, format);
	return (ct);
}


/* VX_INITIALIZE -- Initialize the imaging subsystem.  Read the config file
 * and create the frame buffers, mappings, and colormaps.
 */
#ifdef ANSI_FUNC

static void 
vx_initialize (register VXimDataPtr vxim, int config, int nframes, int reset)
#else

static void
vx_initialize (vxim, config, nframes, reset)
register VXimDataPtr vxim;
int config;
int nframes;
int reset;
#endif
{
	if (reset) 
	    get_fbconfig (vxim);

	vxim->fb_configno = config;
	vxim->nframes = vxim->fb_config[config].nframes;
	vxim->width = vxim->fb_config[config].width;
	vxim->height = vxim->fb_config[config].height;
	vx_initFrame (vxim, vxim->display_frame, nframes,
	    &vxim->fb_config[config-1]);
}


/* VX_INITFRAME -- Initialize a frame buffer.
 */
#ifdef ANSI_FUNC

static void 
vx_initFrame (register VXimDataPtr vxim, int frame, int nframes, FbConfigPtr config)
#else

static void
vx_initFrame (vxim, frame, nframes, config)
register VXimDataPtr vxim;
int frame, nframes;
FbConfigPtr config;
#endif
{
        register FrameBufPtr fb = &vxim->frames[frame-1];

        if (frame < 1 || frame > MAX_FRAMES)
            return;

        /* Create the frame buffer. */
        fb->frameno = frame;
	if (keep_raster) {
	    if (fb->framebuf) 
		free (fb->framebuf);
	    fb->framebuf = (char *) malloc (config->width * config->height);
	}
	vxim->width = config->width;
	vxim->height = config->height;
	vxim->nframes = nframes;
}

/* VX_ERASEFRAME -- Erase a frame.
 */
#ifdef ANSI_FUNC

static void 
vx_eraseFrame (register VXimDataPtr vxim, int frame)
#else

static void
vx_eraseFrame (vxim, frame)
register VXimDataPtr vxim;
int frame;
#endif
{
        register FrameBufPtr fb = &vxim->frames[frame-1];
 
	if (keep_raster)
	    bzero (fb->framebuf, vxim->width * vxim->height);
}


/* GET_FBCONFIG -- Read the XIMTOOL startup file to get the set of possible
 * frame buffer sizes.
 *
 * File format:         configno nframes width height [extra fields]
 *      e.g.,                   1  2  512  512
 *                              2  2  800  800
 *                              3  1 1024 1024          # comment
 */
#ifdef ANSI_FUNC

static void 
get_fbconfig (register VXimDataPtr vxim)
#else

static void
get_fbconfig (vxim)
register VXimDataPtr vxim;
#endif
{
	register char	*ip;
	register FILE	*fp = NULL;
	int	config, nframes, width, height, i;
	char	lbuf[SZ_LINE+1], *fname;
	static char *fb_paths[] = {
		"/usr/local/lib/imtoolrc",
		"/opt/local/lib/imtoolrc",
		"/iraf/iraf/dev/imtoolrc",
		"/local/lib/imtoolrc",
		"/usr/iraf/dev/imtoolrc",
		"/usr/local/iraf/dev/imtoolrc",
		NULL};

	/* Initialize the config table. */
	vxim->fb_configno = 1;
	for (i=0;  i < MAX_FBCONFIG;  i++) {
	    vxim->fb_config[i].nframes = 1;
	    vxim->fb_config[i].width = DEF_FRAME_WIDTH;
	    vxim->fb_config[i].height = DEF_FRAME_HEIGHT;
	}

	/* Now add in some defaults for commonly used sizes based on the
 	 * standard IRAF imtoolrc file, we'll avoid any instrument specific
	 * configurations.
	 */
	vxim->fb_config[0].width = vxim->fb_config[0].height =  512;
	vxim->fb_config[1].width = vxim->fb_config[1].height =  800;
	vxim->fb_config[2].width = vxim->fb_config[2].height = 1024;
	vxim->fb_config[3].width = vxim->fb_config[3].height = 1600;
	vxim->fb_config[4].width = vxim->fb_config[4].height = 2048;
	vxim->fb_config[5].width = vxim->fb_config[5].height = 4096;

	/* Attempt to open the config file. */
	if ((fname=getenv(FBCONFIG_ENV1)) || (fname=getenv(FBCONFIG_ENV2)))
	    fp = fopen (fname, "r");
	if (!fp && (fname = getenv ("HOME"))) {
	    sprintf (lbuf, "%s/%s", fname, FBCONFIG_1);
	    fp = fopen (fname = lbuf, "r");
	    if (fp) {
	        vxim->imtoolrc = (char *) calloc (strlen(fname+1),sizeof(char));
		strncpy (vxim->imtoolrc, fname, strlen(fname));
	    }
	}
	if (!fp)
	    fp = fopen (fname = vxim->imtoolrc, "r");
 	for (i=0; !fp && fb_paths[i]; i++) {
	    if ((fp = fopen (fname = fb_paths[i], "r"))) {
	        vxim->imtoolrc = (char *) calloc (strlen(fb_paths[i]+1),
		    sizeof(char));
		strncpy (vxim->imtoolrc, fb_paths[i],strlen(fb_paths[i]));
		break;
	    }
	}
	if (!fp) {
	    fprintf (stderr, 
		"Warning: No frame buffer configuration file found.\n");
	    return;
	}


	/* Scan the frame buffer configuration file.
	 */
	while (fgets (lbuf, SZ_LINE, fp) != NULL) {
	    /* Skip comment lines and blank lines. */
	    for (ip=lbuf;  *ip == ' ' || *ip == '\t';  ip++)
		;
	    if (*ip == '\n' || *ip == '#')
		continue;
	    if (!isdigit (*ip))
		continue;
	    switch (sscanf (ip, "%d%d%d%d", &config,&nframes,&width,&height)) {
	    case 4:
		break;			/* normal case */
	    case 3:
		height = width;		/* default to square format */
		break;
	    default:
		fprintf (stderr, "vximtool: bad config `%s'\n", ip);
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
		fprintf (stderr, "imtool warning: fb config %d [%d-%dx%d] - ",
		    config, nframes, width, height);
		fprintf (stderr, "frame width should be even, reset to %d\n",
		    --width);
	    }

	    config = max(1, min(MAX_FBCONFIG, config)) - 1;
	    vxim->fb_config[config].nframes = nframes;
	    vxim->fb_config[config].width   = width;
	    vxim->fb_config[config].height  = height;
	}

	fclose (fp);
}



/* USAGE -- Print a list of command-line options.
 */
#ifdef ANSI_FUNC

static void 
Usage (void)
#else

static void
Usage ()
#endif
{
        fprintf (stderr, "Usage:\n\n");
        printoption ("    vximtool");
        printoption ("[-background]");             /* run in background  */
        printoption ("[-config <num>]");           /* initial config     */
        printoption ("[-fifo <pipe>]");            /* fifo pipe          */
        printoption ("[-fifo_only]");              /* use fifo only      */
        printoption ("[-help]");                   /* Print help         */
        printoption ("[-i]");                      /* interactive        */
        printoption ("[-imtoolrc <file>]");        /* fbconfig file      */
        printoption ("[-inet_only | -port_only]"); /* use inet only      */
        printoption ("[-noraster]");          	   /* don't save pix     */
        printoption ("[-nframes <num>]");          /* # of frames        */
        printoption ("[-port <num>]");             /* inet port          */
        printoption ("[-nports <num>]");           /* No. inet ports     */
        printoption ("[-proxy]");             	   /* run a proxy server */
        printoption ("[-verbose]");           	   /* verbose output     */
        printoption ("[-unix <name>]");            /* unix socket        */
        printoption ("[-unix_only]");              /* use unix only      */
        fprintf (stderr,"\n");
}


/* PRINTOPTION -- Pretty-print an option string.
 */
static int cpos = 0;
#ifdef ANSI_FUNC

static void 
printoption (char *st)
#else

static void
printoption(st)
char    *st;
#endif
{
        if (strlen(st) + cpos > 78) {
            fprintf (stderr,"\n\t");
            cpos = 8;
        }
        fprintf (stderr,"%s ",st);
        cpos = cpos + strlen(st) + 1;
}


#ifdef	HAVE_CDL
/* VX_FLIP -- Reverse order of lines in raster.
 */

#ifdef ANSI_FUNC

static void
vx_flip (char *buffer, int nx, int ny)
#else

static void
vx_flip (buffer, nx, ny)
char   *buffer;
int     nx;
int     ny;
#endif
{
        register int    i, j, v;
        register char  *buff1, *buff2;

        for (i = 0; i < ny / 2; i++) {
            buff1 = &buffer[i*nx];
            buff2 = &buffer[(ny-1-i)*nx];
            for (j = 0; j < nx; j++) {
                v = *buff1;
                *(buff1++) = *buff2;
                *(buff2++) = v;
            }
        }
}
#endif


/* ADD_MAPPING --  Add a mapping for the current frame.  
 *
 * File format (two lines):
 *
 *	image title (imtool header label string)\n
 *	a b c d tx ty z1 z2 zt \n
 *	region_name sx sy snx sny dx dy dnx dny\n
 *	object_ref
 *
 * The WCS text is passed in via the data stream as a write to the subunit
 * WCS and left in the buffer "wcsbuf".  Mapping information is parsed 
 * elsewhere if needed, our only purpose here is to extract the frame WCS.
 */

static void
add_mapping (vxim, ctran, wcsbuf, fr)
register VXimDataPtr vxim;
CtranPtr ctran;
char	*wcsbuf;
FrameBufPtr fr;
{
	register MappingPtr mp = &fr->mapping[fr->nmaps];
        register CtranPtr   ct = &mp->ctran;
	register int i, j;
	char buf[SZ_WCSBUF], *format;

        /* Attempt to read the WCS and set up a unitary transformation
         * if the information cannot be read.
         */
        if (sscanf (wcsbuf, "%[^\n]\n%f%f%f%f%f%f%f%f%d",
            buf, &ct->a, &ct->b, &ct->c, &ct->d, &ct->tx, &ct->ty,
            &ct->z1, &ct->z2, &ct->zt) < 7) {

            if (wcsbuf[0])
                fprintf (stderr, "vximtool: error decoding WCS\n");

            strncpy (ct->imtitle, "[NO WCS]\n", SZ_IMTITLE);
            ct->a  = ct->d  = 1;
            ct->b  = ct->c  = 0;
            ct->tx = ct->ty = 0;
            ct->zt = W_UNITARY;
        } else
            strncpy (ct->imtitle, buf, SZ_IMTITLE);

        ct->zt = W_UNITARY;
        ct->valid = 1;


	mp->ref[0] = '\0';
	mp->region[0] = '\0';

	/* Skip over the first two lines of WCS data.
	 */
	strcpy (buf, wcsbuf);
	for (i=0, j=0; j < 2 && buf[i]; i++)
	    if (buf[i] == '\n')
	        j++;

	/* Attempt to read the mapping.
	 */
	mp->id = mp->regid = ++objid;
	if (sscanf (&buf[i], "%s%f%f%d%d%d%d%d%d\n%s\n",
	    mp->region, &mp->sx, &mp->sy, &mp->snx, &mp->sny, 
	    &mp->dx, &mp->dy, &mp->dnx, &mp->dny, mp->ref) < 10) {

	        if (!wcsbuf[0])
	            fprintf (stderr, "vximtool: error decoding WCS mapping\n");
	        strncpy (mp->ref, "none", SZ_IMTITLE);

	        mp->sx  = 1.0;
	        mp->sy  = 1.0;
	        mp->snx = vxim->width;
	        mp->sny = vxim->height;
	        mp->dx  = 1;
	        mp->dy  = 1;
	        mp->dnx = vxim->width;
	        mp->dny = vxim->height;
	}
	memmove (ctran, &mp->ctran, sizeof (Ctran));

	fr->nmaps++;
}


/* PRINT_MAPPINGS -- Debug routine to print all mappings on a frame.
 */
print_mappings (fr)
FrameBufPtr fr;
{
	MappingPtr mp;
	register int i;

	if (fr->nmaps == 0) printf ("No mappings for frame %d\n", fr->frameno);
	for (i=0; i < fr->nmaps; i++) {
	    mp = &fr->mapping[i];
	    printf ("Mapping %d frame=%d:\n", fr->nmaps, fr->frameno);
	    printf ("\t%s %f %f %d %d %d %d %d %d\n\t%s\n", 
	        mp->region, mp->sx, mp->sy, mp->snx, mp->sny, 
	        mp->dx, mp->dy, mp->dnx, mp->dny, mp->ref);
	}
}


/* IIS_READ -- Read exactly "n" bytes from a descriptor. 
 */

#ifdef ANSI_FUNC
static int                                   
iis_read (int fd, void *vptr, int nbytes)

#else
static int                                   
iis_read (fd, vptr, nbytes)
int 	fd; 
void 	*vptr; 
int 	nbytes;
#endif
{
        char    *ptr = vptr;
        int 	nread = 0, nleft = nbytes, nb = 0;

        while (nleft > 0) {
            if ( (nb = read(fd, ptr, nleft)) < 0) {
                if (errno == EINTR)
                    nb = 0;          	/* and call read() again */
                else
                    return(-1);
            } else if (nb == 0)
                break;                  /* EOF */
            nleft -= nb;
            ptr   += nb;
            nread += nb;
        }
        return (nread);              	/* return no. of bytes read */
}


/* IIS_WRITE -- Write exactly "n" bytes to a descriptor. 
 */
#ifdef ANSI_FUNC
static int                                   
iis_write (int fd, void *vptr, int nbytes)

#else

static int                                   
iis_write (fd, vptr, nbytes)
int 	fd; 
void 	*vptr; 
int 	nbytes;
#endif

{
        char 	*ptr = vptr;
        int     nwritten = 0,  nleft = nbytes, nb = 0;

        while (nleft > 0) {
            if ( (nb = write(fd, ptr, nleft)) <= 0) {
                if (errno == EINTR)
                    nb = 0;           	/* and call write() again */
                else
                    return(-1);         /* error */
            }
            nleft    -= nb;
            ptr      += nb;
            nwritten += nb;
        }
        return (nwritten);
}
