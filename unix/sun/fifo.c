/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/fcntl.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <signal.h>
#include <stdio.h>

#define	SUNOS4

/*
 * FIFO.C -- Test fifo i/o.  This is a server process much like IMTOOL,
 * except that all the process does is respond to datastream requests
 * to read and write an internal 512 sq frame buffer maintained as an
 * array in memory.  A log is kept to the stderr of all datastream requests.
 * Used to debug fifo i/o - NOT USED IN THE ONLINE PROGRAMS.
 */

#define	I_DEVNAME	"/dev/imt1o"
#define	O_DEVNAME	"/dev/imt1i"
#define	IO_TIMEOUT	30
#define	BUFSIZE		4000

#define	MEMORY		01
#define	LUT		02
#define	FEEDBACK	05
#define	IMCURSOR	020
#define	SZ_IMCURVAL	160
#define	PACKED		0040000
#define	COMMAND		0100000
#define	IIS_READ	0100000
#define	IMC_SAMPLE	0040000
#define	IMT_FBCONFIG	077

struct	iism70 {
	short	tid;
	short	thingct;
	short	subunit;
	short	checksum;
	short	x, y, z;
	short	t;
};

#ifndef abs
#define	abs(a)		(((a)<0)?(-a):(a))
#endif

#ifndef min
#define min(a,b)	((a)<(b)?(a):(b))
#endif
#ifndef max
#define max(a,b)	((a)<(b)?(b):(a))
#endif

static	int	datain, dataout=0;
static	char	framebuf[512*512];
static	int	fb_width = 512, fb_height = 512;


/* FIFO -- Text fifo i/o.
 */
main (argc, argv)
int	argc;
char	**argv;
{
	fd_set	fds;

	/* Open the input stream, a FIFO pseudodevice file used by
	 * applications to send us commands and data.
	 */
	if ((datain = open (I_DEVNAME, O_RDONLY|O_NDELAY)) == -1) {
	    fprintf (stderr, "Warning: cannot open %s\n", I_DEVNAME);
	    fflush (stderr);
	} else {
	    /* Clear O_NDELAY for reading. */
	    fcntl (datain, F_SETFL, O_RDONLY);
	}

	FD_ZERO (&fds);  FD_SET (datain, &fds);
	while (select (FD_SETSIZE, &fds, NULL, NULL, NULL) > 0) {
	    ev_cmdinput();
	    fflush (stdout);
	    fflush (stderr);
	    FD_ZERO (&fds);  FD_SET (datain, &fds);
	}

	close (datain);
	exit (0);
}


/* EV_CMDINPUT -- Called when command or data input has arrived via the
 * pseudodevice input stream from some applications process.
 */
ev_cmdinput()
{
	register unsigned char *cp;
	register int	sum, i;
	register short	*p;
	int	ndatabytes, nbytes, n, ntrys=0;
	static	int errmsg=0, bswap=0;
	struct	iism70 iis;
	char	buf[BUFSIZE];
	int	fb_index;

	/* Get the IIS header. */
	if (read (datain, (char *)&iis, sizeof(iis)) < sizeof(iis)) {
	    fprintf (stderr, "imtool: command input read error\n");
	    fflush (stderr);
	    return (0);
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
		    fprintf (stderr, "imtool: bad data header checksum\n");
		    fflush (stderr);
		    if (bswap)
			bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		    fprintf (stderr, "noswap:");
		    fflush (stderr);
		    for (i=0, p=(short *)&iis;  i < 8;  i++)
			fprintf (stderr, " %6o", p[i]);
		    fprintf (stderr, "\n");

		    bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		    fprintf (stderr, "  swap:");
		    for (i=0, p=(short *)&iis;  i < 8;  i++)
			fprintf (stderr, " %6o", p[i]);
		    fprintf (stderr, "\n");

		    fprintf (stderr, "datastream synch lost - imtool dies\n");
		    fflush (stderr);
		    exit (1);
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

	fprintf (stderr,
	    "subunit=%06o tid=%06o nbytes=%7d x=%06o y=%06o z=%06o\n",
	    iis.subunit & 077,
	    iis.tid,
	    ndatabytes,
	    iis.x & 0177777,
	    iis.y & 0177777,
	    iis.z & 0177777);
	fflush (stderr);

	switch (iis.subunit & 077) {
	case FEEDBACK:
	    /* The feedback unit is used only to clear a frame.  We also use
	     * it to pass the frame buffer configuration index, encoded in
	     * the low bits of the transfer id (not used by a real IIS).
	     *
	    if ((fb_index = (iis.tid & IMT_FBCONFIG)) != fb_config_index)
		set_fbconfig (fb_index, 0);
	    set_reference_frame (iis.z & 07777);
	    erase (rf_p);
	     */
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

		if (read (datain, (char *)x, ndatabytes) == ndatabytes) {
		    if (bswap)
			bswap2 ((char *)x, (char *)x, ndatabytes);

		    z = x[0];
		    if (!z) z = 1;
		    for (n=0;  !(z & 1);  z >>= 1)
			n++;

		    frame = max (1, n + 1);
		    /*
		    if (frame > fb_nframes) {
			if (frame < MAX_FRAMES)
			    set_fbconfig (fb_config_index, frame);
			else {
			    fprintf (stderr, "imtool warning: ");
			    fprintf (stderr, 
			    "attempt to display nonexistent frame %d\n", frame);
			    frame = fb_nframes - 1;
			}
		    }

		    set_frame (frame);
		     */
		    return (0);
		}
	    }

	case MEMORY:
	    /* Load data into the frame buffer.  Data is assumed to be byte
	     * packed.
	     */
	    if (iis.tid & IIS_READ) {
		/* Read from the display.
		 */
		unsigned char *fb, *ip;
		int     nbytes, nleft, n, nb, x, y;
		long    starttime;

		/* Get the frame to be read from.
		set_reference_frame (iis.z & 07777);
		fb = (unsigned char *) mpr_d(rf_p->fb_pr)->md_image;
		 */

		fb = (unsigned char *)framebuf;
		nbytes = ndatabytes;
		x = iis.x & 01777;
		y = iis.y & 01777;

		fprintf (stderr, "return %d bytes at %d,%d\n", nbytes, x, y);
		fflush (stderr);

		ip = max (fb, min (fb + fb_width * fb_height - nbytes,
		    fb + y * fb_width + x));
		if (ip != fb + y * fb_width + x) {
		    fprintf (stderr,
			"imtool: attempted read out of bounds on framebuf\n");
		    fprintf (stderr,
			"read %d bytes at [%d,%d]\n", nbytes, x, y);
		    fflush (stderr);
		}

		/* Open the data output stream.  We must wait to do this until
		 * a process has opened the other end of the FIFO, else the
		 * open will return -1.  Without the O_NDELAY the open would
		 * block until a process opened the pipe.  Race conditions are
		 * not possible since the client will open both the input and
		 * output channels before sending us a command which requires
		 * a response on the output channel.
		 */
		if ((dataout = open (O_DEVNAME, O_WRONLY|O_NDELAY)) == -1) {
		    fprintf(stderr,"imtool error: cannot open %s for writing\n",
			O_DEVNAME);
		    fflush (stderr);
		} else {
		    /* Clear O_NDELAY for writing. */
		    fcntl (dataout, F_SETFL, O_WRONLY);

		    /* Return the data from the frame buffer. */
		    starttime = time(0);
		    for (nleft = nbytes;  nleft > 0;  nleft -= n) {
			n = (nleft < BUFSIZE) ? nleft : BUFSIZE;
			if ((n = write (dataout, ip, nb=n)) <= 0) {
			    fprintf (stderr, "write(%d,%x,%d)->%d\n",
				dataout,ip,nb,n);
			    fflush (stderr);
			    if (n < 0 || (time(0) - starttime > IO_TIMEOUT)) {
				fprintf (stderr, "timeout on write\n");
				fflush (stderr);
				break;
			    } else
				wmsec (10);
			} else {
			    fprintf (stderr, "write(%d,%x,%d)->%d\n",
				dataout,ip,nb,n);
			    fflush (stderr);
			    ip += n;
			}
		    }

		    close (dataout);
		    dataout = 0;
		}
		return (0);

	    } else {
		/* Write to the display.
		 */
		unsigned char *fb, *op;
		int     nbytes, nleft, nb, x, y;
		long    starttime;

		/* Get the frame to be written into (encoded with a bit for
		 * each frame, 01 is frame 1, 02 is frame 2, 04 is frame 3,
		 * and so on).
		set_reference_frame (iis.z & 07777);
		fb = (unsigned char *) mpr_d(rf_p->fb_pr)->md_image;
		 */

		/* Get a pointer into the frame buffer where the data will
		 * be put.
		 */
		fb = (unsigned char *)framebuf;
		nbytes = ndatabytes;
		x = iis.x & 07777;
		y = iis.y & 07777;

		op = max (fb, min (fb + fb_width * fb_height - nbytes,
		    fb + y * fb_width + x));
		if (op != fb + y * fb_width + x) {
		    fprintf (stderr,
			"imtool: attempted write out of bounds on framebuf\n");
		    fprintf (stderr,
			"write %d bytes to [%d,%d]\n", nbytes, x, y);
		    fflush (stderr);
		}

		/* Read the data into the frame buffer.
		 */
		starttime = time(0);
		for (nleft = nbytes;  nleft > 0;  nleft -= n) {
		    n = (nleft < BUFSIZE) ? nleft : BUFSIZE;
		    if ((n = read (datain, op, nb=n)) <= 0) {
			fprintf (stderr, "read(%d,%x,%d)->%d\n",
			    datain,op,nb,n);
			fflush (stderr);
			if (n < 0 || (time(0) - starttime > IO_TIMEOUT)) {
			    fprintf (stderr, "timeout on read\n");
			    fflush (stderr);
			    break;
			} else
			    wmsec (10);
		    } else {
			/* Set any zeroed pixels to the background color,
			 * if a special background color is specified.
			 */
			fprintf (stderr, "read(%d,%x,%d)->%d\n",
			    datain,op,nb,n);
			fflush (stderr);
			op += n;
		    }
		}
		return (0);
	    }
	    break;

	case IMCURSOR:
	    /* Read or write the logical image cursor.  This is an extension
	     * added to provide a high level cursor read facility; this is
	     * not the same as a low level access to the IIS cursor subunit.
	     * Cursor reads may be either nonblocking (immediate) or blocking,
	     * using the keyboard or mouse to terminate the read, and
	     * coordinates may be returned in either image (world) or frame
	     * buffer pixel coordinates.
	     */
	    return (0);
	    break;

	default:
	    /* Ignore unsupported command input.
	     */
	    break;
	}

	/* Discard any data following the header. */
	if (!(iis.tid & IIS_READ))
	    for (nbytes = ndatabytes;  nbytes > 0;  nbytes -= n) {
		n = (nbytes < BUFSIZE) ? nbytes : BUFSIZE;
		if ((n = read (datain, buf, n)) <= 0)
		    break;
	    }

	return (0);
}


/* SET_REFERENCE_FRAME -- Decode IIS register and set reference frame.  If the
 * frame referenced is greater than the current number of frames, atttempt to
 * increase the number of frames.
 */
static
set_reference_frame (z)
register int	z;
{
	register int	n;

	/* Get the frame number, encoded with a bit for each frame, 01 is
	 * frame 1, 02 is frame 2, 04 is frame 3, and so on.
	 */
	if (!z) z = 1;
	for (n=0;  !(z & 1);  z >>= 1)
	    n++;

/*
	reference_frame = max (1, n + 1);
	if (reference_frame > fb_nframes) {
	    if (reference_frame < MAX_FRAMES)
		set_fbconfig (fb_config_index, reference_frame);
	    else {
		fprintf (stderr, "imtool warning: ");
		fprintf (stderr, 
		    "attempt to reference nonexistent frame %d\n",
		    reference_frame);
		reference_frame = fb_nframes;
	    }
	}

	rf_p = frames + (reference_frame - 1);
 */
}


/* BSWAP2 - Move bytes from array "a" to array "b", swapping successive
 * pairs of bytes.  The two arrays may be the same but may not be offset
 * and overlapping.
 */
static
bswap2 (a, b, nbytes)
char 	*a, *b;		/* input array			*/
int	nbytes;		/* number of bytes to swap	*/
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


#define	mask(s)	(1<<((s)-1))
#define	setvec(vec, a) \
	vec.sv_handler = a; vec.sv_mask = vec.sv_onstack = 0

static int ringring;


/* WMSEC -- Suspend task execution (sleep) for the specified number
 * of milliseconds.
 */
wmsec (msec)
int	msec;
{
	struct	itimerval itv, oitv;
	register struct itimerval *itp = &itv;
	struct	sigvec vec, ovec;
#ifdef SUNOS4
	void	napmsx();
#else
	int	napmsx();
#endif
	int	omask;

	if (msec == 0)
	    return;

	timerclear (&itp->it_interval);
	timerclear (&itp->it_value);
	if (setitimer (ITIMER_REAL, itp, &oitv) < 0)
	    return;

	setvec (ovec, SIG_DFL);
	omask = sigblock(0);

	itp->it_value.tv_usec = (msec * 1000) % 1000000;
	itp->it_value.tv_sec  = (msec * 1000) / 1000000;

	if (timerisset (&oitv.it_value)) {
	    if (timercmp(&oitv.it_value, &itp->it_value, >))
		oitv.it_value.tv_sec -= itp->it_value.tv_sec;
	    else {
		itp->it_value = oitv.it_value;
		/* This is a hack, but we must have time to
		 * return from the setitimer after the alarm
		 * or else it'll be restarted.  And, anyway,
		 * sleep never did anything more than this before.
		 */
		oitv.it_value.tv_sec  = 1;
		oitv.it_value.tv_usec = 0;
	    }
	}

	setvec (vec, napmsx);
	(void) sigvec (SIGALRM, &vec, &ovec);
	ringring = 0;
	(void) setitimer (ITIMER_REAL, itp, (struct itimerval *)0);

	while (!ringring)
	    sigpause (omask &~ mask(SIGALRM));

	(void) sigvec (SIGALRM, &ovec, (struct sigvec *)0);
	(void) setitimer (ITIMER_REAL, &oitv, (struct itimerval *)0);
}


#ifdef SUNOS4
static void
#else
static int
#endif
napmsx()
{
	ringring = 1;
}
