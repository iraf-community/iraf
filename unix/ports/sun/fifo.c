/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>
#include <ctype.h>
#include <stdio.h>

#define	SUNOS4

/*
 * FIFO.C -- Test fifo i/o.  This is a server process much like IMTOOL,
 * except that all the process does is respond to datastream requests
 * to read and write an internal 512 sq frame buffer maintained as an
 * array in memory (only one frame buffer is supported).  A log is kept
 * to the stderr of all datastream requests.
 *
 * Used to debug fifo i/o - NOT USED IN THE ONLINE PROGRAMS.
 *
 * To make:	cc fifo.c -o fifo.e
 *
 * Usage:	fifo.e >& spool		run server, logging output to spool
 *		fifo.e -i		run interactively
 *
 * In interactive mode, cursor value strings may be typed in on the fifo.e
 * stdin in response to cursor read requests from the client.  Otherwise,
 * a constant cursor value "1.0 1.0 101 q" is returned.
 */

#define	I_DEVNAME	"/dev/imt1o"
#define	O_DEVNAME	"/dev/imt1i"
#define	OLD_DEVNAME	"/dev/imt1"
#define	IO_TIMEOUT	30
#define	SZ_FIFOBUF	4000
#define	SZ_WCSBUF	320		/* WCS text buffer size		*/
#define	MAX_FRAMES	1
#define	SZ_FNAME	256

#define	MEMORY		01		/* frame buffer i/o		*/
#define	LUT		02		/* lut i/o			*/
#define	FEEDBACK	05		/* used for frame clears	*/
#define	IMCURSOR	020		/* logical image cursor		*/
#define	WCS		021		/* used to set WCS		*/

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

static	int	interactive=0;
static	int	background=0;
static	int	datain, dataout=0;
static	char	framebuf[512*512];
static	int	frame=1, reference_frame=1, display_frame=1;
static	int	fb_config_index=1, fb_nframes=1;
static	int	Fb_width = 512, Fb_height = 512;
static	char	wcsbuf[MAX_FRAMES][SZ_WCSBUF];


/* FIFO -- Text fifo i/o.
 */
main (argc, argv)
int	argc;
char	**argv;
{
	fd_set	fds;

	if (argc > 1)
	    if (strcmp (argv[1], "-i") == 0)
		interactive++;			/* type in cursor values */

	/* Open the output fifo.  We have to open it ourselves first as a
	 * client to get around the fifo open-no-client error.
	 */
	if ((datain = open (O_DEVNAME, O_RDONLY|O_NDELAY)) != -1) {
	    if ((dataout = open (O_DEVNAME, O_WRONLY|O_NDELAY)) != -1)
		fcntl (dataout, F_SETFL, O_WRONLY);
	    close (datain);
	}

	/* Open the input stream, a FIFO pseudodevice file used by
	 * applications to send us commands and data.
	 */
	if ((datain = open (I_DEVNAME, O_RDONLY|O_NDELAY)) == -1) {
	    if ((datain = open (OLD_DEVNAME, O_RDONLY|O_NDELAY)) == -1)
		fprintf (stderr, "Warning: cannot open %s\n", I_DEVNAME);
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
	char	buf[SZ_FIFOBUF];
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
		    for (i=0, p=(short *)&iis;  i < 8;  i++)
			fprintf (stderr, " %6o", p[i]);
		    fprintf (stderr, "\n");

		    bswap2 ((char *)&iis, (char *)&iis, sizeof(iis));
		    fprintf (stderr, "  swap:");
		    for (i=0, p=(short *)&iis;  i < 8;  i++)
			fprintf (stderr, " %6o", p[i]);
		    fprintf (stderr, "\n");
		    fflush (stderr);
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

	/* Log command. */
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
	    /* The feedback unit is used only to clear a frame.
	     */
	    set_reference_frame (decode_frameno (iis.z & 07777));
	    /* erase (rf_p); */
	    fprintf (stderr, "erase frame %d\n", reference_frame);
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
		    if (frame > fb_nframes) {
			if (frame < MAX_FRAMES) {
			    /* set_fbconfig (fb_config_index, frame); */
			    fprintf (stderr, "set_fbconfig (%d, %d)\n",
				fb_config_index, frame);
			} else {
			    fprintf (stderr, "imtool warning: ");
			    fprintf (stderr, 
			    "attempt to display nonexistent frame %d\n", frame);
			    frame = fb_nframes - 1;
			}
		    }

		    /* set_frame (frame); */
		    fprintf (stderr, "set_frame (%d)\n", frame);
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
		int     nbytes, nleft, n, x, y;
		long    starttime;

		/* Get the frame to be read from. */
		set_reference_frame (decode_frameno (iis.z & 07777));

		fb = (unsigned char *)framebuf;
		nbytes = ndatabytes;
		x = iis.x & 01777;
		y = iis.y & 01777;

		ip = max (fb, min (fb + Fb_width * Fb_height - nbytes,
		    fb + y * Fb_width + x));
		if (ip != fb + y * Fb_width + x) {
		    fprintf (stderr,
			"imtool: attempted read out of bounds on framebuf\n");
		    fprintf (stderr,
			"read %d bytes at [%d,%d]\n", nbytes, x, y);
		}

		/* Log i/o command. */
		fprintf (stderr, "read %d bytes at x=%d, y=%d\n",
		    nbytes, x, y);

		/* Return the data from the frame buffer. */
		starttime = time(0);
		for (nleft = nbytes;  nleft > 0;  nleft -= n) {
		    n = (nleft < SZ_FIFOBUF) ? nleft : SZ_FIFOBUF;
		    if ((n = write (dataout, ip, n)) <= 0) {
			if (n < 0 || (time(0) - starttime > IO_TIMEOUT)) {
			    fprintf (stderr, "IMTOOL: timeout on write\n");
			    break;
			}
		    } else
			ip += n;
		}

		return (0);

	    } else {
		/* Write to the display.
		 */
		unsigned char *fb, *op;
		int     nbytes, nleft, n, x, y;
		long    starttime;

		/* Get the frame to be written into (encoded with a bit for
		 * each frame, 01 is frame 1, 02 is frame 2, 04 is frame 3,
		 * and so on).
		 */
		set_reference_frame (decode_frameno (iis.z & 07777));

		/* Get a pointer into the frame buffer where the data will
		 * be put.
		 */
		fb = (unsigned char *)framebuf;
		nbytes = ndatabytes;
		x = iis.x & 07777;
		y = iis.y & 07777;

		op = max (fb, min (fb + Fb_width * Fb_height - nbytes,
		    fb + y * Fb_width + x));
		if (op != fb + y * Fb_width + x) {
		    fprintf (stderr,
			"imtool: attempted write out of bounds on framebuf\n");
		    fprintf (stderr,
			"write %d bytes to [%d,%d]\n", nbytes, x, y);
		}

		/* Log i/o command. */
		fprintf (stderr, "write %d bytes at x=%d, y=%d\n",
		    nbytes, x, y);

		/* Read the data into the frame buffer.
		 */
		starttime = time(0);
		for (nleft = nbytes;  nleft > 0;  nleft -= n) {
		    n = (nleft < SZ_FIFOBUF) ? nleft : SZ_FIFOBUF;
		    if ((n = read (datain, op, n)) <= 0) {
			if (n < 0 || (time(0) - starttime > IO_TIMEOUT))
			    break;
		    } else {
			/* Set any zeroed pixels to the background color,
			 * if a special background color is specified.
			 */
			if (background)
			    for (cp=op, i=n;  --i >= 0;  cp++)
				if (!*cp)
				    *cp = background;
			op += n;
		    }
		}

		/* Refresh the display, if the current display frame is the
		 * same as the reference frame.
		if (rf_p == df_p) {
		    BRect    fb_r, pw_r;

		    fb_r.r_left   = x * zoom;
		    fb_r.r_top    = y * zoom;
		    fb_r.r_width  = min (nbytes * zoom, fb_width);
		    fb_r.r_height = ((nbytes*zoom*zoom + fb_width-1)/fb_width);

		    Bpw_get_region_rect (gio_pw, &pw_rect);
		    Bpw_lock (gio_pw, &pw_rect);

		    pw_rect.r_left = df_p->fb_xoff;
		    pw_rect.r_top  = df_p->fb_yoff;

		    if (maprect (&fb_rect, &fb_r, &pw_rect, &pw_r))
			if (maprect (&pw_rect, &pw_r, &fb_rect, &fb_r)) {
			    ds_write (gio_pw,
				pw_r.r_left, pw_r.r_top,
				pw_r.r_width, pw_r.r_height,
				PIX_SRC | PIX_COLOR(NGREY-1),
				df_p->fb_pr, fb_r.r_left, fb_r.r_top);

			    if (pw_r.r_top + pw_r.r_height >= pw_rect.r_height
				- cb_height)
				put_colorbar();
			}

		    Bpw_unlock (gio_pw);
		}
		 */

		return (0);
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
		register char *op;
		register int n;
		char   emsg[SZ_WCSBUF];
		char   *text;
		int    frame;

		for (op=emsg, n=SZ_WCSBUF;  --n >=0;  )
		    *op++ = 0;

		frame = decode_frameno (iis.z & 07777);
		if (frame > fb_nframes)
		    strcpy (text=emsg, "[NOSUCHFRAME]\n");
		else {
		    set_reference_frame (frame);
		    text = wcsbuf[reference_frame-1];
		}

		fprintf (stderr, "query wcs:\n");
		write (2, text, SZ_WCSBUF);

		write (dataout, text, SZ_WCSBUF);

	    } else {
		/* Set the WCS for the referenced frame.
		 */
		char    buf[1024];
		int     fb_config, frame;

		frame = decode_frameno (iis.z & 07777);
		if (frame > fb_nframes)
		    if (frame < MAX_FRAMES) {
			/* set_fbconfig (fb_config_index, frame); */
			fprintf (stderr, "set_fbconfig (%d, %d)\n",
			    fb_config_index, frame);
		    }

		set_reference_frame (frame);
		if ((fb_config = iis.t & 077) != fb_config_index) {
		    /* set_fbconfig (fb_config_index, frame); */
		    fprintf (stderr, "set_fbconfig (%d, %d)\n",
			fb_config_index, frame);
		}

		/* Read in and set up the WCS. */
		if (read (datain, buf, ndatabytes) == ndatabytes)
		    strncpy (wcsbuf[reference_frame-1], buf, SZ_WCSBUF);

		fprintf (stderr, "set wcs:\n");
		write (2, buf, SZ_WCSBUF);

		/*
		strcpy (rf_p->fb_ctran.format, W_DEFFORMAT);
		rf_p->fb_ctran.imtitle[0] = '\0';
		rf_p->fb_ctran.valid = 0;
		rf_p->fb_imageno++;
		rf_p->fb_objno = 1;

		wcs_update (rf_p);
		if (rf_p == df_p)
		    window_set (gio_frame, FRAME_LABEL, framelabel(), 0);
		 */
	    }

	    return (0);
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
	    if (iis.tid & IIS_READ) {
		/* Read the logical image cursor.  In the case of a blocking
		 * read all we do is initiate a cursor read; completion occurs
		 * when the user hits a key or button.
		 */
		fprintf (stderr, "read cursor position\n");
		if (iis.tid & IMC_SAMPLE) {
		    /* Sample the cursor position. */
		    /*
		    register struct ctran *ct;
		    int     wcs = iis.z;
		    int     sx, sy;
		    float   wx, wy;

		    wx = sx = last_x + pw_rect.r_left;
		    wy = sy = last_y + pw_rect.r_top;

		    if (wcs) {
			ct = wcs_update (df_p);
			if (ct->valid) {
			    if (abs(ct->a) > .001)
				wx = ct->a * sx + ct->c * sy + ct->tx;
			    if (abs(ct->d) > .001)
				wy = ct->b * sx + ct->d * sy + ct->ty;
			}
		    }
		    */

		    int     wcs = iis.z, key = 'q';
		    float   wx=1.0, wy=1.0;

		    /* Return the cursor value on the output datastream encoded
		     * in a fixed size ascii buffer.
		     */
		    gio_retcursorval (wx, wy, display_frame*100+wcs, key, "");

		} else {
		    /* Initiate a user triggered cursor read. */
		    /* gio_readcursor (iis.z); */
		    int     wcs = iis.z, key = 'q';
		    float   wx=1.0, wy=1.0;
		    gio_retcursorval (wx, wy, display_frame*100+wcs, key, "");
		}

	    } else {
		/* Write (set) the logical image cursor position. */
		/*
		fprintf (stderr, "write cursor position\n");
		register struct ctran *ct;
		int     sx = iis.x,  sy = iis.y;
		float   wx = sx,  wy = sy;
		int     wcs = iis.z;

		if (wcs) {
		    ct = wcs_update (df_p);
		    if (ct->valid) {
			if (abs(ct->a) > .001)
			    sx = (wx - ct->tx) / ct->a;
			if (abs(ct->d) > .001)
			    sy = (wy - ct->ty) / ct->d;
		    }
		}

		gio_setcursorpos (sx - pw_rect.r_left, sy - pw_rect.r_top);
		 */
	    }

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
		n = (nbytes < SZ_FIFOBUF) ? nbytes : SZ_FIFOBUF;
		if ((n = read (datain, buf, n)) <= 0)
		    break;
	    }

	fflush (stderr);
	return (0);
}


/* SET_REFERENCE_FRAME -- Set reference frame.  If the frame referenced is
 * greater than the current number of frames, attempt to increase the number
 * of frames.
 */
static
set_reference_frame (n)
register int	n;
{
	reference_frame = max (1, n);
	if (reference_frame > fb_nframes) {
	    if (reference_frame < MAX_FRAMES) {
		/* set_fbconfig (fb_config_index, reference_frame); */
		fprintf (stderr, "set_fbconfig %d %d\n",
		    fb_config_index, reference_frame);
	    } else {
		fprintf (stderr, "imtool warning: ");
		fprintf (stderr, 
		    "attempt to reference nonexistent frame %d\n",
		    reference_frame);
		reference_frame = fb_nframes;
	    }
	}

	/* rf_p = frames + (reference_frame - 1); */
}


/* DECODE_FRAMENO -- Decode encoded IIS register frame number.
 */
static
decode_frameno (z)
register int	z;
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


/* GIO_RETCURSORVAL -- Return the cursor value on the output datastream to
 * the client which requested the cursor read.
 */
static
gio_retcursorval (wx, wy, wcs, key, strval)
float	wx, wy;			/* cursor coordinates */
int	wcs;			/* encoded WCS value */
int	key;			/* keystroke used as trigger */
char	*strval;		/* optional string value */
{
	register char *op;
	register int n;
	char	curval[SZ_IMCURVAL];
	char	keystr[20];

	for (op=curval, n=SZ_IMCURVAL;  --n >=0;  )
	    *op++ = 0;

	/* If running FIFO in interactive mode, allow the user to type
	 * in the cursor value on the standard input.
	 */
	if (interactive) {
	    fprintf (stderr, "enter cursor value string: ");
	    fflush (stderr);
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
		wx, wy, wcs, keystr, strval);
	}
ret:
	fprintf (stderr, "%s", curval);

	/* Send it to the client program. */
	write (dataout, curval, sizeof(curval));
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
