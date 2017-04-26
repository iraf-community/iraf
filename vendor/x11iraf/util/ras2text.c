#include <stdio.h>
#include <rasterfile.h>

/*
 * RAS2TEXT -- This is a small standalone utility program to convert a color
 * Sun rasterfile to a printable ASCII form suitable for inclusion as an image
 * in a GUI.  The dimensions of the raster are output, along with the RGB 
 * colortable and the hex-encoded and optionally run length encoded pixels.
 *
 * Usage:	ras2text [-hex1 | -hex2] [-rle] fname
 *
 * The decoded Sun raster is written as text to the standard output.
 */

#define	HEX1	1
#define	HEX2	2

struct color {
    unsigned char red, green, blue;
};

main (argc, argv)
int argc;
char **argv;
{
	register unsigned char *op;
	register unsigned char *ip;
	register int i, j, v;
	struct rasterfile r;
	struct color colors[256];
	unsigned char red[256], green[256], blue[256];
	unsigned char hex1[256], hex2[256*2];
	unsigned char *data, *obuf, *cbuf;
	int encoding, compress, debug;
	int fd, npix, nelem, maxval, n;
	char *fname;

	fname = NULL;
	encoding = HEX2;
	compress = 0;
	debug = 0;

	/* Process argument list. */
	for (i=1;  i < argc;  i++)
	    if (argv[i][0] == '-') {
		if (strcmp (argv[i], "-hex1") == 0)
		    encoding = HEX1;
		else if (strcmp (argv[i], "-hex2") == 0)
		    encoding = HEX2;
		else if (strcmp (argv[i], "-rle") == 0)
		    compress++;
		else if (strcmp (argv[i], "-d") == 0)
		    debug++;
		else
		    fprintf (stderr, "unknown switch %s\n", argv[i]);
	    } else
		fname = argv[i];

	/* Open file. */
	if ((fd = open (fname, 0)) < 0) {
	    fprintf (stderr, "cannot open %s\n", fname);
	    exit (1);
	}

	/* Read file header. */
	if (read(fd,&r,sizeof(r)) != sizeof(r) || r.ras_magic != RAS_MAGIC) {
	    fprintf (stderr, "not a Sun rasterfile: %s\n", fname);
	    exit (2);
	}

	/* Print header. */
	printf ("width %d height %d depth %d\n",
	    r.ras_width, r.ras_height, r.ras_depth);
	
	if (debug) {
	    fprintf (stderr, "width=%d height=%d depth=%d length=%d type=%d\n",
		r.ras_width, r.ras_height, r.ras_depth, r.ras_length,
		r.ras_type);
	    fprintf (stderr, "maptype=%d, maplength=%d\n",
		r.ras_maptype, r.ras_maplength);
	}

	if (r.ras_type != RT_STANDARD) {
	    fprintf (stderr, "rasterfile not in standard format\n");
	    exit (3);
	}

	/* Read the colormap. */
	if (n = r.ras_maplength / 3) {
	    if (read (fd, red,   n) != n ||
		read (fd, green, n) != n ||
		read (fd, blue,  n) != n)  {

		fprintf (stderr, "cannot read colormap\n");
		exit (4);
	    }
	}

	/* Read the pixels. */
	if ((data = (unsigned char *) malloc (npix=r.ras_length)) == NULL) {
	    fprintf (stderr, "out of memory\n");
	    exit (5);
	}
	if ((npix = read(fd,data,npix)) != r.ras_length) {
	    fprintf (stderr, "data read error\n");
	    exit (6);
	}

	/* If bitmap convert to byte pixel array. */
	if (r.ras_depth == 1) {
	    unsigned char *bits = data;
	    npix = r.ras_width * r.ras_height;
	    if ((data = (unsigned char *) malloc (npix)) == NULL) {
		fprintf (stderr, "out of memory\n");
		exit (5);
	    }
	    for (i=0, op=data;  i < npix;  i++)
		*op++ = !((bits[i/8] & (1 << (7 - (i % 8)))) != 0);

	    free (bits);
	}

	if ((obuf = (unsigned char *) malloc (npix*2)) == NULL) {
	    fprintf (stderr, "out of memory\n");
	    exit (5);
	}

	/* Find largest pixel value. */
	for (i=0, maxval=v=0;  i < npix;  i++)
	    if (data[i] > v)
		v = data[i];
	maxval = v;
	if (debug)
	    fprintf (stderr, "max pixel value = %d\n", maxval);

	/* Print the colormap. */
	n = r.ras_maplength / 3;
	for (i=0;  i < n;  i++) {
	    colors[i].red = red[i];
	    colors[i].green = green[i];
	    colors[i].blue = blue[i];
	}

	if (n >= maxval)
	    n = maxval + 1;

	printf ("colormap (length %d) = {\n    ", nelem = n);
	for (i=0, n=1;  i < nelem;  i++, n++) {
	    printf ("{%3d %3d %3d}  ",
		colors[i].red, colors[i].green, colors[i].blue);
	    if (n && n > 4) {
		printf ("\n    ");
		n = 0;
	    }
	}
	if (n)
	    printf ("\n");
	printf ("}\n");

	if (r.ras_length <= 0) {
	    fprintf (stderr, "no data\n");
	    exit (7);
	}

	/* Print the pixels.
	 */

	/* Generate binary to hex1 (64 element) lookup table. */
	for (n=0, op=hex1;  n < 256;  n++) {
	    i = (n % 64);
	    if (i < 10)
		*op++ = i + '0';
	    else if (i < 36)
		*op++ = (i - 10) + 'A';
	    else if (i < 62)
		*op++ = (i - 36) + 'a';
	    else if (i == 62)
		*op++ = '$';
	    else
		*op++ = '_';
	}

	/* Generate binary to hex2 (256 element) lookup table. */
	for (n=0, op=hex2;  n < 256;  n++) {
	    i = ((n >> 4) & 017);
	    *op++ = (i < 10) ? i + '0' : (i-10) + 'A';
	    i = (n & 017);
	    *op++ = (i < 10) ? i + '0' : (i-10) + 'A';
	}

	/* Hex encode the data. */
	if (encoding == HEX1) {
	    /* Hex1 encoding uses only one character per pixel but the
	     * pixel range is restricted to 0 to 63.
	     */
	    for (j=0, ip=data, op=obuf;  j < r.ras_height;  j++) {
		for (i=0;  i < r.ras_width;  i++)
		    *op++ = hex1[*ip++];
		if (r.ras_width % 2)
		    ip++;
	    }
	} else if (encoding == HEX2) {
	    /* Hex2 encoding uses 2 characters per pixel and supports
	     * pixel values in the range 0 to 255.
	     */
	    for (j=0, ip=data, op=obuf;  j < r.ras_height;  j++) {
		for (i=0;  i < r.ras_width;  i++) {
		    v = *ip++ * 2;
		    *op++ = hex2[v];
		    *op++ = hex2[v+1];
		}
		if (r.ras_width % 2)
		    ip++;
	    }
	}
	*op = '\0';

	/* Run length compress the data.  The compressed data stream
	 * contains a mixture of literal data codes and repeat codes.
	 * A "@" followed by a hex1-encoded number N causes the most
	 * recent pixel value to be repeated N+1 times, where N < 64.
	 * A "%" followed by a hex2-encoded number N causes the most
	 * recent pixel value to be repeated N+1 times, where N < 256.
	 */
	if (compress) {
	    npix = r.ras_length;
	    if ((cbuf = (unsigned char *) malloc (npix*3)) == NULL) {
		fprintf (stderr, "out of memory\n");
		exit (8);
	    }

	    ip = obuf;
	    op = cbuf;
	    *op++ = v = *ip++;
	    while (*ip) {
		for (n=0;  n < 256 && *ip == v;  ip++, n++)
		    ;
		if (n == 0) {
		    *op++ = v = *ip++;
		} else if (n < 3) {
		    while (--n >= 0)
			*op++ = v;
		} else if (n <= 64) {
		    *op++ = '@';
		    *op++ = hex1[n-1];
		} else if (n <= 256) {
		    *op++ = '%';
		    *op++ = hex2[(n-1)*2];
		    *op++ = hex2[(n-1)*2+1];
		}
	    }
	    *op = '\0';

	    free (obuf);
	    obuf = cbuf;
	}

	/* Output the encoded pixel data.
	 */
	printf ("pixels (%s%s) = {\n    ",
	    (encoding == HEX1) ? "hex1" : "hex2", compress ? " rle" : "");
	for (ip=obuf, n=1;  *ip;  ip++, n++) {
	    putchar (*ip);
	    if (n && n > 72) {
		printf ("\n    ");
		n = 0;
	    }
	}
	if (n)
	    printf ("\n");
	printf ("}\n");

	free (data);
	free (obuf);
	close (fd);
	exit (0);
}
