#include <stdio.h>

/*
 * RAS2BIN -- Convert a hex encoded raster to binary.
 *
 * Usage:	ras2bin -encoding encoding -nx nx -ny ny infile outfile
 *
 *   encoding	hex1, hex2, hex1-rle, hex2-rle
 *   nx, ny	raster size in pixels
 *   input	input hex encoded, possible RLE (run length) encoded
 *		block of text.  whitespace is ignored otherwise the file
 *		should contain only data.
 *   output	output file to receive the binary array of pixels
 *
 * Perhaps this ought to take ras2text output and generate a Sun rasterfile,
 * but currently we just decode the pixel data and output it in binary.
 */
main (argc, argv)
int argc;
char **argv;
{
	char *infile=NULL, *outfile=NULL;
	char pixels[102400];
	int in, out, nx, ny, bias;
	char *encoding;
	int i;

	for (i=1;  i < argc;  i++)
	    if (argv[i][0] == '-') {
		if (strncmp (argv[i], "-en", 3) == 0)
		    encoding = argv[++i];
		else if (strcmp (argv[i], "-nx") == 0)
		    nx = atoi (argv[++i]);
		else if (strcmp (argv[i], "-ny") == 0)
		    ny = atoi (argv[++i]);
		else if (strcmp (argv[i], "-bias") == 0)
		    bias = atoi (argv[++i]);
	    } else if (!infile) {
		infile = argv[i];
	    } else if (!outfile)
		outfile = argv[i];

	if (!infile || !outfile || nx <= 0 || ny <= 0) {
	    fprintf (stderr, "bad usage\n");
	    exit (1);
	}

	printf ("input = %s\n", infile);
	if ((in = open (infile, 0)) < 0) {
	    fprintf (stderr, "cannot open %s\n", infile);
	    exit (2);
	}
	printf ("read %d chars from input\n", read (in, pixels, 102400));
	close (in);

	printf ("output = %s\n", outfile);
	if ((out = creat (outfile, 0644)) < 0) {
	    fprintf (stderr, "cannot open %s\n", outfile);
	    exit (3);
	}

	printRaster (out, pixels, encoding, nx, ny, bias);
	close (out);
}

printRaster (out, pixels, encoding, nx, ny, bias)
int out;
char *pixels;
char *encoding;
int nx, ny;
int bias;
{
	register char *ip;
	register unsigned char *op;
	register int v, i, j;
	static unsigned char hex1[256], hex2[256];
	static int have_tables = 0;
	unsigned char *data, *otop;
	char *sv_pixels = pixels;

	/* Generate hex to binary lookup tables in first call. */
	if (!have_tables) {
	    /* Generate char-to-binary table for the hex1 encoding. */
	    for (i=0;  i < 256;  i++)
		hex1[i] = 0177;
	    for (i='0';  i <= '9';  i++)
		hex1[i] = i - '0';
	    for (i='A';  i <= 'Z';  i++)
		hex1[i] = i - 'A' + 10;
	    for (i='a';  i <= 'z';  i++)
		hex1[i] = i - 'a' + 36;
	    hex1['$'] = 62;
	    hex1['_'] = 63;

	    /* Generate char-to-binary table for the hex2 encoding. */
	    for (i=0;  i < 256;  i++)
		hex2[i] = 0177;
	    for (i='0';  i <= '9';  i++)
		hex2[i] = i - '0';
	    for (i='a';  i <= 'f';  i++)
		hex2[i] = i - 'a' + 10;
	    for (i='A';  i <= 'F';  i++)
		hex2[i] = i - 'A' + 10;

	    have_tables++;
	}

	/* Decode the pixel data. */
	if (!(data = (unsigned char *) malloc (nx * ny)))
	    return (-1);
	otop = data + nx * ny;

	/* Uncompress the input if RLE compression is indicated. */
	if (strcmp (&encoding[strlen(encoding)-4], "-rle") == 0) {
	    int buflen = nx * ny * 2;
	    char *ibuf, *op;
	    int ch;

	    /* Get buffer to hold the uncompressed pixel data array. */
	    if (!(ibuf = (char *) malloc (buflen + 1)))
		goto err;

	    /* Uncompress the pixel array.  */
	    for (ip=pixels, op=ibuf;  *ip;  ) {
		while (isspace (*ip))
		    ip++;

		if ((ch = *ip++) == '@') {
		    if ((i = hex1[*ip++]) >= 0x7f)
			while (*ip && ((i = hex1[*ip++]) >= 0x7f))
			    ;
		    if (op-ibuf + i + 1 > buflen)
			goto err;
		    for (v = *(op-1), i++;  --i >= 0;  )
			*op++ = v;

		} else if (ch == '%') {
		    if ((i = hex2[*ip++]) >= 0x7f)
			while (*ip && ((i = hex2[*ip++]) >= 0x7f))
			    ;
		    if ((j = hex2[*ip++]) >= 0x7f)
			while (*ip && ((j = hex2[*ip++]) >= 0x7f))
			    ;
		    i = ((i << 4) | j) + 1;
		    if (op-ibuf + i > buflen)
			goto err;
		    for (v = *(op-1);  --i >= 0;  )
			*op++ = v;

		} else
		    *op++ = ch;
	    }

	    *op = '\0';
	    pixels = ibuf;
	}

	/* Convert the ascii pixels array to a binary data array.
	 */
	if (strcmp (encoding, "numeric") == 0) {
	    while (isspace (*ip))
		ip++;
	    for (ip=pixels, op=data;  *ip && op < otop;  ) {
		for (v=0;  isdigit(*ip);  )
		    v = v * 10 + *ip++ - '0';
		*op++ = v + bias;
		while (isspace (*ip))
		    ip++;
	    }
	} else if (strncmp (encoding, "hex1", 4) == 0) {
	    for (ip=pixels, op=data;  *ip && op < otop;  ) {
		if ((v = hex1[*ip++]) > 0xf)
		    while (*ip && ((v = hex1[*ip++]) > 0xf))
			;
		*op++ = v + bias;
	    }
	} else if (strncmp (encoding, "hex2", 4) == 0) {
	    for (ip=pixels, op=data;  *ip && op < otop;  ) {
		if ((v = hex2[*ip++]) > 0xf)
		    while (*ip && ((v = hex2[*ip++]) > 0xf))
			;
		if ((i = hex2[*ip++]) > 0xf)
		    while (*ip && ((i = hex2[*ip++]) > 0xf))
			;
		*op++ = ((v << 4) | i) + bias;
	    }
	} else {
err:	    free ((char *)data);
	    if (pixels != sv_pixels)
		free (pixels);
	    return (-1);
	}

	/* Write the pixels. */
	write (out, data, op - data);
	free ((char *)data);
	if (pixels != sv_pixels)
	    free (pixels);

	return (0);
}
