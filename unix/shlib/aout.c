/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <a.out.h>

/* AOUT -- Examine the header of a AOUT file.
 */
main (argc, argv)
int	argc;
char	*argv[];
{
	struct	exec fh;
	int	fd;

	if (argc < 2) {
	    fprintf (stderr, "Usage: aout <filename>\n");
	    exit (0);
	}

	if ((fd = open (argv[1], 0)) == -1) {
	    fprintf (stderr, "cannot open %s\n", argv[1]);
	    exit (1);
	}

	/* Show file header. */
	if (read (fd, &fh, sizeof(fh)) != sizeof(fh))
	    goto readerr;
	else if (fh.a_magic != ZMAGIC) {
	    fprintf (stderr, "not a page-aligned executable format file\n");
	    /* exit (2); */
	}

	printf ("File header:\n");
	printf ("%16s: %10d %10x  %012o\n",
	    "a_magic", fh.a_magic, fh.a_magic, fh.a_magic);
	printf ("%16s: %10d %10x  %012o\n",
	    "a_text", fh.a_text, fh.a_text, fh.a_text);
	printf ("%16s: %10d %10x  %012o\n",
	    "a_data", fh.a_data, fh.a_data, fh.a_data);
	printf ("%16s: %10d %10x  %012o\n",
	    "a_bss", fh.a_bss, fh.a_bss, fh.a_bss);
	printf ("%16s: %10d %10x  %012o\n",
	    "a_syms", fh.a_syms, fh.a_syms, fh.a_syms);
	printf ("%16s: %10d %10x  %012o\n",
	    "a_entry", fh.a_entry, fh.a_entry, fh.a_entry);
	printf ("%16s: %10d %10x  %012o\n",
	    "a_trsize", fh.a_trsize, fh.a_trsize, fh.a_trsize);
	printf ("%16s: %10d %10x  %012o\n",
	    "a_drsize", fh.a_drsize, fh.a_drsize, fh.a_drsize);

	fflush (stdout);
	close (fd);
	exit (0);

readerr:
	fprintf (stderr, "file read error\n");
	exit (4);
}
