/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <filehdr.h>
#include <aouthdr.h>
#include <scnhdr.h>

/* COFF -- Examine the header of a COFF file.
 */
main (argc, argv)
int	argc;
char	*argv[];
{
	struct	filehdr fh;
	struct	aouthdr sh;
	struct	scnhdr nh;
	int	fd;

	if (argc < 2) {
	    fprintf (stderr, "Usage: coff <filename>\n");
	    exit (0);
	}

	if ((fd = open (argv[1], 0)) == -1) {
	    fprintf (stderr, "cannot open %s\n", argv[1]);
	    exit (1);
	}

	/* Show file header. */
	if (read (fd, &fh, sizeof(fh)) != sizeof(fh))
	    goto readerr;
	else if (!ISCOFF(fh.f_magic)) {
	    fprintf (stderr, "not a COFF format file\n");
	    /* exit (2); */
	}

	printf ("File header:\n");
	printf ("%16s: %10d %10x  %012o\n",
	    "f_magic", fh.f_magic, fh.f_magic, fh.f_magic);
	printf ("%16s: %10d %10x  %012o\n",
	    "f_nscns", fh.f_nscns, fh.f_nscns, fh.f_nscns);
	printf ("%16s: %10d %10x  %012o\n",
	    "f_timdat", fh.f_timdat, fh.f_timdat, fh.f_timdat);
	printf ("%16s: %10d %10x  %012o\n",
	    "f_symptr", fh.f_symptr, fh.f_symptr, fh.f_symptr);
	printf ("%16s: %10d %10x  %012o\n",
	    "f_nsyms", fh.f_nsyms, fh.f_nsyms, fh.f_nsyms);
	printf ("%16s: %10d %10x  %012o\n",
	    "f_opthdr", fh.f_opthdr, fh.f_opthdr, fh.f_opthdr);
	printf ("%16s: %10d %10x  %012o\n",
	    "f_flags", fh.f_flags, fh.f_flags, fh.f_flags);

	/* Show system header. */
	if (read (fd, &sh, sizeof(sh)) != sizeof(sh))
	    goto readerr;
	else if (sh.magic != 0413) {
	    fprintf (stderr, "bad magic %o in system header\n", sh.magic);
	    exit (3);
	}

	printf ("System header:\n");
	printf ("%16s: %10d %10x  %012o\n",
	    "magic", sh.magic, sh.magic, sh.magic);
	printf ("%16s: %10d %10x  %012o\n",
	    "vstamp", sh.vstamp, sh.vstamp, sh.vstamp);
	printf ("%16s: %10d %10x  %012o\n",
	    "tsize", sh.tsize, sh.tsize, sh.tsize);
	printf ("%16s: %10d %10x  %012o\n",
	    "dsize", sh.dsize, sh.dsize, sh.dsize);
	printf ("%16s: %10d %10x  %012o\n",
	    "bsize", sh.bsize, sh.bsize, sh.bsize);
	printf ("%16s: %10d %10x  %012o\n",
	    "entry", sh.entry, sh.entry, sh.entry);
	printf ("%16s: %10d %10x  %012o\n",
	    "text_start", sh.text_start, sh.text_start, sh.text_start);
	printf ("%16s: %10d %10x  %012o\n",
	    "data_start", sh.data_start, sh.data_start, sh.data_start);

	fflush (stdout);
	close (fd);
	exit (0);

readerr:
	fprintf (stderr, "file read error\n");
	exit (4);
}
