/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

/*
 * MEDIT -- Replace all occurrences of the given string in a file by a
 * different string of the same length.  Works for binary files as well
 * as text files.  This is a crude but effective way of editing the symbol
 * table of object files, without having to know the data structures.
 * (String *data* will be edited too).
 */

/* Solaris used to need this but it doesn't any longer.
#define bcopy(a,b,n) memmove(b,a,n) */

#define	BUFLEN	16384
static	char	buf[BUFLEN];
static	int	nbytes;

main (argc, argv)
int	argc;
char	*argv[];
{
	register char *ip, *itop, ch;
	char	*fname, *oldname, *newname;
	int	fd, nch, total, nmatch, i;

	if (argc < 4) {
	    fprintf (stderr, "Usage: medit file oldname newname\n");
	    exit (1);
	}

	fname = argv[1];
	if ((fd = open (fname, 2)) == -1) {
	    fprintf (stderr, "Cannot open %s\n", fname);
	    exit (2);
	}

	if ((nbytes = read (fd, buf, BUFLEN)) <= 0) {
	    fprintf (stderr, "Cannot read %s\n", fname);
	    exit (2);
	}

	for (i=2, total=0;  i+1 < argc;  i=i+2, total+=nmatch) {
	    oldname = argv[i];
	    newname = argv[i+1];

	    if (strlen(oldname) != strlen(newname)) {
		fprintf (stderr,
		    "Replacement string must be same length as the original\n");
		exit (3);
	    }

	    ch = oldname[0];
	    nch = strlen (oldname);
	    itop = buf + nbytes - nch;

	    for (ip=buf, nmatch=0;  ip < itop;  ip++)
		if (*ip == ch && (strncmp (ip, oldname, nch) == 0)) {
		    bcopy (newname, ip, nch);
		    ip = ip + nch - 1;
		    nmatch++;
		}

	    printf ("%s, %s -> %s: %d entries edited\n",
		fname, oldname, newname, nmatch);
	}

	if (total) {
	    lseek (fd, 0L, 0);
	    write (fd, buf, nbytes);
	}

	close (fd);
	exit (0);
}
