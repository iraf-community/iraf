/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/*
 * INODE -- Print the inode number and other information for a file or files.
 */
main (argc, argv)
int	argc;
char	*argv[];
{
	struct	stat fi;
	int	argno;
	char	*file;

	printf ("IDEVICE    INO   MODE NLK  UID    DEV    SIZE  FILE\n");
	for (argno=1;  argno < argc;  argno++)
	    if (stat (file=argv[argno], &fi) == -1)
		fprintf (stderr, "cannot stat %s\n", file);
	    else {
		printf ("%7d%7d%7o%4d%5d%8d%8d  %s\n",
		    fi.st_dev, fi.st_ino, fi.st_mode, fi.st_nlink,
		    fi.st_uid, fi.st_rdev, fi.st_size, file);
	    }
}
