/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define import_spp
#define import_knames
#include <iraf.h>

#include "../bootlib/bootlib.h"


#define MAXEXTN		128

static const char *only[MAXEXTN];	/* delete files with these extensions */
static const char *excl[MAXEXTN];	/* exclude these files		*/
static int interactive;		/* verify if file does not have extn	*/
static int recurse;		/* recursively descend directories	*/
static int verbose;		/* print names of deleted files		*/
static int execute;		/* permission to delete files		*/

static void rmbin ( const char *, int, const char * );
static int exclude_file ( const char * );
static int verify_delete ( const char *, const char * );

/*
 * RMBIN -- Delete all binary files in a directory tree or trees.
 *
 *	rmbin [-dinrv] [-o extns] [-e extns] dir1 dir2 ... dirN
 *
 *		-d	disable recursive descent
 *		-e	exclude files with the following extensions
 *		-i	verify before deleting files without extensions
 *		-n	no execute; do not delete files
 *		-o	delete only files with the following extensions
 *		-r	enable recursive descent
 *		-v	print names of files as they are deleted
 *
 * Note that flags may be inserted between directory name arguments to change
 * switches for different directories.
 *
 */
int main ( int argc, char *argv[] )
{
	char path[SZ_PATHNAME+1];
	const char *argp;
	int argno, i;

	ZZSTRT();

	if (argc < 2)
	    goto help_;

	only[0] = NULL;
	excl[0] = NULL;
	path[0] = EOS;

	interactive = 0;
	recurse = 0;
	verbose = 0;
	execute = 1;

	for (argno=1;  (argp = argv[argno]) != NULL;  argno++)
	    if (*argp == '-') {
		for (argp++;  *argp;  argp++)
		    switch (*argp) {
		    case 'd':			/* disable recursion	*/
			recurse = 0;
			break;
		    case 'i':			/* verify deletions	*/
			interactive = 1;
			break;
		    case 'r':			/* enable recursion	*/
			recurse = 1;
			break;
		    case 'n':			/* no execute		*/
			execute = 0;
			/* fall through */
		    case 'v':			/* set verbose mode	*/
			verbose = 1;
			break;

		    case 'e':			/* exclude listed files	*/
			i = 0;
			argno++;
			while (argv[argno] != NULL && *(argv[argno]) == '.' &&
			       *(argv[argno]+1) != EOS) {
			    if ( i < MAXEXTN -1 ) excl[i++] = argv[argno];
			    argno++;
			}
			excl[i] = NULL;
			--argno;
			break;

		    case 'o':			/* only the listed files */
			i = 0;
			argno++;
			while (argv[argno] != NULL && *(argv[argno]) == '.' &&
			       *(argv[argno]+1) != EOS) {
			    if ( i < MAXEXTN -1 ) only[i++] = argv[argno];
			    argno++;
			}
			only[i] = NULL;
			--argno;
			break;

		    default:
			goto help_;
		    }
	    } else
		rmbin (argp, recurse, path);

	ZZSTOP();
	exit (OSOK);
help_:
	fprintf (stderr, "rmbin [-dinrv] [-o extns] [-e extns] dir dir ...\n");
	ZZSTOP();
	exit (OSOK+1);
}


/* RMBIN -- Remove all binaries in a directory or in a directory tree.
 * We chdir to each directory to minimize path searches.
 */
/* path : pathname of current directory */
static void rmbin ( const char *dir, int recurse, const char *path)
{
	char	newpath[SZ_PATHNAME+1];
	char	fname[SZ_PATHNAME+1];
	int	dp, ftype;

	if ((dp = os_diropen (dir)) == ERR) {
	    fprintf (stderr, "cannot open directory `%s'\n", dir);
	    fflush (stderr);
	    return;
	}

	snprintf (newpath, SZ_PATHNAME+1, "%s%s/", path, dir);

	/* Descend into the subdirectory.
	 */
	if (strcmp (dir, ".") != 0)
	    if (os_chdir (dir) == ERR) {
		os_dirclose (dp);
		fprintf (stderr, "cannot change directory to `%s'\n", newpath);
		fflush (stderr);
		return;
	    }

	/* Scan through the directory.
	 */
	while (os_gfdir (dp, fname, SZ_PATHNAME+1) > 0) {
	    if (os_symlink (fname, 0, 0))
		continue;

	    if ((ftype = os_filetype (fname)) == DIRECTORY_FILE)
		rmbin (fname, recurse, newpath);
	    else {
		if (only[0] != NULL) {
		    if (exclude_file (fname))
			continue;
		} else if (ftype != BINARY_FILE || exclude_file (fname))
		    continue;

		/* We have a binary file which is not excluded from deletion
		 * by its extension, so delete it.
		 */
		if (interactive && (verify_delete (fname, newpath) == NO))
		    continue;

		if (verbose) {
		    printf ("%s%s\n", newpath, fname);
		    fflush (stdout);
		}

		if (execute)
		    if (os_delete (fname) == ERR) {
			fprintf (stderr, "cannot delete `%s'\n", fname);
			fflush (stderr);
		    }
	    }
	}

	/* Return from the subdirectory.
	 */
	if (strcmp (dir, ".") != 0)
	    if (os_chdir ("..") == ERR) {
		fprintf (stderr, "cannot return from subdirectory `%s'\n",
		    newpath);
		fflush (stderr);
	    }

	os_dirclose (dp);
}


/* EXCLUDE_FILE -- Check the "only" and "exclude" file lists to see if the
 * file should be excluded from deletion.
 */
static int exclude_file ( const char *fname )
{
	const char *ip, *ep, *extn;
	int ch, i;

	extn = NULL;
	for (ip=fname;  (ch = *ip);  ip++)
	    if (ch == '.')
		extn = ip;

	/* If the file has no extension all we have to do is check if there is
	 * an "only" file list.
	 */
	if (extn == NULL)
	    return (only[0] != NULL ? YES : NO);

	/* Check the only and exclude file lists.
	 */
	ch = *(extn + 1);
	if (only[0] != NULL) {
	    for (i=0;  (ep = only[i]);  i++)
		if (*(ep+1) == ch)
		    if (strcmp (ep, extn) == 0)
			return (NO);
	    return (YES);
	} else if (excl[0] != NULL) {
	    for (i=0;  (ep = excl[i]);  i++)
		if (*(ep+1) == ch)
		    if (strcmp (ep, extn) == 0)
			return (YES);
	    return (NO);
	} else
	    return (NO);
}


/* VERIFY_DELETE -- Ask the user if they want to delete the named file.
 */
/* fname : name of file to be deleted	*/
/* path  : current directory pathname	*/
static int verify_delete ( const char *fname, const char *path )
{
	char lbuf[SZ_LINE+1];
	const char *ip;

	fprintf (stderr, "delete file %s%s? ", path, fname);
	fflush  (stderr);
	fgets   (lbuf, SZ_LINE+1, stdin);

	for (ip=lbuf;  *ip == ' ' || *ip == '\t';  ip++)
	    ;
	if (*ip == 'y' || *ip == 'Y')
	    return (YES);
	else
	    return (NO);
}
