/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#define import_spp
#define import_knames
#include <iraf.h>

#include "../bootProto.h"


#define	MAXEXTN		128
#define	ALL		0	/* delete all files			*/
#define	ALLBUT		1	/* delete all but listed files		*/
#define	ONLY		2	/* delete only listed files		*/

int	verbose;		/* print names of deleted files		*/
int	execute;		/* permission to delete files		*/
int	debug;			/* print debugging info			*/

extern	char *vfn2osfn(char *, int);


extern int  ZZSTRT (void);
extern int  ZZSTOP (void);

static void rmfiles (char *prog, int oneliner);
static void stripdir (char *dir, char *path, char *extnlist[], int mode);
static int  got_one (char *fname, char *extnlist[]);


/*
 * RMFILES -- Delete all files with the listed extensions in the listed
 * directory trees.
 *
 *	rmfiles [-dnv] [-f progfile] dir action extns
 *
 *		-d		debug
 *		-n		no execute; do not delete files
 *		-v		print names of files as they are deleted
 *		-f progfile	name of file containing program script
 *		dir		root directory of tree to be pruned
 *		action		one of "-all", "-allbut", "-only"
 *		extns		extensions of files to be deleted
 *
 * There is no default action as a safety measure.  If -all is specifed,
 * the extension list is ignored.
 */
int main (int argc, char *argv[])
{
	char	prog[SZ_LINE+1];
	char	*argp, *ip, *op;
	int	oneliner, argno;

	ZZSTRT();

	if (argc < 2)
	    goto help_;

	verbose = 0;
	execute = 1;
	debug   = 0;

	for (argno=1;  (argp = argv[argno]) != NULL;  argno++)
	    if (*argp == '-') {
		for (argp++;  *argp;  argp++)
		    switch (*argp) {
		    case 'd':
			debug++;
			break;
		    case 'n':			/* no execute		*/
			execute = 0;
			/* fall through */
		    case 'v':			/* set verbose mode	*/
			verbose = 1;
			break;

		    case 'f':
			argno++;
			if (argv[argno] == NULL) {
			    fprintf (stderr, "illegal `-f progfile' switch\n");
			    exit (OSOK+1);
			}
			rmfiles (argv[argno], oneliner=NO);
			break;

		    default:
			goto help_;
		    }

	    } else {
		/* Program is on command line.  The rest of the command
		 * line is assumed to be the program.
		 */
		for (op=prog;  (ip = argv[argno]) != NULL;  argno++) {
		    while ((*op = *ip++))
			op++;
		    *op++ = ' ';
		}
		*op = EOS;
		
		rmfiles (prog, oneliner=YES);
		break;
	    }


	ZZSTOP();
	exit (OSOK);
help_:
 	fprintf (stderr, "rmfiles [-dnv] [-p prog] [progfile]\n");
	ZZSTOP();
	exit (OSOK+1);

	return (0);
}


/* RMFILES -- Strip (delete) the indicated files in the indicated
 * directories.  We are driven either by a program in the named text file,
 * or in the prog string itself.
 */
static void
rmfiles (
    char  *prog, 		/* program, or program file name	*/
    int	   oneliner 		/* if !oneliner, open program file	*/
)
{
	char	dir[SZ_PATHNAME+1], path[SZ_PATHNAME+1];
	char	*extnlist[MAXEXTN], *ip, *op;
	char	lbuf[SZ_LINE+1];
	int	nextn, mode;
	FILE	*fp = NULL;

	if (debug) {
	    fprintf (stderr, "rmfiles @(%s), exe=%d, ver=%d\n", prog, execute,
		verbose);
	    fflush (stderr);
	}

	/* Is program in a file, or in the "prog" string?
	 */
	if (oneliner)
	    strcpy (lbuf, prog);
	else {
	    /* Open the program file.
	     */
	    if ((fp = fopen (vfn2osfn(prog,0), "r")) == NULL) {
		fprintf (stderr, "cannot open progfile `%s'\n", prog);
		fflush (stderr);
		return;
	    }
	}

	while (oneliner || fgets (lbuf, SZ_LINE, fp) != NULL) {
	    /* Skip comment lines and blank lines, and any whitespace at
	     * the beginning of program lines.
	     */
	    for (ip=lbuf;  isspace(*ip);  ip++)
		;
	    if (*ip == EOS || *ip == '#') {
		if (oneliner)
		    break;
		else
		    continue;
	    }

	    /* Check for a single filename entry of the form `-file filename',
	     * deleting the named file if this type of entry is encountered.
	     */
	    if (strncmp (ip, "-file", 5) == 0) {
		for (ip=ip+5;  isspace(*ip);  ip++)
		    ;
		for (op=path;  (*op = *ip);  ip++, op++)
		    if (isspace(*op))
			break;
		*op = EOS;
		if (*path == EOS)
		    continue;

		if (verbose) {
		    printf ("%s\n", path);
		    fflush (stdout);
		}

		if (execute) {
		    if (os_delete (path) == ERR) {
			fprintf (stderr, "cannot delete `%s'\n", path);
			fflush (stderr);
		    }
		}

		continue;
	    }
	    
	    /* Parse the program line into the directory pathname, mode,
	     * and extension list.  The program entry must be all on one
	     * line.
	     */
	    for (op=dir;  (*op = *ip);  ip++, op++)
		if (isspace(*op))
		    break;
	    *op = EOS;

	    while (isspace(*ip))
		ip++;
	    if (strncmp (ip, "-allbut", 7) == 0) {
		mode = ALLBUT;
		ip += 7;
	    } else if (strncmp (ip, "-all", 4) == 0) {
		mode = ALL;
		ip += 4;
	    } else if (strncmp (ip, "-only", 5) == 0) {
		mode = ONLY;
		ip += 5;
	    } else {
		fprintf (stderr, "error: no action specified: %s\n", lbuf);
		fflush (stderr);
		if (oneliner)
		    return;
		else
		    continue;
	    }

	    /* Construct a list of pointers to the extension strings.
	     */
	    for (nextn=0;  nextn < MAXEXTN;  nextn++) {
		while (isspace(*ip))
		    ip++;
		if (*ip == EOS || *ip == '#')
		    break;

		extnlist[nextn] = ip;
		while (*ip && !isspace(*ip))
		    ip++;
		*ip++ = EOS;
	    }

	    extnlist[nextn] = NULL;

	    if (debug) {
		fprintf (stderr, "rootdir=%s, mode=%d, extns:", dir, mode);
		for (nextn=0;  extnlist[nextn];  nextn++)
		    fprintf (stderr, " %s", extnlist[nextn]);
		fprintf (stderr, "\n");
		fflush (stderr);
	    }

	    /* Strip the named directory tree.
	     */
	    path[0] = EOS;
	    stripdir (dir, path, extnlist, mode);

	    if (oneliner)
		break;
	}

	if (!oneliner)
	    fclose (fp);
}


/* STRIPDIR -- Starting with the named directory, scan that directory and
 * all subdirectories, deleting (or listing) the files therein depending
 * on the mode, which can be ALL, ALLBUT, or ONLY.  We chdir to each directory
 * to minimize path searches.
 */
static void
stripdir (
    char  *dir, 		/* start with this directory		*/
    char  *path, 		/* pathname of current directory	*/
    char  *extnlist[], 		/* list of file extensions		*/
    int	   mode 		/* ALL, ALLBUT, ONLY			*/
)
{
	char	oldpath[SZ_PATHNAME+1];
	char	newpath[SZ_PATHNAME+1];
	char	fname[SZ_PATHNAME+1];
	int	deleteit, dp;

	if (debug) {
	    fprintf (stderr, "stripdir %s%s\n", path, dir);
	    fflush (stderr);
	}

	if ((dp = os_diropen (dir)) == ERR) {
	    fprintf (stderr, "cannot open subdirectory `%s'\n", dir);
	    fflush (stderr);
	    return;
	}

	os_fpathname ("", oldpath, SZ_PATHNAME);
	sprintf (newpath, "%s%s/", path, dir);

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
	while (os_gfdir (dp, fname, SZ_PATHNAME) > 0) {
	    if (os_filetype (fname) == DIRECTORY_FILE) {
		stripdir (fname, newpath, extnlist, mode);
		continue;
	    } else if (mode == ALL) {
		deleteit = YES;
	    } else {
		deleteit = got_one (fname, extnlist);
		if (mode == ALLBUT)
		    deleteit = !deleteit;
	    }

	    if (!deleteit)
		continue;
		
	    if (verbose) {
		printf ("%s%s\n", newpath, fname);
		fflush (stdout);
	    }

	    if (execute) {
		if (os_delete (fname) == ERR) {
		    fprintf (stderr, "cannot delete `%s'\n", fname);
		    fflush (stderr);
		}
	    }
	}

	/* Return from the subdirectory.
	 */
	if (strcmp (dir, ".") != 0)
	    if (os_chdir (oldpath) == ERR) {
		fprintf (stderr, "cannot return from subdirectory `%s'\n",
		    newpath);
		fflush (stderr);
	    }

	os_dirclose (dp);
}


/* GOT_ONE -- Check the file extension, if there is one, to see if the
 * file is on the list of extensions.
 */
static int
got_one (
    char  *fname,		/* file to be examined		*/
    char  *extnlist[]		/* list of extensions		*/
)
{
	register char	*ip, *ep;
	register int	ch, i;
	char	*extn;

	extn = NULL;
	for (ip=fname;  (ch = *ip);  ip++)
	    if (ch == '.')
		extn = ip;

	/* If the file has no extension it is not on the list.
	 */
	if (extn == NULL)
	    return (NO);

	/* Check the list of extensions.
	 */
	ch = *(extn + 1);
	if (extnlist[0] != NULL)
	    for (i=0;  (ep = extnlist[i]);  i++)
		if (*(ep+1) == ch)
		    if (strcmp (ep, extn) == 0)
			return (YES);

	return (NO);
}
