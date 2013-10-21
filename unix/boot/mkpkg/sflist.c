/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define	import_spp
#define	import_error
#include <iraf.h>

#include "mkpkg.h"
#include "extern.h"
#include "../bootProto.h"


/*
 * SFLIST.C -- Special file list package.  The special file list is a list of
 * library module list source files which need special processing on a given
 * host system.  Examples of such files are files which have been optimized in
 * a machine dependent way, e.g., in assembler or C, or files which must be
 * compiled in a nonstandard way due to host compiler bugs.  The special file
 * list makes this special processing possible without having to modify the
 * mkpkg files in the portable system in a host dependent way, concentrating
 * all knowledge of those parts of the system which have been tailored for the
 * local host into a single, easily modifiable table file stored in HLIB.
 *
 * External functions:
 *
 *	      sf_scanlist (cx)			# parse $special file list
 *  sflist = sf_dirsearch (dirname)		# lookup directory in sflist
 *    sfp = sf_filesearch (sflist, filename)	# lookup file in dir file list
 *	         sf_prune (cp)			# free space in string buffer
 *
 * where
 *
 *	struct	context	*cx;
 *	struct	sfile	*sflist, *sfp;
 *	char		*filename, *dirname;
 *
 * The special file list is organized by source directory to speed searches
 * (most directories will not contain any files needing special processing,
 * eliminating the need to lookup the files in module lists in that directory)
 * and to reduce storage requirements for the list.  The special file database
 * thus consists of a list of directories containing special files, and for
 * each directory, a pointer to a linked list of special file entries, one
 * for each special file in the directory.  Since the organization by directory
 * tends to produce a database consisting of very short file lists, we use a
 * linked list rather than a hash table for the file lists.
 *
 * For each special file we record the standard file name, the pathname of
 * the special file to be used, and a command to be pushed back into the MKPKG
 * command input stream to generate the object file for the module.
 * The special file name may be the same as the standard file name, e.g, if
 * the standard file only needs to be compiled in a nonstandard way.  If the
 * mkobj string is null the special file name will simply be returned in the
 * module list, and compiled with XC using the default compile flags.
 */

static	int	sf_ndirs = 0;			/* no. of directories	*/
static	int	sf_nfiles = 0;			/* no. of special files	*/
static	char	*sf_dirs[MAX_SFDIRS];		/* source directories	*/
static	struct	sfile *sf_flist[MAX_SFDIRS];	/* directory file lists */
static	struct	sfile sf_files[MAX_SFFILES];	/* special file list	*/
static	char	nullstr[] = "";


/* SF_SCANLIST -- Called when the $special macro preprocessor directive is
 * encountered to parse a special file list, entering each file listed into
 * the special file list database.  The syntax of a $special special file
 * list directive is as follows:
 *
 *	$special dirname:
 *		stname1	sfname1	mkobj_command1
 *		stname2	sfname2	mkobj_command2
 *				...
 *		stnameN	sfnameN	mkobj_commandN
 *		;
 *
 * where any string value may optionally be quoted, and the mkobj command
 * strings are optional.  The token "&" in <sfname> or <mkobj_command> is
 * replaced by <stname>.
 */
int
sf_scanlist (
  struct context *cx		/* current mkpkg context */
)
{
	register struct	sfile *sfp;
	register char	*ip, *op, *tp;

	char	dirname[SZ_PATHNAME+1];
	char	stname[SZ_PATHNAME+1];
	char	sfname[SZ_PATHNAME+1];
	char	mkobj[SZ_CMD+SZ_PATHNAME+1];
	char	token[SZ_CMD+1];
	struct	sfile *head, *tail;
	int	tok, nfiles, eol=0;
	char	*old_cp;

	old_cp = cp;			/* mark position in sbuf */
	nfiles = 0;

	/* Get the directory name. */
	if (gettok (cx, token, SZ_LINE) != TOK_FNAME) {
	    warns ("missing directory name in special file list", "");
	    goto err;
	} else
	    os_fpathname (token, dirname, SZ_PATHNAME);

	if (debug) {
	    printf ("scan special file list for directory %s\n",
		debug > 1 ? dirname : token);
	    fflush (stdout);
	}

	/* Advance to the start of the module list. */
	while ((tok = gettok (cx, token, SZ_LINE)) != TOK_BEGIN)
	    if (tok == EOF || tok == TOK_END)
		goto err;

	/* Get a pointer to the last element in the special file list for
	 * the named directory.  If this is the first entry for the named
	 * directory, enter the name in the symbol table and set the sflist
	 * pointer to NULL.
	 */
	if ((head = sf_dirsearch (dirname)) == NULL) {
	    sf_dirs[sf_ndirs++] = putstr (dirname);
	    if (sf_ndirs >= MAX_SFDIRS)
		fatals ("too many special file list directories: %s", dirname);
	    tail = NULL;
	} else {
	    for (tail=sfp=head;  sfp;  sfp=sfp->sf_next)
		tail = sfp;
	}

	/* Read successive entries from the special file list for the named
	 * directory, entering each file at the tail of the list.
	 */
	while (!eol && (tok = gettok (cx, token, SZ_LINE)) != TOK_END) {
	    if (tok == EOF || tok == TOK_END)
		break;

	    /* Get standard file name (module name). */
	    if (tok == TOK_NEWLINE)
		continue;			/* blank line */
	    else if (tok != TOK_FNAME)
		goto badline;
	    else
		strcpy (stname, token);

	    /* Get the special file name. */
	    if ((tok = gettok (cx, sfname, SZ_PATHNAME)) == TOK_END)
		eol++;
	    if (tok != TOK_FNAME)
		goto badline;

	    /* Get the mkobj command string, if any. */
	    if ((tok = gettok (cx, token, SZ_LINE)) == TOK_NEWLINE) {
		mkobj[0] = EOS;
	    } else if (tok == TOK_END) {
		mkobj[0] = EOS;
		eol++;
	    } else if (tok != TOK_FNAME) {
		goto badline;
	    } else {
		/* Extract the command string, expanding any "&" filename
		 * references therein.
		 */
		for (ip=token, op=mkobj;  (*op = *ip++);  op++)
		    if (*op == '&') {
			for (tp=stname;  (*op = *tp++);  op++)
			    ;
			--op;
		    }
	    }

	    if (debug)
		printf ("file %s -> %s, mkobj = `%s'\n",
		    stname, (sfname[0] == '&') ? stname : sfname, mkobj);

	    /* Add the file to the tail of the file list. */
	    nfiles++;
	    sfp = &sf_files[sf_nfiles++];
	    if (sf_nfiles >= MAX_SFFILES)
		fatals ("too many special files: %s", stname);
	    
	    sfp->sf_stname = putstr (stname);
	    sfp->sf_sfname = (sfname[0]=='&') ? sfp->sf_stname : putstr(sfname);
	    sfp->sf_mkobj  = mkobj[0] ? putstr(mkobj) : nullstr;
	    sfp->sf_next   = NULL;

	    if (tail) {
		tail->sf_next = sfp;
		tail = sfp;
	    } else
		sf_flist[sf_ndirs-1] = head = tail = sfp;

	    continue;
badline:
	    /* Print message and discard rest of line, but do not quit. */
	    warns ("bad token `%s' in special file list", token);
	    while (!eol && (tok = gettok (cx, token, SZ_LINE)) != TOK_NEWLINE)
		if (tok == TOK_END)
		    break;
		else if (tok == EOF)
		    goto err;
	}

	if (debug) {
	    printf ("%d special files added; total ndirs=%d, nfiles=%d\n",
		nfiles, sf_ndirs, sf_nfiles);
	    fflush (stdout);
	}

	if (nfiles == 0) {
	    warns ("empty special file list for %s", dirname);
	    sf_prune (cp = old_cp);
	    return (ERR);
	} else
	    return (OK);

err:
	/* Discard rest of directive. */
	while (!eol && (tok = gettok (cx, token, SZ_LINE)) != TOK_END)
	    if (tok == EOF || tok == TOK_END)
		break;

	/* Return memory and sfile database space. */
	sf_prune ((cp = old_cp));

	return (ERR);
}


/* SF_DIRSEARCH -- Search the special file database for the named directory,
 * returning a pointer to the special file list for that directory if the
 * directory is found, else NULL.  Note that directory names are stored as
 * host system pathnames (so that any equivalent form of reference may be used
 * in the mkpkg files), and we assume that we are called with the directory
 * pathname already resolved.
 */
struct sfile *
sf_dirsearch (
  char	*dirname		/* host pathname of directory */
)
{
	register int	i;

	if (debug) {
	    printf ("search sflist for directory %s\n", dirname);
	    fflush (stdout);
	}

	for (i=0;  i < sf_ndirs;  i++)
	    if (h_direq (sf_dirs[i], dirname))
		return (sf_flist[i]);

	return (NULL);
}


/* SF_FILESEARCH -- Search the special file list for a directory for the named
 * file.  File names are stored in the list by the name given in the library
 * module list in the mkpkg file.  If the named file is found a pointer to the
 * special file descriptor for that file is returned, otherwise NULL is
 * returned.  Note that "file*" is a prefix match, whereas "file" requires an
 * exact match.
 */
struct sfile *
sf_filesearch (
  struct sfile *sflist,		/* special file list	*/
  char	*stname 		/* standard file name	*/
)
{
	register struct sfile *sfp;
	register char	*p1, *p2;

	for (sfp=sflist;  sfp;  sfp=sfp->sf_next) {
	    for (p1=sfp->sf_stname, p2=stname;  *p1 && *p1 == *p2;  p1++, p2++)
		;
	    if ((*p1 == EOS && *p2 == EOS) || *p1 == '*')
		return (sfp);
	}
	
	return (NULL);
}


/* SF_PRUNE -- Prune the special file database back to the given point in the
 * string buffer.
 */
void
sf_prune (
  register char	*cp		/* lop off everything here and above */
)
{
	register struct sfile *sfp, *sf_top;
	register int	i;

	/* Prune the directory list. */
	for (i=0;  i < sf_ndirs;  i++)
	    if (sf_dirs[i] >= cp || sf_flist[i]->sf_stname >= cp) {
		sf_ndirs = i;
		break;
	    }

	/* Prune the global file list. */
	for (i=0;  i < sf_nfiles;  i++)
	    if (sf_files[i].sf_stname >= cp) {
		sf_nfiles = i;
		break;
	    }

	/* Prune the individual directory file lists. */
	for (i=0, sf_top = &sf_files[sf_nfiles];  i < sf_nfiles;  i++) {
	    sfp = &sf_files[i];
	    if (sfp->sf_next >= sf_top)
		sfp->sf_next = NULL;
	}
}
