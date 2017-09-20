/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#define	import_spp
#define	import_error
#include <iraf.h>

#include "mkpkg.h"
#include "extern.h"
#include "../bootProto.h"


/* DO_MKPKG -- Open the mkpkg file and scan it for the named program.  A program
 * may be either a sequence of preprocessor directives or the module list for
 * a library, as indicated by the ISLIB flag.  In the case of a library build
 * up a list of library modules needing updating, and replace these modules
 * in the library.
 */
int
do_mkpkg (
  struct context *cx,		/* current context	*/
  int	islib 			/* update a library?	*/
)
{
	if (cx->mkpkgfile[0] == EOS)
	    strcpy (cx->mkpkgfile, MKPKGFILE);

	if (debug) {
	    printf ("do_mkpkg (file=%s, library=%s, islib=%d)\n",
		cx->mkpkgfile, cx->library, islib);
	    fflush (stdout);
	}

	if (open_mkpkgfile (cx) == ERR) {
	    char    fname[SZ_PATHNAME+1];
	    struct  context *save_cx;

	    save_cx = topcx;
	    if (cx->prev)
		topcx = cx->prev;

	    sprintf (fname, "%s%s", cx->curdir, cx->mkpkgfile);
	    warns ("cannot open `%s'", fname);

	    topcx = save_cx;
	    return (ERR);
	}

	/* Search the mkpkg file for the module list for the named library,
	 * or the first module list encountered if no library is named.
	 * Any number of preprocessor directives may be executed while
	 * searching; in particular, $EXIT will terminate the search,
	 * causing ERR to be returned by the search procedure to indicate
	 * that no module list was found.
	 */
	if (search_mkpkgfile (cx) == ERR) {
	    if (cx->library[0] != EOS) {
		warns ("no entry in mkpkg file for `%s'", cx->library);
		return (ERR);
	    } else {
		/* Presumably we just executed a bunch of preprocessor
		 * commands and there is no library to update, or it was
		 * already updated by the commands just executed.
		 */
		return (OK);
	    }
	}

	/* The mkpkg file is open and positioned to the entry for a library
	 * (or any other sequence of commands with the given name).  Update
	 * the named library, close the mkpkgfile, and exit.
	 */
	exit_status = scan_modlist (cx, islib);
	close_mkpkgfile (cx);

	return (exit_status);
}


/* SCAN_MODLIST -- Called when positioned to the module list for a library.
 * Scan the module list and compare file and library module dates, building
 * up a list of files to be updated.  If any files were found which need
 * updating recompile them and replace them in the library.  Call the rebuild
 * procedure when done to perform any library rebuild or cleanup operations
 * necessary on the local system.
 */
int
scan_modlist (
  struct context *cx,		/* current mkpkg context	*/
  int	islib
)
{
	char	token[SZ_FNAME+1];
	char	*dflist[MAX_DEPFILES+1];
	struct	sfile *sflist;
	int	root_modlist;
	int	tok;

	/* This is for the case "@(module)" in a library member list, indicating
	 * that the named module is a library member list for the current
	 * library, even though the module name is not the same as the library
	 * name.  For searching purposes the cl->library field contains the
	 * module name until we get here, and now we must overwrite this with
	 * the name of the library being updated.
	 */
	if (islib && cx->sublib)
	    strcpy (cx->library, cx->prev->library);

	if (debug) {
	    printf ("scan_modlist (file=%s, line=%d, library=%s, islib=%d)\n",
		cx->mkpkgfile, cx->lineno, cx->library, islib);
	    fflush (stdout);
	}

	/* Check if this directory contains any files needing special
	 * processing.
	 */
	sflist = sf_dirsearch (cx->dirpath);

	if (cx->prev)
	    root_modlist = (strcmp (cx->library, cx->prev->library) != 0);
	else
	    root_modlist = 1;

	if (islib && root_modlist) {
	    /* Save the pathname of the library in the context descriptor.
	     * We may be changing the current directory later, so a pathname
	     * is required.
	     */
	    os_fpathname (cx->library, cx->libpath, SZ_PATHNAME);
	    if (debug) {
		printf ("pathname of `%s' is `%s'\n", cx->library,
		    cx->libpath);
		fflush (stdout);
	    }

	    /* Scan the library and build up a list of modules and their dates.
	     * This will create a new library if necessary.  If there are any
	     * fatal warns the scan library routine prints its own error
	     * messages and we return, since no further processing of the
	     * library is possible.
	     */
	    if ((exit_status = h_scanlibrary (cx->library)) != OK) {
		warns ("error reading library file `%s'", cx->library);
		return (ERR);
	    }
	}

	/* Scan the module list in the mkpkg file.  An "@subdir" reference
	 * causes us to push a new context and continue scanning the entry
	 * for the same library in a subdirectory.  Any number of preprocessor
	 * directives may be executed while we are scanning the module list.
	 * For each module in the list, test the file dates and add the name
	 * to the file list if the module has to be updated.
	 */
	for (;;) {
next_:	    tok = gettok (cx, token, SZ_FNAME);

	    if (tok == TOK_NEWLINE) {
		; /* ignore blank lines */

	    } else if (islib && tok == TOK_FNAME && token[0] != SUBDIR_CHAR) {
		/* Check if the named module is up to date, and if not,
		 * add to the file list for the library.  The useobj flag
		 * is set if the module is not up to date, but the object
		 * file has already been compiled and should be replaced
		 * in the library.
		 */
		char    srcname[SZ_PATHNAME+1], modname[SZ_PATHNAME+1];
		char    dname[SZ_FNAME+1], fname[SZ_FNAME+1];
		struct	sfile *sfp;
		int	useobj;

		strcpy (modname, token);

		/* If this directory has any files needing special processing,
		 * determine if this is such a file, and if so obtain the name
		 * of the actual source file to be used.
		 */
		sfp = sf_filesearch (sflist, modname);
		strcpy (srcname, sfp ? sfp->sf_sfname : modname);
		if (sfp && debug) {
		    printf ("module %s on special file list: ", modname);
		    if (sfp->sf_mkobj[0])
			printf ("mkobj=`%s'\n", sfp->sf_mkobj);
		    else
			printf ("src=%s\n", srcname);
		    fflush (stdout);
		}

		/* Check that the regular, standard source file has not been
		 * modified more recently than the special file, if any.
		 */
		if (sfp && debug && os_fdate(modname) > os_fdate(srcname))
		    warns ("special file for %s is out of date", modname);

		/* Break filename into the logical directory and local
		 * filenames; if file is remote a local copy will be
		 * created temporarily (see below).  Get list of files
		 * upon which the module is dependent, if any.
		 */
		parse_fname (srcname, dname, fname);
		get_dependency_list (cx, modname, dflist, MAX_DEPFILES);

		if (!up_to_date (cx, srcname, fname, dflist, &useobj)) {

		    /* If file is remote add its name to the remote file list
		     * and "checkout" the file, making it accessible in the
		     * current directory.  The file will be checked back in
		     * after the library is updated.  It may not be necessary
		     * to compile the file locally, but it is too risky to
		     * predict what the host system will do when asked to
		     * compile a file resident in a remote directory.
		     */
		    if (dname[0]) {
			int     clobber, i;

			for (i=0;  i < cx->nrfiles;  i++)
			    if (strcmp (fname, cx->rflist[i]) == 0) {
				/* Multiple modules map to the same remote
				 * source file, which has already been checked
				 * out.  Skip duplicate references to the same
				 * source file.
				 */
				goto next_;
			    }
			cx->rflist[cx->nrfiles++] = putstr (fname);
			h_outcheck (fname, dname, clobber=NO);
		    }

		    /* If the module needs special processing and a mkobj
		     * command string was given, but the source file has not
		     * yet been compiled, push the command back into the input
		     * stream to compile the source, and set the useobj flag
		     * to defeat recompilation of this module.
		     */
		    if (sfp && sfp->sf_mkobj[0]) {
			if (useobj) {
			    warns ("module %s has already been compiled",
				modname);
			} else {
			    m_pushstr (cx, "\n");
			    m_pushstr (cx, sfp->sf_mkobj);
			    useobj++;
			}
		    }

		    /* Add the local filename to the list of files to be
		     * updated.
		     */
		    cx->flist[cx->nfiles++] =
			putstr (useobj ? makeobj(fname) : fname);

		    if (debug) {
			printf ("add %s to file list for %s\n",
			    cx->flist[cx->nfiles-1], cx->library);
			fflush (stdout);
		    }

		    if (cx->nfiles > MAX_FILES)
			fatals ("too many modules listed for library `%s'",
			    cx->library);
		}

	    } else if (tok == TOK_FNAME && token[0] == SUBDIR_CHAR) {
		/* Push a new context, open mkpkg file and continue scanning
		 * in the new subdirectory.
		 */
		struct	context *ncx;
		char	module[SZ_FNAME+1];
		char	subdir[SZ_FNAME+1];
		char	fname[SZ_FNAME+1];

		/* Parse the "module@subdir/fname" string. */
		parse_modname (token, module, subdir, fname);

		/* Push a new context and start over; recursive call.  May
		 * "reopen" (soft) the current mkpkg file or the mkpkg in a
		 * subdirectory.
		 */
		if ((ncx = push_context (cx, module, subdir, fname)) == NULL)
		    exit_status = ERR;
		else {
		    exit_status = do_mkpkg (ncx, islib);
		    cx = pop_context (ncx);
		}

		if (exit_status != OK && !ignore)
		    return (exit_status);

	    } else if (tok == TOK_END || tok == 0) {
		/* We have reached the end of the current module list (;),
		 * executed a $EXIT, or seen EOF on the mkpkg file.  If the
		 * file list is nonempty update the current library, restore
		 * the previous context, and return (from the do_mkpkg, above).
		 */

		/* The file list now contains the names of all the files that
		 * need to be updated.  Compile and update the archive.
		 */
		if (islib && cx->nfiles == 0) {
		    /* No modules were found that need updating.
		     */
		    if (cx->prev != NULL && cx->level > cx->prev->level) {
			char	dirname[SZ_FNAME+1];
			char	*ip, *op;

			/* Prettify the directory name.
			 */
			for (ip=cx->curdir, op=dirname;  (*op = *ip++);  op++)
			    ;
			if (*(op-1) == '/')
			    *(op-1) = EOS;

			printf ("Subdirectory %s is up to date\n", dirname);
			fflush (stdout);
		    }
		} else if (islib) {
		    char    dname[SZ_FNAME+1], fname[SZ_FNAME+1];
		    int     i;

		    /* Compile the modules and update the library.
		     */
		    exit_status = h_updatelibrary (cx->libpath,
			cx->flist, cx->nfiles, getsym(XFLAGS), irafdir);
		    if (exit_status == INTERRUPT)
			fatals ("<ctrl/c> interrupt %s", cx->library);
		    cx->totfiles += cx->nfiles;

		    /* Delete any local copies of (or links to) files that were
		     * checked out of a remote directory.
		     */
		    for (i=0;  i < cx->nrfiles;  i++) {
			parse_fname (cx->rflist[i], dname, fname);
			h_incheck (fname, NULL);
		    }
		}

		/* If the module list just terminated was a partial list,
		 * return immediately to continue processing the next higher
		 * level module list for the same library.
		 */
		if (root_modlist && islib)
		    break;
		else {
		    if (debug) {
			printf ("not root library; return to higher level\n");
			fflush (stdout);
		    }
		    return (exit_status);
		}

	    } else if (islib)
		warns ("bad token `%s' in library module list", token);
	}

	/* We get here when the end of the root module list for a library has
	 * been reached (but only if the module being processed is a library
	 * list).
	 */
	if (cx->totfiles == 0 && !forceupdate) {
	    printf ("Library %s is up to date\n", cx->library);
	    fflush (stdout);
	} else if (exit_status == OK || ignore) {
	    /* Run the system dependent library rebuild operator.
	     */
	    if ((exit_status = h_rebuildlibrary (cx->library)) == INTERRUPT)
		fatals ("<ctrl/c> interrupt %s", cx->library);
	    printf ("Updated %d files in %s\n", cx->totfiles, cx->library);
	    fflush (stdout);
	}

	return (exit_status);
}


/* PARSE_MODNAME -- Parse a module reference into its component parts.
 * 
 * Syntax:	module@subdir/fname
 *	or	@(module)subdir/fname
 */
void
parse_modname (
  char	*modname,		/* "module@subdir/fname"	*/
  char	*module,		/* receives module		*/
  char	*subdir,		/* receives subdir		*/
  char	*fname 			/* receives fname		*/
)
{
	register char	*ip, *op;
	register int	ch;
	char	*path;

	for (ip=modname;  isspace (*ip);  ip++)
	    ;

	/* Module name XXX@ */
	op = module;
	for (;  (*op = *ip) && *op != '@';  op++, ip++)
	    ;
	*op = EOS;

	/* Module name @(XXX) */
	if (op == module && *ip == '@' && *(ip+1) == '(') {
	    for (ip++;  (*op = *ip) && *op != ')';  op++, ip++)
		;
	    *(op+1) = EOS;
	    if (*ip == ')')
		ip++;
	}

	if (*ip == '@')
	    ip++;

	/* Get subdirectory and mkpkg file names.  If a simple identifier is
	 * given it is taken to be the name of the subdirectory, otherwise
	 * ($ or / found) the given pathname is parsed.
	 */
	fname[0] = EOS;
	for (op=subdir, path=ip;  (ch = *op = *ip++);  op++)
	    if (ch == '$' || ch == '/') {
		if (*(op-1) == '\\')
		    *--op = ch;
		else {
		    parse_fname (path, subdir, fname);
		    break;
		}
	    }
}


/* PARSE_FNAME -- Return logical directory and filename fields of a filename.
 */
void
parse_fname (
  char	*path,			/* input filename		*/
  char	*dname,			/* receives directory name	*/
  char	*fname 			/* receives file name		*/
)
{
	register char	*ip, *op;
	register char	*delim;

	delim = NULL;
	for (ip=path, op=fname;  (*op = *ip);  op++, ip++)
	    if (*ip == '$' || *ip == '/') {
		if (*(ip-1) == '\\')
		    *(--op) = *ip;
		else
		    delim = ip;
	    }

	if (delim == NULL) {
	    dname[0] = EOS;
	    return;		/* no directory name */
	}

	for (ip=path, op=dname;  ip <= delim;  )
	    *op++ = *ip++;
	*op = EOS;

	for (op=fname;  (*op++ = *ip++);  )
	    ;
}


/* PUSH_CONTEXT -- Push a new context, i.e., save the current context in the
 * current context descriptor, allocate and initialize a new context
 * descriptor.  Set up the new context, including the current directory,
 * but do not open the new mkpkgfile.
 */
struct context *
push_context (
  register struct context *cx,		/* current context	*/
  char	*module,			/* new module (library)	*/
  char	*newdir,			/* new directory	*/
  char	*fname 				/* mkpkgfile name	*/
)
{
	register struct context *ncx;

	if (debug) {
	    printf ("push_context (module=%s, newdir=%s, fname=%s)\n",
		module, newdir, fname);
	    fflush (stdout);
	}

	/* Update old context.
	 */
	cx->old_nsymbols = nsymbols;
	cx->old_iflev = iflev;
	cx->old_cp = cp;

	if (cx->fp && cx->fp != stdin)
	    cx->fpos = k_ftell (cx);

	/* Initialize new context.
	 */
	ncx = (struct context *) malloc (sizeof (struct context));
	if (ncx == NULL)
	    fatals ("out of memory in `%s'", fname);

	*ncx = *cx;		/* copy old struct to new */

	ncx->pb		= NULL;
	ncx->prev	= cx;
	ncx->totfiles	= 0;
	ncx->nfiles	= 0;
	ncx->nrfiles	= 0;
	ncx->pbchar	= 0;
	ncx->pushback	= 0;
	ncx->sublib	= 0;

	/* In the case of a (XXX) module name reference to a module containing
	 * a sub-member list of the current library, strip the () and set the
	 * sublib flag for scanlibrary().
	 */
	if (module[0]) {
	    if (strcmp (module, "BOF") == 0) {
		ncx->library[0] = EOS;
	    } else if (module[0] == '(') {
		char     *ip, *op;

		for (ip=module+1, op=ncx->library;  (*op = *ip++);  op++)
		    if (*op == ')')
			break;
		*op = EOS;
		ncx->sublib = YES;
	    } else
		strcpy (ncx->library, module);
	}

	if (newdir[0] && strcmp(newdir,".") != 0 && strcmp(newdir,"./") != 0) {
	    /* Record the directory path for printed output.  Note that this
	     * will be a conventional pathname only if each "newdir" reference
	     * is to a subdirectory.
	     */
	    strcat (ncx->curdir, newdir);
	    strcat (ncx->curdir, "/");

	    if (debug) {
		printf ("change directory to `%s'\n", newdir);
		fflush (stdout);
	    }

	    if (os_chdir (newdir) == ERR) {
		warns ("cannot access subdirectory `%s'", newdir);
		free (ncx);
		return (NULL);
	    } else {
		os_fpathname ("", ncx->dirpath, SZ_PATHNAME);
		ncx->level++;
	    }

	    /* Initialize the file date cache, since the filenames therein
	     * often reference the current directory.
	     */
	    m_fdinit (debug);
	}

	if (fname[0])
	    strcpy (ncx->mkpkgfile, fname);

	return (topcx = ncx);
}


/* POP_CONTEXT -- Restore the previous context, including the current
 * directory.
 */
struct context *
pop_context (
  register struct context *cx 		/* current context	*/
)
{
	register struct context *pcx;
	int     root_modlist;
	int	level;

	if (debug) {
	    printf ("pop_context (library=%s)\n", cx->library);
	    fflush (stdout);
	}

	/* Pop the previous context.
	 */
	if (cx->prev != NULL) {
	    level = cx->level;
	    pcx   = cx->prev;

	    root_modlist = (strcmp (cx->library, pcx->library) != 0);
	    if (!root_modlist)
		pcx->totfiles += cx->totfiles;

	    free (cx);
	    topcx = cx = pcx;

	    if (cx->fp && cx->fp != stdin)
		k_fseek (cx, cx->fpos, 0);

	    sf_prune (cp = cx->old_cp);
	    nsymbols = cx->old_nsymbols;
	    iflev    = cx->old_iflev;

	    if (level > pcx->level) {
		if (debug) {
		    printf ("chdir ..\n");
		    fflush (stdout);
		}

		if (os_chdir (pcx->dirpath) == ERR)
		    fatals ("cannot return from subdirectory", cx->curdir);

		/* Initialize the file date cache, since the filenames therein
		 * often reference the current directory.
		 */
		m_fdinit (debug);
	    }
	}

	return (cx);
}


/* GET_DEPENDENCY_LIST -- Each file name in a library membership list occurs
 * on a separate line in the Makelib file.  This file name may be followed by
 * the names of zero or more other files, upon which the primary file is
 * dependent.  The following procedure extracts the names of these files into
 * the string buffer, returning a list of pointers to the filenames to the
 * caller.  Note that the string buffer space is only "borrowed" and the
 * filenames should be used promptly, before the string buffer space is reused.
 */
void
get_dependency_list (
  struct context *cx,		/* current library context	*/
  char	*module,		/* module list is for		*/
  char	*dflist[],		/* receives filename pointers	*/
  int	maxfiles 		/* maxfiles out			*/
)
{
	char	fname[SZ_FNAME+1];
	int	token, nfiles=0;
	char	*save_cp;
	int	i;

	save_cp = cp;

	while ((token = gettok (cx, fname, SZ_FNAME)) != 0) {
	    switch (token) {
	    case TOK_NEWLINE:
		goto done;
	    case TOK_FNAME:
		if (nfiles >= MAX_DEPFILES)
		    warns ("too many dependency files for module `%s'", module);
		dflist[nfiles++] = putstr (fname);
		break;
	    case TOK_END:
		warns ("unexpected EOF in dependency list for `%s'", module);
	    default:
		warns ("bad token `%s' in dependency list", fname);
	    }
	}

done:
	/* A null string pointer marks the end of the list.
	 */
	dflist[nfiles] = NULL;

	if (debug) {
	    printf ("%s:", module);
	    for (i=0;  i < nfiles;  i++)
		printf (" %s", dflist[i]);
	    printf ("\n");
	    fflush (stdout);
	}

	cp = save_cp;
}


/* UP_TO_DATE -- Determine if the named module is up to date.  A module is up
 * to date if:
 *
 *	(1) The lib module is newer than the source file, and
 *	(2) The source file is newer than any of its dependents.
 *
 * If the module is out of date, and an object file exists which is current
 * (newer than the source, which is in turn newer than any dependents),
 * set the USEOBJ flag to tell our caller to use the .o file, rather than
 * recompile the module.
 */
int
up_to_date (
  struct context *cx,		/* current library context	*/
  char	*module,		/* module to compare dates for	*/
  char	*lname,			/* local name of module		*/
  char	*dflist[],		/* list of dependent files	*/
  int	*useobj 		/* obj exists and is usable	*/
)
{
	long	armod_date, newest_date, date;
	long	h_ardate(char *fname);
	char	*fname;
	int	old, i;

	armod_date  = h_ardate (lname);
	newest_date = armod_date;
	(*useobj)   = NO;

	/* Compare lib module date and source file date.
 	 */
	date = os_fdate (module);
	if (date == 0) {
	    warns ("module source file `%s' not found", module);
	    return (YES);
	} else if (armod_date < date) {
	    if (debug > 1) {
		printf ("(%s) ar: %ld fil: %ld\n", module, armod_date, date);
		fflush (stdout);
	    }
	    old = YES;
	    newest_date = date;
	} else
	    old = NO;

	/* Compare dates of archive file and any dependent files.
	 */
	for (i=0;  (fname = dflist[i]) != NULL;  i++) {
	    date = m_fdate (fname);
	    if (date == 0) {
		warns ("dependency file `%s' not found", fname);
	    } else if (armod_date < date) {
		old = YES;
		if (date > newest_date)
		    newest_date = date;
	    }
	}

	if (old == NO) {
	    /* Module is up to date.
	     */
	    return (YES);
	} else {
	    /* Library module is not up to date.  Check if an object file
	     * exists which can be used w/o recompilation.
	     */
	    if (newest_date <= os_fdate (makeobj (module)))
		(*useobj) = YES;
	    return (NO);
	}
}


/* OPEN_MKPKGFILE -- Open the mkpkgfile for the current library context.
 * If the same file is already physically open by this process, this is
 * a "soft" open.
 */
int
open_mkpkgfile (register struct context *cx)
{
	register char	*fname = cx->mkpkgfile;
	struct	context *find_mkpkgfile(struct context *head_cx, char *mkpkgfile, int level);
	struct	context *ax;
	
	if (strcmp (fname, "stdin") == 0 || strcmp (fname, "STDIN") == 0) {
	    cx->fp = stdin;
	} else if ((ax = find_mkpkgfile (cx->prev, fname, cx->level)) == NULL) {
	    cx->fp = fopen (vfn2osfn(fname,0), "r");
	    if (cx->fp)
		k_fseek (cx, 0L, 0);
	} else {
	    cx->fp = ax->fp;
	    if (cx->fp && cx->fp != stdin)
		k_fseek (cx, 0L, 0);
	}

	cx->lineno = 1;
	return (cx->fp == NULL ? ERR : OK);
}


/* CLOSE_MKPKGFILE -- Close a mkpkgfile.  If the file is multiply open (in
 * software) wait until the last context closes the file to physically close
 * the file.
 */
void
close_mkpkgfile (register struct context *cx)
{
	struct context *find_mkpkgfile(struct context *head_cx, char *mkpkgfile, int level);

	if (cx->fp != stdin)
	    if (find_mkpkgfile (cx->prev, cx->mkpkgfile, cx->level) == NULL)
		fclose (cx->fp);
}


/* FIND_MKPKGFILE -- Search the list of open library contexts for an entry
 * which already has the named mkpkgfile open.
 */
struct context *
find_mkpkgfile (
  struct context *head_cx,	/* head of context list		*/
  char	*mkpkgfile,		/* file to search for		*/
  int	level 			/* subdirectory level		*/
)
{
	register struct context *cx;

	for (cx=head_cx;  cx != NULL;  cx=cx->prev)
	    if (cx->level == level && strcmp (cx->mkpkgfile, mkpkgfile) == 0)
		return (cx);

	return (NULL);
}


/* SEARCH_MKPKGFILE -- Search the mkpkgfile for the named entry.  A mkpkg
 * entry consists of a TOK_FNAME (identifier) followed by TOK_BEGIN (colon),
 * e.g., "entry:".  If a specific module is named, go directly there without
 * processing any preprocessor directives.  If no module is named, search
 * for the first entry, executing any preprocessor directives encountered
 * while searching.
 */
int
search_mkpkgfile (register struct context *cx)
{
	char	word1[SZ_FNAME+1], word2[SZ_FNAME+1];
	char	*prev, *curr, *temp;
	int	tok, gettok(register struct context *cx, char *outstr, int maxch);

	if (debug) {
	    printf ("search_mkpkgfile (file=%s, library=%s)\n",
		cx->mkpkgfile, cx->library);
	    fflush (stdout);
	}

	/* If a specific module is desired and we are not in search mode,
	 * go directly to the named module without executing any preprocessor
	 * directives.
	 */
	if (cx->library[0])
	    return (do_goto (cx, cx->library));

	/* Search Makelib file until an entry for the named library is found.
	 * Execute any preprocessor directives encountered while searching.
	 */
	prev = word1;
	curr = word2;

	/* Advance to the next entry.  If an @subdir reference is
	 * encountered, go process the subdirectory in search mode
	 * and then continue locally.
	 */
	while ((tok = gettok (cx, curr, SZ_FNAME)) != TOK_BEGIN) {
	    if (tok == 0 || tok == TOK_END) {
		/* Exit; no entry found.
		 */
		return (ERR);

	    } else if (tok == TOK_FNAME && curr[0] == SUBDIR_CHAR) {
		/* Continue the search in the context of a subdirectory.
		 */
		struct  context *ncx;
		char    module[SZ_FNAME+1];
		char    subdir[SZ_FNAME+1];
		char    fname[SZ_FNAME+1];
		int     islib;

		/* Push a new context and start over; recursive call.
		 * May "reopen" (soft) the current mkpkg file or the mkpkg
		 * in a subdirectory.
		 */
		parse_modname (curr, module, subdir, fname);
		if ((ncx = push_context (cx, module,subdir,fname)) == NULL)
		    exit_status = ERR;
		else {
		    exit_status = do_mkpkg (ncx, islib=NO);
		    cx = pop_context (ncx);
		}

		if (exit_status != OK && !ignore)
		    return (exit_status);

	    } else {
		/* Save the old token; pointer swapping rather than copy
		 * used for efficiency.
		 */
		temp = curr;
		curr = prev;
		prev = temp;
	    }
	}

	strcpy (cx->library, prev);	/* return module name 	*/
	return (OK);
}
