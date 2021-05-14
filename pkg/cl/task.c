/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "param.h"
#include "mem.h"
#include "task.h"
#include "errs.h"
#include "clmodes.h"
#include "proto.h"


/*
 * TASK -- Operators for tasks.
 */

extern int cldebug;
extern char *nullstr;
extern struct param *clabbrev;	/* used to inhibit abbrevs in addltask	*/

struct task *firstask;		/* ptr to original cl task		*/
struct task *newtask;		/* ptr to new, but unlinked, task	*/
struct task *currentask;	/* ptr to ltask currently running	*/
struct package *curpack;	/* current package in effect		*/

XINT pachead;			/* dict index of first package		*/


/* CMDSRCH -- Used by callnewtask() to find the ltask to be run.  Ltname is
 *   the name of the logical task to be run.  pkname is the name of an
 *   explicit package.  If pkname is set, just look through its ltasks,
 *   otherwise circularly search through all packages starting at curpack.
 * Once we have found an ltask, we see if there is a package with the same
 *   (full) name.  If there is, we return a pointer to the special pacltask
 *   with LT_PACCL flag set to signal callnewtask() to just change packages.
 *   if there isn't, just return a pointer to the ltask.
 * Ltasksrch() should be used if you don't want all this package checking...
 * Call error() and don't return on any kind of error.
 * We need a fake rootpackage entry to be able to change the current package
 *   to clpackage; see clpkg().
 */
struct ltask *
cmdsrch (char *pkname, char *ltname)
{
	register struct ltask *ltp = NULL;
	register struct package *pkp, *pkcand;
	static	struct ltask pacltask;	/* used to signal a package change */
	struct	ltask *temptaskset();
	char	*name;

	if (*pkname != '\0') {      /* package name included; just search it.*/
	    pkp = pacfind (pkname);
	    if (pkp == NULL)
		cl_error (E_UERR, e_pcknonexist, pkname);
	    else if ((XINT)pkp == ERR)
		cl_error (E_UERR, e_pckambig, pkname);
	    else
		ltp = ltaskfind (pkp, ltname, 1);

	    if (ltp == NULL) { 
		cl_error (E_UERR, e_tnonexist, ltname);
	    }

	    if ((XINT)ltp == ERR)
		cl_error (E_UERR, e_tambig, ltname);

	} else
	    /* Search all packages. ltasksrch() does not return if it has
	     * problems so we can count on ltp being set here.
	     */
	    ltp = ltasksrch ("", ltname);

	/* If this task did not define a package, just go with it.
 	 * Otherwise, search around for package with same name and use it.
	 * Don't use pacfind() since always want exact matches only.
	 * If can't find the package now, it must have been existed so we
	 * should run the task again.
	 */
	if (!(ltp->lt_flags & LT_DEFPCK))
	    return (ltp);

	name = ltp->lt_lname;
	pkcand = NULL;

	for (pkp = reference(package,pachead);  pkp;  pkp = pkp->pk_npk)
	    if (!strcmp (name, pkp->pk_name)) {
		if (pkcand == NULL)
		    pkcand = pkp;
		else
		    pkcand = (struct package *) ERR;
	    }

	if (pkcand == (struct package *) ERR)
	    cl_error (E_UERR, e_pckambig, name);

	if (pkcand == NULL)
	    return (ltp);
	else {
	    /* Just change to the given package.
	     * If unions could be inited, we could set lt_flags once in
	     *   its declaration above. phooey.
	     * Use lt_pkp to return new package. see callnewtask().
	     */
	    pacltask.lt_flags = (LT_PACCL|LT_CL);
	    pacltask.lt_pkp = pkcand;
	    return (&pacltask);
	}
}


/* LTASKSRCH -- Find ltask of given name along standard path, ie, circularly
 *   through all packages starting with curpack.  If name included package name
 *   explicitly, it will be in pkname and then just look down it.
 * Use abbreviations if enabled. always accept an exact match, even if it
 *   happened to match more than one longer name as an abbreviation.
 * Use cmdsrch() if want to include packages themselves in search path.
 * Always return a valid pointer; call error() and don't return on any kind of
 *   error.
 */
struct ltask *
ltasksrch (char *pkname, char *ltname)
{
	struct	ltask *ltp;
	struct	package *pkp;

	ltp = _ltasksrch (pkname, ltname, &pkp);

	if (*pkname != EOS) {
	    if (pkp == NULL)
		cl_error (E_UERR, e_pcknonexist, pkname);
	    if ((XINT)pkp == ERR)
		cl_error (E_UERR, e_pckambig, pkname);
	}

	if (ltp == NULL)
	    cl_error (E_UERR, e_tnonexist, ltname);
	if (ltp == (struct ltask *) ERR)
	    cl_error (E_UERR, e_tambig, ltname);

	return (ltp);
}


/* _LTASKSRCH -- Same as ltasksrch(), except that cl_error is not called.
 */
struct ltask *
_ltasksrch (char *pkname, char *ltname, struct package **o_pkp)
{
	register struct ltask *ltp, *ltcand;
	register struct package *pkp;
	register char first_char = ltname[0];

	ltcand = NULL;
	if (*pkname != '\0') {
	    /* Package name included; just search it. */
	    pkp = pacfind (pkname);
	    if (pkp != NULL && (XINT)pkp != ERR)
		ltcand = ltaskfind (pkp, ltname, 1);

	} else if (abbrev()) {
	    /* Settle for abbreviation. */
	    int     n = strlen (ltname);
	    int     hit_in_curpack = 0;

	    pkp = curpack;
	    do {
		for (ltp = pkp->pk_ltp;  ltp;  ltp = ltp->lt_nlt) {
		    if (*ltp->lt_lname == first_char) {
			if (!strncmp (ltp->lt_lname, ltname, n)) {
			    if (ltp->lt_lname[n] == '\0') {	/* exact hit */
				*o_pkp = pkp;
				return (ltp);
			    }
			    /* Only accept exact hits for hidden tasks.
			     */
			    if (ltp->lt_flags & LT_INVIS)
				continue;
			    if (ltcand == NULL)
				ltcand = ltp;
			    else if (!hit_in_curpack)
				ltcand = (struct ltask *) ERR;
			}
		    }
		}

		/* If an acceptable abbreviation was found in the current
		 * package, use it, unless an exact match is found in some
		 * other package.
		 */
		if (ltcand && pkp == curpack)
		    hit_in_curpack++;

		/* Circular search. */
		if ((pkp = pkp->pk_npk) == NULL)
		    pkp = reference (package, pachead);

	    } until (pkp == curpack);

	} else {
	    /* Require exact match */
	    pkp = curpack;
	    do {
		for (ltp = pkp->pk_ltp;  ltp;  ltp = ltp->lt_nlt)
		   if (*ltp->lt_lname == first_char)
		       if (!strcmp (ltp->lt_lname, ltname))
		           return (ltp);
		if ((pkp = pkp->pk_npk) == NULL)
		    pkp = reference (package, pachead);
	    } until (pkp == curpack);
	}

	*o_pkp = pkp;
	return (ltcand);
}


/* PACFIND -- Start at pachead and look for package with given name.  Allow
 * abbreviations if enabled. return ERR if ambiguous.  Return its pointer or
 * NULL if not found.
 */
struct package *
pacfind (char *name)
{
	struct	package *pkp;
	struct	package *candidate;
	int	n;

	if (abbrev()) {
	    /* Settle for abbreviation of name.
	     * Check whole list in we can find an exact match.
	     */
	    candidate = NULL;
	    n = strlen (name);
	    for (pkp = reference(package,pachead);  pkp;  pkp = pkp->pk_npk)
		if (!strncmp (pkp->pk_name, name, n)) {
		    if (pkp->pk_name[n] == '\0')
			return (pkp);   /* exact hit */
		    if (candidate == NULL)
			candidate = pkp;
		    else
			candidate = (struct package *) ERR;
		}

	    return (candidate);

	} else for (pkp = reference(package,pachead);  pkp;  pkp = pkp->pk_npk)
	    if (!strcmp (pkp->pk_name, name))
		return (pkp);
	return (NULL);
}


/* DEFPAC -- Return true/false if the named package is/isnot loaded.
 * Call error if an ambiguous abbreviation is given.
 */
int
defpac (char *pkname)
{
	switch ((XINT) pacfind (pkname)) {
	case NULL:
	    return (NO);
	case ERR:
	    cl_error (E_UERR, e_pckambig, pkname);
	default:
	    return (YES);
	}
}


/* LTASKFIND -- Start at given package and look for ltask with given name.
 * Return NULL if not found, ERR if ambiguous or pointer if found.
 */
struct ltask *
ltaskfind (
    struct package *pkp,			/* package to be searched	*/
    char *name,				/* ltask name			*/
    int enable_abbreviations		/* enable abbrev. in search	*/
)
{
	register struct ltask *ltp;
	struct	ltask *candidate;
	int	n;

	if (enable_abbreviations && abbrev()) {
	    /* Settle for abbreviation of nam.
	     * Check whole list in case we can find an exact match.
	     */
	    candidate = NULL;
	    n = strlen (name);
	    for (ltp = pkp->pk_ltp;  ltp;  ltp = ltp->lt_nlt)
		if (!strncmp (ltp->lt_lname, name, n)) {
		    if (ltp->lt_lname[n] == '\0')
			return (ltp);		/* exact hit */
		    if (candidate == NULL)
			candidate = ltp;
		    else
			candidate = (struct ltask *) ERR;
		}

	    return (candidate);

	} else {
	    /* Accept exact match only. */
	    for (ltp = pkp->pk_ltp;  ltp;  ltp = ltp->lt_nlt)
		if (!strcmp (ltp->lt_lname, name))
		    return (ltp);
	}

	return (NULL);
}


/* DEFTASK -- Return true/false if the named ltask is/is not defined.
 * If a specific package is named, look only there; otherwise search
 * the usual path.  Call error if an ambiguous abbreviation is given.
 */
int
deftask (char *task_spec)
{
	char	buf[SZ_LINE];
	char	*pkname, *ltname, *junk;
	struct	package *pkp;
	int	stat;

	strcpy (buf, task_spec);
	breakout (buf, &junk, &pkname, &ltname, &junk);

	if (pkname[0] != '\0') {	/* explicit package named	*/
	    if ((pkp = pacfind (pkname)) == NULL)
		cl_error (E_UERR, e_pcknonexist, pkname);
	    if ((stat = (XINT) ltaskfind (pkp, ltname, 1)) == NULL)
		return (NO);

	} else {			/* search all packages		*/
	    pkp = reference (package, pachead);
	    stat = NULL;

	    while (pkp != NULL) {
		stat = (XINT) ltaskfind (pkp, ltname, 1);
		if (stat == ERR)
		    break;
		else if (stat != NULL)
		    return (YES);
		pkp = pkp->pk_npk;
	    }
	}

	if (stat == ERR)
	    cl_error (E_UERR, e_tambig, ltname);
	if (stat != NULL)
	    return (YES);
	return (NO);
}


/* TASKUNWIND -- Used when aborting from an error or on interrupt, NOT on bye
 *   or eof.  Starting with top task state, keep popping and killing tasks
 *   until find one that is T_INTERACTIVE, closing files and pipes along the
 *   way.
 * Restore dictionary and stack to what they were when the new (now
 *   current) task last started compiling with yyparse().  See runtask().
 * Do NOT update parameter files when a task dies abnormally, just from
 *   a proper "bye" command or eof.
 */
void
taskunwind (void)
{
	while (!(currentask->t_flags & T_INTERACTIVE)) {
	    killtask (currentask);
	    currentask = poptask();
	}

	restor (currentask);
}


/* ADDLTASK -- Make a new ltask off curpack with given ltname/ptname.
 * Check through whole list and warn about redefs unless redef flag is set.
 * Look for .cl (script task) or .par (pset task) specs in ptname, and $
 *   (no pfile) and trailing .bt (io file type) specs in ltname and set
 *   lt_flags accordingly.
 * Actual new ltask entry made with newltask() and it re-uses dictionary space
 *   for the ptask name if possible.
 * Write error messages here and return ERR if problems, else OK.  Be sure they
 *   use the same format as error() for consistency.
 * Do not use abbreviations when checking for possible redefs.
 * Newltask() may call error() if it can not get enough core.
 * N.B. ptname and ltname may be changed IN PLACE to simplify suffix tests.
 */
struct ltask *
addltask (struct package *pkp, char *ptname, char *ltname, int redef)
{
	register char *cp;
	register struct ltask *ltp;
	char	*rindex();
	char	*ltbase;
	int	flags;

	flags = 0;
	ltbase = ltname;
	if (*ltbase == '$')
	    ltbase++;
	else
	    flags |= LT_PFILE;

	/* A leading underscore signifies that the task is not part of the
	 * user interface, and hence should not appear in menus etc.  Set
	 * the LT_INVIS flag, but leave the underscore in the name.
	 */
	if (*ltbase == CH_INVIS)
	    flags |= LT_INVIS;

	/* Check for trailing .bt etc. specs on logical task name.
	 */
	if ((cp = rindex (ltbase, '.')) != NULL) {
	    /* replace '.' with '\0' in hopes of finding valid specs.
	     * if invalid, put back before giving error diagnostic.
	     */
	    *cp++ = '\0';
	    if (!strcmp (cp, "pkg"))
		flags |= LT_DEFPCK;
	    else if (!strcmp (cp, "bt"))
		flags |= LT_STDINB;
	    else if (!strcmp (cp, "tb"))
		flags |= LT_STDOUTB;
	    else if (!strcmp (cp, "bb") || !strcmp (cp, "b"))
		flags |= (LT_STDOUTB|LT_STDINB);
	    else if (strcmp (cp, "tt") && strcmp (cp, "t")) {
		*--cp = '.';
		eprintf ("ERROR: bad binary io spec in `%s'\n", ltbase);
		return (NULL);
	    }
	}

	/* Check to see if this is a redefined task.  Inhibit ltaskfind()
	 * from using abbreviations during redef check.
	 */
	ltp = ltaskfind (pkp, ltbase, 0);
	if (ltp != NULL) {
	    if (!redef)
		eprintf ("WARNING: `%s' is a task redefinition.\n", ltbase);
	} else if (redef)
	    eprintf ("WARNING: `%s' is not a defined task.\n", ltbase);

	/* Check for trailing .cl spec in physical task name to indicate
	 * a script task, or a .par to indicate a pset task.
	 */
	if (ptname && (cp = rindex (ptname, '.')) != NULL) {
	    cp++;
	    if (!strcmp (cp, "cl"))
		flags |= LT_SCRIPT;
	    else if (!strcmp (cp, "par"))
		flags |= (LT_SCRIPT|LT_PSET);
	}

	ltp = newltask (pkp, ltbase, ptname, ltp);
	ltp->lt_flags = flags;

	return (ltp);
}


/* NEWLTASK -- Allocate a new ltask on the dictionary and link in off package
 *   *pkp.  Compile logical name, lname, immediately after.
 * Look for and reuse physical name, pname, if possible else compile next.
 *   this is more than a simple savings of core. all ltasks within a ptask will
 *   have the same lt_pname pointer so, for example, we can test
 *   newtask->t_ltp->lt_pname == currentask->t_ltp->lt_pname to decide if the
 *   next ltask is part of the current ptask.
 * Don't do anything with lt_pname if LT_BUILTIN is set since it uses the
 *   field (in a union) as a pointer to the built-in function. see task.h.
 * Link the new ltask immediately off the package at pkp->pk_ltp. this is so
 *   in a linear search the most recently added task will be seen first.
 * For task redefinitions don't allocate a new logical task.  Re-use the
 *   old block and don't change any of the links to the package and other
 *   tasks.
 * Null out all unused fields.
 */
struct ltask *
newltask (register struct package *pkp, char *lname, char *pname, struct ltask *oldltp)
{
	register struct ltask *ltp, *newltp;

	if (oldltp == NULL) {
	    newltp = (struct ltask *) memneed (LTASKSIZ);
	    newltp->lt_lname = comdstr (lname);
	} else
	    newltp = oldltp;

	/* Look for another ltask with same pname; use it again if find else
	 * compile in a new pname.  Don't do anything, however, if LT_BUILTIN
	 * is set as it does not use this union member this way.
	 */
	if (pname) {
	    for (ltp = pkp->pk_ltp;  ltp != NULL;  ltp = ltp->lt_nlt) {
		if (!(ltp->lt_flags & LT_BUILTIN)) {
		    if (strcmp (ltp->lt_pname, pname) == 0) {
			newltp->lt_pname = ltp->lt_pname;
			goto link;
		    } 
		}
	    }
	    newltp->lt_pname = comdstr (pname);
	} else
	    newltp->lt_pname = "";

link:
	if (oldltp == NULL) {
	    /* Link in as first ltask off this package.
	     */
	    newltp->lt_nlt = pkp->pk_ltp;
	    pkp->pk_ltp = newltp;
	    newltp->lt_pkp = pkp;	/* set the back-link		*/
	}

	newltp->lt_flags = 0;
	return (newltp);
}


/* NEWPAC -- Allocate a new package with given name on the dictionary and
 * link in at pachead. compile name in-line immediately after.
 * null out all unused fields.
 * call error() if no core or if name already exists.
 */
struct package *
newpac (char *name, char *bin)
{
	register struct package *pkp;

	if (pacfind (name) != NULL)
	    cl_error (E_UERR, "package `%s' already exists", name);

	pkp = (struct package *) memneed (PACKAGESIZ);
	pkp->pk_name = comdstr (name);
	pkp->pk_bin = bin ? comdstr(bin) : curpack->pk_bin;

	pkp->pk_npk = reference (package, pachead);
	pachead = dereference (pkp);

	pkp->pk_ltp = NULL;
	pkp->pk_pfp = NULL;
	pkp->pk_flags = 0;

	return (pkp);
}
