/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_finfo
#define import_stdio
#define import_ctype
#include <iraf.h>

#include "config.h"
#include "errs.h"
#include "operand.h"
#include "mem.h"
#include "param.h"
#include "task.h"
#include "grammar.h"
#include "proto.h"


/*
 * PFILES -- Parameter file access procedures.
 */

extern	int cldebug;
extern	char *undefval;
extern	char *nullstr;
extern	char *indefstr, *indeflc;
extern	FILE *yyin;
char	*uparmdir = UPARM;
long	filetime();
static	void mapname();

extern  int c_finfo();


/* NEWPFILE -- Allocate a new pfile on the dictionary and link in at parhead.
 * Set pfp->pf_ltp to ltp.  Null out all unused fields.  Call error() and don't
 * return if not enough core.
 */
struct pfile *
newpfile (
    struct ltask *ltp		/* ltask descriptor */
)
{
	register struct pfile *pfp, *head_pfp;

	pfp = (struct pfile *) memneed (PFILESIZ);
	head_pfp = reference (pfile, parhead);
	if (head_pfp >= pfp)
	    cl_error (E_IERR, "in newpfile: parhead exceeds topd");

	pfp->pf_npf = reference (pfile, parhead);
	parhead = dereference (pfp);

	pfp->pf_pp = NULL;
	pfp->pf_oldpfp = NULL;
	pfp->pf_npset = NULL;
	pfp->pf_psetp = NULL;
	pfp->pf_ltp = ltp;
	pfp->pf_flags = 0;
	pfp->pf_n = 0;

	return (pfp);
}


/* PFILEUNLINK -- Unlink a pfile from the pfile list.
 */
void
pfileunlink (
    register struct pfile *pfp	/* pfile to be unlinked	*/
)
{
	register struct pfile *npf;

	if ((npf = reference (pfile, parhead)) == pfp)
	    parhead = dereference (pfp->pf_npf);
	else {
	    while (npf && npf->pf_npf != pfp)
		npf = npf->pf_npf;
	    if (npf) {
		if (pfp->pf_npf == npf)
		    cl_error (E_IERR, "in pfileunlink: circular reference");
		else
		    npf->pf_npf = pfp->pf_npf;
	    }
	}
}


/* PFILEFIND -- Search the list of loaded pfiles for the pfile for a particular
 * ltask.  Return pfile pointer or NULL.   Note that all loaded pfiles are
 * linked on a single list regardless of which package or task they belong to.
 */
struct pfile *
pfilefind (
    register struct ltask *ltp		/* ltask descriptor */
)
{
	register struct pfile *pfp;

	for (pfp = reference (pfile, parhead);  pfp != NULL;  pfp = pfp->pf_npf)
	    if (pfp->pf_ltp == ltp)
		return (pfp);

	return (NULL);
}


/* PFILESRCH -- Given a pfile filename or the pathname of an ltask which
 * has a pfile, allocate a pfile descriptor and read the pfile into that
 * descriptor.
 */
struct pfile *
pfilesrch (
    char *pfilepath		/* filename or ltask pathname	*/
)
{
	struct	pfile *pfp;

	if (cldebug)
	    eprintf ("pfilesrch %s\n", pfilepath);

	if (is_pfilename (pfilepath)) {
	    if ((pfp = pfileread (NULL, pfilepath, 0)) == NULL)
		cl_error (E_UERR, e_badpfile, pfilepath);
	    strcpy (pfp->pf_pfilename, pfilepath);
	    return (pfp);

	} else {
	    char    *x1, *pk, *t, *x2;
	    struct  ltask *ltp;

	    breakout (pfilepath, &x1, &pk, &t, &x2);
	    ltp = ltasksrch (pk, t);
	    if (!(ltp->lt_flags & LT_PFILE))
		cl_error (E_UERR, e_nopfile, ltp->lt_lname);
	    if ((pfp = pfilefind (ltp)) != NULL)
		return (pfp);			/* already in core.	*/

	    return (pfileload (ltp));
	}
}


/* PFILELOAD -- Load the pfile for the ltask pointed to by ltp.  The input
 * pfile may be the source package pfile (read only), the users UPARM copy
 * of the package pfile, or a named user pfile in the case of a pset-task
 * reference.  Save the filename where the pfile is to be updated in the
 * pfile descriptor, for later use by pfileupdate().  Pfiles are always
 * updated in UPARM, except in the case of named pfiles, which are updated
 * in place.
 */
struct pfile *
pfileload (
    register struct ltask *ltp		/* ltask descriptor */
)
{
	static	long sys_ftime = 0;
	register struct	task *tp;
	register struct	param *pp;
	char	usr_pfile[SZ_FNAME+1];
	char	pkg_pfile[SZ_FNAME+1];
	char	pkgdir[SZ_FNAME+1];
	long	usr_ftime, pkg_ftime;
	char	*ltname, *pkname;
	struct	pfile *pfp;
	char	*sval;

	if (cldebug)
	    eprintf ("pfileload, task %s\n", ltp->lt_lname);

	/* If the ltask operand is a PSET task, the parameter file to be
	 * read is controlled by the value of a pset parameter of the same
	 * name as the ltask, in the main parameter set of the most recently
	 * executed task which includes that pset parameter.  If no running
	 * task references the PSET task then we use the pfile of the PSET
	 * task itself, i.e., we have a conventional task.param parameter
	 * reference.
	 *
	 * If we make it through this block of code without reading a named
	 * pfile and exiting, either nothing has happened (the pset was not
	 * redirected), or the pset was redirected to a different ltask and
	 * we are still faced with the equivalent problem of mapping an ltp
	 * into a pfp, but this time without the compilication of PSET
	 * indirection.
	 */
	if (ltp->lt_flags & LT_PSET) {
            /* Don't use newtask if it is pointing beyond end of stack. */
            tp = (newtask < (struct task *)&stack[topcs]) ? currentask:newtask;

            for (  ;  tp != firstask;  tp = next_task(tp)) {
		pfp = tp->t_pfp;
		if (!pfp || !(pfp->pf_flags & PF_PSETREF))
		    continue;

		/* Search pfile of currently executing task.
		 */
		for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np)
		    if (pp->p_type & PT_PSET)
			if (!strcmp (pp->p_name, ltp->lt_lname)) {
			    /* Found pset parameter with same name as ltask.
			     */
			    if (opundef (&pp->p_valo))
				sval = "";
			    else
				sval = pp->p_val.v_s;

			    if (*sval == EOS) {
				; /* Null string - no indirection */
			    } else if (is_pfilename (sval)) {
				/* Named pfile */
				if ((pfp = pfileread (ltp, sval, 0)) != NULL)
				    return (pfp);
				else
				    cl_error (E_UERR, e_badpfile, sval);
			    } else {
				/* Must be a reference to another task */
				char    *x1, *pk, *t, *x2;

				breakout (sval, &x1, &pk, &t, &x2);
				ltp = ltasksrch (pk, t);
				if (!(ltp->lt_flags & LT_PFILE))
				    cl_error (E_UERR, e_nopfile, ltp->lt_lname);
			    }

			    goto epset_;
			}
	    }
	}
epset_:
	ltname = ltp->lt_lname;
	pkname = ltp->lt_pkp->pk_name;

	/* Determine the UPARM filename of the pfile. */
	mkpfilename (usr_pfile, uparmdir, pkname, ltname, ".par");

	/* As an optimization, all the checking for filetimes, file sizes,
	 * and out of date pfiles is only performed once when a file is
	 * first accessed.  Once a valid up to date UPARM version of a pfile
	 * is obtained a bit is set in the ltask descriptor and thereafter
	 * we need only read the UPARM version of the pfile and exit.  If a
	 * problem occurs reading the pfile, or if the pfile is unlearned,
	 * the bit is cleared and all the checking and initialization is
	 * repeated.
	 */
	if (ltp->lt_flags & LT_UPFOK)
	    if ((pfp = pfileread (ltp, usr_pfile, 1)) != NULL)
		return (pfp);

	/* Get modification (creation) time of usr pfile and filename and
	 * modification time of pkg pfile.  Look for a .par version of the
	 * pkg pfile, and if not found, a .cl version (procedure script).
	 */
	usr_ftime = filetime (usr_pfile, "c");
	c_fnldir (ltp->lt_pname, pkgdir, SZ_FNAME);

	mkpfilename (pkg_pfile, pkgdir, pkname, ltname, ".par");
	if ((pkg_ftime = filetime (pkg_pfile, "m")) <= 0) {
	    mkpfilename (pkg_pfile, pkgdir, pkname, ltname, ".cl");
	    if ((pkg_ftime = filetime (pkg_pfile, "m")) <= 0)
		cl_error (E_UERR, e_nopfile, ltname);
	}

	/* Get the date when the iraf system was last installed or updated.
	 * This is indicated by the modify time of the special file hlib$utime,
	 * which is touched during the system installation process.  The file
	 * may actually be newer than the date of system update/install but
	 * that is harmless.
	 */
	if (sys_ftime <= 0)
	    sys_ftime = filetime ("hlib$utime", "m");

	/* If the system was installed more recently than the package pfile
	 * was modified, use the system modify time instead.
	 */
	if (sys_ftime > 0)
	    if (sys_ftime > pkg_ftime)
		pkg_ftime = sys_ftime;

	if (usr_ftime > 0) {
	    /* We have a user (UPARM) version of the pfile.  If it is newer
	     * than the pkg pfile, use it, else read the pkg pfile and merge
	     * the param values from the user pfile into the new pkg pfile.
	     */
	    if (usr_ftime>pkg_ftime && (pfp=pfileread(ltp,usr_pfile,1)) != NULL)
		ltp->lt_flags |= LT_UPFOK;
	    else {
		if ((pfp = pfileread (ltp, pkg_pfile, 0)) == NULL)
		    cl_error (E_UERR, e_badpfile, pkg_pfile);
		pfilemerge (pfp, usr_pfile);
		strcpy (pfp->pf_pfilename, usr_pfile);
	    }
	} else {
	    /* No user pfile; read pkg pfile.
	     */
	    if ((pfp = pfileread (ltp, pkg_pfile, 0)) == NULL) {
		FILE   *fp;
		if (!is_pfilename (pkg_pfile))
		    if ((fp = fopen (pkg_pfile, "r")) != NULL) {
			if (!procscript (fp))
			    cl_error (E_UERR, e_nopfile, ltname);
			fclose (fp);
		    }
		cl_error (E_UERR, e_badpfile, pkg_pfile);
	    } else
		strcpy (pfp->pf_pfilename, usr_pfile);
	}

	return (pfp);
}


/* PFILEMERGE -- Merge the parameter values from the named (old user) pfile
 * into a loaded parameter set.
 */
int
pfilemerge (
    struct pfile *npf,			/* loaded parameter set	*/
    char *opfile			/* old parameter file	*/
)
{
	register struct param *o_pp, *n_pp, *l_pp;
	int	bastype;
	XINT	save_topd;
	struct	pfile *opf;
	struct	ltask *ltp;

	if (cldebug)
	    eprintf ("pfilemerge, task %s, pfile %s\n",
		(ltp = npf->pf_ltp) ? ltp->lt_lname : "", opfile);

	/* Open old pfile. */
	save_topd = topd;
	if ((opf = pfileread (npf->pf_ltp, opfile, 0)) == NULL)
	    return (ERR);

	/* For each parameter in the old pfile, locate the corresponding
	 * parameter in the new pfile and copy the value.  No other fields
	 * of the parameter structure are copied.
	 */
	for (n_pp = NULL, o_pp = opf->pf_pp;  o_pp;  o_pp = o_pp->p_np) {
	    /* Circular search, starting at position of last parameter.
	     */
	    n_pp = ((l_pp = n_pp) != NULL) ? n_pp->p_np : npf->pf_pp;
	    while (n_pp != l_pp) {
		if (n_pp == NULL)
		    n_pp = npf->pf_pp;
		else if (strcmp (n_pp->p_name, o_pp->p_name) == 0)
		    break;
		else
		    n_pp = n_pp->p_np;
	    }

	    /* If parameter not in new param set or the datatypes do not
	     * match, skip this parameter.
	     */
	    if (n_pp == l_pp)
		continue;
	    if (n_pp->p_type != o_pp->p_type)
		continue;

	    bastype = (n_pp->p_type & OT_BASIC);

	    /* Copy value */
	    n_pp->p_valo.o_type = o_pp->p_valo.o_type;

	    /* Handle arrays. */
	    /* The array descriptors should remain the same, only
	     * the stored values could change.
	     */
	    if (n_pp->p_type & PT_ARRAY) {
		int	dim, d, size_arr;
		short	*lenoff;

		/* Get size of array. */
		dim = n_pp->p_val.v_a->a_dim;
		lenoff = &(n_pp->p_val.v_a->a_len);
		size_arr = 1;
		if (bastype == OT_REAL)
		    size_arr = 2;
		for (d=0;  d < dim;  d++)
		    size_arr *= *(lenoff + 2*d);

		if (bastype != OT_STRING) {
		    int     *p, *q;
		    p = o_pp->p_aval.a_i;
		    q = n_pp->p_aval.a_i;
		    for (d=0;  d < size_arr;  d++)
			*q++ = *p++;
		} else {
		    char     **p, **q;
		    p = o_pp->p_aval.a_s;
		    q = n_pp->p_aval.a_s;
		    for (d=0;  d < size_arr;  d++)
			strcpy (*q++, *p++);
		}

	    } else if (!(o_pp->p_valo.o_type & (OT_INDEF|OT_UNDEF))) {
		if (((o_pp->p_valo.o_type & OT_BASIC) == OT_STRING) &&
		   (n_pp->p_val.v_s != NULL)) {
		   strncpy (n_pp->p_val.v_s, o_pp->p_val.v_s, n_pp->p_lenval-1);
		} else
		    n_pp->p_valo.o_val = o_pp->p_valo.o_val;
	    }
	}

	npf->pf_flags |= PF_UPDATE;

	/* Unlink scratch pfile descriptor and return dictionary space.
	 */
	pfileunlink (opf);
	topd = save_topd;

	return (OK);
}


/* PFILEUPDATE -- Update a parameter set in the pfile from which it was
 * originally read.  Nothing is done unless the parameter set has been
 * modified and needs updating, or if we have a fake (in-core) parameter set.
 */
void
pfileupdate (
    struct pfile *pfp		/* parameter file descriptor */
)
{
	if ((pfp->pf_flags & (PF_FAKE|PF_UPDATE)) != PF_UPDATE)
	    return;

	if (cldebug)
	    eprintf ("pfileupdate %s\n", pfp->pf_pfilename);
	
	/* Do not update the CL parameter file; we always read the system
	 * cl.par file upon startup.
	 */
	if (pfp->pf_ltp == firstask->t_ltp)
	    return;

	pfilewrite (pfp, pfp->pf_pfilename);
	pfp->pf_flags &= ~PF_UPDATE;

	if (pfp->pf_ltp)
	    pfp->pf_ltp->lt_flags |= LT_UPFOK;
}


/* PFILEREAD -- Allocate a pfile descriptor and read the named pfile into it.
 * The input file may be either a parameter file or a CL procedure script.
 */
struct pfile *
pfileread (
    struct ltask *ltp,		/* associated ltask		*/
    char *pfilename,		/* parameter file filename	*/
    int checkmode		/* check for "mode" parameter	*/
)
{
	register char	*ip;
	char	buf[SZ_LINE+1];
	struct	pfile *pfp;
	struct	param *pp;
	int	nerrs, gotmode, status, oldlines;
	FILE	*fp, *yysave;
	XINT	save_topd;

	if (cldebug)
	    eprintf ("pfileread, task %s, pfile %s\n",
		ltp ? ltp->lt_lname : "", pfilename);

	if ((fp = fopen (pfilename, "r")) == NULL)
	    return (NULL);

	save_topd = topd;
	pfp = newpfile (ltp);
	strcpy (pfp->pf_pfilename, pfilename);

	nerrs = 0;
	gotmode = 0;

	if (is_pfilename (pfilename)) {
	    /* Pfile has ".par" filename extension, format is a simple
	     * list of parameter structs, one parameter per line.
	     */
	    while (fgets (buf, PF_MAXLIN, fp) != NULL) {
		/* Skip comment lines and blank lines.
		 */
		for (ip=buf;  (*ip == ' ' || *ip == '\t');  ip++)
		    ;
		if (*ip == PF_COMMENT || *ip == '\n')
		    continue;

		if ((pp = addparam (pfp, ip, fp)) == NULL)
		    nerrs++;
		else if (!strcmp (pp->p_name, "mode")) {
		    if (gotmode) {
			eprintf ("more than one `mode' param\n");
			nerrs++;
		    } else
			gotmode++;
		}
	    }

	    /* When a pfile is udpated in uparm a "mode" parameter is
	     * always written out as the last parameter to mark the end of
	     * the parameter list.  If checkmode is enabled and the mode
	     * parameter is not seen, this indicates the the pfile has
	     * been truncated and should not be used.
	     */
	    if (nerrs > 0 || ferror(fp) || (checkmode && !gotmode))
		goto error_;

	} else if (procscript (fp)) {
	    extern int yyparse ();

	    /* Parse the declarations section of a procedure script.
	     * The procscript() call leaves us positioned to the procedure
	     * statement.
	     */
	    parse_state = PARSE_PARAMS;
	    parse_pfile = pfp;
	    yysave = yyin;
	    yyin = fp;

	    /* Fool the parser into believing we are at the
	     * beginning of a script for any error messages
	     * which come out.
	     */
	    oldlines = newtask->t_scriptln;
	    newtask->t_scriptln = 0;

	    status = yyparse();

	    /* Reset the parse state in case we are in a free script.  */
	    parse_state = PARSE_FREE;
	    newtask->t_scriptln = oldlines;
	    yyin = yysave;

	    if (status) 
		goto error_;

	    if (paramfind (pfp, "mode", 0, YES) == NULL)
		gotmode = NO;
	    else
		gotmode = YES;
	} else
	    goto error_;

	/* Count the number of parameters.  If there are no parameters we
	 * probably have a zero length file, which is an error.
	 */
	for (status=0, pp=pfp->pf_pp;  pp;  pp=pp->p_np)
	    status++;
	if (status == 0)
	    goto error_;

	/* Add `mode' param.  Get the value from the current package
	 * or from the CL if there is no package pfile.
	 */
	if (gotmode == 0) {
	    struct param *qq;

	    /* Allocate the param with "ql" as the ultimate default.
	     */
	    pp = addparam (pfp, "mode,s,h,ql\n", fp);

	    if (curpack != NULL) {
		if (curpack->pk_pfp != NULL) {
		    qq = paramfind (curpack->pk_pfp, "mode", 0, YES);
		    if (qq != NULL && qq != (struct param *)ERR) {
			strcpy (pp->p_val.v_s, qq->p_val.v_s);
			gotmode++;
		    }
		}
	    }
	}

	if (gotmode == 0)	/* CL--This should rarely be needed */
	    if (firstask->t_modep != NULL)
		strcpy (pp->p_val.v_s, firstask->t_modep->p_val.v_s);

	fclose (fp);
	return (pfp);

error_:
	fclose (fp);
	pfileunlink (pfp);
	topd = save_topd;
	return (NULL);
}


/* PFILEWRITE -- Write out the parameters for given pfile into a file.
 * Any existing file is silently clobbered.  The filename extension is
 * always ".par".
 */
int
pfilewrite (
    struct pfile *pfp,		/* pfile descriptor	*/
    char *pfilename		/* file to be written	*/
)
{
	register char	*ip, *op, *dot;
	char	pfname[SZ_PATHNAME+1];
	struct	param *pp;
	int	nparams;
	FILE	*fp;

	if (cldebug)
	    eprintf ("pfilewrite %s\n", pfilename);

	/* Copy the filename, changing the extension to .par if necessary.
	 */
	for (dot=NULL, ip=pfilename, op=pfname;  (*op = *ip++);  op++)
	    if (*op == '.')
		dot = op;
	strcpy (dot ? dot : op, ".par");

	if (cldebug)
	    eprintf ("writing pfile `%s'\n", pfname);

	/* Delete any existing pfile before updating.
	 */
	c_delete (pfname);

	/* Disable interrupts while updating the pfile to eliminate the
	 * possibility of file truncation.  The "mode" parameter is always
	 * written last to mark the end of a valid pfile.
	 */
	intr_disable();
	nparams = 0;

	if ((fp = fopen (pfname, "w")) == NULL)
	    eprintf ("Unable to open parameter file `%s'.\n", pfname);
	else {
	    struct param *modepp = NULL;
	    for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np) {
		if (!(pp->p_mode & M_LOCAL)) {
		    if (!strcmp (pp->p_name, "mode")) {
			modepp = pp;
		    } else if (printparam (pp, fp) == ERR) {
			fclose (fp);
			cl_error (E_IERR|E_P,
			    "Error writing local pfile `%s'", pfname);
		    } else
			nparams++;
		}
	    }

	    if (modepp) {
		printparam (modepp, fp);
		nparams++;
	    }
	    fclose (fp);
	}

	intr_enable();
	return (nparams);
}


/* PFILEINIT -- Initialize or "unlearn" a pfile.  Look for user version of
 * pfile in uparm; if found, delete it.  If pfile is loaded, unlink from
 * pfile list.  Fix up flag bits in ltask descriptor.  We are called from
 * "unlearn" to restore the package default parameters for an ltask or package.
 */
int
pfileinit (struct ltask *ltp)
{
	struct	task *tp;
	struct	pfile *pfp;
	char	pfilename[SZ_FNAME];	/* user pfile			*/
	char	pkgdir[SZ_FNAME+1];
	char	*ltname;		/* name of the new pfile	*/
	char	*pkname;		/* name of its package		*/
	int	running;

	if (cldebug)
	    eprintf ("unlearn pfile for task %s\n", ltp->lt_lname);

	ltname = ltp->lt_lname;
	pkname = ltp->lt_pkp->pk_name;

	/* Determine if the pfile belongs to a loaded package or to a task
	 * which is currently executing.
	 */
	running = 0;
	if (ltp->lt_flags & LT_DEFPCK)
	    running++;
	else {
	    for (tp=currentask;  tp <= firstask;  tp = next_task(tp))
		if (tp->t_ltp == ltp) {
		    running++;
		    break;
		}
	}

	/* Delete any "learned" copy of the pfile in uparm. */
	mkpfilename (pfilename, uparmdir, pkname, ltname, ".par");
	c_delete (pfilename);

	/* Clear the flag that says we have a valid user param file. */
	ltp->lt_flags &= ~(LT_UPFOK);

	/* See if the pfile is in core; if so, unlink all copies.  If the
	 * pfile belongs to a currently executing task we can't unlink it,
	 * so reset the parameter values to the system defaults instead.
	 */
	while ((pfp = pfilefind (ltp)) != NULL)
	    if (running) {
		c_fnldir (ltp->lt_pname, pkgdir, SZ_FNAME);
		mkpfilename (pfilename, pkgdir, pkname, ltname, ".par");
		pfilemerge (pfp, pfilename);
		pfp->pf_flags &= ~PF_UPDATE;
		if (ltp->lt_flags & LT_DEFPCK)
		    break;
	    } else
		pfileunlink (pfp);

	return (OK);
}


/* IS_PFILENAME -- Test whether a string is a pfile filename, i.e., whether
 * or not the string has a ".par" extension.
 */
int
is_pfilename (char *opstr)
{
	register char	*ip;
	char	*dot;

	/* If the named object has a ".par" extension we assume it is a
	 * pfile filename, otherwise we assume it is an ltask pathname.
	 */
	for (ip=opstr, dot=NULL;  *ip;  ip++)
	    if (*ip == '.')
		dot = ip;

	return (dot && strcmp (dot, ".par") == 0);
}


/* MKPFILENAME -- Generate a parameter file name, given a directory prefix
 * the names of the package and ltask, and the filename extension.  The form
 * of the filename depends upon whether the pfile is to be stored in UPARM.
 * UPARM pfile names have the form "uparm$ // pakltask.par", where `pak' is
 * the package prefix, consisting of the first LEN_PKPREFIX-1 characters of
 * the package name plus the final character, and `ltask' is the ltask name
 * squeezed to LEN_PFILENAME characters.  If not writing to UPARM, we just
 * use the full filename.
 */
void
mkpfilename (
    char *buf,			/* receives output filename	*/
    char *dir,			/* dir name or prefix		*/
    char *pkname,		/* package name			*/
    char *ltname,		/* ltask name			*/
    char *extn			/* filename extension		*/
)
{
	char	temp[SZ_FNAME+1];

	strcpy (buf, dir);		/* start with directory name	*/

	if (strcmp (dir, uparmdir) == 0) {
	    strcat (buf, "$");
	    mapname (pkname, temp, LEN_PKPREFIX);
	    strcat (buf, temp);
	    mapname (ltname, temp, LEN_PFILENAME);
	    strcat (buf, temp);
	} else
	    strcat (buf, ltname);

	strcat (buf, extn);		/* add extension for pfile	*/
}


/* MAPNAME -- Apply the N+1 mapping convention (first N-1 plus last chars)
 * to generate a name no longer than N characters.  Returns the number of
 * characters generated.
 */
static void
mapname (char *in, char *out, int maxlen)
{
	register int ip, op;

	ip = 0;
	op = 0;
	while (op < maxlen-1 && (out[op++] = in[ip++]) != '\0')
	    ;
	if (out[op-1] != '\0') {	/* append last char	*/
	    if (in[ip] != '\0') {
		while (in[ip] != '\0')
		    ip++;
		out[op++] = in[ip-1];
	    }
	    out[op++] = '\0';
	}
}


/* FILETIME -- Get the time of creation or of last modify of a file.  If the
 * file does not exist or cannot be accessed zero is returned.
 */
long
filetime (
    char *fname,			/* file name		*/
    char *timecode		/* "c" or "m"		*/
)
{
	struct	_finfo fi;

	if (c_finfo (fname, &fi) == ERR)
	    return (0L);
	else {
	    switch (*timecode) {
	    case 'c':
		return (fi.fi_ctime);
	    case 'm':
		return (fi.fi_mtime);
	    default:
		return (0L);
	    }
	}
}


/* PFILECOPY -- Make a new copy of paramfile at pfp for a new task.  Command
 * line changes, queries and assignments are done to this copy.  Link in the
 * usual fashion off parhead.  Copy all the parameters as well, taking care to
 * make new copies of strings and setting pointers in new params to their own
 * copies.  Return pointer to new entry; no error return.
 * Reset P_CLSET, P_SET and P_QUERY flags so pfcopyback() can tell whether
 *   these events happened for this particular run of the task.
 */
struct pfile *
pfilecopy (register struct pfile *pfp)
{
	register struct param *pp, *newpp;
	struct	pfile *newpfp;
	int	bastype;

	if (cldebug) {
	    if (pfp->pf_ltp)
		eprintf ("copying pfile for `%s'\n", pfp->pf_ltp->lt_lname);
	    else
		eprintf ("copying pfile `%s'\n", pfp->pf_pfilename);
	}

	newpfp = newpfile (pfp->pf_ltp);
	for (pp = pfp->pf_pp;  pp;  pp = pp->p_np) {

	    /* Allocate new parameter */
	    newpp = newparam (newpfp);
	    bastype = pp->p_type & OT_BASIC;

	    /* COPY VALUE */

	    newpp->p_valo = pp->p_valo;

	    /* Handle arrays. */
	    if (pp->p_type & PT_ARRAY) {
		struct arr_desc *parrd, *qarrd;
		int	size_arr;
		short	*lenoff, *qlenoff;
		int	dim, d, *pval, *qval;

		parrd = pp->p_val.v_a;
		dim = parrd->a_dim;
		size_arr = 1;

		lenoff = &(parrd->a_len) ;
		for (d=0;  d < dim;  d++)
		    size_arr *= *(lenoff + 2*d);
		if (bastype == OT_REAL)
		    size_arr *= 2;

		/* Ready to allocate new descriptor and data block */
		qarrd = (struct arr_desc *)memneed (2 + dim);
		newpp->p_val.v_a = qarrd;

		qarrd->a_ptr.a_i = (int *) memneed(size_arr);

		qarrd->a_dim = dim;
		qlenoff = &(qarrd->a_len);
		for (d=0; d<2*dim; d++)
		    *qlenoff++ = *lenoff++;

		if (bastype != OT_STRING) {
		    /* If not string then copy values across. */

		    pval = parrd->a_ptr.a_i;
		    qval = qarrd->a_ptr.a_i;
		    for (d=0;  d < size_arr;  d++)
			*qval++ = *pval++;

		} else {
		    /* Copy strings one by one. */

		    int     len;
		    char    **p, **q;

		    if (pp->p_maxo.o_type == OT_INT)
			len = pp->p_maxo.o_val.v_i;
		    else
			len = SZ_FNAME;

		    p = parrd->a_ptr.a_s;
		    q = qarrd->a_ptr.a_s;
		    for (d=0;  d < size_arr;  d++) {
			*q = memneed (btoi(len));
			strncpy (*q++, *p++, len-1);
			*(q+len-1) = '\0' ;
		    }
		}

	    } else if ((pp->p_valo.o_type & OT_BASIC) == OT_STRING) {
		/* Regular (i.e. scalar) strings.
		 */
		newpp->p_val.v_s = memneed (btoi(pp->p_lenval));
		strncpy (newpp->p_val.v_s, pp->p_val.v_s, pp->p_lenval-1);
	    }

	    /* COPY MIN */
	    newpp->p_mino = pp->p_mino;
	    if ((pp->p_mino.o_type & OT_BASIC) == OT_STRING &&
		!(pp->p_flags & P_UMIN)) {
		newpp->p_min.v_s = memneed (btoi (PF_SZMINSTR));
		strncpy (newpp->p_min.v_s, pp->p_min.v_s, PF_SZMINSTR-1);
	    }

	    /* COPY MAX */
	    newpp->p_maxo = pp->p_maxo;
	    if ((pp->p_maxo.o_type & OT_BASIC) == OT_STRING &&
		!(pp->p_flags & P_UMAX)) {
		newpp->p_max.v_s = memneed (btoi (PF_SZMAXSTR));
		strncpy (newpp->p_max.v_s, pp->p_max.v_s, PF_SZMAXSTR-1);
	    }

	    /* COPY PROMPT */
	    newpp->p_prompt = comdstr (pp->p_prompt);

	    /* Copy all the easy entries last; we made it! */
	    newpp->p_name = pp->p_name;
	    newpp->p_type = pp->p_type;
	    newpp->p_mode = pp->p_mode;
	    newpp->p_flags = pp->p_flags & ~(P_CLSET|P_QUERY|P_SET);
	    newpp->p_listfp = pp->p_listfp;
	    newpp->p_listval = pp->p_listval;
	    newpp->p_lenval = pp->p_lenval;
	}

	newpfp->pf_oldpfp = pfp;
	strcpy (newpfp->pf_pfilename, pfp->pf_pfilename);
	newpfp->pf_flags = (pfp->pf_flags & PF_PSETREF);
	newpfp->pf_flags |= PF_COPY;

	return (newpfp);
}


/* PFCOPYBACK -- Copy the contents of each param that is to be changed
 *   permanently in the given pfile to the corresponding param in original
 *   pfile.  Once thus copied, they are considered permanently changed since
 *   restor() will write out to their pfile.  Call the target pfile pft.
 * Copy only those params for which P_SET is set or for which P_QUERY or
 *   P_CLSET is set provided learn mode is on and the param is not M_HIDDEN.
 *   Since P_SET was cleared by pfilecopy(), it can only be set in the copy
 *   if it was set since the task started.
 * Set PF_UPDATE in pft if, in fact, any copying took place.
 * Don't copy at all if the working file is not a copy; this is primarily
 *   to stop the final copy on eof from the first cl and as a nice safety chk.
 * N.B. we assume pff was made from pft with pfilecopy() and so the params are
 *   in the same order; we also assume none were added.
 *
 * N.B. After copying, unlink the copy pfile from the pfile list, to insure
 *   that hidden params modified on the command line are not preserved after
 *   termination of a task which called KEEP.  Restor() will not lop off the
 *   dead pfile if it is below the new topd set by keep.
 */
void
pfcopyback (struct pfile *pff)
{
	register struct param *pt, *pf;
	struct	pfile *pft;
	int	bastype;
	int	pfflags;
	int	copy;			/* set if a real copy occurred	*/
	int	learn;			/* set if learn is on 		*/

	if (cldebug)
	    eprintf ("pfcopyback %s\n", pff->pf_pfilename);

	if (!(pff->pf_flags & PF_COPY))
	    return;
	pft = pff->pf_oldpfp;

	learn = effmode ((struct param *) NULL) & M_LEARN;
	copy = 0;

	for (pt=pft->pf_pp, pf=pff->pf_pp;  pf&&pt;  pt=pt->p_np, pf=pf->p_np) {
	    pfflags = pf->p_flags;

	    /* Always copy back the list file pointer else the list file, if
	     * opened during task execution, will not be closed.
	     */
	    pt->p_listfp = pf->p_listfp;

	    /* Copy param back if it was set in an explicit assignment,
	     * or if it was set in a query or on the command line, and we are
	     * in learn mode, and the parameter is not hidden.
	     */
	    if (!((pfflags & P_SET) || ((pfflags&(P_QUERY|P_CLSET)) && learn &&
		!(pf->p_mode & M_HIDDEN))))
		continue;

	    bastype = pt->p_type & OT_BASIC;
	    copy++;

	    /* Don't bother copying name since it couldn't have changed.
	     * Other fields copy directly.
	     */
	    pt->p_type = pf->p_type;
	    pt->p_mode = pf->p_mode;

	    /* Use all new flags bits but discard CLSET and QUERY and merge
	     * SET with its original state so either it or the copy can
	     * cause a permanent change to the parameter.
	     */
	    pt->p_flags &= P_SET;
	    pt->p_flags |= pfflags & ~(P_CLSET|P_QUERY);

	    /* Copy value */
	    pt->p_valo.o_type = pf->p_valo.o_type;

	    /* Handle arrays. */
	    /* The array descriptors should remain the same, only
	     * the stored values could change.
	     */
	    if (pt->p_type&PT_ARRAY) {
		int	dim, d, size_arr;
		short	*lenoff;

		/* Get size of array. */
		dim = pt->p_val.v_a->a_dim;
		lenoff = &(pt->p_val.v_a->a_len);
		size_arr = 1;
		if (bastype  ==  OT_REAL)
		    size_arr = 2;
		for (d=0; d<dim; d++)
		    size_arr *= *(lenoff + 2*d);

		if (bastype != OT_STRING) {
		    int	*p, *q;
		    p = pf->p_aval.a_i;
		    q = pt->p_aval.a_i;
		    for (d=0; d<size_arr; d++)
			*q++ = *p++;
		} else {
		    char **p, **q;
		    p = pf->p_aval.a_s;
		    q = pt->p_aval.a_s;
		    for (d=0; d<size_arr; d++)
			strcpy(*q++, *p++) ;
		}

	    } else if (!(pf->p_valo.o_type & (OT_INDEF|OT_UNDEF))) {
		if (((pf->p_valo.o_type & OT_BASIC) == OT_STRING) &&
		   (pt->p_val.v_s != NULL)) {
		    strncpy (pt->p_val.v_s, pf->p_val.v_s, pf->p_lenval-1);
		} else
		    pt->p_valo.o_val = pf->p_valo.o_val;
	    }
		
	    /* Copy min */
	    if (!(pf->p_flags & P_UMIN)) {
		pt->p_mino.o_type = pf->p_mino.o_type;
		if ((pf->p_mino.o_type & OT_BASIC) == OT_STRING &&
		    pt->p_min.v_s != NULL)
		    strncpy (pt->p_min.v_s, pf->p_min.v_s, PF_SZMINSTR-1);
		else
		    pt->p_mino.o_val = pf->p_mino.o_val;
	    }

	    /* Copy max */
	    if (!(pf->p_flags & P_UMAX)) {
		pt->p_maxo.o_type = pf->p_maxo.o_type;
		if ((pf->p_maxo.o_type & OT_BASIC) == OT_STRING &&
		    pt->p_max.v_s != NULL)
		    strncpy (pt->p_max.v_s, pf->p_max.v_s, PF_SZMAXSTR-1);
		else
		    pt->p_maxo.o_val = pf->p_maxo.o_val;
	    }
	}

	if (copy) {
	    if (cldebug) {
		if (pff->pf_ltp) {
		    eprintf ("copied back pfile for `%s'\n",
			pff->pf_ltp->lt_lname);
		} else
		    eprintf ("copied back pfile `%s'\n", pff->pf_pfilename);
	    }
	    pft->pf_flags |= PF_UPDATE;
	}

	/* Unlink pfile to ensure that it never gets reused.
	 */
	pfileunlink (pff);
}


/* ADDPARAM -- Allocate a new param off *pfp and fill with fields derived
 *   from line buf.
 * Buf should have trailing '\n' '\0' as per fgets.
 * Set UNDEF for those fields that are left blank, INDEF for those fields
 *   so indicating.
 * FP is used to read a structure, cursor, long quoted string, or arrays.
 * Return pointer to new param if ok, else NULL.  In order to handle multiple
 *   errors while reading a param file, we print informative info directly
 *   here with eprintf.  This avoids calling error() and gives us a chance
 *   to handle a file with multiple errors and find many of them in one pass.
 * Besides pfileread(), we are also called from various other places, such as
 *   execnewtask(), to add such parameters as $nargs and mode.
 */
struct param *
addparam (struct pfile *pfp, char *buf, FILE *fp)
{
	static char *minfields =
		"must specify at least name,type,mode for `%s'\n";
	static char *nominmax =
		"ranges not allowed for struct/cursor/string/bool param `%s'\n";
	static char *umquotes =
		"unmatched quotes in %s field for `%s'\n";

	register struct param *pp; /* new param being filled up		*/
	register char *s;	/* pointer to compiled string.		*/
	char	*pnamehold;	/* param's name as soon as we know it	*/
	int	len;		/* used to measure string lengths	*/
	int	bastype;	/* OT_BASIC part of type as soon as know*/
	int	arrflag;	/* Is param an array?			*/
	struct arr_desc *parrd;	/* Pointer to array descriptor.		*/
	int	size_arr=0;	/* Size of array.			*/
	extern double atof();
	char	**tbuf;

	pp = newparam (pfp);

	/* P_NAME */

	pnamehold = "<no name>";
	tbuf = &buf;
	if ((s = nextfield (tbuf, fp)) == NULL) {
	    eprintf (minfields, pnamehold);
	    return (NULL);
	} else if (s == (char *)ERR) {
	    eprintf (umquotes, "name", pnamehold);
	    return (NULL);
	} else
	    pnamehold = pp->p_name = s;


	/* P_TYPE */

	if ((s = nextfield (tbuf, fp)) == NULL) {
	    eprintf (minfields, pnamehold);
	    return (NULL);
	} else if (s == (char *)ERR) {
	    eprintf (umquotes, "type", pnamehold);
	    return (NULL);
	} else {
	    if (strcmp (s, "pset") == 0)
		pfp->pf_flags |= PF_PSETREF;
	    if ((pp->p_type = scantype (s)) == ERR) {
		eprintf (" in `%s'\n", pnamehold);
		return (NULL);
	    }
	}
	bastype = pp->p_type & OT_BASIC;
	arrflag = pp->p_type & PT_ARRAY;

	/* P_MODE */

	if ((s = nextfield (tbuf, fp)) == NULL) {
	    eprintf (minfields, pnamehold);
	    return (NULL);
	} else if (s == (char *)ERR) {
	    eprintf (umquotes, "mode", pnamehold);
	    return (NULL);
	} else if ((pp->p_mode = scanmode (s)) == ERR) {
	    eprintf (" in `%s'\n", pnamehold);
	    return (NULL);
	}


	/* P_VAL */

	pp->p_valo.o_type = bastype;

	if ((s = nextfield (tbuf, fp)) == (char *)ERR) {
	    eprintf (umquotes, "value", pnamehold);
	    return (NULL);
	}

	if (pp->p_type & (PT_LIST|PT_FILNAM|PT_PSET)) {
	    pp->p_val.v_s = memneed (btoi(SZ_FNAME));
	    pp->p_val.v_s[SZ_FNAME-1] = '\0';
	    pp->p_lenval = SZ_FNAME;

	    if (pvaldefined (pp, s)) {
	        char  *p;

		/* Change a whitespace-only filename into a null string; this
		 * makes it easier for users to check null filenames in 
		 * scripts.  It makes sense anyway since these are invalid
		 * filenames.
		 */
		p = s;
	        while (*p == ' ' || *p == '\t')
		    p++;
		if (*p == '\0' || *p == '\n')
		    pp->p_val.v_s[0] = '\0';
		else
		    strncpy (pp->p_val.v_s, s, SZ_FNAME-1);
	    } else
		*pp->p_val.v_s = '\0';

	    if (pp->p_type & PT_LIST)
		pp->p_listval = memneed (btoi(SZ_LINE));
	    pp->p_valo.o_type = OT_STRING;

	} else if (pp->p_type & (PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY)) {

	    /* Non-list structs read next line and store at p_val.v_s
	     *   unless the length field begins with a PF_NOSTRUCT.
	     * The storage allocated in the dictionary, and pointed to by
	     *   p_val.v_s, is the max of the number in the value field and
	     *   the length of the structure init string, on the next line.
	     *   it is an error for the init string to be longer than the
	     *   length given, if any, or for either to be greater than
	     *   SZ_LINE-2.
	     * SZ_LINE-2 is the default length if neither a len not init is
	     *   given.
	     * OT_INDEF/UNDEF refer to p_val; p_lenval always set to length
	     *   (max length) of value string if value is a string.
	     * Nextfield() compiles the length spec into the dictionary;
	     *   it's short and not worth trying to dig out...
	     */

	    int readinit = 0;  		 	/* 1 if init is in next ln */

	    if (s == NULL) {
		readinit++;
		len = SZ_LINE-1;			/* supply default */
	    } else {
		if (*s == PF_NOSTRUCT)
		    s++;
		else
		    readinit++;

		len = atoi (s);
		if (len <= 0)
		    len = SZ_LINE-1;			/* supply default */
		else if (len > SZ_LINE-1) {
		    eprintf ("`%s' struct lengths limited to %d\n",
			pnamehold, SZ_LINE-1);
		    return (NULL);
		}
	    }
	    len++;				/* allow for \0 */

	    if (readinit) {
		/* Initialize with next line.  Lots of pathology here...
		 */
		char initbuf[SZ_LINE];
		int initlen;

		if (fgets (initbuf, SZ_LINE, fp) == NULL) {
		    eprintf ("`%s' has no initialized\n",
			pnamehold);
		    return (NULL);
		}

		initlen = strlen (initbuf); /* includes \n, if present */

		if (initbuf[initlen-1] == '\n')
		    initbuf[initlen-1] = '\0';
		else {
		    int c;
		    eprintf ("`%s' initialization too long\n",
			pnamehold);
		    while ((c = fgetc(fp)) != '\n' && c != EOF)
			;
		    return (NULL);
		}

		if (initlen > len) {
		    eprintf ("initialization for `%s' > %d\n",
			pnamehold, len-1);
		    return (NULL);
		}

		pp->p_val.v_s = memneed (btoi (len));
		if (pvaldefined (pp, initbuf))
		    strcpy (pp->p_val.v_s, initbuf);

	    } else {
		/* Allocate space but don't init from next line.
		 */
		pp->p_val.v_s = memneed (btoi (len));
	    }

	    pp->p_val.v_s[len-1] = '\0';	 /* the permanent eos */
	    pp->p_lenval = len;
	    pp->p_valo.o_type = OT_STRING;

	} else if ((bastype == OT_STRING ||
	    (s != NULL && *s == PF_INDIRECT)) && !arrflag) {

	    /* Strings are stored like structs, but are inited from s.
	     * OT_INDEF/UNDEF refer to p_val.
	     */
	    if (pvaldefined (pp, s)) {
		/* String was something conventional.  If shorter than SZ_LINE
		 * call memneed() again to increase the dictionary space. This
		 * ASSUMES that nothing called memneed() since nextfield() did.
		 */
		pp->p_valo.o_type = OT_STRING;
		len = strlen (s) + 1;		/* allow for eos */
		if (len < SZ_LINE) {
		    memneed (btoi(SZ_LINE) - btoi(len));
		    len = SZ_LINE;
		}
	    } else {
		/* Either no string was given or it was INDEF/UNDEF.
		 */
		len = SZ_LINE;
		s = memneed (btoi (len));
	    }

	    pp->p_val.v_s = s;
	    pp->p_val.v_s[len-1] = '\0'; /* add the permanent eos */
	    pp->p_maxo.o_type = OT_INT;
	    pp->p_lenval = len;

	} else if (arrflag) {
	    /* For arrays get the array definition block */

	    int		dim, it;	/* Dimensionality of array.	*/
	    short	itemp;		/* Length and offsets of array.	*/
	    short	*lenoff;	/* Pointer to length or offset. */
	    int		d;

	    /* Dimensionality. */
	    if (s == NULL) {
		eprintf ("Dimensionality not specified for %s.\n", pnamehold);
		return (NULL);
	    }
	    if (ck_atoi (s, &dim) == ERR) {	/* Convert to integer. */
		eprintf ("Non-integer dimensionality for %s.\n", pnamehold);
	        return (NULL);
	    }
	    if (dim <= 0) {			/* Dimensionality > 0 ? */
		eprintf ("Dimensionality not positive for %d.\n", pnamehold);
		return (NULL);
	    }

	    /* Get space for array descriptor. */
	    parrd = (struct arr_desc *) memneed (2 + dim);
	    size_arr = 1;
	    if (bastype == OT_REAL)		/* Doubles take 2 INT's. */
		size_arr = 2;
	    
	    parrd->a_dim = dim;
	    lenoff = &(parrd->a_len);
	    

	    /* Lengths and offsets.
	     */
	    for (d=0;  d < 2*dim;  d++) {
		if ((s = nextfield (tbuf, fp)) == NULL) {
		    eprintf ("Dimensions not specified for %s.\n", pnamehold);
		    return (NULL);
		}

		if (ck_atoi (s, &it) == ERR) {	/* Convert to integer. */
		    eprintf ("Integer length/offset required for %s.\n",
			pnamehold);
		    return (NULL);
	        }

		itemp = it;
		if ((d%2 == 0)  &&  itemp<=0) {/* Length < 0 ? */
		    eprintf ("Illegal negative dimension for %s.\n", pnamehold);
		    return (NULL);
		}

		*lenoff++ = itemp;
		if (d%2 == 0)
		    size_arr = itemp * size_arr;

	    }
	    /* Get the space for the array. */
	    parrd->a_ptr.a_i = (int *) memneed(size_arr);

	    /* The "value" of the parameter is a pointer to the
	     * array descriptor.
	     */
	    pp->p_valo.o_val.v_a = parrd;
	    pp->p_valo.o_type = PT_ARRAY|bastype;

	} else {
	    /* Simple non-string type.
	     */
	    if (pvaldefined (pp, s))
		pp->p_valo = makeop (s, pp->p_type & OT_BASIC);
	}


	/* P_MIN */

	pp->p_mino.o_type = bastype;

	if ((s = nextfield (tbuf, fp)) == (char *)ERR) {
	    eprintf (umquotes, "minimum", pnamehold);
	    return (NULL);
	}

	if (s != NULL && *s != '\0') {
	    if (bastype == OT_BOOL ||
		pp->p_type & (PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET)) {
		eprintf (nominmax, pnamehold);
		return (NULL);
	    } else if (!strcmp(s,indefstr) || !strcmp(s,indeflc)) {
		pp->p_flags |= P_IMIN; 
	    } else if (bastype == OT_STRING || *s == PF_INDIRECT) {
		/* Filename, enumerated string, or indirect reference.
		 */
		pp->p_mino.o_type = OT_STRING;
		pp->p_min.v_s = memneed (btoi(PF_SZMINSTR));
		pp->p_min.v_s[PF_SZMINSTR-1] = '\0';
		strncpy (pp->p_min.v_s, s, PF_SZMINSTR-1);
	    } else {
		/* Type is equivalent to a simple non-string wrt mins.
		 */
		pp->p_mino = makeop (s, pp->p_type & OT_BASIC);
	    }
	} else
	    pp->p_flags |= P_UMIN;


	/* P_MAX */

	pp->p_maxo.o_type = bastype;

	if ((s = nextfield (tbuf, fp)) == (char *)ERR) {
	    eprintf (umquotes, "maximum", pnamehold);
	    return (NULL);
	}

	if (s != NULL && *s != '\0') {
	    if (bastype == OT_BOOL ||
		pp->p_type & (PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET)) {
		eprintf (nominmax, pnamehold);
		return (NULL);
	    } else if (!strcmp(s,indefstr) || !strcmp(s,indeflc)) {
		pp->p_flags |= P_IMAX;
	    } else if (bastype == OT_STRING || *s == PF_INDIRECT) {
		/* Filename, enumerated string, or indirect reference.
		 */
		pp->p_maxo.o_type = OT_STRING;
		pp->p_max.v_s = memneed (btoi(PF_SZMAXSTR));
		pp->p_max.v_s[PF_SZMAXSTR-1] = '\0';
		strncpy (pp->p_max.v_s, s, PF_SZMAXSTR-1);
	    } else {
		/* Type is equivalent to a simple non-string wrt mins.
		 */
		pp->p_maxo = makeop (s, pp->p_type & OT_BASIC);
	    }
	} else
	    pp->p_flags |= P_UMAX;


	/* P_PROMPT */

	if ((s = nextfield (tbuf, fp)) == (char *)ERR) {
	    eprintf (umquotes, "prompt", pnamehold);
	    return (NULL);
	}

	pp->p_prompt = (s == NULL) ? nullstr : s;


	/* ARRAY INITIALIZATION */

	if (arrflag) {
	    int     i, len=0;

	    /* First initialize all fields, since we do not
	     * require initialization of the entire array.
	     */
	    if (bastype == OT_BOOL || bastype == OT_INT) {
		int	*p;
		p = pp->p_aval.a_i;
		for (i=0;  i < size_arr;  i++)
		    *p++ = INDEFL;

	    } else if (bastype == OT_REAL) {
		double	*p;
		size_arr = size_arr / 2;
		p = pp->p_aval.a_r;
		for (i=0;  i < size_arr;  i++)
		    *p++ = INDEFR;

	    } else {				/* Strings. */
		char	**p;

		/* Check if max_length specified in p_max.
		 */
		if (pp->p_maxo.o_type == OT_INT)
		    len = pp->p_max.v_i;
		else
		    len = SZ_FNAME;
		pp->p_lenval = len;

		/* Set up indef strings.
		 */
		p = pp->p_aval.a_s;
		for (i=0;  i < size_arr;  i++) {
		    *p = (char *) memneed (btoi (len) );
		    strcpy(*p, INDEFSTR);
		    *(*p + len - 1) = '\0';
		    p++;
		}
	    }

	    /* Now get any initialization which may be present.
	     * If we reach the end of the parameter before the
	     * array is filled it is not an error and the
	     * values are left with defaults. Values can be
	     * skipped with successive commas.
	     */
	    for (i=0; i<size_arr; i++) {
		if ((s = nextfield (tbuf, fp)) == NULL)
		    break;
		if (s == (char *) ERR) {
		    eprintf (umquotes, pnamehold);
		    return (NULL);
		}

		/* If the field was empty a pointer to the external
		 * string undefval was returned.
		 */
		if (s == undefval)
		    continue;

		if (bastype == OT_BOOL) {
		    makelower(s);
		    if (strcmp(s, "no") )
			*(pp->p_aval.a_i + i) = 1;
		    else
			*(pp->p_aval.a_i + i) = 0;
		} else if (bastype == OT_INT) {
		    *(pp->p_aval.a_i + i) = atoi(s);
		} else if (bastype == OT_REAL) {
		    *(pp->p_aval.a_r + i) = atof(s);
		} else {
		    char *dest;
		    dest = *(pp->p_aval.a_s + i) ;
		    strncpy (dest, s, len-1);
		}
	    }
	}

	/* Is there still more.
	 */
	if (nextfield (tbuf, fp) != NULL) {
	    eprintf ("too many fields for `%s'\n", pnamehold);
	    return (NULL);
	}

	/* Got through whole line without errors.
	 */
	return (pp);
}


/* CK_ATOI -- Check a string for non-numerics before conversion.
 */
int
ck_atoi (char *str, int *val)
{
	char	*s;

	s = str;
	while (*s == ' ' || *s == '\t')
	    s++;

	if (*s == '-')
	    s++;

	while (*s)
	   if (!isdigit(*s++))
		return (ERR);

	*val = atoi(str);
	return (0);
}


/* NEXTFIELD -- Compile the next field of a paramfile line into the dictionary
 *   and return a pointer to the new entry.
 * PP is the address of a pointer to the start of a param field. skip leading
 *   blanks and handle quoted strings. strings ending in \ are continued after
 *   absorbing both the \ and the newline. strings ending with just newlines
 *   will contain the newline. the string may be delimited by ' or ".
 * The callers pointer, *pp, will be set to the beginning of the next field.
 * FP is a file pointer, needed if the field is quoted and extends to another
 *   lines.
 * The field must be part of a line read with fgets (buf, SZ_LINE, fp); we
 *   rely on the max length as well as the trailing \n\0 sequence.
 * Return NULL if no further fields, ERR if don't find closing quote,
 *   else pointer to field as compiled in dictionary.  If the field was
 *   empty return a pointer to the string "undefval".
 */
char *
nextfield (char **pp, FILE *fp)
{
	static char readbuf[SZ_LINE];
	register char c, *p;	/* fast references to field	 	*/
	char	buf[SZ_LINE];	/* working scratch buffer		*/
	char	*bp = buf;	/* pointer into scratch buffer		*/
	char	*start = NULL;	/* start of compiled string in dictnry	*/
	char	quote;		/* set to opening quote; go until match	*/

	p = *pp;
	if (p == NULL)
	    return (NULL);

	/* Skip white space at beginning.  This may include one or
	 * more newlines if they are prefixed by a '\\'.
	 */
	forever {
	    while (*p == ' ' || *p == '\t') 
		p++;
	    if (*p == '\\' && *(p+1) == '\n') {
		if (fgets (readbuf, SZ_LINE, fp) == NULL)
		    return ((char *) ERR);
		p = readbuf;
		continue;
	    } else
		break;
	}

	c = *p;

	if (c == '\0' || c == '\n') {
	    *pp = NULL;
	    return (NULL);
	}

	if (c == '\'' || c == '"') {
	    quote = c;
	    p++;

	    forever {
		c = *p++;
		if (c == '\n') {
		    *bp++ = c;
		    continue;
		} else if (c == '\\') {
		    switch (c = *p++) {
		    case '\n':
			continue;
		    case 'n':
			*bp++ = '\n';
			break;
		    case 't':
			*bp++ = '\t';
			break;
		    case 'r':
			*bp++ = '\r';
			break;
		    case 'f':
			*bp++ = '\f';
			break;
		    case '\'':
		    case '"':
			*bp++ = c;
			break;
		    default:
			*bp++ = '\\';		/* preserve esc seq.	*/
			*bp++ = c;
			break;
		    }
		} else if (c == '\0' || c == quote) {
		    *bp = '\0';
		    if (start == NULL)
			start = comdstr (buf);
		    else
			catdstr (start, buf);

		    if (c == quote)
			break;
		    else {
			if (fgets (readbuf, SZ_LINE, fp) == NULL)
			    return ((char *)ERR);
			p = readbuf;
			bp = buf;
		    }
		} else
		    *bp++ = c;
	    }
	    *bp++ = '\0';

	    /* Skip any white space.  We assume that we needn't skip
	     * lines here.
	     */
	    while (*p == ' ' || *p == '\t')
		p++;

	    c = *p;

	} else {
	    /* Unquoted string.
	     * Changed 2/15/85 by TAM.
	     * This code is no longer seen by quoted strings
	     */
	    while (*p != '\0' &&  *p != '\n' &&  *p != ',' && *p != '#') {
		c = *p;

		/* Allow multi-line definitions by ignoring newlines
		 * prefixed by backslash.
		 */
	 	if (c == '\\' && *(p+1) == '\n') {
		    if (fgets (readbuf, SZ_LINE, fp) == NULL)
			return ((char *)ERR);
		    p = readbuf;
		    continue;
		} else
		    *bp++ = c;

		p++;
	    }
	}

	/* Get rid of comments after the field. */
	if (*p == '#')
	    while (*p != '\0')
		p++;

	c = *p;

	/* At this point we must be at a field terminator, i.e.
	 * comma, newline or null.
	 */
	if (c != ',' && c != '\n' && c != '\0')
	    return ((char *)ERR);


	/* if stopped due to \n or , skip over it.
	 * set caller's pointer to start of next field.
	 * if we've not already compiled a string, compile this field.
	 */
	if (c == '\n' || c == ',')
	    p++;

	if (start == NULL) {
	    if (bp == buf) {
		/* The field was empty (i.e., ",,").  Return point to the
		 * null string "undefval" to flag value as undefined.
		 */
		start = undefval;
	    } else {
		*bp = '\0';
		start = comdstr (buf);
	    }
	    *pp = p;
	} else if (*pp != NULL)
	    *pp = p;

	return (start);
}


/* MAKELOWER -- Convert, in-place, any upper case characters in the string
 * cp to lower.  Using isupper and tolower is fast and portable, but making
 * simple range test and subtraction will save the table space if you know
 * you have ASCII.
 */
char *
makelower (register char *cp)
{
	char *start = cp;
	register char c;

	while ((c = *cp) != '\0') {
	    if ('A' <= c && c <= 'Z')
		*cp = c + ('a' - 'A');
	    cp++;
	}

	return (start);
}


/* SCANMODE -- Read through string s and build up an int full of M_XXX type
 *   mode bits.  Return it if ok, else ERR.
 * We write a diagnostic with eprint() if ERR but not a '\n' so
 *   caller can include more info if necessary.
 * N.B. we assume ERR doesn't map into a reasonable set of flags.
 */
int
scanmode (char *s)
{
	register int mode = 0;
	register char *str, *ip, *op;
	static	char *badstr = "bad mode string `%s'";
	char	strings[4][25];
	int	i, n;
	char *index();

	str = s;
	if (index (str, ',') != NULL || index (str, '+') != NULL) {
	    if (*str == '"' || *str == '\'')
		str++;

	    /* Break str into alpha strings separated by '+', ' ', or ','.
	     * We will not see any more than 4 such strings.
	     */
	    for (n=0, ip=str;  n < 4;  n++) {
		while (*ip == ' ' || *ip == '\t')
		    ip++;
		for (op=strings[n];  (*op = *ip++) != '\0';  op++)
		    if (!isalpha (*op)) {
			*op = '\0';
			break;
		    }
	    }
	    if (n == 0 || n == 5) {
		eprintf (badstr, str);
		return (ERR);
	    }

	    for (i=0;  i < n;  i++) {
		str = strings[i];
		makelower (str);
		if (!strcmp (str, "auto") || !strcmp (str, "a"))
		    mode |= M_AUTO;
		else if (!strcmp (str, "hidden") || !strcmp (str, "h"))
		    mode |= M_HIDDEN;
		else if (!strcmp (str, "learn") || !strcmp (str, "l"))
		    mode |= M_LEARN;
		else if (!strcmp (str, "query") || !strcmp (str, "q"))
		    mode |= M_QUERY;
		else if (!strcmp (str, "menu") || !strcmp (str, "m"))
		    mode |= M_MENU;
		else {
		    eprintf (badstr, str);	
		    return (ERR);
		}
	    }

	} else {
	    for (ip=str;  *ip != '\0';  ip++) {
		/* Handle the case of a set of qlha run together, as in
		 * a parameter file spec.
		 */
		switch (*ip) {
		case PF_AUTO: case PF_AUTO - ('a' - 'A'):
		    mode |= M_AUTO;
		    break;
		case PF_HIDDEN: case PF_HIDDEN - ('a' - 'A'):
		    mode |= M_HIDDEN;
		    break;
		case PF_LEARN: case PF_LEARN - ('a' - 'A'):
		    mode |= M_LEARN;
		    break;
		case PF_QUERY: case PF_QUERY - ('a' - 'A'):
		    mode |= M_QUERY;
		    break;
		case PF_MENU: case PF_MENU - ('a' - 'A'):
		    mode |= M_MENU;
		    break;
		default:
		    eprintf ("Bad mode spec `%c' in `%s'\n", *ip, str);
		    return (ERR);
		}
	    }
	}

	return (mode);
}


/* SCANTYPE -- Read through string s and build up an int full of OT_XXX and
 * PT_XXX type bits.  Return it if ok, else ERR.
 * OT_ bits are not unique so be a bit carefile.
 * we write a diagnostic with eprint() if ERR but not a '\n' so
 *   caller can include more info if necessary.
 * N.B. hope ERR doesn't map into a reasonable set of flags.
 */
int
scantype (register char *s)
{
	static char *badtype =  "bad type spec `%c'";
	static char *cnfltype = "conflicting type spec `%c'";
	register int type;

	type = 0;

	if (*s == '*') {
	    type |= PT_LIST;
	    s++;
	}

	if (*s == 'a' || *s == 'A') {
	    if (type & PT_LIST) {	/* No list structured arrays. */
		eprintf (cnfltype, *s);
		return (ERR);
	    }
	    s++;
	    type |= PT_ARRAY;
	}

	if (s[1] == '\0') {
	    switch (*s) {
	    case 'b': case 'B': type |= OT_BOOL; break;
	    case 'i': case 'I': type |= OT_INT; break;
	    case 'r': case 'R': type |= OT_REAL; break;
	    case 's': case 'S': type |= OT_STRING; break;
	    case 'f': case 'F': type |= (PT_FILNAM|OT_STRING); break;
	    default: eprintf (badtype, *s);
		return (ERR);
	    }

	} else if (*s == 'f') {
	    type |= (PT_FILNAM + OT_STRING);
	    while (*++s != '\0')
		switch (*s) {
		case 'b': case 'B': type |= PT_FBIN; break;
		case 'n': case 'N': type |= PT_FNOE; break;
		case 'r': case 'R': type |= PT_FER; break;
		case 't': case 'T': type |= PT_FTXT; break;
		case 'w': case 'W': type |= PT_FEW; break;
		default: eprintf (badtype, *s);
		    return (ERR);
		}
	} else if (!strcmp (makelower (s), "struct")) {
	    type |= (PT_STRUCT|OT_STRING);
	} else if (!strcmp (makelower (s), "gcur")) {
	    type |= (PT_GCUR|OT_STRING);
	} else if (!strcmp (makelower (s), "imcur")) {
	    type |= (PT_IMCUR|OT_STRING);
	} else if (!strcmp (makelower (s), "ukey")) {
	    type |= (PT_UKEY|OT_STRING);
	} else if (!strcmp (makelower (s), "pset")) {
	    type |= (PT_PSET|OT_STRING);
	} else {
	    eprintf (badtype, *s);
	    return (ERR);
	}

	return (type);
}
