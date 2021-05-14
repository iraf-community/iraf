/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_ctype
#include <iraf.h>

#include "config.h"
#include "clmodes.h"
#include "construct.h"
#include "operand.h"
#include "param.h"
#include "grammar.h"
#include "mem.h"
#include "task.h"
#include "errs.h"
#include "proto.h"


/*
 * MODES -- Handle the parameter mode operations, such as determining effective
 *   mode, checking if in range and queries.
 * Also handle the global modes of the cl, such as abbreviations, menus, and
 *   logging.  Macro defns for all but abbreviations are in clmodes.h; it is
 *   involved enough to be a real function in this file.
 */

#define INIT_DELAY	3		/* sleep params, bkg_query()	*/
#define	DELAY_MULT	1.4
#define	MAXDELAY	(60*5)		/* sleep at most 5 minutes	*/
#define	BKQ_TIMEOUT	(60*60*3)	/* time out after 3 hours	*/
#define	SZ_PROMPTBUF	SZ_LINE		/* avoid string overflow	*/

extern	int cldebug;
extern	char *eofstr;
extern	int bkgno;		/* our job number, if background	*/
extern	int ppid;		/* parent's pid, if background		*/

/* These are set, by setclmodes(), right after the cl's pfile is read. there
 *   is one for each special-function cl parameter.
 * Once set, they are used by the macros in clmodes.h to efficiently determine
 *   the various function settings yet allow them to remain normal parameters.
 */
struct	param *clabbrev;	/* allow abbreviations?			*/
struct	param *clmenus;		/* display tasks in curpack with prompt?*/
struct	param *clshowtype;	/* display task type in menus		*/
struct	param *clkeeplog;	/* keep all input in logfile?		*/
struct	param *cllexmodes;	/* enable lexical mode switching	*/
struct	param *cllogfile;	/* name of the logfile			*/
struct	param *clnotify;	/* notify parent when bkg task is done	*/
struct	param *clecho;		/* echo commands from scripts on stderr	*/
int	cllogmode = LOG_COMMANDS;	/* Logging control flag */


/* Calculate the effective mode for the given parameter, considering
 *   its own mode and the modes for the current task and the cl.
 *   Inhibit query mode if set on the command line or hidden but
 *   enable it if the param is not in range.  The range test cannot be done
 *   here for list params because we'd have to read the list to do it.
 * Return a bit-mapped code (built up of M_XXX bits) of the result.
 * Since learn mode is not defined at the parameter level, pp == NULL
 *   is used to indicate we are just interested in M_LEARN info.
 * Local variables cannot be prompted for so it is an error if their
 *   values are undefined.
 */
int
effmode (struct param *pp)
{
	static	char	*localerr =
		"Attempt to access undefined local variable `%s'.\n";

	register int	mode, modebits;
	struct	operand o;
	int	clmode, ltmode, pkmode, offset;
	int	interactive;

	/* Check if param is a local variable.  If it is undefined
	 * this is an ERR, if defined just return mode 0 to defeat
	 * querying.
	 */
	if (pp != NULL) {
	    if (pp->p_mode & M_LOCAL) {
		if (opundef (&(pp->p_valo)))
		    cl_error (E_UERR, localerr, pp->p_name);
		return (0);
	    }
	}

	/* Determine whether or not the current task was called interactively.
	 * Menu mode is only permitted for tasks called interactively.
	 */
	interactive = 0;
	if (prevtask)
	    interactive = (prevtask->t_flags & (T_INTERACTIVE|T_BATCH));
	if (interactive)
	    modebits = (M_QUERY|M_HIDDEN|M_MENU);
	else
	    modebits = (M_QUERY|M_HIDDEN);

	clmode = scanmode (firstask->t_modep->p_val.v_s);
	ltmode = scanmode (currentask->t_modep->p_val.v_s);
	pkmode = -1;

	mode = 0;
	if (pp != NULL) {
	    /* In determining the effective mode we go up the hierarchy of
	     * parameter, task, package, cl.  The mode is taken from the first
	     * of these which is not automatic.
	     */
	    if ( (mode = (pp->p_mode & modebits)) )
		;
	    else if ( (mode = (ltmode & modebits)) )
		;
	    else {
		/* Check the mode of the package to which the ltask belongs,
		 * which need not be the "current" package.
		 */
		struct pfile *pfp;

		if ( (pfp = currentask->t_ltp->lt_pkp->pk_pfp) ) {
	 	    struct param   *ppx;
	 	    ppx = paramfind (pfp, "mode", 0, YES);
	 	    if ((ppx != NULL)  &&  (ppx != (struct param *)ERR))
	 		pkmode = scanmode (ppx->p_val.v_s);
	 	}

		if (pkmode > 0 && (mode = (pkmode & modebits)))
		    ;
		else if ( (mode = (clmode & modebits)) )
		    ;
		else
		    mode = M_AUTO;
	    }

	    /* Defeat query mode if param set on command line or it's a
	     * hidden param or if menu mode is in effect.
	     */
	    if ((pp->p_flags & P_CLSET) || (pp->p_mode & M_HIDDEN) ||
		(mode & M_MENU))
		mode &= ~M_QUERY;

	    /* Query unconditionally if param is out of range or undefined.
	     */
	    if (!(mode & M_QUERY) && !(pp->p_type & PT_LIST)) {

		/* To check whether an array element is in range we
		 * must get the appropriate element of the array.  However
		 * the stack must be reset so that the element can be accessed
		 * again by the calling routine.
		 */
		if (pp->p_type & PT_ARRAY) {
		    offset = getoffset(pp);

		    poffset (offset);
		    paramget(pp, FN_VALUE);

		    poffset (offset);

		    o = popop();
		    if (!inrange (pp, &o))
			mode |= M_QUERY;

		} else {
		    /* Use temporary scratch variable for range checking in
		     * this case; sometimes the value of an enumerated
		     * parameter would get trashed in the process.  There is
		     * probably some deeper, darker bug lurking down there,
		     * but haven't found it yet, so this will suffice for now.
		     */
		    o = pp->p_valo;
		    if (!inrange (pp, &o))
			mode |= M_QUERY;
		}
	    }
	}

	/* Enable learn mode only for tasks called interactively - don't bother
	 * to learn parameters if the task is called from a script or in batch
	 * mode.
	 */
	if (interactive)
	    mode |= (clmode & M_LEARN) | (ltmode & M_LEARN);

	return (mode);
}


/* TASKMODE -- Determine the effective mode for a task.
 */
int
taskmode (register struct task *tp)
{
	register int	modebits, mode;
	struct	pfile *pfp;
	int	clmode, pkmode, ltmode;
	int	interactive, learn;

	/* Determine whether or not the task was called interactively.
	 * Menu mode is only permitted for tasks called interactively.
	 */
	interactive = 0;
	if (next_task(tp))
	    interactive = (next_task(tp)->t_flags & (T_INTERACTIVE|T_BATCH));
	if (interactive)
	    modebits = (M_QUERY|M_HIDDEN|M_MENU);
	else
	    modebits = (M_QUERY|M_HIDDEN);

	ltmode = scanmode (tp->t_modep->p_val.v_s);
	clmode = scanmode (firstask->t_modep->p_val.v_s);
	learn  = ((ltmode|clmode) & M_LEARN);

	/* If the mode of the task is anything but AUTO we are done.
	 */
	if ( (mode = (ltmode & modebits)) )
	    if (interactive || !(mode & M_MENU))
		return (mode|learn);

	/* If the package to which the task belongs has a pfile and the mode
	 * of the package is anything but AUTO, we are done.
	 */
	if ( (pfp = tp->t_ltp->lt_pkp->pk_pfp) ) {
	    struct param   *ppx;

	    pkmode = ERR;
	    ppx = paramfind (pfp, "mode", 0, YES);
	    if ((ppx != NULL) && (ppx != (struct param *)ERR))
		pkmode = scanmode (ppx->p_val.v_s);

	    if (pkmode != ERR && (mode = (pkmode & modebits)))
		if (interactive || !(mode & M_MENU))
		    return (mode|learn|(pkmode&M_LEARN));
	}

	/* Return the CL mode (menu mode not permitted at the CL level).
	 */
	return (clmode);
}


/* QUERY -- Query the user for the value of a parameter.  Prompt with the
 *  current value if any.  Keep this up until we can push a reasonable value.
 *  Also, store the new value in the parameter (except for list params, where,
 *  since the values are not kept, all that may change is P_LEOF if seen).
 * Give prompt, or name if none, current value and range if int, real or 
 *   filename.  Accept CR to leave value unchanged, else take the string
 *   entered to be the new value.  Repeat until parameter value is in range.
 * We mean to talk straight to the user here; thus, interact with the real
 *   stdio, not the effective t_stdio, so that redirections do not get in
 *   the way.  In batch mode, a forced query is handled by writing a
 *   message on the terminal of the parent cl (the original stderr), and
 *   leaving some info describing the query in a file in uparm (if there is
 *   no uparm, we abort).  We then loop, waiting for the user to run "service"
 *   in the interactive cl to service the query, leaving the answer in a
 *   another file which we read and then delete.  If we wait a long time and
 *   get no response, we timeout.
 */
void
query (struct param *pp)
{
	static	char *oormsg =
		"ERROR: Parameter value is out of range; try again";
	register char *ip;
	char	buf[SZ_PROMPTBUF+1];
	struct	operand o;
	int	bastype, batch, arrflag, offset=0, n_ele, max_ele, fd;
	char	*index(), *nlp, *nextstr();
	char	*bkg_query(), *query_status;
	char	*abuf;

	bastype = pp->p_type & OT_BASIC;
	batch = firstask->t_flags & T_BATCH;
	arrflag = pp->p_type & PT_ARRAY;

	if (arrflag) {			/* We may access the array many     */
	    offset = getoffset (pp);	/* times, so save the offset and    */
					/* push it when necessary.	    */
	    poffset (offset);
	    max_ele = size_array (pp) - offset;
	} else
	    max_ele = 1;


	forever {
	    if (batch) {
		/* Query from a background job.
		 */
		query_status = bkg_query (buf, SZ_PROMPTBUF, pp);

	    } else if (pp->p_type & (PT_GCUR|PT_IMCUR)) {
		/* Read a graphics cursor.
		 */
		char	source[33];
		int	cursor;

		/* Determine the source of graphics cursor input, chosen from
		 * either the graphics or image cursor or the terminal.
		 */
		if (pp->p_type & PT_GCUR) {
		    if (c_envfind ("stdgcur", source, 32) <= 0)
			strcpy (source, "stdgraph");
		} else {
		    if (c_envfind ("stdimcur", source, 32) <= 0)
			strcpy (source, "stdimage");
		}

		if (strcmp (source, "stdgraph") == 0)
		    cursor = STDGRAPH;
		else if (strcmp (source, "stdimage") == 0)
		    cursor = STDIMAGE;
		else
		    goto text_query;		/* get value from terminal */

		/* Read a physical graphics cursor.
		 */
		pp->p_flags &= ~P_LEOF;
		if (cursor == STDIMAGE) {
		    /* The following is a kludge used to temporarily implement
		     * the logical image cursor read.  In the future this will
		     * be eliminated, and the c_rcursor call below (cursor
		     * mode) will be used for stdimage as well as for stdgraph.
		     * The present code (IMDRCUR) goes directly to the display
		     * server to get the cursor value, bypassing cursor mode
		     * and the (currently nonexistent) stdimage kernel.
		     */
		    char    str[SZ_LINE+1], keystr[10];
		    int     wcs, key;
		    float   x, y;

		    if (c_imdrcur ("stdimage",
			&x,&y,&wcs,&key,str,SZ_LINE, 1, 1) == EOF) {
			query_status = NULL;

		    } else {
			if (isprint(key) && !isspace(key))
			    sprintf (keystr, "%c", key);
			else
			    sprintf (keystr, "\\%03o", key);
			sprintf (buf, "%.3f %.3f %d %s %s\n",
			    x, y, wcs, keystr, str);
		        query_status = (char *) ((XINT) strlen(buf));
		    }

		} else if (c_rcursor (cursor, buf, SZ_PROMPTBUF) == EOF) {
		    query_status = NULL;
		} else
		    query_status = (char *) ((XINT) strlen(buf));

	    } else if (pp->p_type & PT_UKEY) {
		/* Read a user keystroke command from the terminal.
		 */
		pp->p_flags &= ~P_LEOF;
		if (c_rdukey (buf, SZ_PROMPTBUF) == EOF)
		    query_status = NULL;
		else
		    query_status = (char *) ((XINT) strlen(buf));

	    } else {
text_query:	fd = spf_open (buf, SZ_PROMPTBUF);
		pquery (pp, fdopen(fd,"a"));
		spf_close (fd);

		c_stgputline ((XINT)STDOUT, buf);
		if (c_stggetline ((XINT)STDIN, buf, SZ_PROMPTBUF) > 0)
		    query_status = (char *) ((XINT) strlen(buf));
		else
		    query_status = NULL;
	    }

	    ip = buf;

	    /* Set o to the current value of the parameter.  Beware that some
	     * of the logical branches which follow assume that struct o has
	     * been initialized to the current value of the parameter.
	     */
	    if (pp->p_type & PT_LIST)
		setopundef (&o);
	    else if (arrflag) {
		paramget(pp, FN_VALUE);
		poffset (offset);
		o = popop();
	    } else
		o = pp->p_valo;

	    /* Handle eof, a null-length line (lone carriage return),
	     * and line with more than SZ_LINE chars.  Ignore leading whitespace
	     * if basic type is not string.
	     */
	    if (query_status == NULL) {
		/* Typing eof will use current value (as will a lone
		 * newline) but if param is a list, it is a meaningful
		 * answer.
		 */
		if (pp->p_type & PT_LIST) {
		    closelist (pp);		/* close an existing file */
		    pp->p_flags |= P_LEOF;
		    o = makeop (eofstr, OT_STRING);
		    break;
		}
		goto testval;
	    }

	    /* Ignore leading whitespace if it is not significant for this
	     * datatype.  Do this before testing for empty line, so that a
	     * return such as " \n" is equivalent to "\n".  I.e., do not
	     * penalize the user if they type the space bar by accident before
	     * typing return to accept the default value.
	     */
	    if (bastype != OT_STRING || (pp->p_type & (PT_FILNAM|PT_PSET)))
		while (*ip == ' ' || *ip == '\t')
		    ip++;

	    if (*ip == '\n') {
		/* Blank lines usually just accept the current value
		 * but if the param is a string and is undefined,
		 * it sets the string to a (defined) nullstring.
		 */
		*ip = '\0';
		if (bastype == OT_STRING && opundef (&o))
		    o = makeop (ip, bastype);
		else
		    goto testval;
	    }

	    if ((nlp = index (ip, '\n')) != NULL)
		*nlp = '\0';			/* cancel the newline	*/
	    else
		goto testval;

	    /* Finally, we have handled the pathological cases...
	     */
	    if ((pp->p_type & PT_LIST) &&
		(!strcmp (ip,eofstr) || !strcmp (ip,"eof"))) {

		closelist (pp);
		pp->p_flags |= P_LEOF;
		o = makeop (eofstr, OT_STRING);
		break;

	    } else {
		if (arrflag) {
		    /* In querying for arrays we may set more than one
		     * element of the array in a single query.  However
		     * we must set the first element.  So we will pretend
		     * to be a scalar until that first element is set
		     * and then enter a loop where we may set other
		     * elements.
		     */
		    abuf = ip;
		    ip = nextstr(&abuf, stdin);
		    if (ip == NULL  ||  ip == (char *) ERR  ||  ip == undefval)
			goto testval;
		}

		o = makeop (ip, bastype);
	    }

testval:
	    /* If parameter value is in range, we are done.  If it is out of
	     * range and we are a batch job or an interactive terminal job,
	     * print an error message and request that the user enter a legal
	     * value.  If the CL is being run taking input from a file, abort,
	     * else we will go into a loop reading illegal values from the
	     * input file and printing out lots of error messages.
	     */
	    if (inrange (pp, &o))
		break;
	    else if (batch)
		eprintf ("\n[%d] %s", bkgno, oormsg);
	    else if (isatty (fileno (stdin)))
		eprintf ("%s\n", oormsg);
	    else
		cl_error (E_UERR, oormsg);
	}

	if (!(pp->p_type & PT_LIST)) {
	    /* update param with new value.
	     */
	    if (cldebug) {
		eprintf ("changing `%s.p_val' to ", pp->p_name);
		fprop (stderr, &o);
		eprintf ("\n");
	    }

	    pushop (&o);
	    paramset (pp, FN_VALUE);
	    pp->p_flags |= P_QUERY;
	}

	pushop (&o);

	if (arrflag  &&  query_status != NULL  &&  *ip != '\0') {
	    /* If we have an array assign values until something
	     * is used up or until we hit any error.
	     */
	    n_ele = 1;
	    forever {
		if (n_ele >= max_ele)		/* End of array. */
		    break;
		ip = nextstr(&abuf, stdin);

		if (ip == NULL)			/* End of query line. */
		    break;

		if (ip == (char *) ERR) {	/* Error on query line. */
		    eprintf("Error loading array value.\n");
		    break;
		}

		if (ip != undefval) {
		    o = makeop (ip, bastype);
		    if ( ! inrange (pp, &o) ) {	/* Not in range. */
			eprintf("Array value outside range.\n");
			break;
		    }

		    offset++;			/* Next element in array. */
		    poffset (offset);

		    pushop (&o);
		    paramset (pp, FN_VALUE);
		} else
		    offset++;

		n_ele++;
	    }
	}
}


/* NEXTSTR -- Get the next string in a prompt.
 */
char *
nextstr (char **pbuf, FILE *fp)
{
	char	*p, *nxtchr();
	static	char	tbuf[SZ_LINE];
	char	quote;
	int	cnt;

	p = *pbuf;

	/* Skip white space. */
	while ( *p == ' '  ||  *p == '\t'  ||  *p =='\n')
	    p = nxtchr(p, fp);

	/* Reached end?	*/
	if (*p == '\0') {
	    *pbuf = p;
	    return (NULL);
	}

	quote = '\0';
	cnt = 0;

	/* Quoted string. */
	if (*p == '\''  ||  *p == '"') {
	    quote = *p;
	    p = nxtchr (p, fp);

	    while (*p != quote) {

		if (*p == '\0'  ||  cnt >= SZ_LINE)
		    return ( (char *) ERR);

		else {
		    tbuf[cnt++] = *p;
		    p = nxtchr(p, fp);
		}
	    }
	    /* Skip quote. */
	    p = nxtchr (p, fp);

	} else {
	    /* Unquoted string. */
	    while (*p != ' '   &&  *p != '\t'   &&  *p != '\n'  &&
	      *p != '\0'  &&  *p != ',') {

		if (cnt >= SZ_LINE)
		    return ( (char *) ERR );

		tbuf[cnt++] = *p;
		p = nxtchr (p, fp);
	    }
	}
	tbuf[cnt] = '\0';

	/* Skip any white-space following. */
	while (*p == ' '  ||  *p == '\t'  ||  *p == '\n')
	    p = nxtchr(p, fp);

	if (*p != ','   &&  *p != '\0')
	    return ( (char *) ERR);

	/* Skip delimiter. */
	if (*p == ',')
	   p = nxtchr(p, fp);

	*pbuf = p;
	if (cnt == 0) {
	    /* Return a quoted null string, otherwise the field was skipped. */
	    if (quote != '\0')
		return (tbuf);
	    else
		return (undefval);
	} else
	    return (tbuf);
}


/* NXTCHR -- Get a pointer to the next char, reading the next line if necessary.
 */
char *
nxtchr (char *p, FILE *fp)
{
	/* P may point to within readbuf on return, so it had better be
	 * static.
	 */
	static	char	readbuf[SZ_LINE];

	if (*p)
	    p++;
start:
	if (*p == '\\') {
	    if (*(p+1) == '\n') {
		if (fgets (readbuf, SZ_LINE, fp) == NULL)
		    /* We assume that the newline is always followed by a
		     * null in return from fgets.
		     */
		    return (p+2);
		else {
		    p = readbuf;
		    goto start;
		}
	    }
	}

	return (p);
}


/* PQUERY -- Print the query message.
 */
void
pquery (register struct param *pp, FILE *fp)
{
	struct	operand o;
	int	offset=0, arrflag=0;

	arrflag = pp->p_type & PT_ARRAY;

	fprintf (fp, *pp->p_prompt == '\0' ? pp->p_name : pp->p_prompt);

	/* Show the ranges if they are defined and this is a parameter
	 * type that has ranges.
	 */
	if (range_check (pp)) {
	    fprintf (fp, " (");
	    if (!(pp->p_flags & (P_IMIN|P_UMIN))) {
		paramget (pp, FN_MIN);
		o = popop();
		fprop (fp, &o);
	    }
	    if ((pp->p_type & OT_BASIC) != OT_STRING)
		fprintf (fp, ":");
	    if (!(pp->p_flags & (P_IMAX|P_UMAX))) {
		paramget (pp, FN_MAX);
		o = popop();
		fprop (fp, &o);
	    }
	    fputc (')', fp);
	}

	/* Print the array indices.  We get the offset and convert back
	 * to the indices.  This works regardless of the offset mode.
	 */
	if (arrflag) {
	    int	dim, d, rem, temp;
	    short *len, *off;

	    offset = getoffset (pp);
	    poffset (offset);		/* Restore stack for later reference */

	    dim = pp->p_val.v_a->a_dim;
	    len = &(pp->p_val.v_a->a_len) ;
	    off = &(pp->p_val.v_a->a_off) ;

	    fputc ('[', fp);
	    temp = offset;
	    for (d=0; d<dim; d++) {

		if (d>0)
		    fputc (',', fp);

		rem = (temp % *len) + *off;
		fprintf (fp, "%d",rem);
		temp = temp / *len;
		len = len + 2;
		off = off + 2;
	    }
  	    fputc (']', fp);
	}
	
	/* Set o to the current value of the parameter.  List files do
	 * not keep a value in core, however, and we certainly do not want
	 * to read the list to get one.
	 */
	if (pp->p_type & PT_LIST)
	    setopundef (&o);
	else {
	    paramget (pp, FN_VALUE);
	    o = popop();

	    /* Restore offset on stack if array. */
	    if (arrflag) {
		poffset (offset);
	    }
	}

	/* Print current value if not undefined.  Ok if just indefinite.
	 */
	if (!opundef (&o)) {
	    if ((o.o_type & OT_BASIC) != OT_STRING || *(o.o_val.v_s) != '\0') {
		fprintf (fp, " (");
		fprop (fp, &o);
		fputc (')', fp);
	    }
	}
	fprintf (fp, ": ");
	fflush (fp);
}


/* BKG_QUERY -- Send the "waiting for parameter input" to the user terminal,
 * and loop until the background query response file is readable.
 * This happens when the user responds to the query by executing "service".
 * Check frequently in the beginning, gradually lengthening the sleep periods
 * so that we do not hog the machine if the user is out to lunch.  Timeout
 * after a suitable interval if no response.
 */
char *
bkg_query (
    char *obuf,			/* same calling sequence as 'fgets' */
    int maxch,
    register struct param *pp
)
{
	char	bqfile[SZ_PATHNAME], qrfile[SZ_PATHNAME];
	int	waitime, delay;
	char	*envget(), *fgets_status;
	FILE	*fp, *in;

	if (notify())
	    eprintf ("\n[%d] stopped waiting for parameter input\n", bkgno);
	get_bkgqfiles (bkgno, ppid, bqfile, qrfile);
	
	/* Get names of the query and query response files and open the query
	 * file to receive the query.  Post query request on the user terminal.
	 * If an old query response file happens to be lying about, delete it.
	 */
	c_delete (bqfile);
	if ((fp = fopen (bqfile, "w")) == NULL)
	    cl_error (E_UERR, "Cannot create file `%s' for query", bqfile);
	c_delete (qrfile);

	/* Print the query prompt into the background query request file.
	 */
	pquery (pp, fp);
	fclose (fp);

	waitime = 0;
	delay = INIT_DELAY;

	/* Loop until the query response file is readable.  Sleep for
	 * progressively longer intervals if no response, then timeout.
	 */
	do {
	    if (waitime > BKQ_TIMEOUT) {
		c_delete (bqfile);
		cl_error (E_UERR, "Timeout on query");
	    } else {
		delay = (delay *= DELAY_MULT) > MAXDELAY ? MAXDELAY : delay;
		c_tsleep (delay);
		waitime += delay;
	    }
	} while (c_access (qrfile,0,0) == NO);

	if ((in = fopen (qrfile, "r")) == NULL)
	    cl_error (E_UERR, "cannot open query response file");

	fgets_status = fgets (obuf, maxch, in);
	fclose (in);
	c_delete (qrfile);

	return (fgets_status);
}


/* SERVICE_BKGQUERY -- Called by the user to service a background query.
 * We must open the background query file for the indicated task and type
 * out the prompt therein for the user.  The user's response in then placed
 * in the query response file, we delete the original query file, and we
 * are done.  When the bkg job wakes up it will read the response file and
 * (assuming there are no errors) continue on.
 */
void
service_bkgquery (
    int bkgno			/* ordinal of job requiring service	*/
)
{
	register int ch;
	char	bqfile[SZ_PATHNAME], qrfile[SZ_PATHNAME];
	char	qrtemp[SZ_PATHNAME];
	char	response[SZ_LINE+1];
	FILE	*fp;

	if (bkg_jobactive (bkgno) == NO)
	    cl_error (E_UERR, "No such job");
	else
	    get_bkgqfiles (bkgno, c_getpid(), bqfile, qrfile);
	c_mktemp ("uparm$QR", qrtemp, SZ_PATHNAME);

	if ((fp = fopen (bqfile, "r")) == NULL)
	    cl_error (E_UERR, "No query is pending for bkg job [%d]", bkgno);

	/* Copy query file verbatim to the user's terminal.  The last line
	 * will not have a newline, but that is ok here.
	 */
	while ((ch = fgetc(fp)) != EOF)
	    putchar (ch);
	fflush (stdout);

	/* Get user's response and write into query response file.
	 * We write the response first into a temp file and then rename the
	 * temp file to eliminate the chance that the bkg job will try to
	 * open and read the response file before the data has all been
	 * written into it (happens on systems that do not lock files
	 * opened by another process for writing).
	 */
	c_delete (qrtemp);
	fgets (response, SZ_LINE, stdin);
	if ((fp = fopen (qrtemp, "w")) == NULL)
	    cl_error (E_UERR, "Cannot open `%s' to respond to query", qrtemp);
	fputs (response, fp);
	fclose (fp);
	c_rename (qrtemp, qrfile);

	/* Do not delete the query file until we successfully respond to
	 * the query (in case of an abort).
	 */
	c_delete (bqfile);
}


/* GET_BKGQFILES -- Get the name of a background query file.  This routine
 * aborts if the directory uparm$ is not defined.  Since we have two processes
 * communicating via files, we must have a fixed directory both processes
 * expect to find the files.  We assume that the user does not start a bkg
 * job and then change uparm$ in the foreground cl.
 */
void
get_bkgqfiles (int bkgno, int pid, char *bkg_query_file, char *query_response_file)
{
	int	filecode;
	char	*envget();

	if (envget (UPARM) == NULL)
	    cl_error (E_UERR,
		"Logical directory 'uparm$' not defined, cannot query");
	
	filecode = bkgno * 10000 + (pid % 10000);
	sprintf (bkg_query_file, "%sBQF%d", envget(UPARM), filecode);
	sprintf (query_response_file, "%sBQR%d", envget(UPARM), filecode);
}


/* INRANGE -- Check whether operand *op is in range, that is, that its o_val
 *   field is within the limits defined by the p_min/max fields in param *pp.
 *   Return YES if it is in range, else NO.  In the case of filenames, also
 *   check that the PT_FXX access attributes are true.  Also, filenames are
 *   considered out of range is they are indefinite (unlike other types; see
 *   below).
 * The basic types for the operand and the parameter must agree.
 * Always return YES for types that do not have ranges (only ints, reals,
 *   and filenames have ranges), when min > max, or when op is INDEF.
 *   Always return NO if op is UNDEFined.
 * This routine uses binexp() and thus the operand stack.
 */
int
inrange (register struct param *pp, register struct operand *op)
{
	register int fulltype, bastype;
	struct	operand omin, test;

	fulltype = pp->p_type;
	bastype = fulltype & OT_BASIC;

	/* If the operand is undefined, it is out of range.  Indefinite is
	 * inrange for int and real type params.
	 */
	if (opundef (op))
	    return (NO);
	if (opindef (op) && bastype & (OT_INT|OT_REAL))
	    return (YES);

	/* If range checking is disabled, and the parameter value is defined,
	 * it is in range.
	 */
	if (range_check (pp) == 0)
	    return (YES);

	if (fulltype & PT_FILNAM) {
	    /* check any access attributes given.
	     */
	    char *filnam = op->o_val.v_s;
	    if (opindef (op))
		return (NO);

	    if ((fulltype & PT_FER)  && c_access (filnam, READ_ONLY, 0) == NO)
		cl_error (E_UERR, "File `%s' is not readable", filnam);
	    if ((fulltype & PT_FEW)  && c_access (filnam, WRITE_ONLY, 0) == NO)
		cl_error (E_UERR, "File `%s' is not writable", filnam);
	    if ((fulltype & PT_FNOE) && c_access (filnam,0,0) == YES)
		cl_error (E_UERR, "File `%s' exists", filnam);

	    if ((fulltype & PT_FTXT) && c_access (filnam, 0, TEXT_FILE) == NO)
		cl_error (E_UERR, "File `%s' is not a text file", filnam);
	    if ((fulltype & PT_FBIN) && c_access (filnam, 0, TEXT_FILE) == YES)
		cl_error (E_UERR, "File `%s' is not a binary file", filnam);
	}

	/* If the param is string valued and the legal values are enumerated,
	 * any minimum match abbreviation is considered in range.  Return the
	 * FULL string in the operand structure.  The legal values of an
	 * enumerated string type parameter are given in the min field as a
	 * string of the form "val|val|val".  Embedded whitespace is not
	 * permitted.
	 */
	if (bastype == OT_STRING && !(pp->p_flags & P_UMIN)) {
	    char	*s, *delim, *match;
	    char	*val, *index();
	    int		n;

	    paramget (pp, FN_MIN);
	    omin = popop();
	    if (omin.o_type != OT_STRING || op->o_type != OT_STRING)
		return (NO);

	    val = op->o_val.v_s;
	    n = strlen (val);
	    match = NULL;

	    for (delim = s = omin.o_val.v_s;  delim && *s;  s=delim+1) {
		delim = index (s, '|');
		if (delim)
		    *delim = '\0';
		if (strncmp (s, val, n) == 0) {
		    if (match) {
			eprintf ("ambiguous abbreviation '%s'\n", val);
			return (NO);
		    } else
			match = s;
		}
	    }

	    if (match != NULL)
		op->o_val.v_s = comdstr (match);
	    return (match != NULL);
	}
	
	/* Check the minimum value, if one is given.
	 */
	if (!(pp->p_flags & (P_IMIN|P_UMIN))) {
	    pushop (op);
	    paramget (pp, FN_MIN);
	    binexp (OP_GE);		/* op >= p_min?			*/
	    test = popop();
	    if (!test.o_val.v_i)	/* if (false) op out of range	*/
		return (NO);
	}

	/* Check the maximum value, if one is given.
	 */
	if (!(pp->p_flags & (P_IMAX|P_UMAX))) {
	    pushop (op);
	    paramget (pp, FN_MAX);
	    binexp (OP_LE);		/* op <= p_max?			*/
	    test = popop();
	    if (!test.o_val.v_i)	/* if (false) op out of range	*/
		return (NO);
	}
	return (YES);
}


/* RANGE_CHECK -- Determine if range checking is in effect.  Range checking
 * is only employed for int, real, string (enumerated) and filename params.
 * If both the min and max fields are set, but max is less than min, checking
 * is disabled.
 */
int
range_check (struct param *pp)
{
	int	fulltype, bastype;
	struct	operand test, omin, omax;

	fulltype = pp->p_type;
	bastype = fulltype & OT_BASIC;

	/* No range checking for bools, or when range values are undefined
	 * or indefinite.
	 */
	if (bastype == OT_BOOL ||
	    fulltype & (PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET))
	    return (NO);
	if (pp->p_flags & (P_IMIN|P_UMIN) && pp->p_flags & (P_IMAX|P_UMAX))
	    return (NO);

	/* Range checking is disabled if the max value is set lower than
	 * the min value.
	 */
	if (!(pp->p_flags & (P_UMIN|P_IMIN|P_UMAX|P_IMAX))) {
	    omax.o_type = omin.o_type = bastype;
	    omin.o_val = pp->p_min;
	    omax.o_val = pp->p_max;
	    pushop (&omin);
	    pushop (&omax);
	    binexp (OP_GT);		/* p_min > p_max?		*/
	    test = popop();
	    if (test.o_val.v_i)		/* if (true) artificially pass	*/
		return (NO);
	}

	return (YES);			/* should range check		*/
}


/* SETCLMODES -- Set up the cl mode reference pointers to point to their
 * special-function params. tp is firstask.  Set the pointers to NULL if the
 * parameter is not found.  Called once by login() after the cl's pfile has
 * been read in.
 */
void
setclmodes (struct task *tp)
{
	register struct param *pp;
	register char *name;
	int	bastype;

	clabbrev = clmenus = clshowtype = clkeeplog = cllexmodes = cllogfile =
	    clnotify = clecho = NULL;

	for (pp = tp->t_pfp->pf_pp; pp != NULL; pp = pp->p_np) {

	    /* Set "CL parameter" bit to aid checking in paramset().
	     * Also, parse any parameters that need it.  (This is necessary
	     * to get the current values of `logmode' when running in bkg.)
	     */
	    pp->p_flags |= P_CL;
	    parse_clmodes (pp, &pp->p_valo);

	    /* Limit the strcmp's to only those params with the right
	     * basic time to speed this up a bit.  Be careful when adding
	     * new entries that they go into the right type.
	     * For now, at least, ignore all list params.
	     */
	    if (pp->p_type & PT_LIST)
		continue;

	    bastype = pp->p_type & OT_BASIC;
	    name = pp->p_name;
	    if (bastype == OT_STRING) {
		if (!strcmp (name, "mode"))
		    firstask->t_modep = pp;
		else if (!strcmp (name, "logfile"))
		    cllogfile = pp;
	    } else if (bastype == OT_BOOL) {
		if (!strcmp (name, "menus"))
		    clmenus = pp;
		else if (!strcmp (name, "showtype"))
		    clshowtype = pp;
		else if (!strcmp (name, "keeplog"))
		    clkeeplog = pp;
		else if (!strcmp (name, "lexmodes"))
		    cllexmodes = pp;
		else if (!strcmp (name, "abbreviate"))
		    clabbrev = pp;
		else if (!strcmp (name, "notify"))
		    clnotify = pp;
		else if (!strcmp (name, "echo"))
		    clecho = pp;
	    }
	}
}


#define NEXT_TOKEN  while (*ip == ' ' || *ip == '\t' || *ip == '\n') ip++; \
		    if (!*ip) break;
#define NEXT_WHITE  while (*ip != ' ' && *ip != '\t' && *ip != '\0') ip++;

/* PARSE_CLMODES -- Called whenever a CL parameter is set at runtime.  A 
 * few of the CL parameters need to be parsed and internal variables set
 * appropriately.  Tokens in the parameter strings are white-space 
 * delimited.
 */
void
parse_clmodes (struct param *pp, struct operand *newval)
{
	register char  *name, *ip;

	name = pp->p_name;

	if (!strcmp (name, "logmode")) {
	    ip = newval->o_val.v_s;
	    while (*ip) {
		NEXT_TOKEN;

		/* Check the next token; only a few matching characters 
		 * are needed.  Default values are set elsewhere, so we
		 * check for all possibilities here.
		 */
		if      (strncmp (ip, "commands", 5) == 0)
		    cllogmode |= LOG_COMMANDS;
		else if (strncmp (ip, "nocommands", 5) == 0)
		    cllogmode &= ~LOG_COMMANDS;

	    	else if (strncmp (ip, "background", 5) == 0)
		    cllogmode |= LOG_BACKGROUND;
	    	else if (strncmp (ip, "nobackground", 5) == 0)
		    cllogmode &= ~LOG_BACKGROUND;

	    	else if (strncmp (ip, "errors", 5) == 0)
		    cllogmode |= LOG_ERRORS;
	    	else if (strncmp (ip, "noerrors", 5) == 0)
		    cllogmode &= ~LOG_ERRORS;

	    	else if (strncmp (ip, "trace", 5) == 0)
		    cllogmode |= LOG_TRACE;
	    	else if (strncmp (ip, "notrace", 5) == 0)
		    cllogmode &= ~LOG_TRACE;

	    	else if (*ip != '\0')
		    eprintf ("unrecognized logging set-option `%s'\n", ip);

		NEXT_WHITE;
	    }

	} else if (!strcmp (name, "logfile")) {
	    reset_logfile();

	} else if (!strcmp (name, "epinit")) {
	    ip = newval->o_val.v_s;
	    while (*ip) {
		NEXT_TOKEN;

		if      (strncmp (ip, "standout", 5) == 0)
		    ep_standout = YES;
		else if (strncmp (ip, "nostandout", 5) == 0)
		    ep_standout = NO;
	    	else if (strncmp (ip, "showall", 5) == 0)
		    ep_showall = YES;
	    	else if (strncmp (ip, "noshowall", 5) == 0)
		    ep_showall = NO;
	    	else if (*ip != '\0')
		    eprintf ("unrecognized eparam set-option `%s'\n", ip);

		NEXT_WHITE;
	    }

	} else if (!strcmp (name, "ehinit")) {
	    ip = newval->o_val.v_s;
	    while (*ip) {
		NEXT_TOKEN;

		if      (strncmp (ip, "verify", 5) == 0)
		    eh_verify = YES;
		else if (strncmp (ip, "noverify", 5) == 0)
		    eh_verify = NO;
	    	else if (strncmp (ip, "standout", 5) == 0)
		    eh_standout = YES;
	    	else if (strncmp (ip, "nostandout", 5) == 0)
		    eh_standout = NO;
	    	else if (strncmp (ip, "bol", 3) == 0)
		    eh_bol = YES;
	    	else if (strncmp (ip, "eol", 3) == 0)
		    eh_bol = NO;
	    	else if (strncmp (ip, "readline", 3) == 0)
#ifdef NO_READLINE
		    eh_readline = NO;
#else
		    eh_readline = YES;
#endif
	    	else if (strncmp (ip, "noreadline", 3) == 0)
		    eh_readline = NO;
	    	else if (strncmp (ip, "longprompt", 3) == 0)
		    eh_longprompt = YES;
	    	else if (strncmp (ip, "nolongprompt", 3) == 0)
		    eh_longprompt = NO;
	    	else if (*ip != '\0')
		    eprintf ("unrecognized ehistory set-option `%s'\n", ip);

		NEXT_WHITE;
	    }

	} else if (!strcmp (name, "szprcache")) {
	    /* Change the size of the process cache.
	     */
	    pr_setcache (newval->o_val.v_i);

	} else if (!strcmp (name, "mode")) {
	    /* Menu mode is not permitted at the CL level.
	     */
	    char   *index();

	    if (index (newval->o_val.v_s, 'm') != NULL)
		cl_error (E_UERR,
		    "menu mode is permitted only for packages and tasks");
	}
}


/* ABBREV -- Determine if abbreviations are allowed.  Abbreviations are
 * only allowed if the currentask is interactive (or batch), or if the
 * currentask is a builtin and the previous task is interactive (or batch),
 * regardless of value of clabbrev parameter.
 */
int
abbrev (void)
{
	/* Enable abbreviations everywhere for now.
	int	cflags = currentask->t_flags;
	int	pflags = prevtask->t_flags;

	if (clabbrev == NULL)
	    return (NO);
	if ((clabbrev->p_valo.o_type & (OT_UNDEF|OT_INDEF)) ||
	    !clabbrev->p_valo.o_val.v_i)
	    return (NO);

 	if (cflags & (T_INTERACTIVE|T_BATCH))
	    return (YES);
	if ((cflags & T_BUILTIN) && (pflags & (T_INTERACTIVE|T_BATCH)))
	    return (YES);
	
	return (NO);
	 */

	return (YES);
}

/* POFFSET--push an offset in an array for a later reference.
 */
void
poffset (int off)
{
	n_indexes++;
	push (off);
	offsetmode(1);
}
