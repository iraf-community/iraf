/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_stdio
#define	import_libc
#define	import_error
#define	import_ctype
#define	import_ttset
#define	import_fset
#define	import_spp
#include <iraf.h>

#include "config.h"
#include "mem.h"
#include "operand.h"
#include "errs.h"
#include "param.h"
#include "grammar.h"
#include "task.h"
#include "eparam.h"
#include "proto.h"


/*
 * EPARAM -- Screen editor for parameter files.
 *
 *	 epset (pset)				# edit any pset by name
 *	eparam (cx, &update, &cmd, &newpset)	# edit incore pfile struct
 *
 * EHIST  -- Screen editor for the history list.
 *
 *	edit_history_directive (raw_cmd, new_cmd)
 *
 * Both of these primary functions use the following internal editing
 * functions (and many more).  These use EDCAP to describe the editor 
 * language to be used, and TERMCAP to describe the terminal to be driven.
 *
 *	e_ttyinit		enter edit mode
 *	e_ttyexit		exit edit mode
 *
 *	editstring		screen editor for a string
 *
 *	e_clear			clear the screen
 *	e_clrline		clear the current line
 *	e_ctrl			send control sequence to the terminal
 *	e_display		display text at addressed coordinates
 *	e_goto			move cursor
 *	e_putline		put a line to terminal with escape translation
 *
 * E_TTYINIT must be called to initialize the editor database and put the
 * terminal into edit mode before calling any of these functions.
 */

extern  int 	cldebug;
static  char  	dbg[SZ_LINE];	   		   /* for formatting msgs */
#define	E_DEBUG(str)	e_display(str,cmdline,1)   /* debug msg on last line */

struct	param *parmlist[G_MAXPARAM];	/* assoc. keyword with param          */
static	struct pfile *pfilep;
static	int keylines[G_MAXPARAM];	/* starting linenos of each keyword   */
static	int firstelement[G_MAXPARAM];	/* first element on row for array     */
static	int topkeys[G_MAXPAGES];	/* array of topkeys for each page     */

static	int maxpage;			/* maximum page number                */
static	int cmdline;			/* last line on screen		      */
static	int maxcol;			/* last column on screen	      */
static	int line, topline, botline;	/* current, top, bottom lines	      */
static	int col, startcol, nextcol;	/* current, first, last columns	      */
static	XINT tty_fd, tty;		/* define the terminal globally       */
static	int botkeyline, nextline,	/* various global variables for       */
	    keyid, numkeys, topkey,	/*   keeping track of lines and keys  */
	    botkey, nextkey;
static	int error_displayed = 0;	/* flag for error messages 	      */

static  int standout;			/* flag for turning standout mode off */
static	int e_ucasein=NO,e_ucaseout=NO;	/* tt case flags for raw mode i/o     */
static	int ep_status = OK;		/* OK=normal exit, ERR=ctrl/c exit    */
static	int ep_filemode = NO;		/* editing a file not a task	      */
static	int ep_nextcmd;			/* next eparam command upon exit      */
static	int ep_update;			/* update pfile upon exit	      */
static	char e_nextpset[SZ_FNAME+1];	/* next pset to be edited	      */
static	struct ep_context *e_cx;	/* current context		      */

/* These global variables are reset by parse_clmodes() in modes.c whenever the
 * appropriate CL parameter is changed.
 */
int	ep_standout = YES;		/* eparam default for standout 	      */
int	ep_showall  = NO;		/* display all params, incl. hiddens  */
int	eh_standout = YES;		/* ehist default for standout         */
int	eh_bol      = NO;		/* start ehist at beginning of line   */
int	eh_verify   = NO;		/* use ehist with history meta-chars  */
#ifdef NO_READLINE
int	eh_readline 	= NO;		/* no readline() available here       */
#else
int	eh_readline 	= YES;		/* use readline() for terminal input  */
#endif
int	eh_longprompt 	= YES;		/* print full package name as prompt  */

char	*e_tonextword(), *e_toprevword(), *index();

char	epar_cmdbuf[SZ_LINE];

int eparam (struct ep_context *cx, int *update, int *nextcmd, char *nextpset);

/* EPSET -- Edit a parameter set.  Once in the parameter set editor, editor
 * colon commands may be used to edit any other parameter set, to save psets
 * in pfiles, load psets from pfiles, and so on.  ERR is returned if the user
 * wants to quit altogether, e.g., when epset is called in a loop.
 */
int
epset (
  char	*pset			/* ltaskname or pfilename */
)
{
	struct  ep_context context[20], *cx;
	char    newpset[SZ_FNAME+1];
	char	runcmd[SZ_LINE+1];
	int     update, cmd;

	cx = context;
	cx->e_mpfp = NULL;
	strcpy (cx->e_pset, pset);

	while (cx >= context) {
	    /* Open the pfile to be edited. */
	    if (cx->e_mpfp == NULL) {
		cx->e_topd = topd;
		cx->e_mpfp = pfilesrch (cx->e_pset);
		cx->e_cpfp = pfilecopy (cx->e_mpfp);
		cx->e_init = YES;
	    }

	    /* Edit pset.  If ERR is returned exit immediately without
	     * updating any pfiles, returning ERR to our caller.
	     */
	    if (eparam (cx, &update, &cmd, newpset) == ERR) {
		for (;  cx >= context;  --cx) {
		    pfileunlink (cx->e_cpfp);
		    if (dereference (cx->e_mpfp) >= cx->e_topd)
			pfileunlink (cx->e_mpfp);
		    topd = cx->e_topd;
		}
		return (ERR);
	    }

	    /* If we are done with this pfile (not descending into a pset)
	     * update the pfile on disk and free memory.
	     */
	    if (cmd != EP_DESCEND) {
		if (update) {
		    pfcopyback (cx->e_cpfp);
		    pfileupdate (cx->e_mpfp);
		} else
		    pfileunlink (cx->e_cpfp);

		if (dereference (cx->e_mpfp) >= cx->e_topd)
		    pfileunlink (cx->e_mpfp);
		cx->e_mpfp = NULL;
		cx->e_cpfp = NULL;
		topd = cx->e_topd;
	    }

	    /* Decide what to do next. */
	    switch (cmd) {
	    case EP_EOF:			/* pop context */
		--cx;
		break;
	    case EP_EDIT:			/* edit a new pfile */
		strcpy (cx->e_pset, newpset);
		break;
	    case EP_DESCEND:			/* push context & edit */
		cx++;
		cx->e_mpfp = NULL;
		strcpy (cx->e_pset, newpset);
		break;
	    case EP_RUN:			/* run the task */
		sprintf (runcmd, "%s (mode='h')\n", newpset);
		if (eh_readline == NO)
		    c_ungetline (fileno (prevtask->t_in), runcmd);
		else
		    strcpy (epar_cmdbuf, runcmd);
		return (OK);
	    default:
		eprintf ("eparam: unrecognized command\n");
		--cx;
		break;
	    }
	}

	return (OK);
}


/* EPARAM -- Edit a parameter set which has already been loaded into a
 * pfile structure.  Most editor colon commands will cause an exit,
 * returning the user command to the caller, e.g., to edit a new pset or
 * quit.  The context of the editor is saved upon exit in the context
 * structure, allowing the editor to be reentered at the same point
 * on the old pset.
 */
int
eparam (
  struct ep_context *cx,		/* eparam editor context	*/
  int	*update,			/* update pset upon exit	*/
  int	*nextcmd,			/* receives next command	*/
  char	*nextpset 			/* receives next pset name	*/
)
{
	char	string[G_MAXSTRING];

	pfilep = cx->e_cpfp;		/* save in global variables     */
	e_cx = cx;

	standout = ep_standout;		/* set standout value 		*/
	e_ttyinit();			/* initialize the terminal      */
	edtinit();			/* and initialize the editor	*/

	/* When we are called to edit a file, the ltask ptr is NULL.
	 */
	if ((ep_filemode = (pfilep->pf_ltp == NULL)))
	    topline--;			/* room for one more param line */

	numkeys = e_makelist (pfilep);	/* initialize parameter list	*/
	if (numkeys < 1)		/* nothing to edit		*/
	    goto exit;

	ep_status = OK;
	ep_nextcmd = EP_EOF;		/* default if no :cmd		*/
	ep_update  = YES;		/* default unless cleared	*/

	if (cx->e_init) {
	    /* New pfile: start at the top. */
	    topkey    = 1;
	    line      = topline;
	    col       = startcol;
	    nextkey   = topkey;
	    nextline  = topline;
	} else {
	    /* Reentering an old pfile: start where we left off. */
	    topkey    = cx->e_topkey;
	    line      = cx->e_line;
	    col       = cx->e_col;
	    nextkey   = cx->e_nextkey;
	    nextline  = cx->e_nextline;
	}

	if (parmlist[topkey]->p_type & PT_ARRAY) 	/* add line for array */
	    line++, nextline++;

	e_repaint();

	/* Main EPARAM loop.
	 */
	while (nextline != cmdline) {
	    keyid = nextkey;
	    line  = nextline;
	    col   = startcol;

	    e_goto (col, line);
	    fflush (stdout);

	    /* Encode value string and call the string editor to give the
	     * user a chance to edit it.
	     */
	    e_encode_vstring (parmlist[keyid], string);

	    if (editstring (string, YES) > 0)
	        e_check_vals (string);

	    e_scrollit();
	}
exit:
	/* Save our context in case we reenter this pfile. */
	cx->e_topkey = topkey;
	cx->e_line = line;
	cx->e_col = col;
	cx->e_nextkey = keyid;
	cx->e_nextline = line;
	cx->e_init = 0;

	e_goto (1, cmdline);
	e_clrline();

	edtexit();
	e_ttyexit();

	*update = ep_update;
	*nextcmd = ep_nextcmd;
	strcpy (nextpset, e_nextpset);

	return (ep_status);
}


/* E_MAKELIST -- Make a list of pointers to each parameter structure to aid
 * speedy access.  Return the number of parameters in the list.  For a
 * multiline prompt environment, we need a table of pointers to the firstline
 * of each keyword.
 */
int
e_makelist (
  struct pfile  *pfileptr
)
{
	register struct param *pp;
	register char c, *p;
	int	numnew;			/* number of newlines 		*/
	int	totlines;		/* count of current total lines */

	topkeys[0] = 1;
	totlines = 0;
	maxpage  = 0;

	/* Scan the parameter list, adding each parameter to the EPARAM
	 * list.  Hidden parameters are skipped if ep_showall=no (in epinit).
	 */
	for (pp = pfileptr->pf_pp, numkeys = 0;  pp != NULL;  pp = pp->p_np) {

	    if ((pp->p_mode & M_HIDDEN) && (ep_showall == NO))
		continue;

	    numkeys++;
	    parmlist[numkeys] = pp;

	    /* Count the number of newlines in the prompt, add to keylines.
	     */
	    numnew = 0;
	    p = pp->p_prompt;

	    while ((c = *p) != '\0') {
		if (c == '\n')
		    numnew++;
		p++;
	    }

	    totlines += numnew + 1;
	    keylines[numkeys] = numnew + 1;
	    firstelement[numkeys] = 1;

	    if (pp->p_type & PT_ARRAY) {
		int	numonrow, nextelement;
		int	dim, d, alines;
		short 	*plen, len, flen;

		keylines[numkeys]++;		/* 1 extra line for arrays */
		totlines++;
		totlines = e_testtop (totlines, numnew+1+1);
	
		dim    = pp->p_val.v_a->a_dim;
		plen   = &(pp->p_val.v_a->a_len);
		flen   = *plen;				/* first length */
		alines = (flen - 1) / MAX_ON_ROW + 1;
		numonrow = (flen > MAX_ON_ROW) ? MAX_ON_ROW : flen;

		for (d=1;  d < dim;  d++) {
		    len = *(plen + 2*d);
		    alines *= len;
		}

		nextelement = 1;
		for (d=1, numkeys++;  d < alines;  d++, numkeys++) {
		    parmlist[numkeys] = pp;
		    keylines[numkeys] = 1;

		    nextelement += numonrow;
		    firstelement[numkeys] = nextelement;
		    
		    totlines++;
		    totlines = e_testtop (totlines, numnew+1+1+d);
		}

		--numkeys;

	    } else {
		totlines = e_testtop (totlines, numnew+1);
	    }
	}

	if (cldebug) {
	    int	i;
	    for (i=1;  i <= numkeys;  i++) {
	        sprintf (dbg, "parmlist: %d %d %d  ", 
			parmlist[i], keylines[i], firstelement[i]);
		E_DEBUG (dbg);
	    }
            sprintf (dbg, " maxpage = %d  ", maxpage);
	    E_DEBUG (dbg);
	    for (i=1;  i<= maxpage;  i++) {
		sprintf (dbg, "topkeys : %d  ", topkeys[i]);
	        E_DEBUG (dbg);
	    }
	    sprintf (dbg, "numkeys = %d  ", numkeys);
	    E_DEBUG (dbg);
	}    

	return (numkeys);
}


/* E_TESTTOP -- Check to see if we have filled up a screen and if so,
 * start a new page.
 */
int
e_testtop (
  int	cur,			/* current line count on screen */
  int	new 			/* new count, returned if new page */
)
{
	if (cur > (botline - topline + 1)) {
	    topkeys[++maxpage] = numkeys;
	    return (new);
	} else
	    return (cur);
}


/* E_REPAINT -- Repaint the current screen.
 */
void
e_repaint (void)
{
	static	char *static_prompt = "--------- parameter array ---------";
	char	outbuf[MAXPROMPT];
	int	i, keylin, ll, cc;
	char	*p;

	/* More keys than can fit on the screen?
	 */
	keylin = topline;
	for (i=topkey; i <= numkeys && (keylin+keylines[i] <= (botline+1)); ) {
	    botkeyline = keylin;
	    keylin += keylines[i++];
	}

	botkey = i - 1;
	if (parmlist[botkey]->p_type & PT_ARRAY)
	    botkeyline += keylines[botkey] - 1;

	e_pheader (pfilep, cmdline, maxcol);

	ll   = line;
	cc   = col;
	line = topline;
	col  = startcol;

	for (keyid=topkey;  keyid <= botkey;  keyid++) {

	    if ((parmlist[keyid]->p_type & PT_ARRAY) &&
		(firstelement[keyid] == 1)) {

		/* Print the array parameter name.  If hidden, enclose it in ()
		 * as in lparam.
		 */
		if (parmlist[keyid]->p_mode & M_HIDDEN)
		    sprintf (outbuf, "(%-7.7s) ", parmlist[keyid]->p_name);
		else
		    sprintf (outbuf, "%-8.8s  ", parmlist[keyid]->p_name);
		e_display (outbuf, line, 1);

		/* Display the prompt over the values, to allow user to
		 * label columns (if desired).
		 */
		p = parmlist[keyid]->p_prompt;
		if (p == NULL || *p == NULL)
		    p = static_prompt;

		/* e_indent_prompt (p, promptbuf, startcol); */
		e_display (p, line, startcol);

	   	line += keylines[keyid] - 1;
		e_drawkey();
	    	line++;

	    } else {
		e_drawkey();
	    	line += keylines[keyid];
	    }

	    fflush (stdout);
	}

	e_moreflag (topkey);

	keyid = topkey;
	e_goto (cc, ll);
	line = ll;
	col  = cc;
}


/* E_PHEADER -- Print the EPARAM form header.
 */
void
e_pheader (
  struct pfile *pfp,		/* pfile pointer		*/
  int	cmdline,		/* terminal command line number	*/
  int	maxcol 			/* max cols on a line		*/
)
{
	static	char	*logo = "  I R A F  ";
	static  char	*title= "Image Reduction and Analysis Facility";
	char	string[SZ_LINE+1];
	int	i, col;

	e_clear();

	/* Print logo and title lines.
	 */
	col = (maxcol - strlen(logo)) / 2;
	e_ctrl ("so");
	e_goto (col, 1);
	e_putline (logo);

	col = (maxcol - strlen(title)) / 2;
	e_ctrl ("se");
	e_ctrl ("us");
	e_goto (col, 2);
	e_putline (title);

	/* Identify object being edited.
	 */
	e_goto (1, 3);
	e_ctrl ("ue");
	if (ep_filemode) {
	    sprintf (string, "PARFILE = %s\r\n", pfp->pf_pfilename);
	    e_putline (string);
	} else {
	    struct  ltask *ltp = pfp->pf_ltp;
	    sprintf (string, "PACKAGE = %s\r\n", ltp->lt_pkp->pk_name);
	    e_putline (string);
	    sprintf (string, "   TASK = %s\r\n", ltp->lt_lname);
	    e_putline (string);
	}

	for (col=0;  col < maxcol;  col++)
	    string[col] = ' ';
        string[maxcol] = '\0';
	e_ctrl ("us");
	e_goto (1, cmdline-1);		/* draw line across bottom of screen */
	e_putline (string);

	e_ctrl ("ue");
	e_ctrl ("so");
	e_goto (maxcol - 18, cmdline);

	for (i=FIRST_CMD; (i<=numcommands) && (command[i].cmd != GET_HELP); i++)
	    ;
	e_putline (command[i].keystroke);	/* show the help command */
	e_ctrl ("se");
	e_putline (" for HELP");

	fflush (stdout);
}


/* E_DRAWKEY -- Format and display the keyline.  It is assumed that for
 * arrays, the prompt occurs above the first array line.  This enables the
 * user to label his columns.  We must handle multiline prompts as well.  
 * For maximum drawing speed output is optimized using line clears and screen 
 * gotos rather than blanks to erase and position text.
 */
void
e_drawkey (void)
{
	char	valuebuf[MAXPROMPT];
	char	tempbuf[MAXPROMPT];
	int	offset, nchars;


	e_encode_vstring (parmlist[keyid], valuebuf);
	e_goto (1, line);
	e_clrline();

	if (parmlist[keyid]->p_type & PT_ARRAY) {
	    e_putline ("\t=   ");
	    e_putline (valuebuf);
	} else {
	    int  hidden;

	    hidden = (parmlist[keyid]->p_mode & M_HIDDEN);

	    /* Print parameter name.  Enclose hidden parameters in (), as in
	     * lparam.  We lose a character in the name, but at least we know
	     * when a parameter is hidden.
	     */
	    if (hidden)
		sprintf (tempbuf, "(%-7.7s=", parmlist[keyid]->p_name);
	    else
	        sprintf (tempbuf, "%-8.8s=", parmlist[keyid]->p_name);
	    e_putline (tempbuf);

	    /* Print the value string right justified in the value field.
	     */
	    nchars = strlen (valuebuf);
	    offset = PROMPTOFFSET - nchars - 1;
	    offset = (VALUEOFFSET > offset) ? VALUEOFFSET : offset;
	    e_goto (offset, line);

	    if (hidden)			/* closing ) for hidden parameters */
		strcat (valuebuf, ")");
	    e_putline (valuebuf);

	    /* Print the (possibly multiline) prompt string.  Do not write over
	     * the value string if it's a long one.
	     */
	    offset += (nchars + 1);		/* offset of prompt string */
	    if (offset < PROMPTOFFSET)
		offset = PROMPTOFFSET;

	    /* Add one to the offset (for ')' in hidden parameters) and display
	     * the prompt.  Continuation lines start at the standard prompt
	     * offset.
	     */
	    e_displayml (parmlist[keyid]->p_prompt, line, ++offset,
		PROMPTOFFSET + 1);
	}
}


/* E_INDENT_PROMPT -- Must handle multiline prompts, i.e. prompt string may
 * have imbedded newlines.  Convert newline into newline plus the number of
 * spaces to indent.
e_indent_prompt (p, bp, indent)
char	*p;
char	*bp;
int	indent;
{
	register int	i;
	register char	c;

	while ((*bp++ = c = *p++) != '\0')
	    if (c == '\n')
		for (i=0;  i < indent;  i++)
		    *bp++ = ' ';
}
 */


/* E_ENCODE_VSTRING -- Get the value as a string for editing.  If it's an array,
 * get several of the values.  If it is an array, make sure the undefined values
 * get a '***', without calling spparval (which would bomb).
 */
void
e_encode_vstring (
  struct param *pp,
  char	*outbuf
)
{
	char valuebuf[G_MAXSTRING];
	char colbuf[16];

	*outbuf = '\0';

	if (pp->p_type & PT_ARRAY) {
	    int	   first, i, nn, numonrow;
	    struct operand o;
	    short  len;			/* the length of the first dim */

	    len = pp->p_val.v_a->a_len;
	    first = firstelement[keyid];

	    nn = len - ((first-1) % len);
	    numonrow = (nn > MAX_ON_ROW) ? MAX_ON_ROW : nn;

	    for (i=first;  i < first+numonrow;  i++) {
		/* First determine if the value is undefined or not.
		 */
		poffset (i-1);
		paramget (pp, FN_VALUE);
		o = popop();

		if (opundef (&o))
		    sprintf (colbuf,"       ***");
		else {
	            if ((pp->p_type & OT_BASIC) == OT_REAL) {
			/* For real numbers, do not use spparval since we may
			 * lose exponents in the formatting.  Limit output but
			 * use the %g format directly.
			 */
			sprintf (colbuf, "%10g ", o.o_val.v_r);
		        if (index (colbuf, '.') == NULL)
			    strcat (colbuf, ".");
		    } else {
			poffset (i-1);
		        spparval (valuebuf, pp);
		    	sprintf (colbuf, "%10.10s ", valuebuf);
		    }
		}

		strcat (outbuf, colbuf);
	    }

	} else {
	    /* Do not use a high level routine such as paramget() to fetch
	     * the parameter value, as we do not want to deal with parameter
	     * indirection here.  Just print the immediate value of the
	     * parameter as a string.
	     */
	    if (opundef (&pp->p_valo))
		*outbuf = EOS;
	    else
		sprop (outbuf, &pp->p_valo);
	}
}


/* E_CHECK_VALS -- Perform range checking and reset the default if the string
 * contains a partial array (yea, even a whole array).  Parse each element of
 * the array and check it.  Also check whether there are enough elements in the
 * array.  In any case, if gquery returns an error, report that to the user.
 */
void
e_check_vals (
  char    *string
)
{
	char *gquery();		/* declare gquery as returning a pointer  */
	char *errstr;		/* pointer to the error string (or 0)     */
	char message[SZ_LINE+1];/* error message string			  */
	int  badnews;		/* a flag if an array element is in error */
	int  isarray;		/* a flag to indicate if this is an array */
	int  numonrow;		/* the number of elements on a row	  */

	isarray = parmlist[keyid]->p_type & PT_ARRAY;
	badnews = 0;

	if (cldebug) {
	    sprintf (dbg, "string = |%s|  ", string);
	    E_DEBUG (dbg);
	}

	if (isarray) {
	    char    outstring[G_MAXSTRING];
	    char    *in, *e_getfield();
	    int	    first, nelem, flen;
	    
	    /* Get the length of the first dimension, and the starting point.
	     */
	    flen = parmlist[keyid]->p_val.v_a->a_len;
	    first = firstelement[keyid];

	    /* Determine how many elements SHOULD be on the row.
	     */
	    nelem = flen - (first-1) % flen;
	    numonrow = (nelem > MAX_ON_ROW) ? MAX_ON_ROW : nelem;

	    in = string;
	    badnews = 0;
	    nelem = 0;

	    /* Parse each element of the string.
	     */
	    while (!badnews) {
		in = e_getfield (in, outstring, G_MAXSTRING);
		if (outstring[0] == '\0')
		    break;
		else
		    nelem++;
	    
		if (e_undef (outstring))
		    errstr = "OK";
		else {
		    poffset (first+nelem-2);	/* push absolute index  */
		    errstr = gquery (parmlist[keyid], outstring);
	        }

		if (strcmp (errstr, "OK") != 0) { 
		    sprintf (message, "%s [%s]?", errstr, outstring);
		    badnews++;
		}
	    }

	    if ((nelem != numonrow) && !(badnews)) {
		sprintf (message, "Expected %d elements on this line",numonrow);
		badnews++;
	    }

	} else {
	    /* Not an array.
	     */
	    errstr = gquery (parmlist[keyid], string);
	    if (strcmp (errstr, "OK") != 0) {
		strcpy (message, errstr);
		badnews++;
	    }
	}

	/* Report any errors. */
	if (badnews)
	    e_rpterror (message);

	/* Reprint the line. */
	e_drawkey();
	e_goto (startcol, line);
	fflush (stdout);
}


/* E_UNDEF -- Recognize the undefined string of 3 asterisks.
 */
int
e_undef (
  register char *s
)
{
	register int	n = 0;

	for (;  (*s != '*') && (*s != '\0');  s++)
	    ;
	for (;  (*s == '*') && (*s != '\0');  s++)
	    n++;

	return (n == 3);
}


static	char	message[SZ_LINE];	/* used by e_rpterror and e_clrerror */

/* E_RPTERROR -- Report the error for the eparam user.
 */
void
e_rpterror (
  char	*errstr
)
{
	char	*range;		/* pointer to the range error string	*/

	if (parmlist[keyid]->p_type == OT_BOOL) {
	    sprintf (message, "%s must be `yes' or `no'", errstr);
	} else if ((parmlist[keyid]->p_type == OT_STRING) 
	    && !(parmlist[keyid]->p_flags & P_UMIN)) {
	    range = enumin (parmlist[keyid]);
	    sprintf (message, "What?  %s", range);
	} else {
	    range = minmax (parmlist[keyid]);
	    sprintf (message, "%s %s", errstr, range);
	}

	/* Display at most one line of error message to avoid having to redraw
	 * the screen.
	 */
	message[maxcol-1] = '\0';
	e_display (message, cmdline, 1);
	e_putline ("\007");
	error_displayed = 1;

	/* Edit the same keyline over again.
	 */
	nextline = line;
	nextkey  = keyid;
	fflush (stdout);
}


/* E_CLRERROR -- Clear the error line, i.e. the last error message.
 */
void
e_clrerror (void)
{
	register int	i, len;

	len = strlen (message);

	for (i=0; i < len; i++)
	    message[i] = ' ';
	message[len] = '\0';
		
	e_display (message, cmdline, 1);
	error_displayed = 0;

	/* Edit the same keyline over again.
	 */
	nextline = line;
	nextkey  = keyid;
	e_goto (startcol, line);
	fflush (stdout);
}


/* E_GETFIELD -- Extract the next newline or comma delimited token from
 * a string.  Returns as the function value a pointer to the first char
 * after the token.
 */
char *
e_getfield (
  register char	*ip,		/* pointer into input string	*/
  char	*outstr,		/* receives token		*/
  int	maxch 			/* max chars out		*/
)
{
	register char	*op, *otop;

	while (*ip == ' ' || *ip == ',')
	    ip++;
	otop = &outstr[maxch];
	for (op=outstr;  *ip != '\0' && *ip != ' ' && *ip != ',';  ) {
	    *op++ = *ip++;
	    if (op >= otop)
		break;
	}
	*op = '\0';

	return (ip);
}


/* E_MOREFLAG -- Signal that there are more parameters above or below the
 * window.
 */
int
e_moreflag (
  register int topkey
)
{
	if ((numkeys == botkey) && (topkey == 1))
	    return (OK);

	if (botkey < numkeys) {
	    e_ctrl ("so");
	    e_ctrl ("us");
	    e_display ("More", botline+1, 1);
	} else {
	    e_ctrl ("us");
	    e_display ("    ", botline+1, 1);
	}

	if (topkey != 1) {
	    e_ctrl ("so");
	    e_display ("More", topline-1, 1);
	} else {
	    e_ctrl ("se");
	    e_ctrl ("ue");
	    e_display ("    ", topline-1, 1);
	}

	e_ctrl ("se");
	e_ctrl ("ue");
	fflush (stdout);

	return (OK);
}


/* E_SCROLLIT -- Scroll the window if possible.
 */
int
e_scrollit (void)
{
	register int i;

	if (nextline == cmdline) {
	    ;

	} else if (nextline > botline) {
	    topkey = nextkey;
	    nextline = topline;
	    if (parmlist[topkey]->p_type & PT_ARRAY)
		nextline += keylines[topkey] - 1;
	    e_repaint();

	} else if (nextline < topline) {
	    for (i=0;  topkeys[i] <= nextkey && topkeys[i] > 0;  i++)
		;
	    topkey = topkeys[i-1];
	    e_repaint();
	    nextline = botkeyline;		/* set in e_repaint */

	} else if (nextline != topline) {
	    for (i=0;  i <= maxpage;  i++) {
		if (topkeys[i] == nextkey && nextkey != topkey) { 
		    topkey = nextkey;
		    nextline = topline;
		    if (parmlist[topkey]->p_type & PT_ARRAY)
			nextline += keylines[topkey] - 1;
		    e_repaint();
		}
	    }
	}

	return (OK);
}


/* EDIT_HISTORY_DIRECTIVE -- Main entry point of EHIST, an interactive history
 * editor.
 *
 * EHIST is similar to the IRAF history commands to fetch a previous command,
 * except that it allows the user to edit it interactively.  The command is
 * highlighted (optionally) and the user's line editor is invoked.
 *
 * This command is invoked by:
 *
 *	ehist		(== ^)   edit the previous command
 *	ehist 3 	(== ^3)  edit command number 3
 *	ehist a* 	(== ^a*) edit the previous command beginning with 'a'
 *
 * A 'return' or EXIT_UPDATE will execute the edited command.
 * An EXIT_NOUPDATE will not execute the edited command.
 */
int
edit_history_directive (
  char	*args,			/* ehistory argument list */
  char	*new_cmd 		/* the command to be executed after editing */
)
{
	static	char *firstchr[MAX_COMMANDS]; /*array of character pointers */
	static	char string[G_MAXSTRING];
	char	arglist[SZ_LINE+1];
	int	execute, nchars, ochars, i;
	int	ice;		/* flag for interactive command editor */
	int	record;		/* record number of the history record */
	int	numchar;	/* number of characters in the new command */
	char	*lc, *sc;

	/* Convert the ehist command into the form "^histcmd", fetch the
	 * command from the history, and start EHIST up.
	 */
	arglist[0] = '^';
	strcpy (&arglist[1], args);
	execute = process_history_directive (arglist, new_cmd);

	standout = eh_standout;		/* set standout value		*/
	e_ttyinit();			/* initialize the terminal	*/
	edtinit();			/* and initialize the editor	*/
        ice = YES;

	while (ice) {
	    /* Count the number of keylines and setup the first character
	     * pointers.
	     */
	    firstchr[1] = new_cmd;
	    for (numkeys=1, sc=new_cmd;  *sc != '\0';  sc++)
		if (*sc == '\n') {
		    numkeys++;
		    firstchr[numkeys] = sc + 1;
		    keylines[numkeys] = 1;
		}

	    numkeys--;
	    firstchr[numkeys+1] = sc;

	    topline  = cmdline - numkeys;
	    botline  = cmdline - 1;
	    startcol = 1;
	    
	    numchar = strlen(new_cmd) - 1;
	    line = topline;
	    if (eh_bol)
		nextcol = startcol;
	    else
		nextcol = startcol + numchar;

	    e_ctrl ("so");
	    e_display (new_cmd, cmdline, 1);
	    e_ctrl ("se");
	    fflush (stdout);

	    *(new_cmd+numchar) = '\0';	/* get rid of the newline at the end. */
	    nextkey = 1;

	    /* Main EHIST loop.
	     */
	    while (nextkey > 0) {
		/* Copy the next command.
		 */
		sc = string, lc = firstchr[nextkey];
		while ((*lc != '\n') && (*lc != '\0')) {
		    /* KLUDGE fix for tabs for the moment. */
		    if ((*sc = *lc) == '\t')
			*sc = ' ';
		    lc++, sc++;
		}
		*sc = '\0';

		keyid = nextkey;
		/* line  = topline + keyid - 1; 24Feb87 */
		line  = topline + keyid;
		col   = nextcol;
	    
		e_goto (col, line);
		fflush (stdout);
		ochars = strlen (string);	    
		nchars = editstring (string, NO);

		/* Shift commands to the right of this one.
		 */
		if (nchars > ochars) {
		    lc = firstchr[numkeys+1] + nchars - ochars;
		    while (lc >= firstchr[keyid+1] - 1) {
			*lc = *(lc - nchars + ochars);
			--lc;
		    }
		}

		/* Insert the revised string inplace.
		 */
		for (sc=string, lc=firstchr[keyid];  *sc != '\0';  sc++, lc++)
		    *lc = *sc;
		*lc = '\n';

		/* Move the following commands if necessary.
		 */
		if (nchars < ochars)
		    for (lc=firstchr[keyid+1];  *lc !='\0';  lc++)
			*(lc+nchars-ochars) = *lc;

		/* Revise the firstchr pointers.
		 */
		for (i = keyid+1;  i <= numkeys;  i++)
		    firstchr[i] = firstchr[i] + nchars-ochars;

		numchar += nchars - ochars;
		keyid   += nextline - line;

	    } /* end of while (nextkey) */

	    *(new_cmd+numchar)   = '\n';
	    *(new_cmd+numchar+1) = '\0';

	    execute = (nextkey < 0) ? 0 : 1;

	    if (nextline < topline) {
		record = what_record() + 1;
		if (get_history (record, new_cmd, SZ_CMDBLK) == ERR)
		    ice = NO;
	    } else if (nextline > botline) {
		record = what_record() - 1;
		if (get_history (record, new_cmd, SZ_CMDBLK) == ERR)
		    ice = NO;
	    } else
		ice = NO;

	} /* end of ice loop */
	
	edtexit();
	e_ttyexit();
	printf ("\n");
	fflush (stdout);

	return (execute);
}


/* EDITSTRING -- A very limited string editor for interactive input.  The number
 * of characters in the edited string is returned as the function value.
 */
int
editstring (
  char	*string,
  int	eparam 				/* flag to indicate eparam or ehis  */
)
{
	char	oldchar;		/* save old character after delete  */
	char    oldword[G_MAXSTRING];   /* save the deleted word            */
	char    oldline[G_MAXSTRING];	/* save the deleted line            */
	char	tempstr[G_MAXSTRING];
	char	*chn;
	char	*cp;			/* pointer to char within string    */
	char	*lc;			/* pointer to last char		    */
	int	oldnum = 0;		/* for DEL_WORD and UNDEL_WORD	    */
	int	numchar;		/* number of characters in string   */
	int	cmd;			/* the command identifier	    */
	int	direction;		/* the cursor direction		    */
	int	gotstring, i, numdel, ch;

    	gotstring = NO;			/* dont have anything yet	    */

	if (eparam) {
	    /* Start out with an empty string, saving the old value of
	     * the parameter in "oldline".
	     */
	    strcpy (oldline, string);
	    numchar = 0;
	    cp  = string;
	    *cp = '\0';
	} else {
	    /* Edit history.  Start at either EOL or BOL depending upon
	     * value of switch set by user.
	     */
    	    numchar = strlen (string);
	    if (eh_bol)
		cp = string;
	    else
		cp = string + numchar;
	}

    	direction = FWD;
    	col = startcol + (cp - string);

	while (!gotstring) {

	    /* Fetch the next keystroke.
	     */
	    ch = fgetc (stdin);
	    if (error_displayed)
		e_clrerror();

	    /* Map to lower case if ucasein switch is set.  The ^ shift escape
	     * sequence is not currently supported.
	     */
	    if (e_ucasein && isupper(ch))
		ch = tolower (ch);

	    if (ch == EOF) {
		/* EOF returned; should not happen, so return.
		 */
		gotstring = YES;
		nextline  = cmdline;
		continue;

	    } else if (eparam && ch == ':' && col == startcol) {
		/* Colon escape.
		 */
		if (e_colon() == EP_EOF) {
		    gotstring = YES;
		    nextline  = cmdline;
		} else {
		    e_goto (col, line);
		    fflush (stdout);
		}
		continue;
	    
	    } else if (ch == ' ' || ch == '\t' || isprint(ch)) {
		/* Normal character.
		 */

		/* KLUDGE fix for tabs for the moment. */
		ch = (ch == '\t') ? ' ' : ch;

		/* Copy what's to the right. */
		for (lc = string + numchar +1;  lc > cp;  --lc)
		    *lc = *(lc-1);
		*cp = ch;			/* substitute the new char   */

		if (cp >= (string + G_MAXSTRING))
		    continue;
		lc = cp;  numchar++;  col++;  cp++;
		e_ctrl ("so");
		e_putline (lc); 
		e_ctrl ("se");
		e_goto (col, line);
		fflush (stdout);
		continue;

	    } else if (ch == '\r') {
		/* Carriage return.
		 */
		if (eparam)
		    gotstring = e_movedown (eparam);
		else {
		    nextkey = 0;
		    nextline  = botline;
		    gotstring = YES;
		}
		continue;

	    } else {
		/* Find out if it is a legitimate edit command.
		 */
		cmd = what_cmd (ch);
	    }

	    /* Perform the editing function.
	     */
	    switch (cmd) {

	    case MOVE_UP:
		gotstring = e_moveup (eparam);
		break;

	    case MOVE_DOWN:
		gotstring = e_movedown (eparam);
		break;

	    case MOVE_RIGHT:
		if (cp < (string+numchar))	/* dont move beyond string */
		    if (col < maxcol)		/* dont move beyond screen */
			cp++; 
		break;

	    case MOVE_LEFT:
		if (cp > string)	 	/* dont move too far */
		    --cp;
		break;

	    case NEXT_WORD:
		if (direction != AFT) {
		    if (cp != (string+numchar))
			cp = e_tonextword (cp);
		    else
			gotstring = e_movedown (eparam);
		    break;
		}
		/* fall through to the PREV_WORD case (no break) */

	    case PREV_WORD:
		if (cp != string)
		    cp  = e_toprevword (cp, string);
		else
		    gotstring = e_moveup (eparam);
		break;

	    case MOVE_EOL:
		/* Move to the end of the current line.
		 */
		if (cp < (string+numchar)) {
		    cp = string + numchar;
		    break;
		}

		if (direction == AFT)
		    gotstring = e_moveup (eparam);
		else
		    gotstring = e_movedown (eparam);
		break;

	    case MOVE_BOL:
		/* Move to the beginning of the current line.
		 */
		cp = string;
		break;

	    case NEXT_LINE:
		if (direction == AFT)
		    gotstring = e_moveup (eparam);
		else
		    gotstring = e_movedown (eparam);
	   	break;

	    case NEXT_PAGE:
		if (eparam) {
		    if (botkey != numkeys) {
			nextline = botline + 1;
			nextkey  = botkey + 1;
		    } else {
			nextline = botkeyline;
			nextkey  = botkey;
		    }
		    gotstring = YES;
		}
		break;

	    case PREV_PAGE:
		if (eparam) {
		    if (topkey != 1) {
			nextline = topline - 1;
			nextkey  = topkey - 1;
		    } else {
			nextline = topline;
			nextkey  = topkey;
		    }
		    gotstring = YES;
		}
		break;

	    case MOVE_START:
		if (eparam) {
		    if (topkey == 1) {
			nextline = topline;
			nextkey  = topkey;
		    } else {
			nextline = botline + 1;
			nextkey  = 1;
		    }
		    gotstring = YES;
		}
		break;

	    case MOVE_END:
		if (eparam) {
		    if (botkey == numkeys) {
			nextline = botkeyline;
			nextkey  = botkey;
		    } else {
			nextline = topline - 1;
			nextkey  = numkeys;
		    }
		    gotstring = YES;
		}
		break;

	    case SET_FWD:
		direction = FWD;
		break;

	    case SET_AFT:
		direction = AFT;
		break;

	    case TOGGLE_DIR:
		if (direction == AFT)
		    direction = FWD;
		else
		    direction = AFT;
		break;

	    case DEL_LEFT:
		chn = cp - 1;
		if (numchar > 0) {
		    oldchar = *chn;
		    strcpy (chn, chn+1);
		    if (cp > string)
			--cp;
		    --numchar;

		    e_display (string, line, startcol);

		    e_goto (startcol + numchar, line);
		    e_putline (" ");
		    fflush (stdout);
		}
		break;

	    case DEL_CHAR:	
		/* Delete the character under the cursor.
		 */
		chn = cp;
		if ((numchar > 0) && (cp < (string+numchar))) {
		    oldchar = *chn;
		    strcpy (chn, chn+1);
		    --numchar;

		    e_display (string, line, startcol);

		    e_goto (startcol + numchar, line);
		    e_putline (" ");
		    fflush (stdout);
		}
		break;

	    case UNDEL_CHAR:
		/* Undelete the last character deleted.
		 */
		for (lc=string+numchar+1;  lc >= cp;  --lc)
		    *lc = *(lc-1);
		*cp = oldchar;
		numchar++;
		e_display (string, line, startcol);
		break;

	    case DEL_WORD:
		if (cp >= (string + numchar))		/* end of line */
			break;

		chn = e_tonextword (cp);

		if ((numchar > 0) && (chn != cp)) {
		    numdel = chn - cp;
		    strncpy (oldword, cp, numdel);
		    oldnum = numdel;
		    strcpy (cp, chn);
		    numchar -= numdel;

		    e_display (string, line, startcol);

		    e_goto (startcol + numchar, line);
		    for (i=0;  i < numdel;  i++)
			e_putline (" ");
		    fflush (stdout);
		}
		break;

	    case UNDEL_WORD:
		if (oldnum > 0) {
		    strcpy (tempstr, cp);	/* save the end */
		    strncpy (cp, oldword, oldnum);
		    strcpy (cp+oldnum, tempstr);
		    numchar = numchar + oldnum;
		    e_display (string, line, startcol);
		}
		break;

	    case DEL_LINE:
		strcpy (oldline, cp);
		*cp= '\0';
		chn     = string + numchar;
		numdel  = chn - cp;
		numchar = cp - string;

		e_display (string, line, startcol);

		e_goto (startcol + numchar, line);
		for (i=0;  i < numdel;  i++)
		    e_putline (" ");
		fflush (stdout);
		break;

	    case UNDEL_LINE:
		/* Erase current value totally; don't want extraneous
		 * characters floating around.
		 */
		e_goto (startcol, line);
		numchar = PROMPTOFFSET - startcol;
		for (i=0;  i < numchar;  i++)
		    e_putline (" ");

		/* Now, get the old line and display it.
		 */
		strcpy (cp, oldline);
		numchar = strlen (string);
		cp = string + numchar;
		e_display (string, line, startcol);
		break;
	
	    case GET_HELP:
		show_editorhelp();

		/* fall through */

	    case REPAINT:
		if (eparam) {
		    nextkey = keyid;
		    e_repaint();
		    keyid = nextkey;
		}
		e_ctrl ("so");
		e_display (string, line, startcol);
		e_ctrl ("se");
		break;

	    case EXIT_NOUPDATE:
		if (eparam) {
		    nextline = cmdline;
		    ep_status = ERR;
		} else {
		    nextkey = -1;
		    nextline= botline;
		}	
		gotstring = YES;
		break;

	    case EXIT_UPDATE:
		if (eparam) {
		    nextline = cmdline;
		    if (numchar > 0)
			e_check_vals (string);
		} else
		    nextline = botline;

		nextkey  = 0;
		gotstring = YES;
		break;

	    default:
		e_putline ("\007");
		break;
	    }

	    col = startcol + cp - string;
	    e_goto (col, line);
	    fflush (stdout);
	}

	return (numchar);
}


/* E_TTYINIT -- Initialize the terminal, i.e., set raw mode and standout mode
 * (if enabled).  Get dimensions of terminal screen.
 */
void
e_ttyinit (void)
{
	/* Open the tty (termcap) descriptor for the terminal.
	 */
	if ((tty = c_ttyodes ("terminal")) == ERR)
	    c_erract (EA_ERROR);

	/* Set raw mode on the standard input.
	 */
	c_fseti (fileno(stdin), F_RAW, YES);

	/* The following is to support monocase (upper case only) terminals,
	 * or normal dualcase terminals in shift lock mode.  Normally the
	 * terminal driver handles this, but since this is a raw mode
	 * interface case mapping is disabled.  Determine if ucasein and
	 * ucaseout have been selected, e.g., with `stty ucasein ucaseout'.
	 */
	e_ucasein  = c_ttstati ((XINT)STDIN,  TT_UCASEIN);
	e_ucaseout = c_ttstati ((XINT)STDOUT, TT_UCASEOUT);

	/* Get the dimensions of the terminal screen from the environment.
	 * These need not agree with the physical screen dimensions given
	 * in the termcap descriptor.
	 */
	c_xttysize (&maxcol, &cmdline);
	startcol = G_STARTCOL;
	topline  = G_TOPLINE;
	botline  = cmdline - (G_CMDLINE - G_BOTLINE);

	tty_fd = fileno(stdout);
}


/* E_COLON -- Process a colon escape.  Prompt with a : on the status line,
 * get the command from the user, and either execute the command or return
 * the command to the procedure which called eparam.  As far as possible,
 * all error checking should be performed before exiting, so that eparam
 * does not exit when an invalid colon escape is entered.  EP_EOF is returned
 * as the function value if eparam is to exit.
 */
int
e_colon (void)
{
	register char	*ip, *op;
	register int	ch;
	char	buf[SZ_LINE+1], *pset;
	struct	param *pp;
	int	ucasein_set;
	int	force, n;

	ucasein_set = c_ttstati ((XINT)STDIN, TT_UCASEIN);

	/* Go to the command line, clear it and read the string value.
	 * The read is performed in raw mode to avoid a line feed and scroll
	 * when the CR is typed.
	 */
again_:
	c_ttygoto (tty_fd, tty, 1, cmdline);
	c_ttyclearln (tty_fd, tty);
	c_ttyctrl (tty_fd, tty, "se", 1);
	c_ttyputline (tty_fd, tty, "\r:", NO);
	c_flush (tty_fd);

	for (op=buf;  (ch = fgetc (stdin)) != EOF;  ) {
	    if (ch == '\177' || ch == '\010') {			/* delete */
		if (op > buf) {
		    *--op = EOS;
		    c_ttyclearln (tty_fd, tty);
		    c_ttyputline (tty_fd, tty, "\r:", NO);
		    c_ttyputline (tty_fd, tty, buf, NO);
		    c_flush (tty_fd);
		} else {
		    /* A delete at bol gets us out of colon mode.  */
		    break;
		}
	    } else if (ch == '\003' || ch == '\025') {		/* ^C, ^U */
		c_ttyclearln (tty_fd, tty);
		goto again_;
	    } else if (ch == '\n' || ch == '\r' || (op - buf) >= SZ_LINE) {
		break;
	    } else {
		fputc (ch, stdout);
		c_flush (tty_fd);
		if (ucasein_set && isupper (ch))
		    *op++ = tolower (ch);
		else
		    *op++ = ch;
	    }
	}
	*op = EOS;

	/* Parse the colon directive.
	 */
	for (ip=buf;  isspace (*ip);  ip++)
	    ;
	if (*ip == EOS) {
	    c_ttyclearln (tty_fd, tty);
	    return (OK);		/* null command */
	}

	ch = *ip++;
	if (ch == 'g' && *ip == 'o')
	    ip++;
	if ((force = (*ip == '!')))
	    ip++;
	for (;  isspace (*ip);  ip++)
	    ;
	pset = ip;

	/* Process the colon directive.
	 */
	switch (ch) {
	case 'q':
	    /* Exit.  The pfile is automatically updated unless :q! is used.
	     */
	    if (force)
		ep_update = NO;
	    return (EP_EOF);

	case 'w':
	    /* Update the pfile currently being edited if no arg, else
	     * write the named pfile.
	     */
	    if (*pset == EOS)
		n = pfilewrite (pfilep, pfilep->pf_pfilename);
	    else if (strcmp (pset, "q") == 0)		/* ":wq" */
		return (EP_EOF);
	    else {
		if (force || c_access (pset, 0,0) == NO)
		    n = pfilewrite (pfilep, pset);
		else {
		    sprintf (buf,
			"File exists - use `w! %s' to overwrite", pset);
		    e_puterr (buf);
		    return (ERR);
		}
	    }

	    sprintf (buf, " - %d parameters written to %s", n,
		(*pset == EOS) ? pfilep->pf_pfilename : pset);
	    e_putline (buf);
	    fflush (stdout);
	    return (OK);

	case 'r':
	    /* Load a new set of parameter values into the parameter set
	     * currently being edited.  If no argument is given the main
	     * task pset is reloaded.
	     */
	    if (*pset == EOS) {
		if (force) {
		    strcpy (e_nextpset, e_cx->e_pset);
		    ep_nextcmd = EP_EDIT;
		    ep_update = NO;
		    return (EP_EOF);
		} else {
		    e_puterr ("Use `r!' to reload current pset");
		    return (ERR);
		}
	    } else {
		if (e_psetok (pset)) {
		    pfilemerge (e_cx->e_cpfp, pset);

                    /* If we're forcing the new parameters, update
                     * the pfile on disk so we can execute it immediately.
                     */
                    if (force) 
                        n = pfilewrite (pfilep, pfilep->pf_pfilename);

		    e_repaint();
		    return (OK);
		} else
		    return (ERR);
	    }

	case 'e':
	    /* Edit the pset whose name is given by the string value of the
	     * current parameter.
	     */
	    if (*pset != EOS) {
		/* Edit a new pset, discarding current context.
		 */
		if (e_psetok (pset)) {
		    strcpy (e_nextpset, pset);
		    ep_nextcmd = EP_EDIT;
		    return (EP_EOF);
		} else
		    return (ERR);

	    } else {
		/* Edit the pset pointed to by the pset parameter currently
		 * under the cursor (only works for pset parameters).
		 */
		pp = parmlist[keyid];
		if (!(pp->p_type & PT_PSET)) {
		    sprintf (buf, "parameter `%s' is not a pset parameter",
			pp->p_name);
		    e_puterr (buf);
		    return (ERR);
		}

		/* Get the pset name.  This is the string value of the pset
		 * parameter, else the name of the parameter itself.
		 */
		e_encode_vstring (pp, buf);
		if (*buf == EOS)
		    pset = pp->p_name;
		else
		    pset = buf;

		if (e_psetok (pset)) {
		    strcpy (e_nextpset, pset);
		    ep_nextcmd = EP_DESCEND;
		    return (EP_EOF);
		} else
		    return (ERR);
	    }
	    
	case 'g':
	    /* Exit and run the task.
	     */
	    if (force)
		ep_update = NO;
	    if (*pset == EOS)
		pset = e_cx->e_pset;

	    if (is_pfilename (pset)) {
		e_puterr ("cannot execute a pfile");
		return (ERR);
	    } else {
		strcpy (e_nextpset, pset);
		ep_nextcmd = EP_RUN;
		return (EP_EOF);
	    }

	default:
	    e_puterr ("Invalid colon escape directive");
	    return (ERR);
	}
}


/* E_PSETOK -- Verify that the named pfile exists and can be read.  Report
 * any problems to the user.
 */
int
e_psetok (
  char	*pset
)
{
	register struct pfile *pfp;
	char	errmsg[SZ_LINE+1], *errfmt, *errarg;
	XINT	save_topd;

	save_topd = topd;
	errarg = pset;
	pfp = NULL;

	if (is_pfilename (pset)) {
	    /* Verify valid file pset.
	     */
	    if (c_access (pset, 0,0) == NO) {
		errfmt = "pfile `%s' does not exist";
		goto error_;
	    } else if ((pfp = pfileread (NULL, pset, 0)) == NULL) {
		errfmt = e_badpfile;
		goto error_;
	    }

	} else {
	    /* Verify valid ltask pset.
	     */
	    char    *x1, *pk, *lt, *x2;
	    struct  package *pkp;
	    struct  ltask *ltp;

	    breakout (pset, &x1, &pk, &lt, &x2);
	    ltp = _ltasksrch (pk, lt, &pkp);

	    if (pkp == NULL) {
		errfmt = e_pcknonexist;
		errarg = pk;
		goto error_;
	    } else if ((XINT)pkp == ERR) {
		errfmt = e_pckambig;
		errarg = pk;
		goto error_;
	    } else if (ltp == NULL) {
		errfmt = e_tnonexist;
		errarg = lt;
		goto error_;
	    } else if ((XINT)ltp == ERR) {
		errfmt = e_tambig;
		errarg = lt;
		goto error_;
	    }

	    if (!(ltp->lt_flags & LT_PFILE)) {
		errfmt = e_nopfile;
		goto error_;
	    } else if ((pfp = pfileload (ltp)) == NULL) {
		errfmt = e_badpfile;
		goto error_;
	    }
	}

	/* If we get here we presumably have a valid pset.  Return memory
	 * and return YES to the caller, indicating that the pset is valid.
	 */
	if (pfp)
	    pfileunlink (pfp);
	topd = save_topd;
	return (YES);

error_:
	sprintf (errmsg, errfmt, errarg);
	e_puterr (errmsg);
	return (NO);
}


/* E_PUTERR -- Put an error message on the command line.
 */
void
e_puterr (
  char	*errmsg
)
{
	c_ttygoto (tty_fd, tty, 1, cmdline);
	c_ttyclearln (tty_fd, tty);
	e_putline (errmsg);
}


/* E_TTYEXIT -- Turn off raw mode and standout mode and close the termcap
 * descriptor, leaving everything as we found it.
 */
void
e_ttyexit (void)
{
	c_fseti (fileno(stdin), F_RAW, NO);	/* unset raw mode */

	c_ttygoto (tty_fd, tty, 1, cmdline);
	c_ttyctrl (tty_fd, tty, "se", 1);
	c_ttycdes (tty);

	fflush (stdout);
}


/* E_MOVEUP -- Move the cursor up one line.
 */
int
e_moveup (
  int	eparam
)
{
	if (keyid != 1) {
	    /* Can go up further.
	     */
	    nextkey = keyid - 1;
	    if (line == topline)	/* over the top */
		nextline = topline - 1;
	    else {
		nextline = line - keylines[nextkey];
		if (eparam) {
		    if ((parmlist[nextkey]->p_type & PT_ARRAY))
			if (firstelement[nextkey] == 1)
			    nextline = line - 1;

		    if ((parmlist[keyid]->p_type & PT_ARRAY))
			if (firstelement[keyid] == 1)
			    nextline = nextline - keylines[keyid] + 1;
		}
		if (nextline < topline)
		    nextline = topline - 1;
	    }

	} else if (!eparam) {
	    nextline = topline - 1;
	    nextkey = -1;
	}

	return (YES);
}


/* E_MOVEDOWN -- Move the cursor down one line.
 */
int
e_movedown (
  int	eparam
)
{
	if (keyid != numkeys) { 
	    /* get downnnnn!! 
	     */
	    nextkey = keyid+1;
	    if (line == botline)
		nextline = botline+1;
	    else {
		nextline = line + keylines[keyid];
		if (eparam) {
		   if ((parmlist[keyid]->p_type & PT_ARRAY))
		       if (firstelement[keyid] == 1)  
			    nextline = line + 1;

		   /* Make room for prompt */
		   if ((parmlist[nextkey]->p_type & PT_ARRAY))
			if (firstelement[nextkey] == 1)
			    nextline = nextline + keylines[nextkey] - 1;
		}
		if (nextline > botline)
		    nextline = botline + 1;
	    }

	} else if (!eparam) {
	    nextline = botline+1;
	    nextkey  = -1;
	}

	if (cldebug) {
	    sprintf (dbg, "nextline=%d, nextkey=%d line=%d keys=%d", 
		nextline, nextkey, line, keylines[nextkey]);
	    E_DEBUG(dbg);
	}

	return (YES);
}


/* E_TONEXTWORD -- Skip forward to the beginning of the next word.
 */
char *
e_tonextword (
  register char	*ip
)
{
	ip++;

	/* Pass over leading characters. */
	while (*ip && !isspace (*ip))
	    ip++;
	
	/* Find the next character. */
	while (*ip && isspace(*ip))
	    ip++;

	return (ip);
}


/* E_TOPREVWORD -- Find the beginning of the previous word.
 */
char *
e_toprevword (
  char	*ip,
  char	*string
)
{
	--ip;

	/* Pass over leading blanks. */
	if (*ip == ' ')
	    for (;  (*ip == ' ') && (ip != string);  --ip)
		;
		
	/* Find the preceding blank. */
	for (;  (*ip != ' ') && (ip != string);  --ip)
	    ;
	if ((*ip != ' ') && (ip == string))
	    ;
	else
	    ip++;

	return (ip);
}	


/* E_CTRL -- Send a control sequence to the terminal.
 */
void
e_ctrl (
  char	*cap
)
{
	/* Check for start standout or start underline mode.
	 */
	if (strcmp(cap,"so") == 0 || strcmp(cap,"us") == 0)
	    if (standout == NO)
		return;

	c_ttyctrl (tty_fd, tty, cap, 1);
}


/* E_GOTO -- High level edcap version of ttygoto (cursor addressing).
 */
void
e_goto (
  int    col, 
  int    line
)
{
	c_ttygoto (tty_fd, tty, col, line);
}


/* E_PUTLINE -- Put a line of text to the terminal.  Do not map any embedded
 * control codes (bell will get lost).
 */
void
e_putline (
  char      *stwing
)
{
	register char	*ip, *op;
	register int	ch, n;
	char	obuf[512];
	int	map_cc=0;

	/* Map output to upper case if `stty ucaseout' mode is set (we have
	 * to do this here because of the raw i/o).
	 */
	if (e_ucaseout) {
	    for (ip=stwing, op=obuf, n=512;  --n >= 0 && (ch = *ip++) != EOS;  )
		*op++ = islower(ch) ? toupper(ch) : ch;
	    *op = EOS;
	    ip = obuf;
	} else
	    ip = stwing;
	    
	/* The flush calls are required to avoid mixing text and control
	 * sequences when doing raw i/o to monocase terminals.
	 */
	if (e_ucaseout)
	    c_flush (tty_fd);
	c_ttyputline (tty_fd, tty, ip, map_cc);
	if (e_ucaseout)
	    c_flush (tty_fd);
}


/* E_CLEAR -- Clear the screen (disables standout mode as a side effect).
 */
void
e_clear (void)
{
	c_ttyctrl (tty_fd, tty, "se", 1);
	c_ttyctrl (tty_fd, tty, "ue", 1);
	c_ttyclear (tty_fd, tty);
}


/* E_CLRLINE -- Clear the current line.
 */
void
e_clrline (void)
{
	c_ttyclearln (tty_fd, tty);
}


/* E_DISPLAY -- Output a possibly multiline string at the given screen
 * coordinates.  Each line is written starting at the same column on the
 * screen.
 */
void
e_display (
  char	*string,		/* string to be printed		*/
  int	sline, 
  int   scol 			/* starting line and column	*/
)
{
	e_displayml (string, sline, scol, scol);
}


/* E_DISPLAYML -- Display a possibly multiline prompt, with the first line
 * starting a different column than the continuation lines.  If a continuation
 * line begins with \r (CR) it will be displayed starting at column 1, rather
 * than starting at column scol.
 */
void
e_displayml (
  char	*string,		/* string to be printed			*/
  int	sline, 			/* starting line and column		*/
  int   scol,
  int	ccol 			/* start col of continuation lines	*/
)
{
	register char	*ip, *op;
	char	lbuf[512], *line;
	int	ocol;

	/* Display a series of newline delimited lines.
	 */
	for (ip=string, op=lbuf;  *ip != EOS;  )
	    for (op=lbuf;  (*op = *ip) != EOS;  op++, ip++)
		if (*op == '\n') {
		    *op = EOS;
		    /* Truncate line at right margin.  If first char is \r,
		     * starting column is column 1 rather than scol.
		     */
		    ocol = scol; line = lbuf;
		    while (*line == '\r') {
			ocol = 1;
			line++;
		    }
		    line[maxcol-ocol+1] = EOS;

		    /* Display the line. */
		    e_goto (ocol, sline++);
		    e_ctrl ("ce");
		    e_putline (line);
		    op = lbuf - 1;
		    scol = ccol;
		}

	/* Display any remaining, nonnewline-delimited line segment.
	 */
	if (op > lbuf) {
	    *op = EOS;
	    ocol = scol; line = lbuf;
	    while (*line == '\r') {
		ocol = 1;
		line++;
	    }
	    line[maxcol-ocol+1] = EOS;
	    e_goto (ocol, sline++);
	    e_putline (line);
	}
}
