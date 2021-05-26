/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "mem.h"
#include "grammar.h"
#include "opcodes.h"
#include "param.h"
#include "task.h"
#include "proto.h"


/*
 * DEBUG -- The various debugging functions.
 *
 * the D_XXX grammar rules use the d_xxx routines to dump various tables
 * for debugging purposes.
 * some of these (see setbuiltins()) are done as builtin ltasks, while others
 *   that show dictionary or stack info are not to avoid the complication of
 *   having to work around the fact that builtins are really separate tasks.
 * all write to stderr.
 */

extern	char *nullstr;
extern	int cldebug;
extern	int cltrace;
static	void dd_f();


/* D_STACK -- Go through the instruction stack, starting at locpc, printing
 * what is found until END opcode discovered.  If ss > 0, just go through ss
 * instructions.  Done directly.
 */
void
d_stack (register XINT locpc, int ss)
{
	register struct codeentry *cep;
	int n, opcode, errs = 0;
	
	do {
	    cep = coderef (locpc);
	    opcode = cep->c_opcode;

	    if ((n = d_instr (stderr, "", locpc)) <= 0) {
		errs++;
		locpc += 2;
	    } else
		locpc += n;

	    if (ss > 0 && --ss == 0)		/* ss > 0 done first!	*/
		errs = 100;			/* simulate end		*/

	} while (opcode != END && errs < 10);
}


/* D_INSTR -- Decode a single instruction on the output file.  The length of
 * the instruction in memel is returned as the function value.
 */
int
d_instr (FILE *fp, char *prefix, register XINT locpc)
{
	register struct codeentry *cep;
	int opcode, extra=0;

	cep = coderef (locpc);
	opcode = cep->c_opcode;

	fprintf (fp, "%s%6d+%d: ", prefix, locpc, cep->c_length);

	switch (opcode) {
	case ABSARGSET:   fprintf (fp, "absargset");	goto string;
	case ADDASSIGN:   fprintf (fp, "addassign");	goto string;
	case ASSIGN:	  fprintf (fp, "assign");	goto string;
	case CALL:	  fprintf (fp, "call\t");	goto string;
	case CATASSIGN:	  fprintf (fp, "catassign");	goto string;
	case DIVASSIGN:	  fprintf (fp, "divassign");	goto string;
	case GSREDIR:	  fprintf (fp, "gsredir");	goto string;
	case INDIRABSSET: fprintf (fp, "indirabsset");	goto string;
	case INSPECT:	  fprintf (fp, "inspect\t");	goto string;
	case INTRINSIC:	  fprintf (fp, "intrinsic");	goto string;
	case MULASSIGN:	  fprintf (fp, "mulassign");	goto string;
	case OSESC:	  fprintf (fp, "os_escape");	goto string;
	case PUSHPARAM:	  fprintf (fp, "pushparam");	goto string;
	case SUBASSIGN:	  fprintf (fp, "subassign");	goto string;
	case SWOFF:	  fprintf (fp, "swoff\t");	goto string;
	case SWON:	  fprintf (fp, "swon");		goto string;
string:
	    fprintf (fp, "\t%s\n", (char *)&cep->c_args);
	    break;

	case PUSHCONST:	  fprintf (fp, "pushconst"); goto op;
op:
	    {   struct operand *op;

		op = (struct operand *) &cep->c_args;
		fprintf (fp, "\t");
		if ((op->o_type & OT_BASIC) == OT_STRING)
		    fprintf (fp, "`");
		fprop (stderr, op);
		if ((op->o_type & OT_BASIC) == OT_STRING)
		    fprintf (fp, "'");
		fprintf (fp, "\n");
	    }
	    break;

	case ADD:	  fprintf (fp, "add\n"); 	break;
	case ADDPIPE:	  fprintf (fp, "addpipe\n"); 	break;
	case ALLAPPEND:	  fprintf (fp, "allappend\n"); 	break;
	case ALLREDIR:	  fprintf (fp, "allredir\n"); 	break;
	case AND:	  fprintf (fp, "and\n"); 	break;
	case APPENDOUT:	  fprintf (fp, "append\n"); 	break;
	case CHSIGN:	  fprintf (fp, "chsign\n"); 	break;
	case CONCAT:	  fprintf (fp, "concat\n"); 	break;
	case DEFAULT:     fprintf (fp, "default\n"); 	break;
	case DIV:	  fprintf (fp, "div\n"); 	break;
	case END:	  fprintf (fp, "end\n"); 	break;
	case EQ:	  fprintf (fp, "eq\n"); 	break;
	case EXEC:	  fprintf (fp, "exec\n"); 	break;
	case FSCAN:	  fprintf (fp, "fscan\n"); 	break;
	case FSCANF:	  fprintf (fp, "fscanf\n"); 	break;
	case GE:	  fprintf (fp, "ge\n"); 	break;
	case GETPIPE:	  fprintf (fp, "getpipe\n"); 	break;
	case GT:	  fprintf (fp, "gt\n"); 	break;
	case IMMED:	  fprintf (fp, "immed\n"); 	break;
	case LE:	  fprintf (fp, "le\n"); 	break;
	case LT:	  fprintf (fp, "lt\n"); 	break;
	case MUL:	  fprintf (fp, "mul\n"); 	break;
	case NE:	  fprintf (fp, "ne\n"); 	break;
	case NOT:	  fprintf (fp, "not\n"); 	break;
	case OR:	  fprintf (fp, "or\n"); 	break;
	case POW:	  fprintf (fp, "pow\n"); 	break;
	case PRINT:	  fprintf (fp, "print\n"); 	break;
	case REDIR:	  fprintf (fp, "redir\n"); 	break;
	case REDIRIN:	  fprintf (fp, "redirin\n"); 	break;
	case RETURN:	  fprintf (fp, "return\n"); 	break;
	case SCAN:	  fprintf (fp, "scan\n"); 	break;
	case SCANF:	  fprintf (fp, "scanf\n"); 	break;
	case SUB:	  fprintf (fp, "sub\n"); 	break;
	case SWITCH:      fprintf (fp, "switch\n"); 	break;

	case BIFF:	  fprintf (fp, "biff\t"); 	goto offset;
	case GOTO:	  fprintf (fp, "goto\t"); 	goto offset;
offset:
	    /* Print offset with sign, - or +, in all cases. */
	    if ((int)cep->c_args <= 0)
		goto oneint;	/* pick up sign there	*/
	    else
		 fprintf (fp, "\t+%d\n", cep->c_args);
	    break;

	case CASE:        fprintf (fp, "case\n"); 	goto oneint;
	case INDIRPOSSET: fprintf (fp, "indirposset"); 	goto oneint;
	case POSARGSET:	  fprintf (fp, "posargset"); 	goto oneint;
	case RMPIPES:	  fprintf (fp, "rmpipes\t"); 	goto oneint;
oneint:
	    fprintf (fp, "\t%d\n", cep->c_args);
	    break;

	/* Used for arrays. */
	case PUSHINDEX:   fprintf (fp, "pushindex"); 	goto oneint;
	case INDXINCR:    fprintf (fp, "indxincr");
	    /* Output two jump offsets. */
	     fprintf (fp, "\t%d, %d\t", cep->c_args, *(&cep->c_args+1));

	    /* Output array index ranges: {beg, end} * N. */
	    {   memel *ip = (memel *) &cep->c_args;
		int i, n = (int)ip[2];

		for (ip += 2, i=0;  i < n;  i++, ip += 2)
		    fprintf (fp, "%d:%d ", (XINT)*ip, (XINT)(*ip+1));
		fprintf (fp, "\n");
		extra = 2*n + 1;
	    }
	    break;

	default:
	    fprintf (fp, "bad opcode, %d, at pc %d\n", opcode, locpc);
	    return (-1);
	}

	return (cep->c_length + extra);
}


/* print neat things about the dictionary and stack.
 * done directly.
 */
void 
d_d (void)
{
	char *stackaddr = (char *)stack;  /*  just so we may subtract	*/
	char *otheraddr;

	eprintf ("\ndictionary indices:\n");
	eprintf ("\tmaxd-1\t%u (%u)\n", maxd-1, dictionary[maxd-1]);
	eprintf ("\ttopd\t%u (%u)\n", topd, dictionary[topd]);
	eprintf ("\tpachead\t%u (`%s')\n", pachead,
	    reference (package, pachead)->pk_name);
	eprintf ("\tparhead\t%u (`%s')\n", parhead,
	    reference (pfile, parhead)->pf_ltp->lt_lname);

	eprintf ("\ndictionary pointers (shown as indices)\n");
	eprintf ("\tcurpack\t%u (`%s')\n", dereference (curpack),
	    curpack->pk_name);
	eprintf ("\tdictionary\t%u\n", dictionary);

	eprintf ("\nstack indices\n");
	eprintf ("\ttopcs\t%d\n", topcs);
	eprintf ("\ttopos\t%d\n", topos);
	eprintf ("\tbasos\t%d\n", basos);
	eprintf ("\tpc\t%d\n", pc);
	otheraddr = (char *)currentask;
	eprintf ("\tcurrentask\t%u (`%s')\n", btoi (otheraddr - stackaddr),
	    currentask->t_ltp->lt_lname);
	otheraddr = (char *)firstask;
	eprintf ("\tfirstask\t%u (`%s')\n", btoi (otheraddr - stackaddr),
	    firstask->t_ltp->lt_lname);
}


/* print all loaded pfiles and their params from parhead.
 * done as a builtin task. depends on the fact that the fake param file
 * has been unlinked from parhead before the builtin is run to avoid showing
 * it. see execnewtask().
 */
void 
d_p (void)
{
	register struct pfile *pfp;
	register struct param *pp;
	register FILE *fp;
	int flags;

	fp = currentask->t_stderr;
	eprintf ("loaded parameter files -\n");
	for (pfp = reference (pfile, parhead); pfp; pfp = pfp->pf_npf) {
	    eprintf ("\n\t%s: ", pfp->pf_ltp->lt_lname);
	    flags = pfp->pf_flags;
	    if (flags & PF_UPDATE) eprintf ("updated, ");
	    if (flags & PF_FAKE) eprintf ("fake, ");
	    if (flags & PF_COPY) eprintf ("copy, ");
	    if (flags & PF_PSETREF) eprintf ("contains pset pars, ");
	    eprintf ("\n");
	    for (pp = pfp->pf_pp; pp != NULL; pp = pp->p_np)
		printparam (pp, fp);
	}
}


/* print info about the tasks currently on the control stack.
 * done as a builtin. no attempt is made to hide the task running for this 
 * builtin.
 */
void 
d_t (void)
{
	struct task *tp;
	int    flags;

	eprintf ("stacked tasks (most recent first)\n\n");
	for (tp=currentask; (XINT)tp<(XINT)&stack[STACKSIZ]; tp=next_task(tp)) {
	    flags = tp->t_flags;
	    eprintf ("%s:\t", tp->t_ltp->lt_lname);
	    if (flags & T_SCRIPT) eprintf ("script, ");
	    if (flags & T_CL) eprintf ("cl, ");
	    if (flags & T_INTERACTIVE) eprintf ("interactive, ");
	    if (flags & T_MYOUT) eprintf ("new out, ");
	    if (flags & T_MYIN) eprintf ("new in, ");
	    if (flags & T_MYERR) eprintf ("new err, ");
	    if (flags & T_MYSTDGRAPH) eprintf ("new stdgraph, ");
	    if (flags & T_MYSTDIMAGE) eprintf ("new stdimage, ");
	    if (flags & T_MYSTDPLOT) eprintf ("new stdplot, ");
	    if (flags & T_BUILTIN)
		eprintf ("builtin, ");
	    else
		eprintf ("mode = `%s' ", tp->t_modep->p_val.v_s);
	    eprintf ("\n");
	}
}


/* print all loaded packages and their ltasks from pachead.
 * builtin.
 */
void 
d_l (void)
{
	register struct package *pkp;
	register struct ltask *ltp;
	int flags;

	eprintf ("loaded packages -\n");
	for (pkp = reference (package,pachead); pkp; pkp = pkp->pk_npk) {
	    eprintf ("(%u) package `%s':\n", pkp, pkp->pk_name);
	    for (ltp = pkp->pk_ltp; ltp != NULL; ltp = ltp->lt_nlt) {
		flags = ltp->lt_flags;
		eprintf ("\t(%u)\t%s: ", ltp, ltp->lt_lname);
		if (flags & LT_BUILTIN)
		    eprintf ("builtin, ");
		else
		    eprintf ("in %s (%d), ", ltp->lt_pname,
			ltp->lt_pname);
		if (flags & LT_SCRIPT) eprintf ("script, ");
		if (!(flags & LT_PFILE)) eprintf ("no pfile, ");
		if (flags & LT_STDINB) eprintf ("b_in, ");
		if (flags & LT_STDOUTB) eprintf ("b_out, ");
		if (flags & LT_INVIS) eprintf ("invisible, ");
		eprintf ("\n");
	    }
	}
}


/* D_F -- Determine the number of logical (e.g. dev$null, stropen) and physical
 * (host system) file slots available.
 */
void 
d_f (void)
{
	dd_f ("logical:  ", "dev$null");
	dd_f ("physical: ", "hlib$iraf.h");
}

static void
dd_f (char *msg, char *fname)
{
	FILE	*fp[128];
	int	fn;

	eprintf (msg);
	fn = 0;
	while ((fp[fn] = fopen (fname, "r")) != NULL) {
	    eprintf ("%d,", fileno(fp[fn]));
	    if (++fn >= 128)
		break;
	}
	eprintf ("\n");
	while (fn > 0)
	    fclose (fp[--fn]);
}


/* enable debugging messages.
 * builtins.
 */
void
d_on (void)
{
	cldebug = 1;
}

/* disable debugging.
 */
void
d_off (void)
{
	cldebug = 0;
}

/* Enable/disable instruction tracing.
 */
void
d_trace (int value)
{
	cltrace = value;
}


/* Dump operand stack until underflow occurs.
 */
void
e_dumpop (void)
{
	struct	operand o;

	forever {
	    o = popop();
	    oprop (&o);
	}
}


/* Format a multiline exec-task message string for debug output.
 */
void
d_fmtmsg (FILE *fp, char *prefix, char *message, int width)
{
	register char *ip, *op, *cp;
	char lbuf[SZ_COMMAND], obuf[SZ_COMMAND];
	int len_prefix, nchars;

	len_prefix = strlen (prefix);

	for (ip=message, op=obuf;  *ip;  ) {
	    /* Get next message line. */
	    for (cp=lbuf, nchars=0;  (*cp++ = *ip);  ip++, nchars++) {
		if (*ip == '\\' && *(ip+1) == '\n') {
		    *cp++ = 'n';
		    nchars += 2;
		    ip += 2;
		    break;
		} else if (*ip == '\n') {
		    *(cp-1) = '\\';
		    *cp++ = 'n';
		    nchars += 2;
		    ip++;
		    break;
		}
	    }
	    *cp++ = '\0';

	    /* Flush output line if it is full. */
	    if (len_prefix + op-obuf + nchars > width) {
		if (op > obuf) {
		    *op++ = '\0';
		    fprintf (fp, "%s%s\n", prefix, obuf);
		    op = obuf;
		} else {
		    fprintf (fp, "%s%s\n", prefix, lbuf);
		    op = obuf;
		    continue;
		}
	    }

	    /* Copy line to output buffer. */
	    for (cp=lbuf;  *cp;  )
		*op++ = *cp++;
	}

	/* Flush anything left in output buffer. */
	if (op > obuf) {
	    *op++ = '\0';
	    fprintf (fp, "%s%s\n", prefix, obuf);
	}
}
