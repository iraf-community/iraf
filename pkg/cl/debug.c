/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "operand.h"
#include "mem.h"
#include "grammar.h"
#include "opcodes.h"
#include "config.h"
#include "param.h"
#include "task.h"

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

extern char *nullstr;
extern int cldebug;

/* D_STACK -- Go through the instruction stack, starting at locpc, printing
 * what is found until END opcode discovered.  If ss > 0, just go through ss
 * instructions.  Done directly.
 */
d_stack (locpc, ss)
register unsigned locpc;
int ss;
{
	register struct codeentry *cep;
	int opcode;
	int errs;

	errs = 0;
	do {
		cep = coderef (locpc);
		eprintf ("%6d: ", locpc);
		opcode = cep->c_opcode;

		switch (opcode) {
		case ABSARGSET:	 eprintf ("absargset"); goto string;
		case ADDASSIGN:	 eprintf ("addassign"); goto string;
		case ASSIGN:	 eprintf ("assign\t"); goto string;
		case CALL:	 eprintf ("call\t"); goto string;
		case CATASSIGN:	 eprintf ("catassign"); goto string;
		case DIVASSIGN:	 eprintf ("divassign"); goto string;
		case GSREDIR:	 eprintf ("gsredir"); goto string;
		case INDIRABSSET: eprintf ("indirabsset"); goto string;
		case INSPECT:	 eprintf ("inspect\t"); goto string;
		case INTRINSIC:	 eprintf ("intrinsic"); goto string;
		case MULASSIGN:	 eprintf ("mulassign"); goto string;
		case OSESC:	 eprintf ("os_escape"); goto string;
		case PUSHPARAM:	 eprintf ("pushparam"); goto string;
		case SUBASSIGN:	 eprintf ("subassign"); goto string;
		case SWOFF:	 eprintf ("swoff\t"); goto string;
		case SWON:	 eprintf ("swon"); goto string;
string:
			eprintf ("\t%s\n", (char *)&cep->c_args);
			break;

		case PUSHCONST:	eprintf ("pushconst"); goto op;
op:
			{   struct operand *op;

			    op = (struct operand *) &cep->c_args;
			    eprintf ("\t");
			    if ((op->o_type & OT_BASIC) == OT_STRING)
				eprintf ("`");
			    fprop (stderr, op);
			    eprintf ("'\n");
			}
			break;

		case ADD:	 eprintf ("add\n"); break;
		case ADDPIPE:	 eprintf ("addpipe\n"); break;
		case ALLAPPEND:	 eprintf ("allappend\n"); break;
		case ALLREDIR:	 eprintf ("allredir\n"); break;
		case AND:	 eprintf ("and\n"); break;
		case APPENDOUT:	 eprintf ("append\n"); break;
		case CHSIGN:	 eprintf ("chsign\n"); break;
		case CONCAT:	 eprintf ("concat\n"); break;
		case DEFAULT:    eprintf ("default\n"); break;
		case DIV:	 eprintf ("div\n"); break;
		case END:	 eprintf ("end\n"); break;
		case EQ:	 eprintf ("eq\n"); break;
		case EXEC:	 eprintf ("exec\n"); break;
		case FSCAN:	 eprintf ("fscan\n"); break;
		case GE:	 eprintf ("ge\n"); break;
		case GETPIPE:	 eprintf ("getpipe\n"); break;
		case GT:	 eprintf ("gt\n"); break;
		case IMMED:	 eprintf ("immed\n"); break;
		case LE:	 eprintf ("le\n"); break;
		case LT:	 eprintf ("lt\n"); break;
		case MUL:	 eprintf ("mul\n"); break;
		case NE:	 eprintf ("ne\n"); break;
		case NOT:	 eprintf ("not\n"); break;
		case OR:	 eprintf ("or\n"); break;
		case POW:	 eprintf ("pow\n"); break;
		case PRINT:	 eprintf ("print\n"); break;
		case REDIR:	 eprintf ("redir\n"); break;
		case REDIRIN:	 eprintf ("redirin\n"); break;
		case RETURN:	 eprintf ("return\n"); break;
		case SCAN:	 eprintf ("scan\n"); break;
		case SUB:	 eprintf ("sub\n"); break;
		case SWITCH:     eprintf ("switch\n"); break;

		case BIFF:	 eprintf ("biff\t"); goto offset;
		case GOTO:	 eprintf ("goto\t"); goto offset;
offset:
		    /* print offset with sign, - or +, in all cases	*/
		    if ((int)cep->c_args <= 0)
			goto oneint;	/* pick up sign there	*/
		    else
			eprintf ("\t+%d\n", cep->c_args);
		    break;

		case CASE:       eprintf ("case\n"); goto oneint;
		case INDIRPOSSET: eprintf ("indirposset"); goto oneint;
		case POSARGSET:	 eprintf ("posargset"); goto oneint;
		case RMPIPES:	 eprintf ("rmpipes\t"); goto oneint;
oneint:
		    eprintf ("\t%d\n", cep->c_args);
		    break;

		/* no longer used; keep in case needed for later expansion.
		case CLTASK:	 eprintf ("cltask\t"); goto twoint;
		case CLXFER:	 eprintf ("xfer\t"); goto twoint;
		case CLXMIT:	 eprintf ("xmit\t"); goto twoint;
twoint:
		    eprintf ("\t%d, %d\n", cep->c_args, *(&cep->c_args+1));
		    break;
		 */

		default:
		    eprintf ("bad opcode, %d, at pc %d\n", opcode, locpc);
		    errs++;
		}

		locpc += cep->c_length;
		if (ss > 0 && --ss == 0)	/* ss > 0 done first!	*/
		    errs = 100;			/* simulate end		*/

	} while (opcode != END && errs < 10);
}


/* print neat things about the dictionary and stack.
 * done directly.
 */
d_d()
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
d_p()
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
d_t()
{
	register struct task *tp;
	int flags;

	eprintf ("stacked tasks (most recent first)\n\n");
	for (tp=currentask; (int)tp < (int)&stack[STACKSIZ]; tp=next_task(tp)) {
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
d_l()
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
d_f()
{
	dd_f ("logical:  ", "dev$null");
	dd_f ("physical: ", "hlib$iraf.h");
}

static
dd_f (msg, fname)
char	*msg;
char	*fname;
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
d_on()
{
	cldebug = 1;
}

/* disable debugging.
 */
d_off()
{
	cldebug = 0;
}


/* Dump operand stack until underflow occurs.
 */
e_dumpop()
{
	struct	operand o;

	forever {
	    o = popop();
	    oprop (&o);
	}
}
