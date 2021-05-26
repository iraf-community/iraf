/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_stdarg
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "opcodes.h"
#include "mem.h"
#include "errs.h"
#include "task.h"
#include "proto.h"


/*
 * COMPILE -- compile instructions at compile time, compile constants,
 * params and misc at runtime on stacks or dictionary.
 */

memel *dictionary;		/* base of dictionary			*/
XINT   pc;			/* program-counter			*/
XINT   topd, maxd;		/* current top and highest d. indices	*/

extern	int	cldebug, cltrace;

/* compile opcode and optional arguments into stack.
 * interpret "args" according to what is being compiled.
 * if (all goes well during compilation)
 *	{advance pc, return base addr of new codeentry}
 * else
 *	{leave pc unchanged, return (ERR)}
 * TODO: be more sophisticated in guarding against compiling past topcs.
 */

/*VARARGS1*/
int
compile (int opcode, ...)
{
	register struct codeentry *cep;
	register int status = OK;
	va_list argp;


	if (pc > topcs - 20) {
	    eprintf ("INTERNAL ERROR: pc/topcs collision: %d/%d\n", pc, topcs);
	    return (ERR);
	}

	va_start (argp, opcode);

	cep = coderef (pc);
	cep->c_opcode = opcode;
	cep->c_scriptln = currentask->t_scriptln;
	cep->c_length = 3;	/* initial length is opcode+scriptln+length
				 * The order of this is important so the access
				 * to the c_args is handled properly.
				 */


	switch (opcode) {
	
	/* all these opcodes have one string argument, at args */
	case ABSARGSET:
	case ADDASSIGN:
	case ASSIGN:
	case CALL:
	case CATASSIGN:
	case DIVASSIGN:
	case GETPIPE:
	case GSREDIR:
	case INDIRABSSET:
	case INSPECT:
	case INTRINSIC:
	case OSESC:
	case MULASSIGN:
	case PUSHPARAM:
	case SUBASSIGN:
	case SWOFF:
 	case SWON: {
                char *sp = va_arg (argp, char *);
		status = comstr (sp, &cep->c_args);
		if (status != ERR)
		    cep->c_length += status;
		}
		break;

	/* these opcodes use c_args as a pointer to an operand.
	 * it is copied in-line following the new instruction in the stack.
	 * further, if type is OT_STRING, compile the string in-line following
	 * the operand and change o_val.v_s to point to it.
	 */
	case PUSHCONST: {
		register memel *argsaddr;
		struct operand *op, *dp;

		op = va_arg (argp, struct operand *);
		argsaddr = (memel *) &cep->c_args;
		dp = (struct operand *) argsaddr;
		*dp = *op;
		argsaddr += OPSIZ;
		cep->c_length += OPSIZ;
		if ((op->o_type & OT_BASIC) == OT_STRING) {
		    status = comstr (op->o_val.v_s, argsaddr);
		    if (status != ERR) {
			dp->o_val.v_s = (char *) argsaddr;
			cep->c_length += status;
		    }
		}
		} /* end of case PUSHCONST */
		break;

	/* these opcodes use no arguments */
	case ADD:
	case ALLAPPEND:
	case ALLREDIR:
	case AND:
	case APPENDOUT:
	case CHSIGN:
	case CONCAT:
	case DEFAULT:
	case DIV:
	case END:
	case EQ:
	case EXEC:
	case FSCAN:
	case FSCANF:
	case GE:
	case GT:
	case IMMED:
	case LE:
	case LT:
	case MUL:
	case NE:
	case NOT:
	case OR:
	case POW:
	case PRINT:
	case REDIR:
	case REDIRIN:
	case RETURN:
	case SCAN:
	case SCANF:
	case SUB:
	case FIXLANGUAGE:
		break;

	/* these opcodes have one simple integer argument;
	 * rather than put it after the instruction and point c_args there,
	 * just use c_args itself.
	 */
	case ADDPIPE:
	case BIFF:
	case GOTO:
	case INDIRPOSSET:
	case PUSHINDEX:
	case POSARGSET:
	case RMPIPES:
		cep->c_args = va_arg (argp, int);
		cep->c_length++;
		break;

	/* SWITCH has one argument which will be supplied after the
	 * entire switch block has been compiled.
	 */
	case SWITCH:
		cep->c_length ++;
		break;


	/* The CASE statement has a variable number of arguments
	 * depending on how many different values are set for
	 * this case block.  Just allocate the block and let
	 * the parser fill in the argument list.
	 */
	case CASE:
		cep->c_length += va_arg (argp, int);
		cep->c_args = INDEFI;	    /* sentinel be filled in later */
		break;

	/* The INDXINCR statment has two integer args. */
	case INDXINCR: {
		memel *pargs;

		cep->c_length += 2;
		pargs = (memel *) &(cep->c_args);
		*pargs++ = va_arg (argp, int);
		*pargs = va_arg (argp, int);
		break;
		}

	default:
		cl_error (E_IERR, e_badsw, opcode, "compile()");
		status = ERR;
	}

	if (cltrace >= 3)
    	    d_instr (stderr, "\t", pc);

	if (status != ERR) {
		XINT oldpc = pc;
		pc += cep->c_length;
		return (oldpc);
	}
	return (ERR);
}


/* COMSTR -- compile string s into an arbitrary core address loc, which must be
 * on an int boundry.  Allow for trailing '\0'.  Return number of whole ints 
 * taken up by string else ERR if no room.  
 * (comdstr() should be used to copy a string into the dictionary)
 */
int
comstr (register char *s, memel *loc)
{
	register char *to, *from;

	from = to = (char *)loc;
	while ( (*to++ = *s++) )
	    ;
	return (btoi((memel)to - (memel)from));
}

/* copy string s into the dictionary at topd, returning pointer to its
 *   beginning and incrementing topd properly.
 * allow for trailing '\0'.
 */
char *
comdstr (char *s)
{
	char *start;

	start = memneed (btoi (strlen (s) + 1));
	strcpy (start, s);
	return (start);
}

/* concat new string, ns, after existing string, es, in dictionary.
 * only works, of course, if memneed() was not called since es was compiled
 * originally.
 */
void
catdstr (char *es, char *ns)
{
	int eslen = strlen (es) + 1;

	memneed (btoi (eslen + strlen (ns)) - btoi (eslen));
	strcat (es, ns);
}
