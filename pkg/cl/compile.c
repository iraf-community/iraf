/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "opcodes.h"
#include "mem.h"
#include "errs.h"

/*
 * COMPILE -- compile instructions at compile time, compile constants,
 * params and misc at runtime on stacks or dictionary.
 */

unsigned pc;			/* program-counter			*/
unsigned *dictionary;		/* base of dictionary			*/
unsigned topd, maxd;		/* current top and highest d. indices	*/

extern	int	cldebug;

/* compile opcode and optional arguments into stack.
 * interpret "args" according to what is being compiled.
 * if (all goes well during compilation)
 *	{advance pc, return base addr of new codeentry}
 * else
 *	{leave pc unchanged, return (ERR)}
 * TODO: be more sophisticated in guarding against compiling past topcs.
 */

/*VARARGS1*/
compile (opcode, args, args2)
int opcode, args, args2;
{
	register struct codeentry *cep;
	register status = OK;

	if (pc > topcs - 20) {
	    eprintf ("INTERNAL ERROR: pc/topcs collision: %d/%d\n", pc, topcs);
	    return (ERR);
	}

	cep = coderef (pc);
	cep->c_opcode = opcode;
	cep->c_length = 2;	/* initial length is opcode+length	*/

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
		register char *sp;
		sp = (char *)args;
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
		register unsigned *argsaddr;
		struct operand *op, *dp;

		op = (struct operand *) args;
		argsaddr = &cep->c_args;
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
		cep->c_args = args;
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
		cep->c_length += args;
		break;

	/* The INDXINCR statment has two integer args. */
	case INDXINCR: {
		unsigned int	*pargs;

		cep->c_length += 2;
		pargs = & (cep->c_args);
		*pargs++ = args;
		*pargs = args2;
		break;
		}

	default:
		cl_error (E_IERR, e_badsw, opcode, "compile()");
		status = ERR;
	}

	if (status != ERR) {
		unsigned oldpc = pc;
		pc += cep->c_length;
		return (oldpc);
	}
	return (ERR);
}


/* COMSTR -- compile string s into an arbitrary core address loc, which must be
 *   on an int boundry. 
 * allow for trailing '\0'.
 * return number of whole ints taken up by string else ERR if no room.
 * (comdstr() should be used to copy a string into the dictionary)
 */
comstr (s, loc)
register char *s;
unsigned *loc;
{
	register char *to, *from;

	from = to = (char *)loc;
	while (*to++ = *s++)
	    ;
	return (btoi((unsigned)to - (unsigned)from));
}

/* copy string s into the dictionary at topd, returning pointer to its
 *   beginning and incrementing topd properly.
 * allow for trailing '\0'.
 */
char *
comdstr (s)
char *s;
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
catdstr (es, ns)
char *es, *ns;
{
	int eslen = strlen (es) + 1;

	memneed (btoi (eslen + strlen (ns)) - btoi (eslen));
	strcat (es, ns);
}
