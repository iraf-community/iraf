/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_xnames
#define	import_math
#include <iraf.h>

#include <ctype.h>
#include "config.h"
#include "operand.h"
#include "errs.h"
#include "task.h"
#include "param.h"
#include "proto.h"

extern	int	cldebug;

/*
 * UNOP -- Perform unary operations or expressions on one operand.
 *
 * Always perform the arithmetic in native machine type, eg, don't do integer
 * arithmetic by converting to floating and back.
 */

#define	UNSET		(-1)		/* value not set yet		*/


/* UNOP -- pop top operand from stack and push back result of performing the
 * unary operation whose code is in opcode.  An indef operand is not considered
 * fatal but is propagated through.  Call error() and do not return if find an
 * internal error or an undefined string operation.
 */
void 
unop (int opcode)
{
	register int out_type;		/* bool, int, real, string	*/
	register int in_type;		/* bool, int, real, string	*/
	struct	operand o, result;
	double	rval=0.0, rresult;	/* input value, result		*/
	long	ival=0, iresult;
	char	*sval=NULL, *sresult=NULL;
	char	fname[SZ_PATHNAME];
	char	ch, sbuf[SZ_LINE];
	char	*envget();
	int 	i;

	o = popop();			/* pop operand from stack	*/
	in_type = o.o_type;

	/* Exit if indefinite and we're not testing for it. */
	if (opindef(&o)) {		
	   if (opcode != OP_ISINDEF) {
	       result.o_type = OT_INT;
	       setopindef (&result);
	       goto pushresult;
	    } else
		in_type = OT_BOOL;
	}


	/* Check that operand is a legal type.  Determine the type of the
	 * result.  Set the input value (ival, rval, sval).
	 */

	out_type = UNSET;

	switch (opcode) {
	case OP_ABS:
	case OP_MINUS:
	    out_type = in_type;
	    /* fall through */

	case OP_INT:
	case OP_NINT:
	    if (out_type == UNSET)
		out_type = OT_INT;	/* force integer result here	*/
	    /* fall through */

	case OP_COS:
	case OP_EXP:
	case OP_LOG:
	case OP_LOG10:
	case OP_SIN:
	case OP_SQRT:
	case OP_REAL:
	case OP_TAN:
	case OP_FRAC:
	    /* Check that an improper operation is not being performed upon
	     * a string operand.  If the output result is int or real, the
	     * only legal operations are explicit type coercion via the INT
	     * and REAL intrinsic functions.
	     */
	    if (in_type == OT_STRING)
		switch (opcode) {
		case OP_INT:
		case OP_REAL:
		    break;
		default:
		    cl_error (E_UERR, e_badstrop, o.o_val.v_s);
		}

	    if (out_type == UNSET)	/* force real result here	*/
		out_type = OT_REAL;
	    break;

	case OP_STRLEN:
	    out_type = OT_INT;
	    /* fall through */

	case OP_ACCESS:			/* these all require string op	*/
	case OP_IMACCESS:
	case OP_DEFPAC:
	case OP_DEFPAR:
	case OP_DEFVAR:
	case OP_DEFTASK:
	    if (out_type == UNSET)
		out_type = OT_BOOL;
	    /* fall through */

	case OP_ENVGET:
	case OP_MKTEMP:
	case OP_OSFN:
	case OP_STRLWR:
	case OP_STRUPR:
	    if (in_type != OT_STRING)
		cl_error (E_UERR, "operand must be of type string");
	    /* fall through */

	case OP_STR:
	    if (out_type == UNSET)
		out_type = OT_STRING;
	    break;

	case OP_ISINDEF:
	    out_type = OT_BOOL;
	    break;

	default:
	    cl_error (E_IERR, e_badsw, opcode, "unop()");
	}

	/* Set the appropriate handy input value variable; check that the
	 * input type is not a boolean.
	 */
	switch (in_type) {
	case OT_BOOL:
	    if (opcode == OP_STR)
		ival = o.o_val.v_i;		/* str(bool) is ok	*/
	    else if (opcode == OP_MINUS)
		cl_error (E_UERR, "Arithmetic negation of a boolean operand");
	    else if (opcode != OP_ISINDEF)
		cl_error (E_UERR,
		"Intrinsic function called with illegal boolean argument");
	    break;
	case OT_INT:
	    ival = o.o_val.v_i;
	    rval = (double)ival;
	    break;
	case OT_REAL:
	    rval = o.o_val.v_r;
	    if (rval > MAX_LONG || -rval > MAX_LONG)
		ival = INDEFL;
	    else
		ival = (long)rval;
	    break;
	case OT_STRING:
	    sval = o.o_val.v_s;
	    break;
	default:
	    cl_error (E_IERR, e_badsw, opcode, "unop()");
	}

	/* Perform the operation.
	 */
	switch (opcode) {
	case OP_ABS:
	    if (out_type == OT_REAL)
		rresult = (rval < 0) ? -rval : rval;
	    else
		iresult = (ival < 0) ? -ival : ival;
	    break;
	case OP_ACCESS:
	    iresult = (c_access (sval, 0, 0) == YES);
	    break;
	case OP_IMACCESS:
	    iresult = (c_imaccess (sval, 0) == YES);
	    break;
	case OP_COS:
	    rresult = cos (rval);
	    break;
	case OP_DEFPAC:
	    iresult = defpac (sval);
	    break;
	case OP_DEFPAR:
	    iresult = defpar (sval);
	    break;
	case OP_DEFVAR:
	    iresult = defvar (sval);
	    break;
	case OP_DEFTASK:
	    iresult = deftask (sval);
	    break;
	case OP_EXP:
	    rresult = exp (rval);
	    break;
	case OP_FRAC:
	    if (rval < 0.0e0) {
		rresult = -rval;
		rresult = -(rresult - (int) rresult);
	    } else
		rresult = rval - (int) rval;
	    break;
        case OP_ISINDEF:
            if (in_type == OT_STRING)
                iresult = (strcmp (o.o_val.v_s, "INDEF") == 0);
            else
                iresult = opindef(&o);
	    break;
	case OP_ENVGET:
	    if ((sresult = envget (sval)) == NULL)
		cl_error (E_UERR, "Environment variable '%s' not found", sval);
	    break;
	case OP_OSFN:
	    c_fmapfn (sval, fname, SZ_PATHNAME);
	    sresult = fname;
	    break;
	case OP_STRLEN:
	    iresult = strlen (sval);
	    break;
	case OP_INT:
	    if (in_type == OT_STRING) {
		if (sscanf (sval, "%ld", &iresult) != 1)
		    cl_error (E_UERR, "Cannot coerce string `%s' to int", sval);
	    } else
		iresult = ival;
	    break;
	case OP_LOG:
	    if (rval <= 0)
		cl_error (E_UERR, "log of a negative or zero argument");
	    rresult = log (rval);
	    break;
	case OP_LOG10:
	    if (rval <= 0)
		cl_error (E_UERR, "log10 of a negative or zero argument");
	    rresult = log10 (rval);
	    break;
	case OP_MINUS:
	    if (out_type == OT_REAL)
		rresult = -rval;
	    else
		iresult = -ival;
	    break;
	case OP_MKTEMP:
	    c_mktemp (sval, fname, SZ_PATHNAME);
	    sresult = fname;
	    break;
	case OP_NINT:
	    if (in_type == OT_REAL)
		iresult = nint (rval);
	    else
		iresult = ival;
	    break;
	case OP_REAL:
	    if (in_type == OT_STRING) {
		if (sscanf (sval, "%lf", &rresult) != 1)
		    cl_error (E_UERR,
			"Cannot coerce string `%s' to real", sval);
	    } else
		rresult = rval;
	    break;
	case OP_SIN:
	    rresult = sin (rval);
	    break;
	case OP_STR:
	    pushop (&o);
	    opcast (OT_STRING);
	    o = popop();
	    sresult = o.o_val.v_s;
	    break;
	case OP_STRLWR:
	    for (i=0; (ch = o.o_val.v_s[i]) != EOS; i++)
		sbuf[i] = tolower (ch);
	    sbuf[i] = EOS;
	    sresult = sbuf;
	    break;
	case OP_STRUPR:
	    for (i=0; (ch = o.o_val.v_s[i]) != EOS; i++)
		sbuf[i] = toupper (ch);
	    sbuf[i] = EOS;
	    sresult = sbuf;
	    break;
	case OP_SQRT:
	    if (rval < 0)
		cl_error (E_UERR, "sqrt of a negative number");
	    rresult = sqrt (rval);
	    break;
	case OP_TAN:
	    rresult = tan (rval);
	    break;

	default:
	    cl_error (E_IERR, e_badsw, opcode, "unop()");
	}

	switch (out_type) {
	case OT_BOOL:
	case OT_INT:
	    result.o_val.v_i = iresult;
	    break;
	case OT_REAL:
	    result.o_val.v_r = rresult;
	    break;
	case OT_STRING:
	    result.o_val.v_s = sresult;
	    break;
	default:
	    cl_error (E_UERR, "illegal datatype in intrinsic");
	}
	result.o_type = out_type;

pushresult:
	pushop (&result);
}


/* UNEXP -- Pop top operand and replace with boolean result operand of applying
 *   logical operation in opcode.
 * Result is always an operand with o_type OP_BOOL and o_val.v_i as
 *   returned from relation.
 * Propagate bad operands through, but call error() and do not return
 *   on internal errors or undefined operations.
 * It is illegal to perform a boolean operation on a non-boolean operand;
 *   there is no automatic type coercion for booleans.
 */
void 
unexp (int opcode)
{
	struct	operand o, result;
	int	type;

	o = popop();
	type = o.o_type;

	if (opindef (&o)) {
	    result.o_type = OT_BOOL;
	    setopindef (&result);
	    goto pushresult;
	}

	switch (opcode) {
	case OP_NOT:
	    if (type != OT_BOOL)
		cl_error (E_UERR, "Boolean negation of a non-boolean operand");
	    result.o_val.v_i = !o.o_val.v_i;
	    break;
	default:
	    cl_error (E_IERR, e_badsw, opcode, "unexp()");
	}

	result.o_type = OT_BOOL;

pushresult:
	pushop (&result);
}
