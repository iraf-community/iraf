/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "errs.h"
#include "operand.h"
#include "param.h"
#include "grammar.h"
#include "mem.h"
#include "task.h"		/* to get currentask for prop		*/
#include "construct.h"
#include "eparam.h"
#include "proto.h"


/*
 * OPERAND -- Primitives for operations upon operands, as used on the
 * operand stack (runtime arithmetic).
 */

extern int cldebug;
extern char *truestr;
extern char *falsestr;
extern char *nullstr;
extern char *indefstr;
extern char *indeflc;
extern char *eofstr;
extern char *epsilonstr;


/* SPROP -- Format the value of a parameter into the output string.
 */
void
sprop (register char *outstr, register struct operand *op)
{
	register int type;
	char	*index();

	if (opundef (op))
	    cl_error (E_IERR, "can not print an undefined operand");
	if (opindef (op)) {
	    strcpy (outstr, indefstr);
	    return;
	}

	type = op->o_type & OT_BASIC;
	switch (type) {
	case OT_BOOL:
	    sprintf (outstr, op->o_val.v_i == NO ? falsestr : truestr);
	    break;
	case OT_INT:
	    sprintf (outstr, "%d", op->o_val.v_i);
	    break;
	case OT_REAL:
	    /* unix's %g suppresses '.' if no fractional part */
	    sprintf (outstr, "%g", op->o_val.v_r);
	    if (index (outstr, '.') == NULL)
		strcat (outstr, ".");
	    break;
	case OT_STRING:
	    strcpy (outstr, op->o_val.v_s);
	    break;
	default:
	    /* cannot happen because there are only 2 bits for 4 types.
	    cl_error (E_IERR, e_badsw, type, "fprop()");
	     */
	    ;
	}
}


/* SPPARVAL -- Print value field of a parameter into a string.
 */
void
spparval (char *outstr, struct param *pp)
{
	struct	operand o;

	if (!(pp->p_valo.o_type & OT_UNDEF)) {
	    paramget (pp, FN_VALUE); 
	    o = popop();
	    sprop (outstr, &o);
	} else
	    outstr[0] = '\0';
}


/* Print an operand on stream fp.
 * o_val is printed with proper format; no trailing nl.
 * handle indefinite and abort on undefined.
 */
void
fprop (FILE *fp, struct operand *op)
{
	/* Use MAXPROMPT to give greatest length we expect to print.
	 */
	char	outstr[MAXPROMPT+1], *out;
	char	newstr[SZ_LINE], *new;

	sprop (outstr, op);

	/* Convert embedded newlines to \n.
	 */
	new = newstr;
	out = outstr;
	for (;  *out != '\0' && ((new-newstr) < SZ_LINE-1 );  out++, new++)  {
	    if (*out == '\n') {
		*new++ = '\\';
		*new = 'n';
	    } else {
		*new = *out;
	    }
	}
	*new = '\0';

	fputs (newstr, fp);
	if (ferror (fp))
	    cl_error (E_IERR, "write error within fprop()");
}


/* print operand, using fprop, to our t_stdout.
 */
void
oprop (struct operand *op)
{
	fprop (currentask->t_stdout, op);
}


/* print operand, using fprintf, to currentask.
 */
void
prop (struct operand *op)
{
	fprop (currentask->t_out, op);
}


/* pop the top element, which must be of type string, and use it as the
 *   name of a parameter which is then found and pushed.
 * call error() if popped op is not a string; DO NOT CAST into string.
 */
void
opindir (void)
{
	struct operand nameop;
	struct param *indirpp;
	char *pk, *t, *p, *f;

	nameop = popop();
	if ((nameop.o_type & OT_BASIC) != OT_STRING)
	    cl_error (E_IERR, "non-string operand seen by opindir()");
	breakout (nameop.o_val.v_s, &pk, &t, &p, &f);
	indirpp = paramsrch (pk, t, p);
	validparamget (indirpp, *f);
}


/* Pop top operand and replace it with one cast to type newtype.
 * Newtype is assumed to not have OT_INDEF or OT_UNDEF set.
 * Call error() if trying to convert strings to something else unless
 * it is a length 1 string conversion to integer which we take to be
 * conversion from char to int.
 *
 * Do nothing if already the correct type, regardless of whether it is indef
 *   or undef.
 * N.B. we use intimate knowledge of the stack layout to do the simple cases.
 */
void
opcast (int newtype)
{
	struct operand o, result;
	struct operand *op;

	/* Do nothing if already the correct type,
	 * regardless of whether it is indef or undef.
	 */
	op = (struct operand *) &stack[stack[topos]+1];
	if ((op->o_type & OT_BASIC) == newtype)
	    return;

	o = popop();
	result.o_type = newtype;

	if (opindef (&o)) {
	    /* manufacture another indefinite but with the new type */
	    setopindef (&result);
	    goto pushresult;
	}

	switch (newtype) {
	default:
	    /* Coerce all unknowns to type integer.  Actually this cannot
	     * happen since the 4 types are encoded in 2 bits.
	     */
	    newtype = OT_INT;
	    /* continue... */

	case OT_BOOL:
	    /* Coercion of booleans is not permitted */
	    if (o.o_type != OT_BOOL)
		cl_error (E_UERR,
		    "Non-boolean operand used where boolean expected");
	    break;

	case OT_INT:
	    switch (o.o_type) {
	    case OT_BOOL:
		cl_error (E_UERR, "Attempt to coerce a boolean to an integer");
	    case OT_INT:
		result.o_val.v_i = o.o_val.v_i;
		break;
	    case OT_REAL:
		result.o_val.v_i = o.o_val.v_r;
		break;
	    case OT_STRING:
		if (*o.o_val.v_s != '\0'  &&  *(o.o_val.v_s+1) == '\0')
		    result.o_val.v_i = (int) *o.o_val.v_s;
		else
		    cl_error (E_UERR, e_nostrcnv);
		break;
	    default:
		goto err;
	    }
	    break;

	case OT_REAL:
	    switch (o.o_type) {
	    case OT_BOOL:
		cl_error (E_UERR, "Attempt to coerce a boolean to a real");
	    case OT_INT:
		result.o_val.v_r = o.o_val.v_i;
		break;
	    case OT_REAL:
		result.o_val.v_r = o.o_val.v_r;
		break;
	    case OT_STRING:
		cl_error (E_UERR, e_nostrcnv);
	    default:
		goto err;
	    }
	    break;

	case OT_STRING: {
	    char numstr[SZ_LINE];
	    switch (o.o_type) {
	    case OT_BOOL:
		result.o_val.v_s =
			o.o_val.v_i == NO ? falsestr : truestr;
		break;
	    case OT_INT:
		sprintf (numstr, "%d", o.o_val.v_i);
		result.o_val.v_s = numstr;
		break;
	    case OT_REAL:
		sprintf (numstr, "%g", o.o_val.v_r);
		result.o_val.v_s = numstr;
		break;
	    case OT_STRING:
		strcpy (numstr, o.o_val.v_s);
		result.o_val.v_s = numstr;
		break;
	    default: goto err;
	    }

	    /* Must do pushop here to use numstr */
	    pushop (&result);
	    return;

	    } /* end case OT_STRING */
	}

pushresult:
	pushop (&result);
	return;

err:
	cl_error (E_IERR, e_badsw, o.o_type, "opcast()");
}


/* MAKEOP -- Read through string s and create and return an operand of given
 * type.  Type must be strictly OT_BASIC.  See the various cases for
 * considerations unique to each.  Set OT_UNDEF if string does not look like
 * it is the correct type or it is null length; set OT_INDEF if s is the
 * indefstr..  Null length strings of type OT_STRING are not considered
 * undefined, however.
 */
struct operand 
makeop (char *str, int type)
{
	register char *s, *ip;
	register char c;
	char	*index(), *format;
	char	hexnum[MAX_DIGITS];
	char	firstchar;
	struct	operand o;

	maybeindex = 0;
	s = str;
	if (type & ~OT_BASIC)
	    cl_error (E_IERR, e_badsw, type, "makeop()");

	/* Leading whitespace is ignored except in strings. */
	o.o_type = type;
	if (type != OT_STRING)
	    while (*s == ' ' || *s == '\t')
		s++;

	if (type != OT_STRING && 
		(!strcmp (indefstr, s) || !strcmp (indeflc, s))) {
	    setopindef (&o);
	    return (o);
	}
	if (*s == '\0' && type != OT_STRING) {
	    setopundef (&o);
	    return (o);
	}

	switch (type) {
	case OT_BOOL:
	    /* s is converted, IN PLACE, to lower case */
	    makelower (s);
	    /* Accept either "y" or "yes", "n" or "no" */
	    if (((s[0] == truestr[0]) && (s[1] == '\0')) ||
		(strcmp (s, truestr) == 0))
		o.o_val.v_i = YES;
	    else if (((s[0] == falsestr[0]) && (s[1] == '\0')) ||
		(strcmp (s, falsestr) == 0))
		o.o_val.v_i = NO;
	    else
		setopundef (&o);
	    break;

	case OT_INT:
	    /* trailing 'b' or 'B' means convert as octal.
	     * trailing 'x' or 'X' means convert as hex.
	     * Set format to appropriate scanf format.  Note we must test
	     * for hex number first, since 'b' is legal in hex numbers.
	     */
	    firstchar = *s;
	    if (*s != '\''  &&  *s != '"')
	 	makelower (s);

	    if (index (s, 'x') != NULL) {
		strcpy (hexnum, "0x");
		strcat (hexnum, s);
		format = "%x";
	    } else if (index (s, 'b') != NULL) {
		format = "%o";
	    } else
		format = "%d";

	    if (sscanf (s, format, &o.o_val.v_i) != 1) {
		/* Check if string has exactly one character.
		 * Use firstchar because it hasn't been forced to lower case.
		 */
		if (*s  && !(*(s+1)) )
		    o.o_val.v_i = firstchar;
		/* Quoted character? */
		else if ( (*s == '\''  ||  *s == '"')  &&  (*s == *(s+2) ) &&
		  !(*(s+3)) )
		    o.o_val.v_i = *(s+2);
		else
		    setopundef (&o);
	    }

	    break;

	case OT_REAL: {
	    /* If there is only a single colon this might be
	     * an array index range.  If so set flag.
	     * Check for decimal point after first colon also.
	     */
	    char *colon;

	    if ( (colon=index (s, ':') ) != NULL) {
		if (index (colon+1, ':') == NULL  &&
		  index (colon+1, '.') == NULL)
		    maybeindex++;

		o = sexa (s);
	    } else if (sscanf (s, "%lf", &o.o_val.v_r) != 1)
		setopundef (&o);
	    break;
	}
	case OT_STRING:
	    /* set v_s to s and strip off any surrounding quotes.
	     * trailing " or ' will be reset, IN-PLACE, to '\0'.
	     */
	    ip = s;
	    c = *ip++;
	    if (c == '\'' || c == '"') {
		while (*ip)
		    ip++;
		if (*--ip == c) {
		    s++;		/* skip leading quote		*/
		    *ip = '\0';		/* remove trailing quote	*/
		}
	    }
	    o.o_val.v_s = s;
	}

	return (o);
}
