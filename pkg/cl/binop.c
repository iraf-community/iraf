/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#define import_math
#define import_ctype
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "errs.h"
#include "param.h"
#include "mem.h"
#include "task.h"
#include "proto.h"


/*
 * BINOP.C -- Perform binary operations or expressions on two operands.
 *
 * Try to perform the arithmetic in native machine type, eg, don't do integer
 * arithmetic by converting to floating and back.
 */

/* Strint() looks for an integer on the left or right side of string s.
 * If none found return NULL, else return pointer to the first
 *   character after it if looking on leftside or pointer to
 *   first of the digit characters if looking on right side.
 * Make a few defines to make it easier to communicate with.
 * Used by binop() to handle fancy string arithmetic.
 *
 * N.B.: The use of the '+' operator to increment the number part of
 * a string has been restricted to strings of the form "abcde0123".
 * Hence, the "leftside" logic in the following routine is no longer used.
 */

#define	LEFTSIDE	0		/* value of side		*/
#define	RIGHTSIDE	1

char *
strint (register char *s, int side)
{
	if (side == LEFTSIDE) {
	    while (isdigit (*s))
		s++;
	} else {
	    char *sstart = s;
	    while (*s)
		s++;
	    while (s > sstart && isdigit (s[-1]))
		--s;
	}

	return (*s == '\0' ? NULL : s);
}


/* BINOP -- Pop the top two operands from the stack and perform the binary
 * operation whose code is in opcode.  Push an operand with the proper result
 *   and (possibly promoted) type.
 * If either is of type OT_STRING, result will be string and care must be
 *   taken not to pushop() the result to avoid clobbering them until done.
 * Order of operands will be as stacked from left to right during parser
 *   recognition, eg, a-b pushes a, then b.
 * Booleans are 0/1 arithmetically, or truestr/falsetr stringly.
 * INDEF operands propagate through.  We should never see an UNDEF operand.
 * Call error() and do not return if internal error or undefined string
 *   operation.
 */
void
binop (int opcode)
{
	register int typ1, typ2;
	struct	operand o1, o2, result;
	char	res[2*SZ_LINE];
	char	*o1sp;
	double	dresult=0.0;
	int	iresult=0, typecode=0;	/*  > 0 if real		*/
	long	lval;

	o2 = popop();		/* operands will be on stack backwards	*/
	o1 = popop();
	typ1 = o1.o_type & OT_BASIC;
	typ2 = o2.o_type & OT_BASIC;

	if (opindef (&o1) || opindef (&o2)) {
	    setopindef (&result);
	    goto pushresult;
	}

	/* Verify that no illegal datatype conversions are implied.  Arithmetic
	 * on booleans is illegal; arithmetic is legal on strings only in
	 * certain circumstances.
	 */
	if (typ1 == OT_BOOL || typ2 == OT_BOOL)
	    switch (opcode) {
	    case OP_ADD:
	    case OP_SUB:
	    case OP_MUL:
	    case OP_DIV:
	    case OP_POW:
		cl_error (E_UERR,
		"Illegal boolean operand in arithmetic expression");
		break;

	    case OP_MAX:
	    case OP_MIN:
	    case OP_MOD:
	    case OP_RADIX:
	    case OP_ATAN2:
	    case OP_STRIDX:
	    case OP_STRLDX:
	    case OP_STRSTR:
	    case OP_STRLSTR:
		cl_error (E_UERR,
		"Intrinsic function called with illegal boolean argument");
		break;

	    case OP_CONCAT:
		;				/* bool -> string ok. */
	    }

	if (typ1 == OT_REAL || typ2 == OT_REAL)
	    typecode = OT_REAL;
	else
	    typecode = OT_INT;

	switch (opcode) {
	case OP_ADD:
	    break;			/* any datatype is ok here	*/
	case OP_CONCAT:
	    typecode = OT_STRING;
	    break;			/* any datatype is ok here	*/
	case OP_RADIX:
	    if (typ2 != OT_INT)
		cl_error (E_UERR, "radix: second arg must be integer radix");
	    typecode = OT_STRING;
	    break;
	case OP_STRIDX:
	case OP_STRLDX:
	case OP_STRSTR:
	case OP_STRLSTR:
	    if (typ1 != OT_STRING || typ2 != OT_STRING)
		cl_error (E_UERR, "stridx: both arguments must be of type string");
	    typecode = OT_INT;
	    break;
	case OP_SUB:
	case OP_MUL:
	case OP_DIV:
	case OP_POW:
	case OP_MAX:
	case OP_MIN:
	case OP_MOD:
	case OP_ATAN2:
	    if (typ1 == OT_STRING || typ2 == OT_STRING) {
		if (typ1 == OT_STRING)
		    cl_error (E_UERR, e_badstrop, o1.o_val.v_s);
		else
		    cl_error (E_UERR, e_badstrop, o2.o_val.v_s);
	    }
	    break;

	default:
	    cl_error (E_IERR, e_badsw, opcode, "binop()");
	}

	/* The following code deals with operations which take string type
	 * operands or which produce a string result.
	 */
	if (typ1 == OT_STRING || typ2 == OT_STRING || typecode == OT_STRING) {
	    switch (opcode) {
	    case OP_ADD:
		o1sp = o1.o_val.v_s;

		if (typ1 != OT_STRING)
		    cl_error (E_UERR,
			"Illegal expression of the form 'number + string'");

		if (typ2 == OT_STRING) {
		    strcpy (res, o1sp);
		    strcat (res, o2.o_val.v_s);
		} else if (typ2 == OT_REAL) {
		    cl_error (E_UERR, e_strplusreal, o1sp);

		} else {			/* typ2 is OT_INT	*/
		    char    *cp, format[MAX_DIGITS];
		    int	    newnum;

		    cp = strint (o1sp, RIGHTSIDE);
		    if (cp != NULL) {
			/* Crack numeric string on rightside of string
			 * operand; add integer; reformat new string,
			 * trying to maintain number of digits in number.
			 */
			strncpy (res, o1sp, cp - o1sp);
			newnum = atoi(cp) + (int)VALU(&o2);
			sprintf (format, "%%0%dd", strlen (cp));
			sprintf ((char *)(res + (cp - o1sp)),
			    format, newnum);
			if (newnum < 0)
			    cl_error (E_UERR,
			    "String + integer expression produces '%s' ", res);

		    } else {
			strcpy (res, o1sp);
			for (cp=res;  *cp;  cp++)
			    ;
			sprintf (cp, "%d", (int)VALU(&o2));
		    }
		}
		break;

	    case OP_CONCAT:
		/* Convert operands to type string if necessary.
		 */
                {
		    char s2[SZ_LINE];

		    if (typ1 != OT_STRING) {
			/* Save the o2 string since the operand cast here
			 * will overwrite it.
			 */
			if (typ2 == OT_STRING) 
		            strcpy (s2, o2.o_val.v_s);
			pushop (&o1);
			opcast (OT_STRING);
			o1 = popop();
		    }
		    strcpy (res, o1.o_val.v_s);

		    if (typ2 != OT_STRING) {
			pushop (&o2);
			opcast (OT_STRING);
			o2 = popop();
		    } 

		    /* If we had to convert the first operand, use the saved
		     * string.
		     */
		    if (typ1 != OT_STRING && typ2 == OT_STRING)
			strcat (res, s2);
		    else
			strcat (res, o2.o_val.v_s);

		    break;
		}

	    case OP_RADIX:
		if (typ1 == OT_STRING) {
		    if (sscanf (o1.o_val.v_s, "%ld", &lval) != 1)
			cl_error (E_UERR, "Cannot coerce '%s' to integer",
			    o1.o_val.v_s);
		} else if (typ1 == OT_REAL) {
		    lval = (long) o1.o_val.v_r;
		} else
		    lval = (long) o1.o_val.v_i;

		sprintf (res, "%r*", o2.o_val.v_i, lval);
		break;

	    case OP_STRIDX:
		/* index = stridx (chars, string); "chars" may be a string.
		 * Return index of first occurence of any of the "chars"
		 * in "string", or ZERO if none found.
		 */
		{
		    char    *ip, *cp, ch;

		    iresult = 0;
		    for (ip=o2.o_val.v_s; !iresult && (ch = *ip) != EOS; ip++) {
			for (cp=o1.o_val.v_s;  *cp != EOS;  cp++) {
			    if (*cp == ch) {
				iresult = (ip - o2.o_val.v_s + 1);
				break;
			    }
		        }
		    }
		}

		result.o_val.v_i = iresult;
		result.o_type = OT_INT;
		goto pushresult;
		break;

	    case OP_STRLDX:
		/* index = strldx (chars, string); "chars" may be a string.
		 * Return index of last occurence of any of the "chars"
		 * in "string", or ZERO if none found.
		 */
		{
		    char    *ip, *cp, ch;
		    int	    len;

		    iresult = 0;
		    len = strlen (o2.o_val.v_s);
		    for (ip=&o2.o_val.v_s[len-1]; 
			!iresult && (ch = *ip) != EOS && ip >= o2.o_val.v_s;
			ip--) {
			    for (cp=o1.o_val.v_s;  *cp != EOS;  cp++) {
			        if (*cp == ch) {
				    iresult = (ip - o2.o_val.v_s + 1);
				    break;
			        }
			    }
		    }
		}

		result.o_val.v_i = iresult;
		result.o_type = OT_INT;
		goto pushresult;
		break;

	    case OP_STRSTR:
		/* index = strstr (s1, s2);
		 * Return index of first occurance of the string 's1' in 's2',
		 * or ZERO if none found.
		 */
		{
		    char    *ip, *cp, *fp, *tp, first_char, ch;

		    first_char = o1.o_val.v_s[0];
		    
		    /* Null patterns match any string. */
		    if (first_char == NULL) {
			result.o_val.v_i = 1;
			result.o_type = OT_INT;
			goto pushresult;
		    } else
		        iresult = 0;

		    /* Search s2 for first_char, if found check for complete
		     * match of s1, else move on.
		     */
		    for (ip=o2.o_val.v_s; !iresult && (ch = *ip) != EOS; ip++) {
			if (ch == first_char) {
			    fp = ip;
			    cp = o1.o_val.v_s;  
			    tp = ip;
			    while (*cp != EOS && *cp == *tp) {
				cp++; tp++;
			    }
			    if (*cp == EOS) {
				iresult = (fp - o2.o_val.v_s + 1);
				break;
			    }
		        }
		    }
		}

		result.o_val.v_i = iresult;
		result.o_type = OT_INT;
		goto pushresult;

	    case OP_STRLSTR:
		/* index = strstr (s1, s2);
		 * Return index of last occurance of the string 's1' in 's2',
		 * or ZERO if none found.
		 */
		{
		    char    *ip, *cp, *fp, first_char, ch;
		    int	    len;

		    first_char = o1.o_val.v_s[0];
		    
		    /* Null patterns match any string. */
		    if (first_char == NULL) {
			result.o_val.v_i = 1;
			result.o_type = OT_INT;
			goto pushresult;
		    } else
		        iresult = 0;

		    /* Search s2 for first_char, if found check for complete
		     * match of s1, else move on.
		     */
		    len = strlen (o2.o_val.v_s);
		    for (ip=&o2.o_val.v_s[len-1];
			!iresult && (ch = *ip) != EOS && ip >= o2.o_val.v_s;
			ip--) {
			    if (ch == first_char) {
			        fp = ip;
			        cp = o1.o_val.v_s;  
			        while (*cp != EOS && *cp == *ip) {
				    cp++; ip++;
				}
			        if (*cp == EOS) {
				    iresult = (fp - o2.o_val.v_s + 1);
				    break;
			        } else
				    ip = fp;
		            }
		    }
		}

		result.o_val.v_i = iresult;
		result.o_type = OT_INT;
		goto pushresult;
	    }

	    /* Cannot "goto pushresult" because would lose res core */
	    result.o_type = OT_STRING;
	    result.o_val.v_s = res;
	    pushop (&result);
	    return;
	}


	/* Hereafter, we only deal with operands of type int or real.
	 */
	if (typecode != OT_REAL)
	    typecode = 0;

	switch (opcode) {
	case OP_ADD:
		if (typecode)	dresult = VALU(&o1) + VALU(&o2); 
			else	iresult = o1.o_val.v_i + o2.o_val.v_i;
		break;

	case OP_SUB:
		if (typecode)	dresult = VALU(&o1) - VALU(&o2); 
			else	iresult = o1.o_val.v_i - o2.o_val.v_i;
		break;

	case OP_MUL:
		if (typecode)	dresult = VALU(&o1) * VALU(&o2); 
			else	iresult = o1.o_val.v_i * o2.o_val.v_i;
		break;

	case OP_DIV:
		if (typecode) {
		    if (VALU(&o2) == 0.0)
			cl_error (E_UERR, e_fdivzero, opcode, "binop()");
		    else
			dresult = VALU(&o1) / VALU(&o2); 
		} else {
		    if (o2.o_val.v_i == 0)
			cl_error (E_UERR, e_idivzero, opcode, "binop()");
		    else
			iresult = o1.o_val.v_i / o2.o_val.v_i;
		}
		break;

	case OP_POW:
		{		/* VMS & inconsistancy */
		    double val1 = VALU(&o1),val2 = VALU(&o2);
		    double sign = 1;

		    /* Exponentiation of negative numbers to real powers
		     * is not defined in general, so if we have coerced
		     * an integer exponent to real we change the mantissa to
		     * positive and deal with the sign separately.
		     */
		    if ((o2.o_type == OT_INT)  &&  (val1 < 0)) {
			sign = (o2.o_val.v_i % 2) ? -1 : 1 ;
			if (val1 < 0)
			    val1 = -val1;
		    }

		    dresult = sign * pow (val1, val2);
		    if (!typecode)
			iresult = dresult+0.5*sign; /* round */
		}
		break;

	case OP_MAX:
		if (typecode) {
			/* ritchie compiler doesn't seem to allow ?: here.
			 * result = (VALU(&o1) > VALU(&o2)) ? o1 : o2;
			 */
			if (VALU(&o1) > VALU(&o2))
			    result = o1;
			else
			    result = o2;
		} else {
			if (o1.o_val.v_i > o2.o_val.v_i)
			    result = o1;
			else
			    result = o2;
		}
		goto pushresult;

	case OP_MIN:
		if (typecode) {
			/* ritchie compiler doesn't seem to allow ?: here.
			 * result = (VALU(&o1) < VALU(&o2)) ? o1 : o2;
			 */
			if (VALU(&o1) < VALU(&o2))
			    result = o1;
			else
			    result = o2;
		} else {
			if (o1.o_val.v_i < o2.o_val.v_i)
			    result = o1;
			else
			    result = o2;
		}
		goto pushresult;

	case OP_MOD:
		if (typecode) {
			double x1 = VALU(&o1), x2 = VALU(&o2);
			dresult = x1 - ((int)(x1/x2))*x2;
		} else
			iresult = o1.o_val.v_i % o2.o_val.v_i;
		break;

	case OP_ATAN2:
		{	/* VMS & inconsistancy.	*/
		double val1 = VALU(&o1), val2 = VALU(&o2);
		dresult = atan2 (val1, val2);
		}
		typecode++;	/* force real result		*/
		break;

	default:
		cl_error (E_IERR, e_badsw, opcode, "binop()");
	}

	if (typecode) {
		result.o_val.v_r = dresult;
		result.o_type = OT_REAL; 
	} else {
		result.o_val.v_i = iresult;
		result.o_type = OT_INT;
	}
	
pushresult:
	pushop (&result);
}


/* BINEXP -- pop top two operands and push result of applying operand.
 * result o_type will be OT_BOOL and o_val.v_i as returned from relation.
 * both or neither operand may be a string; cannot be mixed.
 * order of operands  will be as stacked from left to right during parser
 *   recognition, eg, a<b pushes a, then b.
 * INDEF operands propagate through. we should never see an UNDEF operand.
 * all error() and do not return on internal error or bad string operations.
 */
void
binexp (int opcode)
{
	register int typ1, typ2;
	struct	operand o1, o2, result;
	int	strres=0, dostr=0;

	o2 = popop();		/* operands will be on stack backwards	*/
	o1 = popop();
	typ1 = o1.o_type & OT_BASIC;
	typ2 = o2.o_type & OT_BASIC;

	if ((typ1 != OT_BOOL || typ2 != OT_BOOL) &&
	    (opcode == OP_OR || opcode == OP_AND))
		cl_error (E_UERR,
		    "Non-boolean operand in a boolean expression");

	if (opcode != OP_EQ && opcode != OP_NE)
	    if (opindef (&o1) || opindef (&o2)) {
		result.o_type = OT_BOOL;
		/*
		result.o_val.v_i = 0;
		printf ("Warning: INDEF operand value in a boolean expression");
		*/
		setopindef (&result);
		goto pushresult;
	    }

	if ((typ1 == OT_STRING) && (typ2 == OT_STRING)) {
	    strres = strcmp (o1.o_val.v_s, o2.o_val.v_s);
	    dostr++;

	} else if ((typ1 == OT_STRING) || (typ2 == OT_STRING)) {
	    if (typ1 == OT_STRING)
		cl_error (E_UERR, e_badstrop, o1.o_val.v_s);
	    else
		cl_error (E_UERR, e_badstrop, o1.o_val.v_s);
	}


	switch (opcode) {
	case OP_LT:
	    if (dostr)
		result.o_val.v_i = strres < 0;
	    else
		result.o_val.v_i = VALU(&o1) < VALU(&o2); 
	    break;

	case OP_GT:
	    if (dostr)
		result.o_val.v_i = strres > 0;
	    else
		result.o_val.v_i = VALU(&o1) > VALU(&o2); 
	    break;

	case OP_LE:
	    if (dostr)
		result.o_val.v_i = (strres <= 0);
	    else
		result.o_val.v_i = (VALU(&o1) <= VALU(&o2)); 
	    break;

	case OP_GE:
	    if (dostr)
		result.o_val.v_i = (strres >= 0);
	    else
		result.o_val.v_i = (VALU(&o1) >= VALU(&o2)); 
	    break;

	case OP_EQ:
	    if (opindef (&o1) || opindef (&o2))
		result.o_val.v_i = (opindef (&o1) == opindef (&o2));
	    else {
		if (dostr)
		    result.o_val.v_i = (strres == 0);
		else
		    result.o_val.v_i = (VALU(&o1) == VALU(&o2)); 
	    }
	    break;

	case OP_NE:
	    if (opindef (&o1) || opindef (&o2))
		result.o_val.v_i = (opindef (&o1) != opindef (&o2));
	    else {
		if (dostr)
		    result.o_val.v_i = (strres != 0);
		else
		    result.o_val.v_i = (VALU(&o1) != VALU(&o2)); 
	    }
	    break;

	case OP_OR:
	    if (dostr)
		result.o_val.v_i = strlen (o1.o_val.v_s) ||
				   strlen (o2.o_val.v_s);
	    else
		result.o_val.v_i = (o1.o_val.v_i || o2.o_val.v_i);
	    break;

	case OP_AND:
	    if (dostr)
		result.o_val.v_i = strlen (o1.o_val.v_s) &&
				   strlen (o2.o_val.v_s);
	    else
		result.o_val.v_i = (o1.o_val.v_i && o2.o_val.v_i);
	    break;

	default:
	    cl_error (E_IERR, e_badsw, opcode, "binexp()");

	}

	result.o_type = OT_BOOL;

pushresult:
	pushop (&result);
}
