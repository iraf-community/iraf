/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define	import_varargs
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "param.h"
#include "grammar.h"
#include "task.h"
#include "errs.h"

/*
 * SCAN -- free-format and formatted scan functions.
 */

extern	int cldebug;
extern	char *nullstr;
extern	char *eofstr;
extern	char *indefstr;
extern	char *indeflc;

static	int nscan_val=0;	/* value returned by NSCAN intrinsic	*/

#define	SZ_ARGBUF	512
#define	AP_START	256

/* The following must agree with libc$scanf.c. */
#define SCAN_STRING     0
struct _input {
        int     i_type;                 /* file input if !0, else str   */
        union {
        FILE    *fp;                    /* file pointer if file         */
        char    *ip;                    /* char pointer if string       */
        } u;
};


/* SCAN -- Perform the bulk of the scan,fscan intrinsic functions to do
 * free-formatted reads into nargs params.  Formatting is done by makeop()
 * according to the type of the corresponding destination param.
 * Destination may be "stdout".
 *
 * Nargs is the number of operands on the stack we need to deal with.
 *   They are all strings.  The scan procedure is actually called to
 *   process calls to both the SCAN and FSCAN intrinsics.  If scan was
 *   called, the argument "source" will be the string "stdin".  If source
 *   is null, the source is given by the first operand on the stack; it
 *   may be the special string "stdin".  Thereafter, there are exactly
 *   nargs-1 string operands each of which is the name of a destination
 *   parameter to be assigned.  The operand order must be such that the
 *   first one popped is the name of the parameter to which the first field
 *   of the scan line is to be assigned.
 *
 * EOF or OK is returned as the function value.  The number of items
 *   successfully scanned is returned by a subsequent call to NSCAN().
 *
 * query if readlist yields undefined.
 * error() may be called on various conditions.
 */

cl_scan (nargs, source)
int	nargs;
char	*source;
{
	char	buf[SZ_LINE];
	char	*bp, *start, c;
	char	*pk, *t, *p, *f;
	char	field;
	struct	operand o;
	struct	param *pp;
	int	eoftst;

	eoftst = 0;

	/* Fill buf with the line to be scanned.
	 */
	if (strcmp (source, "stdin") == 0) {
	    /* Read from the standard input (SCAN call).
	     */
	    if (fgets (buf, SZ_LINE, currentask->t_stdin) == NULL)
		eoftst++;
	    else
		lentst (buf);
	    /* First arg is an output param, not source, so increment
	     * nargs.
	     */
	    nargs++;

	} else {
	    /* Get source name from first operand (FSCAN call)
	     */
	    o = popop();
	    makelower (o.o_val.v_s);
	    if (!strcmp (o.o_val.v_s, "stdin")) {
		if (fgets (buf, SZ_LINE, currentask->t_stdin) == NULL)
		    eoftst++;
		else
		    lentst (buf);
	    } else {
		breakout (o.o_val.v_s, &pk, &t, &p, &f);
		pp = paramsrch (pk, t, p);
		paramget (pp, *f);
		opcast (OT_STRING);
		o = popop();

		if (pp->p_flags & P_LEOF)
		    eoftst++;
		else {
		    if (opundef (&o)) {
			query (pp);		/* pushes op */
			opcast (OT_STRING);
			o = popop();
		    }
		    strncpy (buf, o.o_val.v_s, SZ_LINE);
		}
	    }
	}

	if (eoftst) {
	    o.o_type = OT_INT;
	    o.o_val.v_i = CL_EOF;
	    while (nargs-- > 0)
		popop();		/* flush op stack		*/
	    pushop (&o);
	    return;
	}

	/* Take each portion of buf and assign to the given parameter.
	 */
	bp = buf;
	nscan_val = 0;

	while (nargs-- > 0) {		/* get each destination name	*/
	    o = popop();
	    makelower (o.o_val.v_s);

	    if (!strcmp (o.o_val.v_s, "stdout"))
		pp = NULL;
	    else {
		breakout (o.o_val.v_s, &pk, &t, &p, &f);
		field = *f;
		pp = paramsrch (pk, t, p);	/* never returns NULL	*/
	    }

	    /* Assign rest of line if struct type parameter.  For simple
	     * string or filename type params, the next whitespace delimited
	     * word is broken out (see below).
	     */
	    if (pp != NULL &&
		((pp->p_type & (PT_STRUCT|PT_IMCUR|PT_GCUR|PT_UKEY)) &&
		!(pp->p_type & (PT_FILNAM|PT_PSET|PT_LIST)))) {

		if (nargs != 0)
		    cl_error (E_UERR,
			"Struct type param must be final Scan argument");
		start = bp;

	    } else {
		while (*bp == ' ' || *bp == '\t')
		    bp++;
		/* It is not an error if not all params can be filled by scan.
		 * Simply break off scan, pop the unused args off the stack,
		 * and return as the function value the number of items
		 * sucessfully scanned.
		 */
		if (*bp == '\0')
		    break;
		start = bp;
		for (c = *bp;  c!=' ' && c!='\t' && c!='\0';  c = *bp)
		    bp++;
		if (c != '\0')
		    *bp++ = '\0';
	    }

	    if (pp == NULL)
		fputs (start, currentask->t_stdout);
	    else {
		o = makeop (start, pp->p_type & OT_BASIC);
		if (opundef (&o))
		    break;		/* cannot convert as basic type	*/
		pushop (&o);
		paramset (pp, field);
	    }

	    nscan_val++;
	}

	/* If we broke out of the above loop because of an unsuccessful
	 * conversion, we must pop the remaining unused operands off the stack.
	 */
	while (--nargs >= 0)
	    popop();

	o.o_type = OT_INT;
	o.o_val.v_i = nscan_val;
	pushop (&o);
}


/* CL_SCANF -- Formatted scan.  Like SCAN except that a C-scanf like format
 * statement is used to decode the input text.
 */
cl_scanf (format, nargs, input)
char	*format;
int	nargs;
char	*input;
{
	char	argbuf[SZ_ARGBUF];
	char	buf[SZ_LINE];
	char	*pk, *t, *p, *f;
	struct	operand o, pv;
	struct	param *pp;
	struct	_input in;
	int	nscan_val, eoftst;
	va_list	ap_start;
	va_list	ap;

	eoftst = 0;

	/* Fill buf with the line to be scanned.
	 */
	if (strcmp (input, "stdin") == 0) {
	    /* Read from the standard input (SCANF).
	     */
	    if (fgets (buf, SZ_LINE, currentask->t_stdin) == NULL)
		eoftst++;
	    else
		lentst (buf);
	    /* First arg is an output param, not source, so increment nargs. */
	    nargs++;

	} else {
	    /* Get source name from first operand (FSCANF).
	     */
	    o = popop();
	    makelower (o.o_val.v_s);
	    if (!strcmp (o.o_val.v_s, "stdin")) {
		if (fgets (buf, SZ_LINE, currentask->t_stdin) == NULL)
		    eoftst++;
		else
		    lentst (buf);
	    } else {
		breakout (o.o_val.v_s, &pk, &t, &p, &f);
		pp = paramsrch (pk, t, p);
		paramget (pp, *f);
		opcast (OT_STRING);
		o = popop();

		if (pp->p_flags & P_LEOF)
		    eoftst++;
		else {
		    if (opundef (&o)) {
			query (pp);		/* pushes op */
			opcast (OT_STRING);
			o = popop();
		    }
		    strncpy (buf, o.o_val.v_s, SZ_LINE);
		}
	    }
	}

	/* Check for EOF. */
	if (eoftst) {
	    o.o_type = OT_INT;
	    o.o_val.v_i = CL_EOF;
	    while (nargs-- > 0)
		popop();		/* flush op stack		*/
	    pushop (&o);
	    return;
	}

	/* Process the stacked operands and build the argument list for
	 * the scanf call.  Each argument pointer points directly to the
	 * stored parameter value in the parameter descriptor.
	 */
	ap = (va_list) &argbuf[AP_START];
	ap_start = ap;

	while (nargs-- > 0) {
	    /* Stacked operand is parameter name. */
	    o = popop();
	    makelower (o.o_val.v_s);
	    breakout (o.o_val.v_s, &pk, &t, &p, &f);
	    pp = paramsrch (pk, t, p);

	    /* Add address of parameter value to argument list.  First set
	     * the value with PARAMSET, to make sure that the pset knows
	     * that the value has been modified.
	     */
	    switch (pp->p_valo.o_type & OT_BASIC) {
	    case OT_BOOL:
		o = makeop ("yes", OT_BOOL);  pushop (&o);
		paramset (pp, FN_VALUE);
		va_arg(ap, int *) = &pp->p_valo.o_val.v_i;
		break;
	    case OT_INT:
		o = makeop ("0", OT_INT);  pushop (&o);
		paramset (pp, FN_VALUE);
		va_arg(ap, int *) = &pp->p_valo.o_val.v_i;
		break;
	    case OT_REAL:
		o = makeop ("0", OT_REAL);  pushop (&o);
		paramset (pp, FN_VALUE);
		va_arg(ap, double *) = &pp->p_valo.o_val.v_r;
		break;
	    case OT_STRING:
		o = makeop ("", OT_STRING);  pushop (&o);
		paramset (pp, FN_VALUE);
		va_arg(ap, char *) = pp->p_valo.o_val.v_s;
		break;
	    default:
		cl_error (E_UERR, "scanf: cannot scan into %s\n", o.o_val.v_s);
	    }
	}

	/* Perform the scan. */
	in.i_type = SCAN_STRING;  in.u.ip = buf;
	nscan_val = u_doscan (&in, format, &ap_start);

	o.o_type = OT_INT;
	o.o_val.v_i = nscan_val;
	pushop (&o);
}


/* GET_NSCANVAL -- Return the number of items successfully scanned in the
 * last call to SCAN.
 */
get_nscanval()
{
	return (nscan_val);
}


/* LENTST -- Test that the scan line just read did not overflow the line
 * buffer.
 */
lentst (buf)
char	*buf;
{
	char	*index();
	char	*bp;

	bp = index (buf, '\n');
	if (bp != NULL)
	    *bp = '\0';
	else
	    cl_error (E_UERR, "scan limited to %d char lines", SZ_LINE-1);
}
