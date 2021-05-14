/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "param.h"
#include "grammar.h"
#include "task.h"
#include "clmodes.h"
#include "proto.h"


/* Contains modified portions of modes.c for range checking etc. for use
 * by EPARAM.  The problem with modes.c is that it not only checks ranges, 
 * but does direct i/o to the terminal.
 */

extern	int cldebug;
static  char *e1 = "Not in batch";
static  char *e2 = "Parameter value is out of range";


/* GQUERY -- Determine if the value of a parameter given by the user is OK.
 * Also, store the new value in the parameter; in the case of a list
 * structured parameter, the new value is the name of a new list file.
 * This routine is called by EPARAM to verify that the new parameter value
 * is inrange and set the new value if so.
 */
char *
gquery (struct param *pp, char *string)
{
	register char *ip;
	char	buf[SZ_LINE];
	char	*query_status, *nlp, *errmsg;
	int	arrflag=0, offset=0, bastype=0, batch=0;
	struct	operand o;
	char	*strcpy(), *index();

	bastype = pp->p_type & OT_BASIC;
	batch   = firstask->t_flags & T_BATCH;
	arrflag = pp->p_type & PT_ARRAY;

	if (arrflag)
	    offset = getoffset(pp);

	if (batch) {
	    errmsg = e1;
	    return (errmsg);
	} else
	    query_status = strcpy (buf, string);

	ip = buf;

	/* Set o to the current value of the parameter.  Beware that some
	 * of the logical branches which follow assume that struct o has
	 * been initialized to the current value of the parameter.
	 */
	if (pp->p_type & PT_LIST) {
	    setopundef (&o);
	} else if (arrflag) {
	    poffset (offset);
	    paramget (pp, FN_VALUE);
	    o = popop ();
	} else
	    o = pp->p_valo;

	/* Handle eof, a null-length line (lone carriage return),
	 * and line with more than SZ_LINE chars.  Ignore leading whitespace
	 * if basic type is not string.
	 */
	if (query_status == NULL)
	    goto testval;

	/* Ignore leading whitespace if it is not significant for this
	 * datatype.  Do this before testing for empty line, so that a
	 * return such as " \n" is equivalent to "\n".  I.e., do not
	 * penalize the user if they type the space bar by accident before
	 * typing return to accept the default value.
	 */
	if (bastype != OT_STRING || (pp->p_type & PT_LIST))
	    while (*ip == ' ' || *ip == '\t')
		ip++;

	if (*ip == '\n') {
	    /* Blank lines usually just accept the current value
	     * but if the param in a string and is undefined,
	     * it sets the string to a (defined) nullstring.
	     */
	    if (bastype == OT_STRING && opundef (&o)) {
		*ip = '\0';
		o = makeop (ip, bastype);
	    } else
		goto testval;
	}

	/* Cancel the newline. */
	if ((nlp = index (ip, '\n')) != NULL)
	    *nlp = '\0';

	/* Finally, we have handled the pathological cases.
	 */
	if (pp->p_type & PT_LIST)
	    o = makeop (string, OT_STRING);
	else
	    o = makeop (ip, bastype);

testval:   
	if (*string == '@')
	    errmsg = "OK";
	else if (pp->p_type & PT_LIST)
	    errmsg = "OK";
	else if (inrange (pp, &o))
	    errmsg = "OK";
	else {
	    errmsg = e2;
	    return (errmsg);
	}

	if (cldebug) {
	    eprintf ("changing `%s.p_val' to ", pp->p_name);
	    fprop (stderr, &o);
	    eprintf ("\n");
	}

	/* Update param with new value.
	 */
	pushop (&o);
	if (arrflag)
	    poffset (offset);

	paramset (pp, FN_VALUE);
	pp->p_flags |= P_SET;

	return ("OK");
}


/* MINMAX -- Format the minimum and maximum values of a parameter, if any.
 */
char *
minmax (register struct param *pp)
{
	static char  message[SZ_LINE];

	/* Show the ranges if they are defined and this is a parameter
	 * type that has ranges.
	 */
	if (range_check (pp)) {
	    char   str[SZ_LINE];
	    struct operand o;

	    o.o_type = pp->p_type & OT_BASIC;

	    sprintf (message, " (minimum=");
	    if (!(pp->p_flags & (P_IMIN|P_UMIN))) {
		o.o_val = pp->p_min;
		sprop (str, &o);
		strcat (message, str);
	    }
	    strcat (message, ": maximum=");
	    if (!(pp->p_flags & (P_IMAX|P_UMAX))) {
		o.o_val = pp->p_max;
		sprop (str, &o);
		strcat(message, str);
	    }
	    strcat (message, ")");
	} else
	    message[0] = EOS;

	return (message);
}


/* ENUMIN -- Format the enumeration string for a parameter.
 */
char *
enumin (register struct param *pp)
{
	static char  message[SZ_LINE];

	if (!(pp->p_flags & (P_IMIN|P_UMIN))) {
	    char    str[SZ_LINE];
	    struct  operand o;

	    sprintf (message, " choose: ");

	    o.o_type = pp->p_type & OT_BASIC;
	    o.o_val  = pp->p_min;
	    sprop (str, &o);
	    strcat (message, str);
	} else
	    message[0] = EOS;

	return (message);
}
