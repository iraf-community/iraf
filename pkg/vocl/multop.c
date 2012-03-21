/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "clmodes.h"
#include "operand.h"
#include "mem.h"
#include "grammar.h"
#include "opcodes.h"
#include "param.h"
#include "task.h"
#include "errs.h"
#include "construct.h"
#include "ytab.h"		/* pick up yacc token #defines		*/


#define	TRIM_LEFT	1
#define	TRIM_RIGHT	2

extern  int do_error;           /* runtime error handling               */
extern  ErrCom  errcom;

extern int optbl[];
extern char *ifnames[];


/* MULTOP -- 
 */
void
multop (
    int     opcode,
    int     op_index,
    int     nargs
)
{
	int	i, n, subi[2];
	int	trim_side = TRIM_LEFT|TRIM_RIGHT;
	char	*trim = " \t";
	char	sbuf[SZ_LINE+1], from[SZ_LINE+1], to[SZ_LINE+1];
	char	*sb = sbuf;
	struct	operand o;
	int 	op = optbl[op_index];	


	memset (to, 0, SZ_LINE+1);
	memset (from, 0, SZ_LINE+1);
	memset (sbuf, 0, SZ_LINE+1);

	switch (op & OP_MASK) {
	case OP_NSCAN:
	    if (nargs > 0)
		cl_error (E_UERR, "nscan has no arguments");
	    o.o_type = OT_INT;
	    o.o_val.v_i = get_nscanval();
	    pushop (&o);
	    break;
		
	case OP_MAX:
	case OP_MIN:
	    if (nargs <= 0)
	        cl_error (E_UERR, e_geonearg, ifnames[op_index]);
	    /* just leave top op if its the only one.
	     */
	    if (nargs > 1) {
	        op &= OP_MASK;	/* avoid masking for every loop	*/
	        while (--nargs)
	    	binop (op);
	    }
	    break;

	case OP_STRSUB:
	    if (nargs != 3)
	        cl_error (E_UERR, "strsub requires 3 arguments");

	    opcast (OT_STRING);			/* get old value	*/
	    o = popop();
	    strcpy (to, o.o_val.v_s);

	    opcast (OT_STRING);			/* get new value	*/
	    o = popop();
	    strcpy (from, o.o_val.v_s);

	    opcast (OT_STRING);			/* get string arg	*/
	    o = popop();

	    strcpy (sbuf, o.o_val.v_s);		/* substitute strings	*/
	    str_replace (&sb, from, to);

	    o.o_val.v_s = sbuf;
	    pushop (&o);
	    break;

	case OP_SUBSTR:
	    if (nargs != 3)
	        cl_error (E_UERR, "substr requires 3 arguments");

	    for (n=1;  n >= 0;  n--) {	/* get indices		*/
	        opcast (OT_INT);
	        o = popop();
	        subi[n] = o.o_val.v_i;
	    }

	    opcast (OT_STRING);		/* get string arg	*/
	    o = popop();

	    if (subi[1] >= subi[0]) {
	        n = subi[1] - subi[0] + 1;
	        strncpy (sbuf, &o.o_val.v_s[subi[0]-1], n);
	    } else {
	        /* Reverse the string. */
	        n = subi[0] - subi[1] + 1;
	        for (i = 0; i < n; i++)
	    	sbuf[i] = o.o_val.v_s[subi[0]-i-1];
	    }
	    sbuf[n] = '\0';

	    o.o_val.v_s = sbuf;
	    pushop (&o);
	    break;

	case OP_TRIML:
	    trim_side &= ~TRIM_RIGHT;
	    goto trim_;
	case OP_TRIMR:
	    trim_side &= ~TRIM_LEFT;
	    goto trim_;
	case OP_TRIM:
	    {
		int	o1, o2;
		struct operand istr;
		char *index();
		extern void *memset();
trim_:
		if (nargs >= 2) {
		    /* Get the chars to trim, otherwise its whitespace.  */
		    opcast (OT_STRING);
		    trim = popop().o_val.v_s;
		}
		istr = popop();

	  	o1 = 0;  
		o2 = strlen (istr.o_val.v_s) - 1;

		memset (sbuf, 0, SZ_LINE);
		if (trim_side & TRIM_LEFT)
		    while (index (trim, (int)istr.o_val.v_s[o1])) o1++;
		if (trim_side & TRIM_RIGHT) {
		    while (index (trim, (int)istr.o_val.v_s[o2])) o2--;  
		    istr.o_val.v_s[++o2] = '\0';
		}
		strncpy (sbuf, &istr.o_val.v_s[o1], o2-o1+1);

	        o.o_type = OT_STRING;
	        o.o_val.v_s = sbuf;
	        pushop (&o);
	    }
	    break;

	case OP_ERRPOP:
	    if (nargs > 0)
	        cl_error (E_UERR, "errpop has no arguments");
	    o.o_type = OT_INT;
	    o.o_val.v_i = errcom.errflag;
	    do_error = YES;
	    errcom.nhandlers--;
	    pushop (&o);
	    break;
		
	case OP_ERRPEEK:
	    if (nargs > 0)
	        cl_error (E_UERR, "errpeek has no arguments");
	    o.o_type = OT_INT;
	    o.o_val.v_i = errcom.errflag;
	    pushop (&o);
	    break;
		
	case OP_ERRMSG:
	    if (nargs > 0)
	        cl_error (E_UERR, "errmsg has no arguments");
	    o.o_type = OT_STRING;
	    o.o_val.v_s = errcom.errmsg;
	    pushop (&o);
	    break;
		
	case OP_ERRCODE:
	    if (nargs > 0)
	        cl_error (E_UERR, "errcode has no arguments");
	    o.o_type = OT_INT;
	    o.o_val.v_i = errcom.errcode;
	    pushop (&o);
	    break;
		
	case OP_ERRTASK:
	    if (nargs > 0)
	        cl_error (E_UERR, "errmsg has no arguments");
	    o.o_type = OT_STRING;
	    o.o_val.v_s = errcom.task;
	    pushop (&o);
	    break;

	default:
	    goto err;
	}

	return;

err:    cl_error (E_IERR, e_badsw, op, "intrfunc()");
}
