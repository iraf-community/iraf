/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_ctype
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "param.h"
#include "grammar.h"
#include "mem.h"
#include "task.h"
#include "errs.h"
#include "clmodes.h"
#include "construct.h"
#include "proto.h"


/*
 * PARAM -- Operations upon parameters.
 */

extern int cldebug;
extern char *undefval;
extern char *nullstr;
extern char *eofstr;
extern char *indefstr;
extern char *indeflc;

XINT parhead;			/* dict index of first pfile 		*/


#define	INDEX_OFFSET	0	/* Offsets using index list.		*/
#define DIRECT_OFFSET	1	/* Offsets put on stack directly.	*/
int	mode_offset = INDEX_OFFSET;

char	*loc_field = "Attempt to access undefined field in local variable %s.\n";

/* PARAMFIND -- Search for a parameter with the given name off pfile *pfp.
 * If name is null, then search for one in n'th pos, counting from 0.
 *   not counting M_HIDDEN params.
 * Return NULL if cannot find one with given name or at given position
 *   or ERR if allowing abbreviations and pname is ambiguous.
 * Never return ERR if looking for a positional arg; some callers of paramfind()
 *   Depend on this and don't check for ERR; beware if change it.
 */
struct param *
paramfind (struct pfile *pfp, char *pname, int pos, int exact)
{
	register char first_char;
	register struct	param *pp;
	struct	ltask *ltp;

	if (pfp == NULL)
	    return (NULL);

	if (cldebug) {
	    eprintf ("paramfind() looking down pfile `%s'/%x for ",
		(ltp = pfp->pf_ltp) ? ltp->lt_lname : "", pfp);
	    if (pname != NULL && *pname != '\0')
		eprintf ("`%s'\n", pname);
	    else
		eprintf ("position %d\n", pos);
	}

	/* Check for both ways "name may be null" */
	if (pname == NULL || *pname == '\0') {

	    for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np)
		if (!(pp->p_mode & M_HIDDEN) && pos-- == 0)
		    return (pp);

	} else if (abbrev() && !exact) {
	    /* Settle for abbreviation of name */
	    struct param *candidate;
	    int n;

	    candidate = NULL;
	    n = strlen (pname);
	    first_char = pname[0];

	    for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np) {
		if (*pp->p_name == first_char)
		    if (!strncmp (pp->p_name, pname, n)) {
			if (pp->p_name[n] == '\0')
			    return (pp);			 /* exact hit */
			if (candidate == NULL)
			    candidate = pp;
			else
			    candidate = (struct param *) ERR;
		    }
		}

	    return (candidate);

	} else {
	    /* Name must be exact. */
	    for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np) {
		if (!strcmp (pp->p_name, pname))
		    return (pp);
	    }
	}
	
	return (NULL);
}


/* PARAMSET -- Pop top operand and assign to given field of param *pp,
 *   with possible type conversion via opcast() to pp->p_type.
 *   Be darn sure to pop an operand in all cases!
 * All preallocated string storage ends with null; take care to preserve this
 *   by never copying into full length. assigning into the name of a
 *   list-structured param closes the file if it's open and clears EOF.
 *   We don't check if the popped op is undefined.
 *
 * Parameter indirection complicates setting the p_value, p_min, and p_max
 *   fields (the only fields for which indirection is permitted).  When one
 *   of these fields is indirect it is a string valued operand containing
 *   as value a string of the form ")indirparam".  Hence, the value, min, or
 *   max field may be of type string while the parameter itself (p_type) is
 *   of some other datatype.  Indirection will be overriden if the operand
 *   to be set is a data value rather than an indirect reference string.
 *   If the operand is a data value the parameter field may change its datatype.
 *   If the operand is an indirect reference the field must already be of type
 *   string with sufficient string storage allocated for the new string.
 *   String storage must be allocated when the pfile is loaded.
 *
 * Enumerated types are implemented as a string of | separated fields
 *   stored in the p_min field.  The p_min field must have been set to some
 *   string value when the pfile was loaded or storage will not have been
 *   allocated.  While the enumerated type is supported only for string valued
 *   params, integers may be stored as strings in a string valued parameter
 *   to permit enumerating the legal values of an integer parameter, e.g.:
 *
 *	order of interpolator (3|5|7) (5):
 */
void
paramset (register struct param *pp, char field)
{
	struct	operand o;
	int	bastype;	/* OT_BASIC portion of p_type		*/
	int	valtype;	/* OT_BASIC type of current value	*/
	int	optype;		/* OT_BASIC type of operand		*/
	int	arrflag;	/* Array indicator.			*/
	int	list;		/* set if p->p_type & PT_LIST		*/
	int	len;		/* max length of storage, if in-line	*/

	o = popop();

	list = pp->p_type & PT_LIST;
	arrflag = pp->p_type & PT_ARRAY;
	bastype = pp->p_type & OT_BASIC;
	valtype = pp->p_valo.o_type & OT_BASIC;
	optype  = o.o_type & OT_BASIC;

	/* Check if unauthorized access to local variable.
	 */
	if (pp->p_mode&M_LOCAL  &&  field != FN_VALUE  &&  field != FN_NULL)
	    cl_error (E_UERR, loc_field, pp->p_name);

	/* If a CL parameter, value may need parsing to set some internal
	 * variables (logging, eparam, etc.).  Take care of this before
	 * changing the value of the parameter, in case the new value is
	 * illegal.
	 */
	if (pp->p_flags & P_CL)
	    parse_clmodes (pp, &o);

	switch (field) {
	case FN_NAME:
	    cl_error (E_UERR,
		"may not change name of parameter `%s'", pp->p_name);
	case FN_TYPE:
	    cl_error (E_UERR,
		"may not change type of parameter `%s'", pp->p_name);

	case FN_MODE: 
	    if (optype != OT_STRING)
		cl_error (E_UERR, "modes are strings");
	    if (opindef (&o))
		cl_error (E_UERR, "tried to set mode of `%s' to %s",
		    pp->p_name, indefstr);
	    o.o_type = pp->p_mode;		/* reuse briefly as a temp */
	    if ((pp->p_mode = scanmode (o.o_val.v_s)) == ERR) {
		pp->p_mode = o.o_type;		/* restore from temp	*/
		cl_error (E_UERR, "bad mode string `%s'", o.o_val.v_s);
	    }
	    break;
		
	case FN_NULL:
	case FN_VALUE:
	    /* Assigning into a list param closes an existing file,
	     * changes the name of the list file, and clears P_LEOF.
	     */
	    if (list) {
		closelist (pp);
		pp->p_flags &= ~P_LEOF;
	    }

	    /* If parameter indirection is in effect the datatype of the value
	     * field will be string, while the parameter type itself may be
	     * any datatype.  If we are overriding redirection with a real
	     * value for the parameter then the datatype of p_valo may change.
	     */
	    if (!list  &&  bastype != OT_STRING &&
		(valtype != OT_STRING || optype != OT_STRING)) {
		/* Set nonstring datatype.
		 */
		if (optype != bastype) {
		    pushop (&o);
		    opcast (bastype);
		    o = popop();
		}

		if (!arrflag)
		    pp->p_valo = o;
		else {
		    /* We must generate reference to appropriate value. */
		    int	  offset;
		    int   *p_i;
		    double *p_r;
		    
		    offset = getoffset (pp);

		    if (bastype == OT_BOOL  ||  bastype == OT_INT) {
			p_i = pp->p_aval.a_i + offset;
			*p_i = o.o_val.v_i;
		    } else if (bastype == OT_REAL) {
			p_r = pp->p_aval.a_r + offset;
			*p_r = o.o_val.v_r;
		    }
		}
		break;	/* break from switch */
	    }

	    len = pp->p_lenval;
	    if (optype != OT_STRING) {
		pushop (&o);
		opcast (bastype);
		o = popop();
	    }

	    if (bastype == OT_STRING  &&  arrflag) {
		char	**p_s;
		int	offset;

		offset = getoffset (pp);
		p_s = pp->p_aval.a_s + offset;
		strncpy (*p_s, o.o_val.v_s, len-1);
		break /* out of switch */;
	    }

	    pp->p_valo.o_type = o.o_type;
	    if (!opindef (&o))
		strncpy (pp->p_val.v_s, o.o_val.v_s, len-1);
	    break;

	case FN_MIN:				 /* minimum */
	    if (bastype == OT_BOOL ||
		pp->p_type & (PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET))
		cl_error (E_UERR, e_nominmax);

	    /* If string type and no values were enumerated in the pfile,
	     * no storage will have been allocated in the min field for the
	     * enumeration list and we must abort.  Otherwise space is avail
	     * for PF_SZMINSTR chars.
	     */
	    if (bastype == OT_STRING && pp->p_flags & P_UMIN)
		cl_error (E_UERR, "string storage not allocated for p_min");

	    if (bastype == OT_STRING ||
		(valtype == OT_STRING && optype == OT_STRING)) {
		if (optype != OT_STRING) {
		    pushop (&o);
		    opcast (OT_STRING);
		    o = popop();
		}

		if (opindef (&o))
		    pp->p_flags |= P_IMIN;
		else {
		    strncpy (pp->p_min.v_s, o.o_val.v_s, PF_SZMINSTR-1);
		    pp->p_flags &= ~(P_IMIN|P_UMIN);
		    pp->p_mino.o_type = o.o_type;
		}

	    } else {
		if (optype != bastype) {
		    pushop (&o);
		    opcast (bastype);
		    o = popop();
		}
		pp->p_mino = o;
		if (opindef (&o))
		    pp->p_flags |= P_IMIN;
		else {
		    pp->p_min = o.o_val;
		    pp->p_flags &= ~(P_IMIN|P_UMIN);
		}
	    }
	    break;

	case FN_MAX:				 /* maximum */
	    if (bastype == OT_BOOL || 
		(bastype == OT_STRING && !(pp->p_type & PT_FILNAM))) {
		cl_error (E_UERR, e_nominmax);
	    }

	    if (pp->p_type & PT_FILNAM) {
		pushop (&o);
		opcast (OT_STRING);
		o = popop();
		if (opindef (&o))
		    pp->p_flags |= P_IMAX;
		else {
		    strncpy (pp->p_max.v_s, o.o_val.v_s, PF_SZMAXSTR-1);
		    pp->p_flags &= ~(P_IMAX|P_UMAX);
		    pp->p_maxo.o_type = o.o_type;
		}

	    } else {
		pushop (&o);
		opcast (bastype);
		o = popop();
		pp->p_maxo = o;
		if (opindef (&o))
		    pp->p_flags |= P_IMAX;
		else {
		    pp->p_max = o.o_val;
		    pp->p_flags &= ~(P_IMAX|P_UMAX);
		}
	    }
	    break;

	case FN_PROMPT:		/* the prompt string; length <= before */
	    pushop (&o);
	    opcast (OT_STRING);
	    o = popop();
	    if (opindef (&o))
		*pp->p_prompt = '\0';
	    else {
		len = strlen (pp->p_prompt);
		strncpy (pp->p_prompt, o.o_val.v_s, len - 1);
	    }
	    break;

	default:
	    cl_error (E_IERR, e_badsw, field, "paramset()");
	}
}


/* VALIDPARAMGET -- Push given field of param onto stack.  Read next entry
 * in file if list-structured.  If getting FN_NULL, query if in query mode
 * or if pp is out of range.  Call error if return value would be undefined.
 */
void
validparamget (register struct param *pp, char field)
{
	struct operand o;

	paramget (pp, field);
	o = popop();
	if (opundef(&o))
	    cl_error (E_UERR,
	    "The requested field of parameter `%s' is undefined", pp->p_name);
	if (field == FN_NULL && pp->p_flags & P_LEOF)
	    cl_error (E_UERR, "EOF from list param `%s' in expression",
		pp->p_name);
	pushop (&o);
}


/* PARAMGET -- Push given field of param onto stack.  Read next entry in file
 * if list-structured.  If getting FN_NULL, query if in query mode or if pp
 * is out of range.  Value returned may be undefined.
 */
void
paramget (register struct param *pp, char field)
{
	char	mode[5];	/* used to turn bits into string	*/
	struct	operand result;
	char	buf[20];	/* to stuff the expanded type in	*/
	char 	*bp;
	int	bastype;
	int	arrflag;

	bastype = pp->p_type & OT_BASIC;
	arrflag = pp->p_type & PT_ARRAY;

	/* Check if unauthorized access to local variable.
	 */
	if (pp->p_mode&M_LOCAL  &&  field != FN_VALUE  &&  field != FN_NULL)
	    cl_error (E_UERR, loc_field, pp->p_name);

	switch (field) {
	case FN_NAME:
	    result.o_type = OT_STRING;
	    result.o_val.v_s = pp->p_name;
	    break;

	case FN_TYPE:
	    result.o_type = OT_STRING;
	    switch (pp->p_type & OT_BASIC) {
	    case OT_STRING:
		result.o_val.v_s = "s";
		break;
	    case OT_INT:
		result.o_val.v_s = "i";
		break;
	    case OT_REAL:
		result.o_val.v_s = "r";
		break;
	    case OT_BOOL:
		result.o_val.v_s = "b";
		break;
	    default:
		result.o_val.v_s = "?";
		break;
	    }
	    break;

	case FN_XTYPE:
	    result.o_type = OT_STRING;

	    bp = buf;
	    if (pp->p_type & PT_LIST)
	        *bp++ = '*';
	    else if (arrflag)
	        *bp++ = 'a';

	    switch (bastype) {
	    case OT_BOOL:
	        *bp++ = 'b';
	        break;
	    case OT_INT:
	        *bp++ = 'i';
	        break;
	    case OT_REAL:
	        *bp++ = 'r';
	        break;
	    case OT_STRING:
	        *bp++ = 's';
	        break;
	    }

	    /* Overwrite the string descriptor that appears with PT_FILNAM,
	     * PT_STRUCT and the cursors.
	     */
	    if (pp->p_type & PT_FILNAM) {
	    	    *--bp = 'f';
	        if (pp->p_type & PT_FBIN)
		    *++bp = 'b';
	        if (pp->p_type & PT_FNOE)
		    *++bp = 'n';
	        if (pp->p_type & PT_FER)
		    *++bp = 'r';
	        if (pp->p_type & PT_FTXT)
		    *++bp = 't';
	        if (pp->p_type & PT_FEW)
		    *++bp = 'w';
	        *++bp = '\0';

	    } else if (pp->p_type & PT_STRUCT) {
	        strcpy (--bp, "struct");
	    } else if (pp->p_type & PT_GCUR) {
	        strcpy (--bp, "gcur");
	    } else if (pp->p_type & PT_IMCUR) {
	        strcpy (--bp, "imcur");
	    } else if (pp->p_type & PT_UKEY) {
	        strcpy (--bp, "ukey");
	    } else if (pp->p_type & PT_PSET) {
	        strcpy (--bp, "pset");
	    } else
	        *bp = '\0';

	    *bp = '\0';

	    result.o_val.v_s = buf;
	    break;


	case FN_MODE:
	    makemode (pp, mode);
	    result.o_type = OT_STRING;
	    result.o_val.v_s = mode;
	    break;

	case FN_NULL:
	    /* Without an explicit field we give the meaningful "worth"
	     * of the param, which is not necessarilly the 4th param field.
	     * If PT_LIST, read entry from list.
	     */
	    if (effmode (pp) & M_QUERY) {
		/* Just query to get result. */
		query (pp);
		result = popop();
	    } else {
		/* Use pp to get result; query if not in range.
		 */
		if (pp->p_type & PT_LIST) {
		    result = readlist (pp);		/* may set P_LEOF  */
		} else if (arrflag) {
		    /* If an array get appropriate value.
		     */
		    int offset;

		    offset = getoffset(pp);
		    result.o_type = bastype;
		    if (bastype == OT_BOOL  ||  bastype == OT_INT)
			result.o_val.v_i = *(pp->p_aval.a_i + offset);
		    else if (bastype == OT_REAL)
			result.o_val.v_r = *(pp->p_aval.a_r + offset);
		    else if (bastype == OT_STRING)
			result.o_val.v_s = *(pp->p_aval.a_s + offset);
		} else
		    result = pp->p_valo;

		/* Do not range check if we have an indirect reference.
		 */
		if (!((result.o_type & OT_BASIC) == OT_STRING &&
		    *result.o_val.v_s == PF_INDIRECT))
		    if (!(pp->p_flags & P_LEOF) && !inrange (pp, &result)) {
			query (pp);
			result = popop();
		    }
	    }
	    break;

	case FN_VALUE:
	    /* Explicit reference to the "value" field means return the
	     * value, or if indirect, the file name for the indirection.
	     */
	    if (arrflag) {
		int offset;

		offset = getoffset(pp);
		result.o_type = bastype;
		if (bastype == OT_BOOL  ||  bastype == OT_INT)
		    result.o_val.v_i = *(pp->p_aval.a_i + offset);
		else if (bastype == OT_REAL)
		    result.o_val.v_r = *(pp->p_aval.a_r + offset);
		else if (bastype == OT_STRING)
		    result.o_val.v_s = *(pp->p_aval.a_s + offset);
	    } else
	    	result = pp->p_valo;
	    break;

	case FN_LENGTH:
	    result.o_type = OT_INT;
	    result.o_val.v_i = pp->p_lenval;
	    break;

	case FN_MIN:
	    if (pp->p_flags & P_UMIN)
		setopundef (&result);
	    else if (pp->p_flags & P_IMIN)
		setopindef (&result);
	    else
		result = pp->p_mino;
	    break;

	case FN_MAX:
	    if (pp->p_flags & P_UMAX)
		setopundef (&result);
	    else if (pp->p_flags & P_IMAX)
		setopindef (&result);
	    else
		result = pp->p_maxo;
	    break;

	case FN_PROMPT:
	    result.o_type = OT_STRING;
	    result.o_val.v_s = pp->p_prompt;
	    break;

	default:
	    cl_error (E_IERR, e_badsw, field, "paramget()");
	}

	/* Parameter indirection.  If the value of the parameter is given as
	 * ")paramspec" use the value of the referenced parameter.  Multiple
	 * levels of indirection are permitted.
	 */
	if ((result.o_type & OT_BASIC) == OT_STRING &&
	    *result.o_val.v_s == PF_INDIRECT) {

	    char	redir[SZ_FNAME];
	    struct	param *np;
	    char	*pk, *t, *p, *f;

	    strncpy (redir, &result.o_val.v_s[1], SZ_FNAME-1);
	    redir[SZ_FNAME-1] = EOS;
	    breakout (redir, &pk, &t, &p, &f);

	    /* Task "_" is shorthand for the name of the current package.  */
	    if (((t == NULL || *t == EOS) && *redir == '.') ||
		strcmp (t, "_") == 0)
		t = pp->p_pfp->pf_ltp->lt_pkp->pk_name;

	    np = paramsrch (pk, t, p);
	    if (np == pp)
		cl_error (E_UERR, "self referential indirection on param `%s'",
		    pp->p_name);
	    paramget (np, *f);

	} else {
	    /* Check for indefinite values. */
	    if (arrflag && (field == FN_VALUE || field == FN_NULL)) {
		if ((result.o_type == OT_BOOL || result.o_type == OT_INT) &&
		    result.o_val.v_i == INDEFL) {

		    setopindef (&result);

		} else if (result.o_type == OT_REAL &&
		    result.o_val.v_r == INDEFR) {

		    setopindef (&result);
		}
	    }

	    pushop (&result);
	}
}


/* MAKEMODE -- Fill in characters of string s according to which mode bits
 * are on in param pp.  S should be at least 5 characters long, in the
 * (impossible) worse case.
 */
void
makemode (struct param *pp, char *s)
{
	register int m = pp->p_mode;

	if (m & M_AUTO)
	    *s++ = PF_AUTO;
	if (m & M_QUERY)
	    *s++ = PF_QUERY;
	if (m & M_HIDDEN)
	    *s++ = PF_HIDDEN;
	if (m & M_LEARN)
	    *s++ = PF_LEARN;
	*s = '\0';
}


/* NEWPARAM -- Allocate a new, empty, param on the dictionary and link in
 *   at end of list of params off pfile *pfp.  Put the new entry at the end of
 *   the list and update pfp->pf_lastpp.
 * This is so as to preserve the order in which the params were added to allow
 *   positional argument matching.
 * Null out all unused fields except the three union values.
 */
struct param *
newparam (struct pfile *pfp)
{
	register struct param *newpp;

	newpp = (struct param *) memneed (PARAMSIZ);

	if (pfp->pf_pp == NULL)
	    pfp->pf_lastpp = pfp->pf_pp = newpp;
	else {
	    pfp->pf_lastpp->p_np = newpp;
	    pfp->pf_lastpp = newpp;
	}

	newpp->p_pfp = pfp;
	newpp->p_flags = newpp->p_type = newpp->p_mode = 0;
	newpp->p_valo.o_type = newpp->p_mino.o_type = newpp->p_maxo.o_type = 0;
	newpp->p_name = newpp->p_prompt = nullstr;
	newpp->p_listval = NULL;
	newpp->p_listfp = NULL;
	newpp->p_lenval = 0;
	newpp->p_np = NULL;

	return (newpp);
}


/* PARAMSRCH -- Hunt for and return pointer to param in given package and ltask.
 * If no ltask specified, use standard search path, ie, check the params for
 *   the current ltask, then the current package, then the cl.
 * Else find pfile for the given ltask, reading it in if it's not in core.
 *   do not accept the ltask name if it's not defined.
 * If the param is list-structured, open the list file if it isn't already
 *   and P_LEOF is not set; thus, paramget() should close the list file
 *   and set P_LEOF when it sees EOF and leave it set so we can't open
 *   it again.  Do done of this if we just want the .value field.
 * If dealing with a task that has no param file, try to satisfy the request
 *   from positional args.  If that fails, make one that will query.
 *   Positional args were made named $n by posargset, or the like, and are
 *   accessed by name.  A named reference returns the next (as counted in
 *   pf_n) positional arg so two references by the same name will not return
 *   the same value.  However, if there are no more positional args, then
 *   one is made and will cause a query to the same param on each reference.
 * Call error() and do not return if cannot find it.
 */
struct param *
paramsrch (char *pkname, char *ltname, char *pname)
{
	register struct param *pp;
	struct	pfile *pfp;
	struct	param *lookup_param();

	/* First search for a regular parameter.  If this fails then we 
	 * handle the case when currentask has no pfile.
	 */
	pp = lookup_param (pkname, ltname, pname);

	if (currentask->t_pfp->pf_flags & PF_FAKE) {
	    if (((XINT)pp == ERR || pp == NULL) && *pname != '$') {
		/* If dealing with a task that has no param file, try to
		 * satisfy the request from positional args.  If that fails,
		 * make one that will query.
		 */
		pfp = currentask->t_pfp;
		pp = paramfind (pfp, (char *)NULL, pfp->pf_n++, NO);

		if (pp == NULL) {
		    pp = newfakeparam (pfp, pname, 0, OT_STRING, SZ_FNAME);
		    pp->p_mode |= M_QUERY;

		    /* If, instead, we query and set P_OK, a prompt will not
		     * be generated again if the same param is rereferenced.
		     * That's great but problem is that satisfying from
		     * positional args cannot work like this since the name
		     * isn't saved.
		    query (pp);
		    popop();  
		    pp->p_flags |= P_OK;
		     */
		}
	    }
	}

	if ((XINT)pp == ERR)
	    cl_error (E_UERR, e_nopfile, ltname);
	if (pp == NULL)
	    cl_error (E_UERR, e_pnonexist, pname);

	return (pp);
}


/* DEFPAR -- Determine if the named parameter exists.  Name may include
 * package, task and param names, task and param names, or just the param name,
 * with appropriate searching as necessary.  False is returned if either the
 * task has no param file or the param does not exist.
 */
int
defpar (char *param_spec)
{
	char	sbuf[SZ_LINE];
	char	*pkname, *ltname, *pname, *junk;

	strcpy (sbuf, param_spec);
	breakout (sbuf, &pkname, &ltname, &pname, &junk);

	switch ((XINT) lookup_param (pkname, ltname, pname)) {
	case NULL:
	case ERR:
	    return (NO);
	default:
	    return (YES);
	}
}


/* DEFVAR -- Determine if the named environment variable exists.
 */
int
defvar (char *envvar)
{
	char	sbuf[SZ_LINE];

	if (c_envfind (envvar, sbuf, SZ_LINE) <= 0)
	    return (NO);
	else
	    return (YES);
}


/* LOOKUP_PARAM -- Hunt for and return pointer to param in given package
 *   and ltask.  If task does not have param file, NULL is returned.  If pfile
 *   exists but is not loaded, it is loaded before searching for parameter.
 *   Returns valid pp if sucessful;  NULL if param file exists but contains no
 *   such param, and ERR if there is no param file.
 * All other problems (package, task unknown or ambiguous) result in an abort.
 * Called by PARAMSRCH and by DEFPAR.
 */
struct param *
lookup_param (char *pkname, char *ltname, char *pname)
{
	register struct param *pp;
	register struct package *pkp;
	register struct ltask *ltp;
	struct	pfile *pfp;
	struct	pfile *pfiles[64];
	struct	param *candidate;
	int	ambig, npfiles, i;

	pp = NULL;

	if (*ltname == '\0') {
	    /* No ltask or package given so check standard places.  If the
	     * current task is cl the search order is curpack,cl.  Otherwise,
	     * the search order is curtask,package,cl, where `package' is
	     * the package to which the current task belongs, NOT the current
	     * package.  The current task is the task which is currently
	     * executing; while a task is executing, any psets referenced
	     * by the main task pfile are loaded and linked into a list off
	     * the main pfile.  Note that this also hold for the pkg pfile,
	     * since the pkg-task is always executing while any tasks therein
	     * are executing (unless the pkg script exits with a keep()).
	     */
	    npfiles = 0;
	    if (currentask->t_ltp == firstask->t_ltp) {
		/* The current task is the cl() task.
		 */
		pfiles[npfiles++] = NULL;
		pfiles[npfiles++] = curpack->pk_pfp;

	    } else {
		/* The current task is a normal compiled or script task.
		 * Search the main pfile for the task, any pset-files
		 * referenced by the main pfile, and lastly the package pfile
		 * and any pset-files referenced by the package pfile.
		 */
		struct  pfile *pfp_head[2];
		int     i;

		pfp_head[0] = currentask->t_pfp;
		pfp_head[1] = currentask->t_ltp->lt_pkp->pk_pfp;

		for (i=0;  i <= 1;  i++)
		    if ((pfp = pfp_head[i]) != NULL) {
			pfiles[npfiles++] = pfp;
			if (pfp->pf_flags & PF_PSETREF) {
			    while ( (pfp = pfp->pf_npset) ) {
				pfiles[npfiles++] = pfp;
				if (npfiles >= 62)
				    cl_error (E_IERR,
					"lookup_param: too many pfiles");
			    }
			}
		    }
	    }

	    pfiles[npfiles++] = firstask->t_pfp;	/* firstask == cl */

	    /* Search for the named parameter in all the pfiles in the search
	     * path.  If an exact match is found in any pfile we are done.
	     * If abbreviations are enabled and a non-unique abbreviation is
	     * indicated, keep searching pfiles and abort only if an exact
	     * match is not found in some other pfile.
	     */
	    candidate = NULL;
	    ambig = 0;
	    for (i=0;  i < npfiles;  i++) {
		pfp = pfiles[i];
		if (pfp != NULL && (pp=paramfind (pfp, pname, 0, NO)) != NULL) {
		    if ((XINT)pp == -1) {
			ambig++;
		    } else if (!strcmp (pp->p_name, pname)) {
			ambig = 0;
			break;			/* exact match */
		    } else if (candidate != NULL && candidate != pp) {
			ambig++;
		    } else {
			candidate = pp;
		    }
		}
	    }

	    if (ambig)
		cl_error (E_UERR, e_pambig, pname, "<searchpath>");
	    else if (pp == NULL)
		pp = candidate;

	} else {
	    if (*pkname != '\0') {
		/* If the package name is given, search only that package.
		 */
		pkp = pacfind (pkname);
		if ((XINT)pkp == ERR)
		    cl_error (E_UERR, e_pckambig, pkname);
		if (pkp == NULL)
		    cl_error (E_UERR, e_pcknonexist, pkname);

		/* Search for ltask; it must exist and the given name must
		 * be an unambiguous abbreviation.
		 */
		ltp = ltaskfind (pkp, ltname, 1);
		if (ltp == NULL)
		    cl_error (E_UERR, e_tnonexist, ltname);
		if ((XINT)ltp == ERR)
		    cl_error (E_UERR, e_tambig, ltname);

	    } else {
		/* Ltask name given but not package name.  Do circular search
		 * for ltask; abort if not found or ambiguous.
		 */
		ltp = ltasksrch ("", ltname);
	    }

	    /* Get param file pointer and find parameter.  Return ERR if no
	     * pfile.
	     */
	    if ((pfp = pfilefind (ltp)) == NULL) {
		if (ltp->lt_flags & LT_PFILE)
		    pfp = pfileload (ltp);
		else				/* no pfile */
		    return ((struct param *)ERR);
	    }
	    pp = paramfind (pfp, pname, 0, NO);
	    if ((XINT)pp == ERR)
		cl_error (E_UERR, e_pambig, pname, ltp->lt_lname);
	}

	return (pp);
}


/* PRINTPARAM -- Convert the info in param pp to text and print it on
 * file fp.  Return ERR if have a write error, else OK.
 * Don't write M_FAKE params unless we are writing to stderr.
 * Put quotes around strings; convert escape chars into escape sequences.
 * Don't call error() so caller can have a chance to close the file.
 */
int
printparam (struct param *pp, register FILE *fp)
{
	register int type, bastype;
	register char *bp;
	char	*index();
	char	buf[20];
	int	arrflag;
	int	size_arr=0;
	int	i;		/* a misc variable.	*/

	if ((pp->p_mode & M_FAKE) && fp != stderr)
	    return (OK);

	type = pp->p_type;
	bastype = type & OT_BASIC;
	arrflag = type & PT_ARRAY;


	/* NAME */
	fputs (pp->p_name, fp);
	fputc (PF_DELIM, fp);


	/* TYPE */
	bp = buf;
	if (type & PT_LIST)
	    *bp++ = '*';
	else if (arrflag)
	    *bp++ = 'a';

	switch (bastype) {
	case OT_BOOL:
	    *bp++ = 'b';
	    break;
	case OT_INT:
	    *bp++ = 'i';
	    break;
	case OT_REAL:
	    *bp++ = 'r';
	    break;
	case OT_STRING:
	    *bp++ = 's';
	    break;
	}

	/* Overwrite the string descriptor that appears with PT_FILNAM,
	 * PT_STRUCT and the cursors.
	 */
	if (type & PT_FILNAM) {
		*--bp = 'f';
	    if (type & PT_FBIN)
		*++bp = 'b';
	    if (type & PT_FNOE)
		*++bp = 'n';
	    if (type & PT_FER)
		*++bp = 'r';
	    if (type & PT_FTXT)
		*++bp = 't';
	    if (type & PT_FEW)
		*++bp = 'w';
	    *++bp = '\0';

	} else if (type & PT_STRUCT) {
	    strcpy (--bp, "struct");
	} else if (type & PT_GCUR) {
	    strcpy (--bp, "gcur");
	} else if (type & PT_IMCUR) {
	    strcpy (--bp, "imcur");
	} else if (type & PT_UKEY) {
	    strcpy (--bp, "ukey");
	} else if (type & PT_PSET) {
	    strcpy (--bp, "pset");
	} else
	    *bp = '\0';

	fputs (buf, fp);
	fputc (PF_DELIM, fp);


	/* MODE */
	makemode (pp, buf);
	fputs (buf, fp);
	fputc (PF_DELIM, fp);

	/* VALUE.
	 * Set i if pp is a struct or cursor.
	 * Print the max length of structs or cursors even if they are not
	 *   defined.
	 */
	i = type & (PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY);
	if (opindef(&pp->p_valo) && !i) {
	    fputs (indefstr, fp);
	} else if (opundef(&pp->p_valo) && !i) {
	    ;
	} else if (type & (PT_LIST|PT_FILNAM|PT_PSET)) {
	    /* Put quotes around string, may contain special chars */
	    qputs (pp->p_val.v_s, fp);
	} else if (bastype == OT_STRING && !arrflag) {
	    if (i)
		/* -1 to allow for +1 added for \0 in addparam(). */
		fprintf (fp, "%d", pp->p_lenval - 1);
	    else {
		/* Quote string, may contain special chars */
		qputs (pp->p_val.v_s, fp);
	    }
	} else if (arrflag) {
	    /* Print array descriptor info, and get size of array for
	     * printing values later.
	     */
	    int		dim, d;
	    short	*lenoff;

	    size_arr = 1;
	    dim = pp->p_val.v_a->a_dim;
	    lenoff = & (pp->p_val.v_a->a_len) ;
	    fprintf (fp,"%d,", dim);
	    for (d=0; d<2*dim; d++) {
		if (d%2 == 0)
		    size_arr *= *lenoff;
		fprintf(fp, "%d,", *lenoff++);
	    }

	    /* Terminate the line. */
	    fprintf(fp, "\\\n");
	
	} else 
	    fprop (fp, &pp->p_valo);

	if (!arrflag)
	    fputc (PF_DELIM, fp);

	/* MINIMUM.
	 * Set i if this param has a min/max field. reuse in max printing.
	 */
	i = (bastype != OT_BOOL &&
	    !(type & (PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET)));
	if (pp->p_flags & P_IMIN)
	    fputs (indefstr, fp);
	else if (pp->p_flags & P_UMIN)
	    ;
	else if (i)
	    fprop (fp, &pp->p_mino);
	fputc (PF_DELIM, fp);


	/* MAXIMUM */
	if (pp->p_flags & P_IMAX)
	    fputs (indefstr, fp);
	else if (pp->p_flags & P_UMAX)
	    ;
	else if (i)
	    fprop (fp, &pp->p_maxo);
	fputc (PF_DELIM, fp);


	/* PROMPT. */
	if (*pp->p_prompt != '\0')
	    qputs (pp->p_prompt, fp);
	if (!arrflag)
	    fputc ('\n', fp);
	else
	    fprintf (fp, ",\\\n");

	/* Structs and cursors get printed on their own line.
	 */
	if (!(type & PT_LIST) &&
	    (type&(PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY))) {

	    if (opindef (&pp->p_valo))
		fputs (indefstr, fp);
	    else if (opundef (&pp->p_valo))
		;
	    else
		fputs (pp->p_val.v_s, fp);
	    fputc ('\n', fp);
	}

	if (arrflag) {
	    /* For a first approximation use a fixed number of
	     * values per line.
	     */
	    int    count=0, lcount=0, n_per=0, *p_i= NULL;
	    double *p_r= NULL;
	    char   **p_s = NULL;

	    if (bastype == OT_BOOL) {
		n_per = 20;
		p_i = pp->p_aval.a_i;
	    } else if (bastype == OT_INT) {
		n_per = 10;
		p_i = pp->p_aval.a_i;
	    } else if (bastype == OT_REAL) {
		n_per = 4;
		p_r = pp->p_aval.a_r;
	    } else if (bastype == OT_STRING) {
		n_per = 2;
		p_s = pp->p_aval.a_s;
	    }

	    count = 0;
	    lcount = 0;

	    for (; count<size_arr; count++, lcount++) {
		if (lcount > n_per) {
		    fprintf(fp, "\\\n");
		    lcount = 0;
		}
		if (bastype == OT_BOOL) {
		    if (*p_i != INDEFL) {
			if (*p_i++)
			    fprintf (fp, "yes");
			else
			    fprintf (fp, "no");
		    } else
			p_i++;

		} else if (bastype == OT_INT) {
		    if (*p_i == INDEFL)
			p_i++;
		    else
		        fprintf (fp, "%d", *p_i++);

		} else if (bastype == OT_REAL) {
		    if (*p_r == INDEFR)
			p_r++;
		    else
			fprintf (fp, "%g", *p_r++);

		} else if (bastype == OT_STRING) {
		    /* The undefined string is the null string, so
		     * we needn't check for it.
		     */
		    qputs (*p_s++, fp);
		}

		if (count < size_arr-1)
		    fprintf (fp, ",");
		else
		    fprintf (fp, "\n");
	    }
	}

	if (ferror (fp))
	    return (ERR);

	return (OK);
}


/* QPUTS -- Print a string on the output stream, converting all recognized
 * control characters (newline, tab, and string delimiters) into escape
 * sequences, so that they can later be read back in unmodified.
 */
void
qputs (register char *str, register FILE *fp)
{
	register char	ch;

	fputc ('"', fp);
	while ((ch = *str++) != '\0') {
	    switch (ch) {
	    case '\n':
		fputs ("\n", fp);		/* avoid super long lines */	
		break;
	    case '\t':
		fputs ("\\t", fp);
		break;
	    case '\r':
		fputs ("\\r", fp);
		break;
	    case '\f':
		fputs ("\\f", fp);
		break;
	    case '\\':
		fputc ('\\', fp);
		ch = *str++;
		fputc (ch, fp);
		break;
	    case '\'':
		fputs ("\\'", fp);
		break;
	    case '"':
		fputs ("\\\"", fp);
		break;
	    default:
		fputc (ch, fp);
	    }
	}
	fputc ('"', fp);
}


/* PVALDEFINED -- Decide whether string s is indefinite (one of indefstr or
 * indeflc) or undefined (s == undefval), and set pp->p_type bits accordingly.
 * Return YES if neither of these conditions exist, else NO.  Note that
 * the null string a null string per se does not qualify as an undefined
 * value.
 */
int
pvaldefined (struct param *pp, char *s)
{
	int val;

	val = NO;
	if (s == NULL || s == undefval)
	    setopundef (&pp->p_valo);
	else if (!strcmp (s, indefstr) || !strcmp (s, indeflc))
	    setopindef (&pp->p_valo);
	else
	    val = YES;
	return (val);
}


/* NEWFAKEPARAM -- Make a fake parameter off pfp.  Use newparam to actually
 *   allocate space.  If name is NULL, name the parameter $pos, else name it
 *   name.  Add one to pos because users see names as one-indexed.
 * Type of param is type; if OT_STRING allocation is for SZ_FNAME characters.
 * Check for pos > 99 as we only allowing room for 2 digits in $name for.
 * Check for both kinds of null strings, just in case.
 */
struct param *
newfakeparam (
    struct pfile *pfp,
    char *name,
    int pos,
    int type,
    int string_len	/* if new param is type string, size of string */
)
{
	register struct param *pp;

	pp = newparam (pfp);
	if (name == NULL || *name == '\0') {
	    if (++pos > 99)
		cl_error (E_UERR, "too many fake positional params");
	    pp->p_name = memneed (btoi(4)); /* need room for "$nn\0"  */
	    sprintf (pp->p_name, "$%d", pos);
	} else
	    pp->p_name = comdstr (name);

	if (cldebug)
	    eprintf ("adding fake param `%s', type code %d\n",
		pp->p_name, type);

	type &= OT_BASIC;
	pp->p_valo.o_type = type;
	pp->p_mino.o_type = type;
	pp->p_maxo.o_type = type;

	if (type == OT_STRING) {
	    /* Allocate specified amount of space, add the eos and init
	     * max length.  Other types need no initialization.
	     */
	    pp->p_val.v_s = memneed (btoi(string_len+1));
	    pp->p_val.v_s[string_len] = '\0';  /* the permanent eos. */
	    pp->p_lenval = string_len+1;
	}

	pp->p_type = type;
	pp->p_valo.o_type = OT_UNDEF;
	pp->p_mode = M_FAKE;
	pp->p_flags = (P_UMIN|P_UMAX);

	return (pp);
}


/* GETOFFSET -- Getoffset returns the offset from the beginning of the array
 * for using the index values stored on the stack.
 */
int
getoffset (struct param *pp)
{
	int	dim, offset, index;
	short	*plen, *poff, len, off;

	if (mode_offset == DIRECT_OFFSET) {
	    n_indexes--;
	    if (n_indexes < 0)
		cl_error(E_UERR, e_indexunf);
	    offset = pop() ;
	    mode_offset = INDEX_OFFSET;

	} else {
	    dim = pp->p_val.v_a->a_dim;
	    plen = &(pp->p_val.v_a->a_len) ;
	    poff = plen + 1;

	    offset = 0;

	    while (dim-- > 0) {
		len = *(plen + 2*dim);
		off = *(poff + 2*dim);

		if (offset > 0)
		    offset *= len;

		n_indexes--;
		if (n_indexes < 0)
		    cl_error(E_UERR, e_indexunf);

		index = pop();


		if (index < off  ||  index > off+len-1)
		    cl_error(E_UERR, "Array subscript error.  Index %d is %d.",
		      dim+1, index);
		offset += index-off;

	    }
	}

	return (offset);
}


/* OFFSETMODE -- Offsetmode() permits the user to choose whether to calculate
 * the offsets using an index list, or to push the offset onto the stack
 * directly.
 */
void
offsetmode (int mode)
{
	if (mode)
	    mode_offset = DIRECT_OFFSET;
	else
	    mode_offset = INDEX_OFFSET;
}


/* SIZE_ARRAY -- Get the number of elements in an array.
 */
int
size_array (struct param *pp)
{
	int dim, d, size;
	short *len;

	size = 1;

	if (pp->p_type & PT_ARRAY ) {
	    dim = pp->p_val.v_a->a_dim;
	    len = &(pp->p_val.v_a->a_len) ;

	    for (d=0;  d < dim;  d++)
		size *= *(len+2*d);
	}

	return (size);
}
