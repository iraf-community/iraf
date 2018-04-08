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
#include "proto.h"


/*
 * DECL -- contains routines used by the parser for referencing parameters
 * and for parameter declarations.
 */

extern	int	cldebug;

char	*badopt      = "Invalid %s option for `%s'.";
char	*illegal_opt = "Illegal option for `%s'.";
char	*dup_def     = "Duplicate definition of `%s' ignored.\n";


/* GETLIMITS -- Get the limits for the n'th index of a parameter.
 * Returns ERR if the parameter is not defined, or has fewer than n indexes.
 */
int
getlimits (
  char	*pname,
  int	 n, 
  int   *i1, 
  int   *i2
)
{
	struct param *pp;
	char	*pk, *t, *p, *f;
	int 	dim;
	short	*len, *off;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);

	/* Paramsrch calls error if it cannot find the param, so we
	 * needn't check here.
	 */
	if (!(pp->p_type & PT_ARRAY))
	    return (ERR);

	dim = pp->p_val.v_a->a_dim;
	if (n >= dim)
	    return (ERR);

	len = &(pp->p_val.v_a->a_len);
	len = len + 2*n;
	off = len + 1;

	*i1 = *off;
	*i2 = *off + *len - 1;
	return (OK);
}


/* GET_DIM -- Get the dimensionality of an parameter.  If not an array return 0.
 */
int
get_dim (char *pname)
{
	struct param *pp, *lookup_param();
	char	*pk, *t, *p, *f;
	int 	dim;

	breakout (pname, &pk, &t, &p, &f);

	/* We can't use paramsrch here because the string we are
	 * looking for might be a builtin, and paramsrch would fail.
	 */
	pp = lookup_param (pk, t, p);

	if (pp == NULL || (XINT) pp == ERR)
	    dim = -1;
	else if (!(pp->p_type & PT_ARRAY))
	    dim = 0;
	else
	    dim = pp->p_val.v_a->a_dim;

	return (dim);
}


/* MAKETYPE -- Set the type of a parameter.
 */
int
maketype (int type, int list)
{
	register int	p = -1;

	switch (type) {
	case V_BOOL:	p = OT_BOOL;
			break;
	case V_INT:	p = OT_INT;
			break;
	case V_REAL:	p = OT_REAL;
			break;
	case V_STRING:	p = OT_STRING;
			break;
	case V_FILE:	p = OT_STRING | PT_FILNAM;
			break;
	case V_GCUR:	p = OT_STRING | PT_GCUR;
			break;
	case V_IMCUR:	p = OT_STRING | PT_IMCUR;
			break;
	case V_UKEY:	p = OT_STRING | PT_UKEY;
			break;
	case V_PSET:	p = OT_STRING | PT_PSET;
			break;
	case V_STRUCT:	p = OT_STRING | PT_STRUCT;
			break;
	}

	if (list)
	    p |= PT_LIST;

	return (p);
}


/* DO_ARRAYINIT -- Initialize an array from values in a declaration statement.
 * This routine must also allocate the array descriptor block.
 *
 * On entry the control stack contains pointers to operands containing
 * the initialization info.  Buried beneath this may be the dimension
 * and offset information needed for the the array descriptor.  The
 * dimensionality of the array is passed in nindex, except when
 * the user wishes to default the dimension of a one-dimensional
 * array to the number of values in the initialization block.
 * In that case nindex has been passed as 0.
 * 
 * This program ASSUMES that successive calls to memneed return
 * contiguous blocks of memory.  This is because we don't know
 * the size of the array at first, and we can only allocate the
 * space needed to hold the values which have been initialized.
 * After we have popped the stack down to array descriptor info
 * we may find that some values are not initialized and so we
 * may need to allocate more memory.
 */
void
do_arrayinit (
  struct param *pp,
  int	 nval, 
  int    nindex
)
{
	char	*block1=NULL, *block2=NULL;
	int	dim, asiz, asiz2, asiz2x, bastype, i;
	int	slen=0;
	short	*off, *len;
	struct	arr_desc  *parr;
	struct	operand	  *o;
	union 	arrhead	  ar;

	if (cldebug)
	    eprintf ("do_arrayinit: nindex=%d  nval=%d\n", nindex, nval);
	bastype = pp->p_type & OT_BASIC;
	if (bastype == OT_STRING)
	    slen = pp->p_lenval;

	dim = nindex;
	if (dim == 0)
	    dim = 1;
	asiz = 0;

	/* Allocate an array descriptor.
	 */
	parr = (struct arr_desc *) memneed (2+dim);

	if (nval > 0) {
	    asiz = nval;
	    if (bastype == OT_REAL)
		asiz = dtoi (asiz);
	    block1 = memneed (asiz);
	    ar.a_i = (int *) block1;
	    i = nval;

	    while (i--) {
		o = (struct operand *) pop();

		switch (bastype) {

		case OT_BOOL:
		    if (o->o_type != OT_BOOL  &&  o->o_type != OT_INT) {
			eprintf ("Invalid type in array initialization.\n");
			*(ar.a_i + i) = INDEFL;
		    } else
			*(ar.a_i + i) = o->o_val.v_i;
		    break;

		case OT_INT:
		    if (o->o_type != OT_INT) {
			eprintf ("Invalid type in array initialization.\n");
			*(ar.a_i + i) = INDEFL;
		    } else
			*(ar.a_i + i) = o->o_val.v_i;
		    break;

		case OT_REAL:
		    switch (o->o_type) {
		    case OT_INT:
			ar.a_r[i] = (double) (o->o_val.v_i);
			break;
		    case OT_REAL:
			ar.a_r[i] = o->o_val.v_r;
			break;
		    default:
			eprintf ("Invalid type in array initialization.\n");
			ar.a_r[i] = INDEFR;
			break;
		    }
		    break;

		case OT_STRING:
		    ar.a_s[i] = o->o_val.v_s;
		}	/* End of switch. */
	    }
	}

	/* Get array descriptor info.
	 */
	if (nindex > 0) {
	    len = &(parr->a_len);
	    off = &(parr->a_off);
	    parr->a_dim = nindex;

	    asiz2 = 1;

	    i = nindex;
	    while (i--) {
		off[2*i] = pop();
		len[2*i] = pop();
		asiz2 *= len[2*i];
	    }

	    if (bastype == OT_REAL)
		asiz2x = dtoi (asiz2);
	    else
		asiz2x = asiz2;

	    if (asiz2x > asiz) {	/* Need to allocate more space. */
		block2 = memneed (asiz2x-asiz);

		if (nval == 0) {
		    block1 = block2;
		    ar.a_i = (int *) block1;
		}

		if (btoi(block2-block1) != asiz)
		    cl_error (E_IERR, "Memory sync error during array init.\n");

		/* Initialize undefined elements. 
		 */
		for (i = nval;  i < asiz2;  i++)
		     switch (bastype) {
			case OT_INT:
			case OT_BOOL:
			    ar.a_i[i] = INDEFL;
			    break;
			case OT_REAL:
			    ar.a_r[i] = INDEFR;
			    break;
			case OT_STRING:
			    ar.a_s[i] = memneed (btoi(slen));
			    *(ar.a_s[i]) = '\0';
			    *(ar.a_s[i] + SZ_FNAME - 1) = '\0';
		    }
	    } else if (nval > asiz2)
		/* We just leave the extra values in the dictionary.
		 * It's not serious enough to make it an error.
		 */
		eprintf ("Warning: Too many initialization values for `%s'.\n",
		   pp->p_name);

	} else {	/* User didn't give dimensions. */
	    parr->a_len = nval;
	    parr->a_off = 1;
	    parr->a_dim = 1;
	}

	/* At this point initialized string parameters point to the string
	 * which was returned as an operand.  Many array elements could
	 * point to the same storage.  Allocate a constant amount
	 * of storage for each of the initialized strings and copy
	 * the initial value into it.
	 */
	if (bastype == OT_STRING) {
	    for (i=0; i<nval; i++) {
		char	*s;
		s = memneed (btoi (slen));
		strncpy (s, ar.a_s[i], slen-1);
		*(s+SZ_FNAME-1) = '\0';
		ar.a_s[i] = s;
	    }
	}

	/* Finally connect the various elements.
	 */
	pp->p_val.v_a = parr;
	pp->p_aval = ar;
}


/* DO_SCALARINIT -- Initialize a scalar.  Mostly copied from ADDPARAM.
 */
void
do_scalarinit (
  struct param	*pp,
  int	inited
)
{
	struct	operand	*o, undefoper;
	extern	char	*e_invaldef;
	int 	len, bastype;
	char	*s;

	pp->p_valo.o_type = bastype = pp->p_type & OT_BASIC;

	if (inited) {
	    o = (struct operand *)pop();
	    if (o->o_type == OT_STRING)
		s = o->o_val.v_s;
	    else
		s = undefval;
	} else {
	    o = &undefoper;
	    s = undefval;
	    undefoper.o_type = OT_STRING;
	    undefoper.o_val.v_s = undefval;
	}

	if (pp->p_type & (PT_LIST|PT_FILNAM|PT_PSET)) {
	    if (o->o_type != OT_STRING)
		cl_error (E_UERR, e_invaldef, pp->p_name);

	    pp->p_val.v_s = memneed (btoi(SZ_FNAME));
	    pp->p_val.v_s[SZ_FNAME-1] = '\0';

	    if (pvaldefined (pp, s)) {
	        char  *p;

		/* Change a whitespace-only filename into a null string; this
		 * makes it easier for users to check null filenames in 
		 * scripts.  It makes sense anyway since these are invalid
		 * filenames.
		 */
		p = s;
	        while (*p == ' ' || *p == '\t')
		    p++;
		if (*p == '\0' || *p == '\n')
		    pp->p_val.v_s[0] = '\0';
		else
		    strncpy (pp->p_val.v_s, s, SZ_FNAME-1);
	    } else
		pp->p_val.v_s[0] = '\0';

	    if (pp->p_type & PT_LIST)
		pp->p_listval = memneed (btoi(SZ_LINE));

	    pp->p_valo.o_type = OT_STRING;

	} else if (pp->p_type & (PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY)) {
	    if (o->o_type != OT_STRING)
		cl_error(E_UERR, e_invaldef, pp->p_name);

	    len = pp->p_lenval;
 	    pp->p_val.v_s = memneed (btoi (len));

	    if (pvaldefined (pp, s))
		strcpy (pp->p_val.v_s, s);
	    else
		pp->p_val.v_s[0] = '\0';

	    pp->p_val.v_s[len-1] = '\0';	 /* the permanent eos */
	    pp->p_valo.o_type = OT_STRING;

	} else if (bastype == OT_STRING || (s != NULL && *s == PF_INDIRECT)) {
	    /* Strings are stored like structs, but are inited from s.
	     * OT_INDEF/UNDEF refer to p_val.
	     */
	    pp->p_lenval = SZ_LINE;
	    if (pvaldefined (pp, s)) {
		/* String was something conventional.  If shorter than SZ_LINE
		 * call memneed() to allocate sufficient space and copy
		 * the value into it.
		 */
		char   *news;

		pp->p_valo.o_type = OT_STRING;
		len = strlen (s) + 1;		/* allow for eos */
		news = memneed (btoi (pp->p_lenval));

		if (len < pp->p_lenval) {
		    strcpy (news, s);
		    s = news;
		} else {
		    pp->p_lenval = len;
		    pp->p_val.v_s = s;
		}

	    } else {
		/* Either no string was given or it was INDEF/UNDEF.
		 */
		len = SZ_LINE;
		s = memneed (btoi (pp->p_lenval));
	    }

	    pp->p_val.v_s = s;
	    pp->p_val.v_s[len-1] = '\0'; /* add the permanent eos */
	    pp->p_maxo.o_type = OT_INT;

	} else {
	    /* Simple non-string type.
	     */
	    if (inited)
		pp->p_valo = *o;
	    else
		pp->p_valo.o_type = bastype | OT_UNDEF;
	}

	if (cldebug)
	    eprintf ("do_scalar_init: pp->p_flags=%o\n", pp->p_flags);
}


/* SCANFTYPE -- Get file type for file parameter.
 */
int
scanftype (
  struct param	*pp,
  struct operand *o
)
{
	int	type;
	char	*s;

	if (o->o_type != OT_STRING)
	    return (ERR);

	type = 0;
	s = o->o_val.v_s;

	while (*++s != '\0')
	    switch (*s) {
	    case 'b': case 'B': type |= PT_FBIN; break;
	    case 'n': case 'N': type |= PT_FNOE; break;
	    case 'r': case 'R': type |= PT_FER;  break;
	    case 't': case 'T': type |= PT_FTXT; break;
	    case 'w': case 'W': type |= PT_FEW;  break;
	    default: return (ERR);
	    }

	pp->p_type |= type;
	return (OK);
}


/* C_SCANMODE -- Get the mode for a parameter.
 */
int
c_scanmode (
  struct param	*pp,
  struct operand *o
)
{
	if (o->o_type != OT_STRING)
	    return (ERR);

	pp->p_mode = scanmode (o->o_val.v_s);
	return (OK);
}


/* SCANLEN -- Get the length for structs and strings.
 */
int
scanlen (
  struct param	*pp,
  struct operand *o
)
{
	if (o->o_type != OT_INT  ||
	    !(pp->p_type & (OT_STRING|PT_LIST|PT_STRUCT)))
	    return (ERR);

	pp->p_lenval = o->o_val.v_i;
	return (OK);
}


/* SCANMIN -- Get the minimum for a parameter.
 */
int
scanmin (
  struct param	*pp,
  struct operand *o
)
{
	int 	bastype, otype;

	bastype = pp->p_type & OT_BASIC;
	otype = o->o_type;

	if (pp->p_type & (OT_BOOL|PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET))
	    return (ERR);

	if (otype == OT_STRING)
	    if ((bastype == OT_STRING || *(o->o_val.v_s) == PF_INDIRECT)) {

	        /* Filename, enumerated string, or indirect reference.
	         */
	        pp->p_mino.o_type = OT_STRING;
	        pp->p_min.v_s = memneed (btoi(PF_SZMINSTR));
	        pp->p_min.v_s[PF_SZMINSTR-1] = '\0';
	        strncpy (pp->p_min.v_s, o->o_val.v_s, PF_SZMINSTR-1);
	        pp->p_flags &= ~P_UMIN;
	        return (OK);
	    }

	pushop (o);
	opcast (bastype);
	pp->p_mino = popop();

	pp->p_flags &= ~P_UMIN;
	return (OK);
}


/* SCANENUM -- Get the legal values for an enumerated string an store in the
 * min field of the parameter.
 */
int
scanenum (
  register struct param	*pp,
  register struct operand *o
)
{
	register int bastype;

	bastype = pp->p_type & OT_BASIC;

	if (bastype != OT_STRING || o->o_type != OT_STRING)
	    return (ERR);

	return (scanmin (pp, o));
}


/* SCANMAX -- Get the maximum for a param.
 */
int
scanmax (
  struct param	*pp,
  struct operand *o
)
{	
	int	otype;

	otype = pp->p_type & OT_BASIC;

	if (pp->p_type & (OT_BOOL|PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET))
	    return (ERR);

	if (otype == OT_STRING && o->o_type == OT_STRING)
	    if (*o->o_val.v_s == '@') {
		/* Filename, enumerated string, or indirect reference.
		 */
		pp->p_maxo.o_type = OT_STRING;
		pp->p_max.v_s = memneed (btoi(PF_SZMAXSTR));
		pp->p_max.v_s[PF_SZMAXSTR-1] = '\0';
		strncpy (pp->p_max.v_s, o->o_val.v_s, PF_SZMAXSTR-1);

		pp->p_flags &= ~P_UMAX;
		return (OK);
	    }

	/* Type is equivalent to a simple non-string wrt mins.
	 */
	pushop (o);
	opcast (otype);
	pp->p_maxo = popop();
	pp->p_flags &= ~P_UMAX;
	return (OK);
}


/* PROC_PARAMS -- Check that all of the parameters in the procedure statement
 * are now defined.  If the mode for these parameters is not declared
 * set it to AUTO mode.  Also rearrange the parameters so they
 * agree with order of definition in the procedure statement.
 */
void
proc_params (int npar)
{
	struct	operand	*o;
	struct	param	*pp, *fp, *lp, *op, *tp;

	if (npar <= 0)
	    goto setmodes_;

	fp = lp = NULL;

	while (npar--) {
	    o = (struct operand *) pop();
	    if (o->o_type != OT_STRING)
		cl_error (E_UERR,"Invalid parameter in procedure statement.\n");

	    if (npar >= MAX_PROC_PARAMS)
		eprintf (
		    "Too many parameters: `%s' cannot be used positionally.",
		    o->o_val.v_s);

	    parlist [npar] = o;

	    pp = paramfind (parse_pfile, o->o_val.v_s, 0, YES);
	    if (pp == NULL)
		cl_error (E_UERR, "Required parameter `%s' not defined.",
		    o->o_val.v_s);

	    if (pp->p_mode & M_HIDDEN) {
		/* This parameter was declared as hidden, but was in the
		 * procedure statement.  Override it with a mode of auto,
		 * giving the user a warning.
		 */
		eprintf ("Warning: mode for parameter `%s' overridden.\n",
		   pp->p_name);
	 	pp->p_mode &= ~M_HIDDEN;
	        pp->p_mode |=  M_AUTO;
	    } else if (!pp->p_mode)
	        pp->p_mode = M_AUTO;

	    tp = parse_pfile->pf_pp;
	    op = NULL;

	    /* Since we've already found pp, this loop must terminate with a
	     * break.
	     */
	    while (tp != NULL) {
		if  (tp == pp)
		    break;
		else {
		    op = tp;
		    tp = tp->p_np;
		}
	    }

	    /* Take param out of list and add to properly ordered list.
	     */
	    if (op == NULL)
		parse_pfile->pf_pp = tp->p_np;
	    else
		op->p_np = tp->p_np;

	    if (lp == NULL)
		lp = tp;

	    tp->p_np = fp;
	    fp = tp;
	}

	lp->p_np = parse_pfile->pf_pp;
	parse_pfile->pf_pp = fp;

	while (fp->p_np != NULL)	/* Find last parameter. */
	    fp = fp->p_np;
	parse_pfile->pf_lastpp = fp;

setmodes_:
	/* Insure that all parameters have a mode.  The default in a procedure
	 * script is hidden.  
	 */
	tp = parse_pfile->pf_pp;
	while (tp != NULL) {
	    if (!tp->p_mode)
		tp->p_mode = M_HIDDEN;
	    tp = tp->p_np;
	}
}


/* INITPARAM -- Get a new parameter and initialize appropriate fields.
 */
struct param *
initparam (
  struct operand *op,
  int	isparam, 
  int   type,
  int   list
)
{
	struct	param *pp;
	extern	char *e_lookparm;
	int	slen;

	pp = paramfind (parse_pfile, op->o_val.v_s, 0, YES);

	if (pp == NULL) {
	    pp = newparam (parse_pfile);

	    slen = strlen(op->o_val.v_s) + 1;
	    pp->p_name = memneed (btoi(slen));
	    strcpy (pp->p_name, op->o_val.v_s);
	    pp->p_type = maketype (type, list);

	    /* Do not initialize the mode of a parameter in a procedure 
	     * script.  They will be initialized in proc_params().
	     */
	    if (parse_state != PARSE_PARAMS) {
	    	if (isparam)
		    pp->p_mode = M_HIDDEN;
	        else
		    pp->p_mode = M_LOCAL;
	    }

	    pp->p_mino.o_type = 0;
	    pp->p_maxo.o_type = 0;
	    pp->p_flags |= (P_UMAX|P_UMIN);
	    pp->p_prompt = undefval;
	    pp->p_lenval = SZ_FNAME;

	} else if (pp == (struct param *) ERR) {
	    cl_error (E_UERR, e_lookparm, op->o_val.v_s);

	} else {
	    pp = NULL;
	    eprintf (dup_def, op->o_val.v_s);
	}

	return (pp);
}


/* PROCSCRIPT -- Is this a procedure script? 
 */
int
procscript (FILE *fp)
{
	char	*p, buf[PF_MAXLIN+1];
	int	result;
	long	fpos, curpos;

	result = NO;
	fpos   = 0L;
	curpos = ftell (fp);

	currentask->t_scriptln = 0;
	if (curpos != 0)
	    fseek (fp, 0L, 0);

	while (fgets (buf, PF_MAXLIN, fp) != NULL) {
	    if (fpos > 0)
	        currentask->t_scriptln++;

	    for (p = buf; *p == ' '  ||  *p == '\t'; p++)
		;
	    if (strncmp (p, "procedure", 9) == 0) {
		result = YES;
		break;
	    } else if ((*p == '#') || (*p == '\n')) {
		fpos = ftell (fp);
		continue;
	    } else
		break;
	}

	/* Rewind the file so that the parser sees the procedure statement.
	 * If NOT a procedure script, rewind the file entirely, as the lexical
	 * analyzer needs to see the comments to work properly (because of the
	 * #{ ... #} lexmodes toggle sequences).
	 */
	if (result)
	    fseek (fp, fpos, 0);
	else
	    fseek (fp, 0L, 0);

	return (result);
}


/* SKIP_TO -- Within a file, skip to the statement beginning with the key.
 */
int
skip_to (
  FILE	*fp,
  char	*key
)
{
	char	*p, buf[PF_MAXLIN+1];
	int 	count, len;
	long	fpos;

	len   = strlen (key);
	count = currentask->t_scriptln;
	fpos  = 0L;

	while (fgets (buf, PF_MAXLIN, fp) != NULL) {

	    count++;
	    for (p = buf; *p == ' '  ||  *p == '\t'; p++)
		;

	    if (strncmp (p, key, len) == 0) {
		/* Seek back to beginning of line.
		 */
		fseek (fp, fpos, 0L);
		return (--count);
	    }

	    fpos = ftell (fp);
	}

	return (ERR);
}


/* DO_OPTION -- Set parameter attributes which have been explicitly
 * defined by the user.
 */
void
do_option (
  struct param	*pp,
  struct operand *oo,
  struct operand *o
)
{
	char	*opt;

	/* Determine the options and take appropriate action.
	 */
	opt = oo->o_val.v_s;

	if (!strcmp (opt, "mode")) {
	    /* (There is a scanmode() in pfiles.c.)
	     */
	    if (c_scanmode (pp, o) == ERR)
		cl_error (E_UERR, badopt, "MODE", pp->p_name);

	} else if (!strcmp (opt, "filetype")) {
	    if (scanftype (pp, o) == ERR)
		cl_error (E_UERR, badopt, "FILETYPE", pp->p_name);

	} else if (!strcmp (opt, "min")) {
	    if (scanmin (pp, o) == ERR) 
		cl_error (E_UERR, badopt, "MIN", pp->p_name);

	} else if (!strcmp (opt, "max")) {
	    if (scanmax (pp, o) == ERR)
		cl_error (E_UERR, badopt, "MAX", pp->p_name);

	} else if (!strcmp (opt, "enum")) {
	    if (scanenum (pp, o) == ERR)
		cl_error (E_UERR, badopt, "ENUM", pp->p_name);

	} else if (!strcmp (opt, "len") || !strcmp (opt, "length")) {
	    if (scanlen (pp, o) == ERR)
		cl_error (E_UERR, badopt,"LEN", pp->p_name);

	} else if (!strcmp (opt, "prompt")) {
	    int	   slen;

	    if (o->o_type != OT_STRING)
		cl_error (E_UERR, badopt, "PROMPT", pp->p_name);

	    slen = btoi (strlen(o->o_val.v_s) + 1);
	    pp->p_prompt = memneed (slen);
	    strcpy (pp->p_prompt, o->o_val.v_s);

	} else
	    cl_error (E_UERR, illegal_opt, pp->p_name);
}
