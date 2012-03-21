/*
 * PARAM.H -- In-core broken-out form of parameter file ("pfile") entry.
 * main line is a list of pfile structs, one per parameter file, starting
 * at parhead; these each head a list of params found in that file.
 *
 * USES operand.h and config.h
 */

/* ----------
 * reference chart showing how
 * the bits in p_type are set and the p_val/p_min/p_max fields are used for
 * various kinds of parameter "type" specs possible in a parameter file.
 

all legal p_type bit	val/min/max fields: which v_x and its meaning
   combinations								spec as
OT_XXXX  PT_XXXX							written
B I R S  L F S/C A	p_val		p_min		p_max		in file
- - - -  - -  -  -      --------------- --------------- --------------- -------
x			v_i, bool	-		-		b
  x			v_i, int	v_i, min val	v_i, max val	i
    x			v_r, real	v_r, min val	v_r, max val	r
      x			v_s, string	-		v_i, max length	s
x		 x      v_a, bool arr.  -		-		ab
  x		 x	v_a, int arr.   v_i, min val	v_i, max val	ai
    x		 x	v_a, real arr.	v_r, min val.	v_r, max val	ar
      x		 x	v_a, str. arr.	-		v_i, max length as
x        x		v_s, fname	-				*b
  x      x		v_s, fname	v_i, min val*	v_i, max val*	*i
    x    x		v_s, fname	v_r, min val*	v_r, max val*	*r
      x  x		v_s, fname	-		-		*s
      x    x		v_s, fname	v_s, min fname	v_s, max fname	f
      x  x x		v_s, fname	v_s, min fname*	v_s, max fname*	*f
      x       x		v_s, struct	-		v_i, max length	struct
      x  x    x		v_s, fname	-				*struct


Notes:
1) S/C refers to any one of PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET; their
   param representation is identical.  Similarly, the file spec "struct" may
   be gcur, imcur, pset, ukey, or pset.
2) * min/max applies to contents of list file after it is read and converted
   to the given base type, not to p_val.
3) "fname" means exactly MAXFILNAM chars are allocated, in-line, with
   the parameter regardless of how many are used. there is a permanent '\0'
   at v_s[MAXFILNAM-1].
4) note that PT_STRUCT|PT_GCUR|PT_IMCUR|PT_UKEY|PT_PSET|PT_FILNAM all imply
   OT_STRING but that, among these, only PT_FILNAM have ranges.  They may be
   considered qualifiers of OT_STRING.
5) the max length of a list entry is always MAXLIN.
6) these are not all the same as in the parameter file, such as struct
   length being stored in p_max. these must be properly placed when handling
   *.field param requests and when printing the in-core param structs back out.
7) min and max fields for arrays refer to all elements within the array.
8) only the scalar types bool, int, real and string may be arrays, and
   arrays may not be list-directed.
9) for a string array, the array is a list of pointers.
*/

#define PF_INDIRECT	')'	/* indirection metacharacter, ")param"	*/
#define	PF_DELIM	','	/* field delimiter within pfile		*/
#define	PF_MAXLIN	(132+2)	/* max pfile line length, plus \n \0	*/
#define	PF_COMMENT	'#'	/* starts a line of comment		*/
#define	PF_NFIELDS	7	/* number of fields in a pfile line	*/
#define	PF_NOSTRUCT	'*'	/* next line is NOT struct initialization*/
#define	PF_SZMINSTR	160	/* p_min field for string type params	*/
#define	PF_SZMAXSTR	64	/* p_max field for string type params	*/

struct param {
	char	*p_name;	/* name of parameter			*/
	struct	pfile *p_pfp;	/* pointer back to pfile		*/
	int	p_type;		/* type bits; see below			*/
	int	p_mode;		/* bit-packed mode fields. see below.	*/
	struct	operand p_valo;	/* value; or length if struct, file if list*/
	struct	operand p_mino;	/* p_val min and 			*/
	struct	operand p_maxo;	/*       max values			*/
	char	*p_prompt;	/* prompt string			*/
	FILE	*p_listfp;	/* if PT_LIST: fp of list file, if open	*/
	char	*p_listval;	/* buffer for list element (SZ_LINE)	*/
	struct	param *p_np;	/* pointer to next param, or NULL	*/
	short	p_flags;	/* see p_flags below			*/
	short	p_lenval;	/* buflen of p_valo.o_val.v_s if string	*/
};

/* Shorthand for referencing the values of the value, min, and max
 * fields.  e.g.  p_val.v_s = *char
 */
#define	p_val		p_valo.o_val
#define	p_min		p_mino.o_val
#define	p_max		p_maxo.o_val
#define p_aval		p_valo.o_val.v_a->a_ptr


/* names of bits in p_type.
 * these describe more information about the parameter.
 * lower 4 bits are same as for operands; see operand.h.
 */
#define	PT_LIST		0000020	/* values are in a file, not in pfile	*/
#define	PT_FILNAM	0000040	/* string is a bonafide filename	*/
#define	PT_STRUCT	0000100	/* used for structs			*/
#define	PT_GCUR		0000200	/* graphics cursor values structure	*/
#define	PT_IMCUR	0000400	/* image cursor values structure	*/
#define	PT_UKEY		0001000	/* user keystroke values sructure	*/
#define	PT_PSET		0002000	/* parameter set pointer parameter	*/

/* attributes if PT_FILNAM */
#define	PT_FER		0004000	/* file must exist and be readable	*/
#define	PT_FEW		0010000	/*            "           writable	*/
#define	PT_FNOE		0020000	/* file must not exist			*/
#define	PT_FTXT		0040000	/* file is a text file			*/
#define	PT_FBIN		0100000	/*      "    binary  "			*/

#define PT_ARRAY	0200000	/* parameter is an array		*/

/* names of mode bits in p_mode.
 */
#define	M_AUTO		0001	/* auto mode: be as quiet as possible	*/
#define	M_QUERY		0002	/* query: ask user about value		*/
#define	M_HIDDEN	0004	/* hidden: param normally not visible	*/
#define	M_LEARN		0010	/* learn: write out local copy when done*/
#define	M_MENU		0020	/* menu: call eparam at exec time	*/
#define	M_FAKE		0040	/* never flush this param to a pfile	*/
#define M_LOCAL		0100	/* Local var, not param.		*/


/* p_flags bits. 
 * misc characteristics of the parameter.
 * see pfilecopy() and pfcopyback() for details of P_SET/CLSET/QUERY.
 */
#define	P_IMIN		0001	/* min value is indefinite		*/
#define	P_UMIN		0002	/* min value is undefined		*/
#define	P_IMAX		0004	/* max value is indefinite		*/
#define	P_UMAX		0010	/* max value is undefined		*/
#define	P_LEOF		0020	/* set when see eof on list file	*/
#define	P_SET		0040	/* set in explicit assignment statement	*/
#define	P_CLSET		0100	/* set on command line of task		*/
#define	P_QUERY		0200	/* set from a query			*/
#define P_CL		0400	/* parameter is a CL parameter		*/

/* mode code letters in param file; recognized in either case */
#define	PF_AUTO		'a'
#define	PF_QUERY	'q'
#define	PF_HIDDEN	'h'
#define	PF_LEARN	'l'
#define	PF_MENU		'm'

/* ----------
 * one per loaded parameter file.
 * the ltask at ltp is used to get the param file's name (ltp->lt_lname),
 * its directory (osdir(lt_pname)), and package prefix (lt_pkp->pk_name).
 * pf_n use varies. always incremented for each command line argument set by
 *   posargset, etal. LT_BUILTIN tasks then use it directly to determine how
 *   many params there are since $nargs is not added in that case. other
 *   PF_FAKE pfiles use it to create $nargs then reset it to 0 and use it
 *   to count each unmatched param reference that is satisfied by a postional
 *   arg (see paramsrch). Other than to set $nargs, it is unused by tasks that
 *   do not have fake pfiles.
 * N.B. the way restor() is written, it is important that a param list is
 * never created with some params above and some below its task's topd.
 */
struct pfile {
	struct	pfile *pf_npf;		/* ptr to next pfile, else NULL	*/
	struct	pfile *pf_oldpfp;	/* ptr to old pfile, if copy	*/
	struct	ltask *pf_ltp;		/* ptr to this pfile's ltask	*/
	struct	pfile *pf_npset;	/* ptr to next pset in group	*/
	struct	param *pf_psetp;	/* ptr to pset-param if pset	*/
	struct	param *pf_pp;		/* ptr to first params 		*/
	struct	param *pf_lastpp;	/* last param off pfile		*/
	short	pf_n;			/* no. of params; see above	*/
	short	pf_flags;		/* see flags below		*/
	char	pf_pfilename[SZ_FNAME+1];	/* file to be updated	*/
};

/* pf_flags */
#define	PF_UPDATE	001	/* at least one param has P_SET set	*/
#define	PF_FAKE		002	/* made on the fly for an ltask without
				 * a pfile. should never be written out.
				 */
#define	PF_COPY		004	/* this is only the working copy of tasks
				 * pfile; it is never to be written out.
				 */
#define	PF_PSETREF	010	/* pfile contains a pset parameter	*/

/* size of param and pfile structs, IN INTS, for proper dictionary control.
 */
#define	PARAMSIZ	btoi (sizeof (struct param))
#define	PFILESIZ	btoi (sizeof (struct pfile))

/* Variable types used in parsing of declaration types.
 */
#define V_BOOL		0
#define V_INT		1
#define V_REAL		2
#define V_STRING	3
#define V_GCUR		4
#define V_IMCUR		5
#define V_UKEY		6
#define V_PSET		7
#define V_STRUCT	8
#define	V_FILE		9


char	*nextfield();		/* cracks next pfile line field		*/
char	*makelower();		/* upper to lower, in place and return	*/

struct	param *paramfind();	/* searches for a param on a given pfile*/
struct	param *paramsrch();	/* search, make sure param is there	*/
struct	param *lookup_param();	/* search standard path for a param	*/
struct	param *newparam();	/* allocate and link a new param	*/
struct	param *addparam();	/* make a new param off given pfile	*/
struct	param *newfakeparam();	/* add a fake param to pfile		*/
struct	pfile *pfilesrch();	/* read named pfile or ltask pfile	*/
struct	pfile *pfileload();	/* load pfile for ltask into memory	*/
struct	pfile *pfileread();	/* read and make params from a pfile	*/
struct	pfile *pfilefind();	/* look for pfile with given name	*/
struct	pfile *newpfile();	/* add a new pfile off parhead		*/
struct	pfile *pfilecopy();	/* make an in-core copy of a pfile	*/

int	defpar();		/* determine whether param exists	*/
int	defvar();		/* determine whether envvar exists	*/
