%{

#define import_spp
#define import_libc
#define import_stdio
#define import_ctype
#include <iraf.h>

#include "config.h"
#include "mem.h"
#include "operand.h"
#include "param.h"
#include "grammar.h"
#include "opcodes.h"
#include "clmodes.h"
#include "task.h"
#include "construct.h"
#include "errs.h"
#include "proto.h"


/* CL parser, written as a yacc grammar:
 *   build up an (rpn) instruction sequence begining at the base of the
 *   operand stack as the grammar is recognized.
 *
 * The parser may be called during parameter initialization (initiated by
 * the CALL meta-code instruction), and to parse the executable portion
 * (from the EXEC instruction).
 *
 * CONSTANT's are put on the dictionary by addconst() rather than the operand
 *   stack to avoid conflict with the code being created.  They are accessed
 *   by using the yylval of IDENT and CONSTANT as dictionary indices that
 *   point to struct operands.  This is facilitated with the stkop() macro.
 *   Make sure that topd and topcs are restored on return to discard these
 *   temporaries.
 * When building offsets for branches, such as BIFF and GOTO, allow
 *   for the advancement of the pc by the size of the instruction (in ints).
 *   See opcodes.c for the code executed by the branch instructions.
 */

extern	int cldebug;
#define	lint			/* turns off sccsid in Yacc parser	*/

/* shorthand way to get at operands in dictionary. x will be values returned
 * from addconst() by way of $n's from CONSTANT and IDENT tokens; see gram.c
 * and its uses in grammar.l. also see pushop() for a description of the stack.
 */
#define	stkop(x)	(reference (operand, (x)))

int	dobkg = 0;		/* set when want to do code in bkground	*/
int	npipes = 0;		/* number of pipes in a command		*/
XINT	pipe_pc;		/* pc of last ADDPIPE instruction	*/
int	posit = 0;		/* positional argument count		*/
int	inarglist = 0;		/* set when in argument list		*/
int	parenlevel = 0;		/* level of paren nesting in command	*/
int	index_cnt;		/* Index counter in array ref's		*/
char	curr_param[SZ_FNAME];	/* Parameter name of ref's		*/
char	curr_task[SZ_FNAME];	/* ltaskname of command 		*/
XINT	stmt_pc;		/* PC at beginning of current statement */
int	varlist;		/* Declaration is list directed.	*/
int	vartype;		/* Type of declaration.			*/
int	do_params;		/* Are param definitions legal here?	*/
int	errcnt;			/* Syntax error count.			*/
int	inited;			/* Was variable already initialized.	*/
struct	param *pp;		/* Pointer to param being compiled.	*/
int	n_aval;			/* Number of array init values.		*/
int	lastref;		/* Was last ref an array?		*/
int	for_expr;		/* Was there an expression in FOR?	*/
char	*ifseen;		/* Have we just processed an IF?	*/

/* context-sensitive switches. technique is ok, but beware of nesting!
 */
static	int absmode = 0;	/* set by first absolute mode arg in cmd*/
static	int newstdout = 0;	/* set if stdout redirected in arg	*/
static	int bracelevel = 0;	/* set while in s_list to inhibit &	*/
static	int tbrace = 0;		/* fake braces for declarations		*/
static	int dobrace = 0;	/* handling braces.			*/
static	int sawnl = 0;		/* set when EOST was \n, else 0		*/
static	int printstmt = 0;	/* set when parsing FPRINT statement	*/
static	int scanstmt = 0;	/* set when parsing SCAN statement	*/

/* printf-format error messages.
 */
char	*posfirst = "All positional arguments must be first\n";
/* char	*look_parm= "Error searching for parameter `%s'."; */
char	*inval_arr= "Invalid array type for `%s'.";
char	*inv_index= "Invalid index definition for `%s'.";
char	*arrdeferr= "Error in array initialization for `%s'.";
/* char	*arrinbrack="Array initialization must be in brackets for `%s'."; */
char	*badparm =  "Parameter definition of `%s' is illegal here.";
char	*illegalvar="Illegal variable declarations.";
char	*locallist= "Local list variables are not permitted.";
char	*twoinits = "Two initializations for parameter `%s'.";
char	*exlimits = "Explicit range required for loop in external param.\n";

extern	char	cmdblk[SZ_CMDBLK+1];	/* Command buffer in history.c */
extern	char	*ip_cmdblk;		/* Pointer to current char in command.*/
extern	char	*err_cmdblk;		/* ip_cmdblk when error detected. */

char	*index();
struct	param *initparam();
struct	label *getlabel(), *setlabel();

/* arbitrary large number for bracelevel in a procedure script 
 */
#define MAX_ERR    10
#define EYYERROR  { err_cmdblk = ip_cmdblk; YYERROR; }

%}

%token	Y_SCAN Y_SCANF Y_FSCAN Y_FSCANF Y_OSESC 
%token	Y_APPEND Y_ALLAPPEND Y_ALLREDIR Y_GSREDIR Y_ALLPIPE
%token	D_D D_PEEK
%token	Y_NEWLINE Y_CONSTANT Y_IDENT
%token	Y_WHILE Y_IF Y_ELSE
%token  Y_FOR Y_BREAK Y_NEXT
%token  Y_SWITCH Y_CASE Y_DEFAULT
%token	Y_RETURN Y_GOTO
%token	Y_PROCEDURE Y_BEGIN Y_END 
%token	Y_BOOL Y_INT Y_REAL Y_STRING Y_FILE Y_STRUCT
%token	Y_GCUR Y_IMCUR Y_UKEY Y_PSET

%right	'=' YOP_AOADD YOP_AOSUB YOP_AOMUL YOP_AODIV YOP_AOCAT
%left	YOP_OR
%left	YOP_AND
%left	YOP_EQ YOP_NE 
%left	'<' '>' YOP_LE YOP_GE
%left	YOP_CONCAT
%left	'+' '-'
%left	'*' '/' '%'
%left	YOP_NOT UMINUS		/* supplies precedence for unary minus	*/
%left	YOP_POW

%start	block

%%

block	:	/* empty */ {
		    /* Done once on entry but after at least one call to
		     * yylex().  Good for initing parser flags.
		     * Note: this does not get called in procedure scripts.
		     */
		    if (cldebug)
			eprintf ("parse init (block)...\n");

		    errcnt     = 0;
		    err_cmdblk = 0;
		    dobkg      = 0;
		    inarglist  = 0;
		    parenlevel = 0;
		    bracelevel = 0;
		    tbrace     = 0;
		    dobrace    = 0;
		    do_params  = YES;
		    last_parm  = NULL;
		    ifseen     = NULL;
		    label1     = NULL;
		    parse_pfile= currentask->t_pfp;
		}

	|	'.' NL {
		    /* Prepare to rerun whatever was compiled last.
		     * Does not work for the debug commands builtin here.
		     */
		    if (parse_state != PARSE_FREE) {
			eprintf ("Illegal parser state.\n");
			EYYERROR;
		    }
		    rerun();
		    YYACCEPT;
		}

	|	block {
		    if (parse_state == PARSE_PARAMS) {
			eprintf ("Illegal parser state.\n");
			EYYERROR;
		    }
		}
		debug xstmt {
		    if (sawnl && bracelevel == 0) {
			if (!errcnt)
			    compile (END);
			if (ifseen) {
			    /* Simulate an unput of what has been read
			     * from the current line.
			     */
			    ip_cmdblk = ifseen;
			}
			YYACCEPT;
		    }
		}

	|	script_params {
		    /* Parse the parameters in a script file.  This will
		     * normally be done on a call by pfileread().
		     */
		    if (parse_state != PARSE_PARAMS) {
			eprintf ("Illegal parser state.\n");
			errcnt++;
		    }
		    YYACCEPT;
		}

	|	script_body {
		    /* Parse the executable statements in a script.
		     */
		    if (parse_state != PARSE_BODY) {
			eprintf ("Illegal parser state.\n");
			errcnt++;
		    }
		    if (!errcnt)
			compile (END);
		    YYACCEPT;
		}

	|	error NL {
		    /* This catches errors that the two other error lines
		     * can't get, e.g. a missing `}' at the end of a script, 
		     * or errors occuring in interactive input.
		     */
		    yyerrok;

		    /* Discard everything and compile a null statement.
		     */
		    if (!errcnt) {
			do_params = YES;
			pc = currentask->t_bascode;
			if (parse_state != PARSE_PARAMS)
			    compile (END);

			topd = currentask->t_topd;
			topcs = currentask->t_topcs;

			/* Unlink any added parms.  Resetting of topd will
			 * already have reclaimed space.
			 */
			if (last_parm) {
			    last_parm->p_np = NULL;
			    currentask->t_pfp->pf_lastpp = last_parm;
			    last_parm = NULL;
			}
	 	    }

		    /* Print cmdblk and show position of error.
		     */
		    p_position();
		    if (currentask->t_flags & T_SCRIPT) 
			cl_error (E_UERR, "syntax error, line %d",
			    currentask->t_scriptln);
		    else
			cl_error (E_UERR, "syntax error");

		    YYACCEPT;
		}
	;

debug	:	/* empty */
	|	D_XXX EOST {
		    /* debug are those debugging functions that
		     * should be run directly and not through a
		     * builtin task due to stack or other changes,
		     * ie, don't change what we are trying to show.
		     */
		    printf ("\n");
		} debug
	;

D_XXX	:	D_D {
		    d_d(); /* show dictionary/stack pointers */
		}
	|	D_PEEK Y_CONSTANT { /* show a dictionary location	*/
		    if (stkop($2)->o_type & OT_INT) {
			int	idx;
			idx = stkop($2)->o_val.v_i;
			eprintf ("%d:\t%d (0%o)\n", idx, stack[idx],
				stack[idx]);
		    } else
			eprintf ("usage: D_PEEK <d. index>\n");
		}
	|	'~' {
		    d_stack (pc, 0);		/* show compiled code	*/
		}
	;

script_params :	proc_stmt
		var_decls
		begin_stmt { 
			/* Check for required params.
			 */
			if (!errcnt)
			    proc_params(n_procpar);
		}
	;

script_body:	begin_stmt {
			/* Initialize parser for procedure body.
			 */
			if (cldebug)
			    eprintf ("parse init (script_body)...\n");
ready_();

			errcnt     = 0;
			err_cmdblk = 0;
			dobkg      = 0;
			inarglist  = 0;
			parenlevel = 0;
			dobrace    = 0;
			bracelevel = PBRACE; /* disable lexmodes; force "end" */
			tbrace     = 0;
 			do_params  = NO;
			last_parm  = NULL;
			ifseen     = NULL;
			label1     = NULL;
			parse_pfile= currentask->t_pfp;
		}
		s_list
		opnl
		end_stmt
	;

proc_stmt:	Y_PROCEDURE {  
			/* Initialize parser for procedure parameters.
			 */
			if (cldebug)
			    eprintf ("parse init (proc_stmt)...\n");

			errcnt     = 0;
			err_cmdblk = 0;
			dobkg      = 0;
			inarglist  = 0;
			parenlevel = 0;
			bracelevel = PBRACE;
			tbrace     = 0;
		        dobrace    = 0;
			do_params  = YES;
		        last_parm  = NULL;
			label1     = NULL;
		}
		param bparam_list EOST
	;

bparam_list:	/* Nothing at all, not even parens. */
		{
		    n_procpar = 0;
		}
	|	LP param_list RP
	;

/* The definition of the parameter list excludes lists of the 
 * form  a,,b
 */
param_list:	/* empty */ {
		    n_procpar = 0;
		}
	|	xparam_list
	;

xparam_list:	param { 
		    n_procpar = 1;
		    if (!errcnt)
			push (stkop($1));
		}
	|	xparam_list DELIM param {
		    n_procpar++;
		    if (!errcnt)
			push (stkop($3));
		}
	;

var_decls:	/* No params. */
	|	var_decl_block
	;

var_decl_block:	var_decl_line
	|	var_decl_block var_decl_line
	;

var_decl_line:	EOST	/* Blank. */
	|	var_decl_stmt
	|	error NL {
		    /* This catches errors in the parameter declarations
		     * of a procedure script.
		     */
		    yyerrok;

		    /* Discard everything and compile a null statement.
		     */
		    if (!errcnt) {
			do_params = YES;
			pc = currentask->t_bascode;
			if (parse_state != PARSE_PARAMS)
			    compile (END);

			topd = currentask->t_topd;
			topcs = currentask->t_topcs;

			/* Unlink any added parms.  Resetting of topd will
			 * already have reclaimed space.
			 */
			if (last_parm) {
			    last_parm->p_np = NULL;
			    currentask->t_pfp->pf_lastpp = last_parm;
			    last_parm = NULL;
			}
	 	    }

		    /* Print cmdblk and show position of error.  We know
		     * we're parsing a procedure script, so print the line
		     * number too.
		     */
		    p_position();
		    cl_error (E_UERR, "syntax error, line %d", 
			currentask->t_scriptln);
		}
	;

var_decl_stmt:	typedefs {
	 	        /* For in-line definitions we don't want
			 * to freeze stuff on the dictionary, so
			 * only allow additions if the dictionary
			 * is the same as at the beginning of the task.
			 */
			if (!errcnt) {
			    if (parse_state != PARSE_PARAMS) {
				if (currentask->t_topd != topd)
				    cl_error (E_UERR, illegalvar);
				last_parm = currentask->t_pfp->pf_lastpp;
			    }
			}

		        /* Increment bracelevel temporarily to defeat command
			 * mode, in case this is an in-line declaration and
			 * lexmodes=yes.
			 */
			bracelevel += PBRACE;
			tbrace++;

		} var_decl_list EOST {
		      	/* Update dictionary to include these definitions.
			 */
			if (!errcnt) {
			    if (parse_state != PARSE_PARAMS) {
				currentask->t_topd = topd;
				last_parm = 0;
			    }
			}

			/* Restore command mode */
			bracelevel -= PBRACE;
			tbrace--;
		}
	;

typedefs:	Y_BOOL		{ vartype = V_BOOL; }
	|	Y_STRING	{ vartype = V_STRING; }
	|	Y_REAL		{ vartype = V_REAL; }
	|	Y_FILE		{ vartype = V_FILE; }
	|	Y_GCUR		{ vartype = V_GCUR; }
	|	Y_IMCUR		{ vartype = V_IMCUR; }
	|	Y_UKEY		{ vartype = V_UKEY; }
	|	Y_PSET		{ vartype = V_PSET; }
	|	Y_INT		{ vartype = V_INT; }
	|	Y_STRUCT	{ vartype = V_STRUCT; }
	;

var_decl_list:	var_decl_plus
	|	var_decl_plus DELIM var_decl_list
	;

var_decl_plus:	var_decl {
			if (!errcnt) {
			    if (pp != NULL) {
				if (n_aval > 1)
				    pp->p_type |= PT_ARRAY;

				if (pp->p_type & PT_ARRAY)
				    do_arrayinit (pp, n_aval, index_cnt);
		      		else
		 		    do_scalarinit (pp, inited);
			    }
			}
		}

		/* Semi-colon in following rule is not input by user, but
		 * rather by lexical analyzer to help close compound
		 * statements.
		 */
	|	var_decl '{' options_list ';' '}'  {
			if (!errcnt) {
			    if (pp != NULL) {
				if (!do_params)
				    cl_error (E_UERR, badparm, pp->p_name);

				if (n_aval > 1)
				    pp->p_type |= PT_ARRAY;

				if (pp->p_type & PT_ARRAY)
				    do_arrayinit (pp, n_aval, index_cnt);
		      		else
		 		    do_scalarinit (pp, n_aval);
			    }
			}
		}
	;

var_decl:	var_def {
			inited = NO;
			n_aval = 0;
		}
	|	var_def '=' {
			n_aval = 0;
		}
		init_list {
			inited = YES;
		}
	;

var_def	:	var_name {
		    index_cnt = 0;
		    if (!errcnt)
			pp = initparam (stkop($1), do_params, vartype, varlist);
		}
	|	var_name {
		    int  itemp;

		    if (!errcnt) {
			pp = initparam (stkop($1), do_params, vartype, varlist);

			if (pp != NULL) {
			    itemp = (pp->p_type & OT_BASIC) == pp->p_type;
			    itemp = itemp && !varlist;
			    if (itemp)
				pp->p_type |= PT_ARRAY;
			    else
				cl_error (E_UERR, inval_arr, pp->p_name);
			}
		    }
		}
		'[' init_index_list ']'
	;

var_name:	param {
			varlist = NO;
			index_cnt = 0;
		}
	|	'*' param {
			if (!do_params) {
			    eprintf (locallist);
			    EYYERROR;
			}
			varlist = YES;
			index_cnt = 0;
			$$ = $2;
		}
	;

init_index_list:
		/* A null index list means get the length of the array 
		 * from the initializer.
		 */
	|	init_index_range
	|	init_index_list DELIM init_index_range
	;

init_index_range:
		const {
		    if (!errcnt) {
			if (pp != NULL) {
			    if (stkop($1)->o_type == OT_INT) {
				push (stkop($1)->o_val.v_i);
				push (1);
			    } else if (maybeindex) {
				/* Confusion between sexagesimal and index
				 * range.  Maybeindex is set only when operand
				 * is real.
			 	 */
				int  i1,i2;
				sexa_to_index (stkop($1)->o_val.v_r, &i1, &i2);
				push (i2-i1+1);
				push (i1);
			    } else {
				eprintf (inv_index, pp->p_name);
				EYYERROR;
			    }
			    index_cnt++;
			}
		    }
		}
	|	const ':' const {
			if (!errcnt) {
			    if (pp != NULL) {
				if (stkop($1)->o_type != OT_INT  ||
				    stkop($3)->o_type != OT_INT)
				    cl_error (E_UERR, inv_index, pp->p_name);
				else {
				    push (stkop($3)->o_val.v_i -
				          stkop($1)->o_val.v_i + 1);
				    push (stkop($1)->o_val.v_i);
			        }
			        index_cnt++;
			    }
			}
		}
	;

init_list:	init_elem
	|	init_list DELIM	init_elem
	;

init_elem:	const {
			if (!errcnt) {
			    if (pp != NULL) {
				push (stkop($1) );
				n_aval++;
			    }
			}
		}
	|	Y_CONSTANT LP const RP  /* PL/I notation. */
		{
			int   cnt;
			
			if (!errcnt)
			    if (pp != NULL) {
			    	if (stkop($1)->o_type != OT_INT)
				    cl_error (E_UERR, arrdeferr, pp->p_name);

			        cnt = stkop($1)->o_val.v_i;
			        if (cnt <= 0)
				    cl_error (E_UERR, arrdeferr, pp->p_name);

			        while (cnt-- > 0) {
				    push (stkop($3));
				    n_aval++;
			        }
			    }
		}
	;

const	:	Y_CONSTANT
	|	number
	;

/* The parser and lexical analyzer don't see negative numbers as an
 * entity.  So we must join signs to their constants.
 */
number	:	sign Y_CONSTANT {
		      	if (stkop($2)->o_type == OT_INT) {
			    stkop($2)->o_val.v_i *= $1;
			    $$ = $2;
			} else if (stkop($2)->o_type == OT_REAL) {
			    stkop($2)->o_val.v_r *= $1;
			    $$ = $2;
			} else {
			    eprintf ("Invalid constant in declaration.\n");
			    EYYERROR;
			}
		}
	;

sign	:	'+'	{ $$ =  1; }
	|	'-'	{ $$ = -1; }

options_list:	init_list DELIM options {
			/* Check if we already had an initialization. 
			 */
		      	if (!errcnt) {
			    if (inited && pp != NULL) {
				eprintf (twoinits, pp->p_name);
				EYYERROR;
			    }
			}
		}
	|	init_list {
		      	if (!errcnt) {
			    if (inited && pp != NULL) {
			        eprintf (twoinits, pp->p_name);
				EYYERROR;
			    }
			}
		}
	|	options
	;

options	:	option
	|	options DELIM option
	;

option	:	Y_IDENT '=' const {
			if (!errcnt)
			    if (pp != NULL)
			    	do_option (pp, stkop($1), stkop($3));
		}
	;

begin_stmt:	Y_BEGIN NL
	;

/* In normal expressions, a param means the name of a parameter, but in
 * command line arguments, it may be a string constant.  Pull out param
 * from expr to let the arg rule deal with it specially.
 */

expr	:	expr0
	|	ref {
		    if (!errcnt)
		        compile (PUSHPARAM, stkop($1)->o_val.v_s);
		}
	;

/* EXPR0 is everything but a simple parameter.  This is needed for argument
 * lists so that a simple parameter may be treated as a special case of a
 * string constant.  EXPR1 also excludes constants.  This is needed
 * to eliminate ambiguities in the grammar which would arise from
 * the handling of the lexical ambiguity of sexagesimal constants
 * and array index ranges.
 */
expr0	:	expr1
	|	Y_CONSTANT {
		    if  (!errcnt)
		        compile (PUSHCONST, stkop($1));
		}
	|	Y_GCUR {
		    /* "gcur" is both a keyword and a CL global parameter,
		     * and must be built into the grammar here to permit
		     * reference of the parameter in expressions.
		     */
		    if (!errcnt)
			compile (PUSHPARAM, "gcur");
		}
	|	Y_IMCUR {
		    if (!errcnt)
			compile (PUSHPARAM, "imcur");
		}
	|	Y_UKEY {
		    if (!errcnt)
			compile (PUSHPARAM, "ukey");
		}
	|	Y_PSET {
		    if (!errcnt)
			compile (PUSHPARAM, "pset");
		}
	;

expr1	:	LP expr RP

	|	expr '+' opnl expr {
		    if (!errcnt)
			compile (ADD);
		}
	|	expr '-' opnl expr {
		    if (!errcnt)
			compile (SUB);
		}
	|	expr '*' opnl expr {
		    if (!errcnt)
			compile (MUL);
		}
	|	expr '/' opnl expr {
		    if (!errcnt)
			compile (DIV);
		}
	|	expr YOP_POW opnl expr {
		    if (!errcnt)
			compile (POW);
		}
	|	expr '%' opnl expr {
		    struct	operand o;
		    if (!errcnt) {
			o.o_type = OT_INT;
			o.o_val.v_i = 2;
			compile (PUSHCONST, &o);
			compile (INTRINSIC, "mod");
		    }
		}
	|	expr YOP_CONCAT opnl expr {
		    if (!errcnt)
			compile (CONCAT);
		}
	|	expr '<' opnl expr {
		    if (!errcnt)
			compile (LT);
		}
	|	expr '>' opnl expr {
		    if (!errcnt)
			compile (GT);
		}
	|	expr YOP_LE opnl expr {
		    if (!errcnt)
			compile (LE);
		}
	|	expr YOP_GE opnl expr {
		    if (!errcnt)
			compile (GE);
		}
	|	expr YOP_EQ opnl expr {
		    if (!errcnt)
			compile (EQ);
		}
	|	expr YOP_NE opnl expr {
		    if (!errcnt)
			compile (NE);
		}
	|	expr YOP_OR opnl expr {
		    if (!errcnt)
			compile (OR);
		}
	|	expr YOP_AND opnl expr {
		    if (!errcnt)
			compile (AND);
		}
	|	YOP_NOT expr {
		    if (!errcnt)
			compile (NOT);
		}
	|	'-' expr %prec UMINUS {
		    if (!errcnt)
			compile (CHSIGN);
		}

	|	Y_SCAN LP {
		    /* Free format scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} scanarg RP {
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
		        o.o_val.v_i = pop();	/* get total number of args*/
			compile (PUSHCONST, &o);
			compile (SCAN);
		    }
		}
	|	Y_SCANF LP {
		    /* Formatted scan. */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} scanfmt DELIM scanarg RP {
		    if (!errcnt) {
			struct	operand o;

			/* Compile number of arguments. */
			o.o_type = OT_INT;
		        o.o_val.v_i = pop();
			compile (PUSHCONST, &o);

			compile (SCANF);
		    }
		}

	|	Y_FSCAN LP {
		    /* Free format scan from a parameter.  */
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} scanarg RP {
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
			o.o_val.v_i = pop();	/* get total number of args*/
			compile (PUSHCONST, &o);
			compile (FSCAN);
		    }
		}

	|	Y_FSCANF LP Y_IDENT DELIM {
		    /* Formatted scan from a parameter.
		     * fscanf (param, format, arg1, ...)
		     */
		    if (!errcnt) {
			compile (PUSHCONST, stkop ($3));
		        push (1);	/* use control stack to count args */
		    }
		} scanfmt DELIM scanarg RP {
		    if (!errcnt) {
			struct	operand o;

			/* Compile number of arguments. */
			o.o_type = OT_INT;
			o.o_val.v_i = pop();
			compile (PUSHCONST, &o);

			compile (FSCANF);
		    }
		}

	|	intrinsx LP {
		    if (!errcnt)
			push (0);	/* use control stack to count args */
		} intrarg RP {
		    if (!errcnt) {
			struct	operand o;
			o.o_type = OT_INT;
			o.o_val.v_i = pop();
			compile (PUSHCONST, &o);
			compile (INTRINSIC, stkop($1)->o_val.v_s);
		    }
		}
	;

/* Variable types are keywords, so any types which are also intrinsic
 * functions are added here.
 */
intrinsx:	intrins
	|	Y_INT {
			/* The YACC value of this must match normal intrinsics
			 * so we must generate an operand with the proper
			 * string. 
			 */
			if (!errcnt)
			    $$ = addconst ("int", OT_STRING);
		}
	|	Y_REAL {
			if (!errcnt)
			    $$ = addconst ("real", OT_STRING);
		}
	;

scanfmt	:	expr {
		    if (!errcnt) {
		        push (pop() + 1);		/* inc num args	*/
		    }
		}
	;

scanarg	:	/* empty.  This is bad for scan but we don't want to
		 * generate a cryptic syntax error.  See also intrarg.
		 * This rule reduces the strings from right to left.
		 * Note the lexical analyzer strips optional newlines
		 * after comma delimiters, so we don't need an opnl here.
		 */
	|	Y_IDENT {
                    if (!errcnt) {
                        compile (PUSHCONST, stkop ($1));
                        push (pop() + 1);               /* inc num args */
                    }
		}
	|	Y_IDENT DELIM scanarg {
                    if (!errcnt) {
                        compile (PUSHCONST, stkop ($1));
                        push (pop() + 1);               /* inc num args */
                    }
		}
	;

intrarg	:	/* empty. this is to allow () but it also allows
		 * (x,,x). may want to prune this out.
		 */
	|	expr {
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		}
	|	intrarg DELIM expr {
		    if (!errcnt)
			push (pop() + 1);		/* inc num args	*/
		}
	;


/* Statements. */

stmt	:	c_stmt
	|	assign	EOST
	|	cmdlist	EOST
	|	immed	EOST
	|	inspect	EOST
	|	osesc	EOST
	|	popstk	EOST
	|	if
	|	ifelse
	|	while
	|	for
	|	switch
	|	case
	|	default
	|	next	EOST
	|	break	EOST
	|	goto	EOST
	|	return	EOST
	|	label_stmt
	|	nullstmt
	;

		/* A compound statement may be followed by zero or one 
		 * newlines.
		 */
c_stmt	:	c_blk
	|	c_blk NL
	;

c_blk	:	'{' {
		    bracelevel++;
		} s_list opnl {
		    --bracelevel;
		} '}'
	;

s_list	:	/* empty */
	|	s_list opnl xstmt
	;

/* Put "implicit" parentheses around right hand side of assignments to
 * permit easy arithmetic even with lexmodes=yes.
 */
assign	:	ref equals expr0 {
			--parenlevel;
 			if (!errcnt)
		      	    compile (ASSIGN, stkop($1)->o_val.v_s);
		}
	|	ref equals ref {
			/* Old code pushed a constant rather than a param
			 * when not within braces.  This doesn't seem
			 * to be what most people want.
			 */
			--parenlevel;
			if (!errcnt) {
			    compile (PUSHPARAM, stkop($3)->o_val.v_s);
		            compile (ASSIGN, stkop($1)->o_val.v_s);
		    	}
		}
	|	ref {
			parenlevel++;
		}
		assign_oper expr {
		      	--parenlevel;
			if (!errcnt)
		  	    compile ($3, stkop($1)->o_val.v_s);
		}
	;

	/* Breaking out the '=' avoids grammar ambiguities.
	 */
equals	:	'=' {
			parenlevel++;
		}
	;

assign_oper:	YOP_AOADD 	{ $$ = ADDASSIGN; }
	|	YOP_AOSUB 	{ $$ = SUBASSIGN; }
	|	YOP_AOMUL 	{ $$ = MULASSIGN; }
	|	YOP_AODIV 	{ $$ = DIVASSIGN; }	
	|	YOP_AOCAT 	{ $$ = CATASSIGN; }	
	;

cmdlist	:	command {
		    npipes = 0;
		} cmdpipe {
		    if (!errcnt) {
			compile (EXEC);
			if (npipes > 0)
			    compile (RMPIPES, npipes);
		    }
		}
	;

cmdpipe	:	/* empty */
	|	cmdpipe pipe {
		    /* Pipefiles must be allocated at run time using a stack
		     * to permit pipe commands within loops, and to permit
		     * scripts called in a pipe to themselves contain pipe
		     * commands.  ADDPIPE allocates a new pipefile on the
		     * pipe stack and pushes its name on the operand stack.
		     * GETPIPE pushes the pipefile at the top of the pipe
		     * stack onto the operand stack.  RMPIPES removes N pipes
		     * from the pipe stack, and deletes the physical pipefiles.
		     */

		    if (!newstdout) {
			/* When the runtime code creates the pipe it needs to
			 * know the identity of the two tasks sharing the pipe
			 * to determine what type of pipe to create (text or
			 * binary).  Save the pc of the ADDPIPE instruction
			 * so that we can backpatch it below with a pointer to
			 * the name of the second task in the pipe (ADDPIPE
			 * will be called during startup of the first task
			 * hence will know its name).
			 */
			pipe_pc = compile (ADDPIPE, NULL);

			if ($2 == 1)
			    compile (REDIR);
			else
			    compile (ALLREDIR);
			compile (EXEC);

		    } else {
			eprintf ("multiple redirection\n");
			YYERROR;
		    }

		} command {
		    /* Compile the GETPIPE instruction with the name of the
		     * second task in the current pipe, and backpatch the
		     * matching ADDPIPE instruction with the PC of the GETPIPE.
		     */
		    (coderef(pipe_pc))->c_args = compile (GETPIPE, curr_task);
		    compile (REDIRIN);
		    npipes++;		/* Overflow checking is in ADDPIPE */
		}
	;

pipe	:	'|' opnl {
		    $$ = 1;
		}
	|	Y_ALLPIPE opnl {
		    $$ = 2;
		}
	;

command	:	tasknam {
		    char    *ltname;

		    ltname = stkop($1)->o_val.v_s;
		    compile (CALL, ltname);
		    strcpy (curr_task, ltname);

		    /* The FPRINT task is special; the first arg
		     * is the destination and must be compiled as
		     * a string constant no matter what.  Set flag
		     * so that 'arg' compiles PUSHCONST.
		     */
		    printstmt = (strcmp (ltname, "fprint") == 0);

		    /* Ditto with SCAN; all the arguments are call by
		     * reference and must be compiled as string constants.
		     */
		    scanstmt = (strcmp (ltname, "scan") == 0 ||
				strcmp (ltname, "scanf") == 0);

		    absmode = 0;
		    posit = 0;
		    newstdout = 0;
		    parenlevel = 0;
		} BARG {
		    inarglist = 1;
		} args EARG {
		    inarglist = 0;
		    parenlevel = 0;
		    scanstmt = 0;
		}
	;

args	:	DELIM {
		    /* (,x) equates to nargs == 2.  Call posargset with
		     * negative dummy argument to bump nargs.
		     */
		    if (!errcnt) {
			compile (POSARGSET, -1);
			posit++;
			printstmt = 0;
			scanstmt = 0;
		    }
		} arglist	
	|	arglist
	;

arglist	:	arg 
	|	arglist DELIM arg
	;

arg	:	/* nothing - compile a null posargset to bump nargs */
		{
		    if (!errcnt) {
			if (posit > 0) {		/* not first time */
			    compile (POSARGSET, -posit);
			    printstmt = 0;
			    scanstmt = 0;
			}
			posit++;
		    }
		}
	|	expr0 {
		    if (absmode) {
			eprintf (posfirst);
			EYYERROR;
		    } else
			if (!errcnt)
			    compile (POSARGSET, posit++);
		}
	|	ref {
		    if (absmode) {
			eprintf (posfirst);
			EYYERROR;
		    } else if (!errcnt) {
			if (scanstmt) {
			    char    pname[SZ_FNAME];
			    char    *pk, *t, *p, *f;
			    struct  pfile *pfp;
			    struct  operand o;

			    /* If no task name specified check the pfile for
			     * the task containing the scan statement for the
			     * named parameter.
			     */
			    breakout (stkop($1)->o_val.v_s, &pk, &t, &p, &f);
			    pfp = currentask->t_pfp;
			    if (*pk == NULL && *t == NULL &&
				pfp && paramfind(pfp,p,0,1)) {

				sprintf (pname, "%s.%s",
				    currentask->t_ltp->lt_lname, p);
				if (*f) {
				    strcat (pname, ".");
				    strcat (pname, f);
				}
			    } else
				strcpy (pname, stkop($1)->o_val.v_s);

			    o = *(stkop($1));
			    o.o_val.v_s = pname;
			    compile (PUSHCONST, &o);
			    compile (INDIRPOSSET, posit++);

			} else if (parenlevel == 0 || printstmt) {
			    compile (PUSHCONST, stkop($1));
			    compile (INDIRPOSSET, posit++);
			    /* only first arg of fprint stmt is special. */
			    printstmt = 0;

			} else {
			    compile (PUSHPARAM, stkop($1)->o_val.v_s);
			    compile (POSARGSET, posit++);
			}
		    }
		}
	|	ref '=' expr0 {
		    absmode++;
		    if (!errcnt)
			compile (ABSARGSET, stkop($1)->o_val.v_s); 
		}
	|	ref '=' ref {
		    absmode++;
		    if (!errcnt) {
			if (parenlevel == 0) {
			    compile (PUSHCONST, stkop($3));
			    compile (INDIRABSSET, stkop($1)->o_val.v_s); 
			} else {
			    compile (PUSHPARAM, stkop($3)->o_val.v_s);
			    compile (ABSARGSET, stkop($1)->o_val.v_s);
			}
		    }
		}
	|	param '+' {
		    absmode++;
		    if (!errcnt)
			compile (SWON, stkop($1)->o_val.v_s);
		}
	|	param '-' {
		    absmode++;
		    if (!errcnt)
			compile (SWOFF, stkop($1)->o_val.v_s);
		}
	|	'<' file {
		    if (!errcnt)
			compile (REDIRIN);
		}
	|	'>' file {
		    newstdout++;
		    if (!errcnt)
			compile (REDIR);
		}
	|	Y_ALLREDIR file {
		    newstdout++;
		    if (!errcnt)
			compile (ALLREDIR);
		}
	|	Y_APPEND file {
		    newstdout++;
		    if (!errcnt)
			compile (APPENDOUT);
		}
	|	Y_ALLAPPEND file {
		    newstdout++;
		    if (!errcnt)
			compile (ALLAPPEND);
		}
	|	Y_GSREDIR file {
		    if (!errcnt)
			compile (GSREDIR, stkop($1)->o_val.v_s);
		}
	;

file	:	expr0 {
		    absmode++;
		    /* constant already pushed by expr0.
		     */
		}
	|	param {
		    absmode++;
		    if (!errcnt) {
			if (parenlevel == 0)
			    compile (PUSHCONST, stkop($1));
			else
			    compile (PUSHPARAM, stkop($1)->o_val.v_s);
			}
		}
	;

immed	:	equals expr0 {
			--parenlevel;
			if (!errcnt)
			    compile (IMMED);
		}
	|	equals ref {
		      	--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop($2)->o_val.v_s);
		}
	;

inspect	:	ref equals {
			--parenlevel;
			if (!errcnt)
			    compile (INSPECT, stkop($1)->o_val.v_s);
		}
	;

osesc	:	Y_OSESC {
		    if (!errcnt)
			compile (OSESC, stkop($1)->o_val.v_s);
		}
	;

popstk	:	equals {
		    --parenlevel;
		    if (!errcnt)
			compile (IMMED);
		}
	;

if	:	if_stat {
		    /* pop BIFF addr and set branch to just after statement
		     */
		    XINT   biffaddr;
		    if (!errcnt) {
			biffaddr = pop();
		    	coderef (biffaddr)->c_args = pc - biffaddr - 3;
		    }
		} 
	;

if_stat	:	Y_IF LP expr RP {
			/* save BIFF addr so branch can be filled in 
			 */
			if (!errcnt)
			    push (compile (BIFF, 0));
		} opnl xstmt {
			/* The shift/reduce conflict in the IF-IF/ELSE
			 * construct can cause errors in compilation
			 * because the IF statement can also be a
			 * terminal symbol, i.e. it may be all that
			 * is parsed in one call to the parser.
			 * The parser must look ahead one token
			 * to find if there is an else statement
			 * following.  If there is no following
			 * token an EOF may be detected prematurely.
			 * When the IF statement is being parsed not
			 * inside any braces, then when the next token
			 * is not an ELSE care must be taken that this
			 * token is seen on a subsequent invocation
			 * of the parser.  The `ifseen' flag is
			 * used within the support for the lexical
			 * analyzer located in `history.c'.
			 */
			if (cldebug) 
			    eprintf ("ytab: setting ifseen=yes\n");

			if (currentask->t_flags & T_INTERACTIVE)
			    ifseen = ip_cmdblk;
			else
			    ifseen = cmdblk;
		}
	;

ifelse	:	if_stat Y_ELSE {
		    XINT  biffaddr;

		    ifseen = NULL;
		    if (!errcnt) {
			/* Pop and save BIFF address, compile and push addr 
			 * of GOTO, and set BIFF branch to just after GOTO.
			 */
			biffaddr = pop();
			push (compile (GOTO, 0));
			coderef (biffaddr)->c_args = pc - biffaddr - 3;
		    }
		} opnl xstmt {
		    XINT  gotoaddr;
		    if (!errcnt) {
			/* Pop GOTO addr and set branch to just after statement
			 */
			gotoaddr = pop();
			coderef (gotoaddr)->c_args = pc - gotoaddr - 3;
		    }
		}
	;

while	:	Y_WHILE LP {
		    /* Save starting addr of while expression.
		     */
		    if (!errcnt) {
			push (pc);
			loopincr();
		    }
		} expr RP {
		    /* Save BIFF addr so branch can be filled in.
		     */
		    if (!errcnt)
			push (compile (BIFF, 0));
		} opnl xstmt {
		    XINT  biffaddr;

		    if (!errcnt) {
			/* Pop and save addr of BIFF instruction.	   */
			biffaddr = pop();
			/* Pop addr of expression and build a goto there.  */
			compile (GOTO, pop() - pc - 3);
			/* Now can set BIFF branch to just after statement.*/
			coderef (biffaddr)->c_args = pc - biffaddr - 3;
			loopdecr();
		    }
		}
	;

	/* The line of code:
	 *
	 *	for (e1, e2, e3) stmt
	 *
	 * is compiled into:
	 *
	 *		e1
	 *   loop1: 	if (!e2) goto end
	 *		goto loop3
	 *   loop2: 	e3
	 *		goto loop1
	 *   loop3: 	stmt
	 *		goto loop2
	 *     end:
	 *
	 * Note that e1 and e3 are assignments while e2 is an expression.
	 */

for	:	Y_FOR LP opnl xassign ';' opnl {
			if (!errcnt)
			    push(pc);				/* Loop1: */
		} 
		xexpr ';' opnl {
			if (!errcnt) {
			    if (for_expr)
				ppush (compile(BIFF, 0));	/* if (!e2) */

			    /* Add 3 to skip following GOTO. 
			     */
			    ppush (pc+3);			/* Loop2: */
			    ppush (compile(GOTO,0));		/* goto Loop3 */

			    /* Save current location as the destination
			     * for NEXT statements.
			     */
			    loopincr();
			}
		}
		xassign RP opnl {
			XINT  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-3); 	/* Goto loop1 */
			    stmtaddr = pop();
			    coderef(stmtaddr)->c_args = pc - stmtaddr - 3;
			}
		}
		stmt {
			XINT  stmtaddr;

			if (!errcnt) {
			    stmtaddr = pop();
			    compile (GOTO, stmtaddr-pc-3); /* goto loop2 */

			    if (for_expr) {
				stmtaddr = pop();
				coderef(stmtaddr)->c_args = pc - stmtaddr - 3;
			    }
			    loopdecr();
			}
		}
	;

/* The following allow skipping of fields in the FOR statement.
 */

xassign	:	assign
	|	/* empty */
	;

xexpr	:	expr {
			for_expr = YES;
		}
	|	/* empty */ {
			for_expr = NO;
		}
	;

	/* The compiled code for the switch statement
	 * consists of a SWITCH, followed by a series of
	 * CASE and DEFAULT blocks, followed by a jump table.
	 * The first operand in each CASE and DEFAULT block
	 * is a CASE or DEFAULT operand which is never
	 * executed, but is used to store the values which
	 * will enter this block.  Executable statements
	 * follow.
	 *
	 * The jump table consists of the addresses of the
	 * CASE and DEFAULT blocks.  The DEFAULT block
	 * comes first, and is 0 if no default has
	 * been given.  The list of addresses is terminated
	 * by a 0 address.
	 *
	 * The last statement of each CASE and DEFAULT
	 * statement is a branch back to a GOTO following
	 * the SWITCH.  This GOTO points to after the jumptable.
	 */

switch	:	Y_SWITCH opnl LP opnl expr opnl RP opnl
		{
			if (!errcnt) {
			    push (compile(SWITCH));

			    /* Compile GOTO which will branch past end of
			     * switch.  This is needed if there is no DEFAULT.
			     */
			    compile (GOTO, 0);
			}
		} xstmt {
			/* Set up jumptable and pop space on stack.
			 */
			if (!errcnt)
			    setswitch();
		}
	;

case	:	Y_CASE {
			if (!errcnt) {
			    ncaseval = 0;
			    if (!in_switch()) {
				eprintf ("Improper CASE statement.\n");
				EYYERROR;
			    }
			}
		} const_expr_list ':' opnl {
			XINT  pcase;

			if (!errcnt) {
			    pcase = compile (CASE, ncaseval);

			    /* Fill in argument list. 
			     */
			    caseset (&(coderef(pcase)->c_args), ncaseval);
			    push (pcase);
			}
		} xstmt {
			/* Branch to end of switch block 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		}
	;

default	:	Y_DEFAULT ':' opnl {
		      	/* Compile an operand to store the current PC.
			 */
			if (!errcnt) {
			    if (!in_switch()) {
				eprintf ("Improper DEFAULT statement.\n");
				EYYERROR;
			    }
			    push (compile(DEFAULT));
			}
		} xstmt {
		      	/* Branch past jump table. 
			 */
			if (!errcnt)
			    push (compile(GOTO, 0));
		}
	;	

next	:	Y_NEXT {
			/* All NEXT statements are backward references,
			 * so we simply store the addresses in an array.
			 */
			if (!errcnt) {
			    if (nestlevel)
				compile (GOTO, nextdest[nestlevel-1]-pc-3);
			    else {
				eprintf ( "NEXT outside of loop.\n");
				EYYERROR;
			    }
			}
		}
	;

break	:	Y_BREAK {
			/* Each BREAK is a forward reference.  For the
			 * first BREAK in each loop we compile a
			 * GOTO statement which will be the object of
			 * all BREAK statements within the loop.  When
			 * the loop is terminated the target of this
			 * GOTO will be set.
			 */
			int  dest;

			if (!errcnt) {
			    if (!nestlevel) {
				eprintf ("Break outside of loop.\n");
				EYYERROR;
			    } else if ((dest = brkdest[nestlevel-1]) != 0)
		    		compile (GOTO, dest-pc-3);
			    else {
				brkdest[nestlevel-1] = pc;
				compile (GOTO, 0);
			    }
			}
		}
	;

return	:	Y_RETURN {
			if (!errcnt)
			    compile (END);
		}
	|	Y_RETURN expr {
			/* Return values currently not implemented.
			 */
			eprintf ("Warning: return value ignored.\n");
			if (!errcnt)
			    compile (END);
		}
	;

	/* Require end to terminate with a new-line, because
	 * it should be at the end of the file.
	 */
end_stmt:	Y_END NL {
			bracelevel -= PBRACE;
			if (bracelevel < 0) {
			    eprintf ("Too few left braces.\n");
			    EYYERROR;
			} else if (bracelevel > 0) {
			    eprintf ("Too few right braces.\n");
			    EYYERROR;
			}
		}
	;

label_stmt:	Y_IDENT ':' opnl {
			/* Put symbol in table in dictionary and
			 * process indirect references if present.
			 */
			struct label *l;

			if (!errcnt) {
			    l = getlabel (stkop($1));

			    if (l == NULL) {
				l = setlabel (stkop($1));
				l->l_loc = pc;
			    } else if (l->l_defined) {
			        eprintf ("Identical labels.\n");
				EYYERROR;
			    } else {
				/* Get this GOTO out of the
				 * indirect list so we can use
				 * the argument as the destination
				 */
				XINT  gotopc;
				gotopc = l->l_loc;
				unsetigoto (gotopc);

				/* Fix the indirect reference. 
				 */
				coderef(gotopc)->c_args = pc - gotopc - 3;
			    }
			    (l->l_defined)++;
			}
		}
		xstmt
	;

goto	:	Y_GOTO Y_IDENT {
			/* Get the address corresponding to the label.
			 */
			struct label *l;

			if (!errcnt) {
			    l = getlabel (stkop($2));

			    if (l != NULL)
				compile (GOTO, l->l_loc - pc - 3);
			    else {
				/* Ready for indirect GOTO 
				 */
				l = setlabel (stkop($2));
				l->l_loc = pc;
				setigoto (compile(GOTO, 0));
				l->l_defined = 0;
			    }
			}
		}
	;

nullstmt:	';'
	|	';' NL
	;

/* xstmt is defined so that to handle implicit do loops created by
 *  open array references e.g. a[*,3]=a[3,*].
 */

xstmt	:	/* empty */ { 
			/* Save pc before compiling statement for loop back
			 */
			stmt_pc = pc;
			n_oarr = 0;
			i_oarr = 0;
			ifseen = NULL;
		}
		stmt {
		      	/* If there was an open reference compile the
			 * loop increment and goback.
			 */
			XINT push_pc;

			if (!errcnt) {
			    if (n_oarr) {
				compile (INDXINCR, stmt_pc-pc-4, 2*n_oarr+1);

				/* We are going to store initialization
				 * info for the implicit loop here.
				 * It is loopincr's responsibility to 
				 * branch around it.  This data is what
				 * should be pointed to by the special
				 * PUSHINDEX compiled at the first open
				 * array reference.
				 */
				push_pc = pop();  /* Location of PUSHINDEX */
				coderef(push_pc)->c_args = pc - push_pc - 3;

				stack[pc++] = n_oarr;
				for (i_oarr=0; i_oarr<n_oarr; i_oarr++) {
				    stack[pc++] = oarr_beg[i_oarr];
				    stack[pc++] = oarr_end[i_oarr];
				}

				/* Clear n_oarr.  This must be done here
				 * because we may have the end of a compound
				 * statement following on the heels of the
				 * end of the simple statement with the
				 * implicit loop.
				 */
				n_oarr = 0;
				i_oarr = 0;
			    }
			}
		}
	|	var_decl_stmt
	|	error NL {
		    /* This should get most errors in executable statements
		     * or in the local variable declarations in a script.
		     */
		    yyerrok;

		    /* Get rid of any fake braces.
		     */
		    bracelevel -= tbrace;

		    /* Discard everything and compile a null statement.
		     */
		    if (!errcnt) {
			do_params = YES;
			pc = currentask->t_bascode;
			if (parse_state != PARSE_PARAMS)
			    compile (END);

			topd = currentask->t_topd;
			topcs = currentask->t_topcs;

			/* Unlink any added parms.  Resetting of topd will
			 * already have reclaimed space.
			 */
			if (last_parm) {
			    last_parm->p_np = NULL;
			    currentask->t_pfp->pf_lastpp = last_parm;
			    last_parm = NULL;
			}
 		    }

		    /* Tell user about the syntax error, printing the 
		     * offending line and position if possible.
		     */
		    if (currentask->t_flags & T_SCRIPT) 
			eprintf ("** Syntax error, line %d\n",
			    currentask->t_scriptln);
		    else
			eprintf ("** Syntax error\n");
		    p_position();

		    if (!(currentask->t_flags & T_SCRIPT)) {
			/* If interactive, we're finished if not within braces.
			 */
			if (!bracelevel)
			    YYACCEPT;
		    }

		    /* Note that we do not call cl_error() here to abort, but
		     * continue on parsing the script for more syntax errors.
		     */
		    if (++errcnt > MAX_ERR)
			cl_error (E_UERR, "Too many syntax errors.");
		}
	;

const_expr_list	:	const_expr
		|	const_expr DELIM const_expr_list
		;

const_expr	:	Y_CONSTANT {
				if (!errcnt) {
				    push(stkop($1)) ; 
				    ncaseval++;
				}
			}
		;

	/* Use opnl when blank lines are permitted,
	 * or where a statement may be broken into more
	 * than one line.  The lexical analyzer (actually
	 * get_command in history.c) ensures that all blank
	 * lines are deleted.  So we don't have to use
	 * a recursive definition here.
	 */

opnl	:	/* empty */
	|	NL
	;

ref	:	param {
			int  dim, d, i1, i2, mode;

			/*  In command arguments, when not in parentheses
			 *  we just pass the param as a string constant.
			 */
			if (!errcnt) {
			    lastref = NO;
			    if (!inarglist || parenlevel) {
				i_oarr = 0;
				index_cnt = 0;

				strncpy (curr_param, stkop($1)->o_val.v_s, 
					SZ_FNAME);

				/* If a '.' is found in the name we have a 
				 * reference to an external task, or to a 
				 * specific field.  In these cases we don't 
				 * want implicit looping.
				 */
				if (index (curr_param, '.') == NULL) {
				    if ((dim = get_dim (curr_param)) > 0) {
					lastref = YES;
			        	for (d = 0; d < dim; d++) {
					    getlimits (curr_param, d, &i1, &i2);
					    mode = make_imloop (i1, i2);
					    if (mode)
						compile (PUSHINDEX, -1);
					    else
						push (compile(PUSHINDEX, 0));	
					}
			    		n_oarr = dim;
				    }
				}
			    }
			}
		}
	|	param {
		    if (!errcnt) {
			strncpy (curr_param, stkop($1)->o_val.v_s, SZ_FNAME);
			index_cnt = 0;
		    }
		}
		'[' index_list ']' 
		{
		    if (i_oarr > 0  &&  n_oarr == 0)
			n_oarr = i_oarr;
		    i_oarr = 0;
		    lastref = YES;
		}
	;

index_list:	index {
			index_cnt = 1;
		}
	|	index {
			index_cnt++;
		}
		DELIM index_list
	;

index	:	expr1 {
			if (!errcnt)
			    compile (PUSHINDEX, 0);
		}
	|	ref  /* This isn't included in expr1 */
		{ 
			if (!errcnt) {
			    compile (PUSHPARAM, stkop($1)->o_val.v_s);
			    compile (PUSHINDEX, 0);
			}
		}
	|	'*' {
			int  i1, i2, mode;

			if (!errcnt) {
			    if (index(curr_param, '.') != NULL) {
				eprintf (exlimits);
				EYYERROR;
			    }
			    if (getlimits (curr_param, index_cnt, &i1, &i2) 
				== ERR) {
				eprintf ("Implicit index error for %s.\n",
					curr_param);
				EYYERROR;
			    }
			    mode = make_imloop (i1, i2);
			    if (mode)
				compile (PUSHINDEX, mode);
			    else
				push (compile (PUSHINDEX, mode));
			}
		}
	|	Y_CONSTANT {
			/*  There is an ambiguity in the grammar between
			 *  sexagesimal constants, and array range references.
			 *  Since the sexagesimal constants are recognized
			 *  in the lexical analyzer we can't just change the
			 *  grammar.  The kludge around this is to have
			 *  makeop set a flag telling us that the last
			 *  constant it compiled COULD have been an index
			 *  range.  We check the flag here and if it is
			 *  set we convert back and compile an implicit loop
			 *  otherwise we just push the constant.
			 */
			int  i1, i2, mode;

			if (!errcnt) {
			    if (maybeindex) {
				sexa_to_index (stkop($1)->o_val.v_r, &i1, &i2);
				mode = make_imloop (i1, i2);
				if (mode)
				    compile (PUSHINDEX, mode);
				else
				    push (compile (PUSHINDEX, mode));
			    } else {
				compile (PUSHCONST, stkop($1));
				compile (PUSHINDEX, 0);
			    }
			}
		}
	;

/* these are just to make the grammar a bit easier to read.
 * can yank them out to shrink parser a bit...
 */

intrins	:	Y_IDENT {
		    $$ = $1;
		}
	;

param	:	Y_IDENT {
		    $$ = $1;
		}
	;

tasknam	:	Y_IDENT {
		    $$ = $1;
		}
	;

EOST	:	NL
	|	';' {
		    /* If statements are delimited by ';'s, do not execute
		     * until next newline EOST is received.
		     */
		    sawnl = 0;
		}
	;
	
DELIM	:	','
	;

BARG	:	/* empty */
	|	LP
	;

EARG	:	/* empty */
	|	RP
	;

/* These eliminate several interior actions.
 */

LP	:	'('	{ parenlevel++; }
	;

RP	:	')'	{ --parenlevel; }
	;

NL	:	Y_NEWLINE  { sawnl = 1; }
	;

%%

#include "lexyy.c"
#include "lexicon.c"
