/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "mem.h"
#include "operand.h"
#include "param.h"
#include "grammar.h"
#include "task.h"
#include "opcodes.h"
#include "errs.h"
#include "construct.h"
#include "proto.h"

/*
 * OPCODES -- This is the instruction set that forms the internal language of
 *   the CL.  The runtime interpreter (in runtime.c) executes these functions
 *   as they are discovered in the compiled code.  The code is generated
 *   incrementally as the grammar is recognized in grammar.y by calls to
 *   compile().  The argument, argp, if needed, is the true addr of the start
 *   of the instruction arguments.
 * If anything goes wrong, error() is called but DOES NOT RETURN; see errs.c.
 *
 * Comments indicate stack usage. expected operands are before the `.' 
 *   (rightmost being on "top" of stack), resulting operands are after.
 *
 * At the end of this file is the opcode jumptable. The order of the entries
 * must agree with the definitions of the opcode constants in operand.h.
 * see runtime.c.
 */

extern	int cldebug;
extern	char *nullstr;
int	binpipe;			/* last pipe binary or text ? */
char	*comdstr();
extern	struct param *ppfind();		/* search task psets for param */
extern	int currentline;

void
o_undefined (void)
{
	cl_error (E_IERR, e_uopcode, 0);
}

/* <new value for named argument> .
 * Assign the top operand to the named parameter.  Also, make the type of the
 * fake parameter the same as the type of the operand.
 */
void
o_absargset (memel *argp)
{
	char	*argname = (char *) argp;
	char	*pk, *t, *p, *f;
	struct	pfile *pfp;
	struct	param *pp;

	pfp = newtask->t_pfp;
	if (pfp->pf_flags & PF_FAKE) {
	    /* use full argname and always assign to value field.
	     */
	    struct operand o;
	    int	string_len=0;
	    o = popop();
	    if ((o.o_type & OT_BASIC) == OT_STRING)
		string_len = strlen (o.o_val.v_s);
	    pp = newfakeparam (pfp, argname, 0, o.o_type, string_len);
	    pushop (&o);
	    f = argname;
	    *f = FN_NULL;

	} else {
	    breakout (argname, &pk, &t, &p, &f);
	    if (*pk)
		cl_error (E_UERR, e_simplep, p);
	    pp = ppfind (pfp, t, p, 0, NO);
	    if (pp == NULL)
		cl_error (E_UERR, e_pnonexist, p);
	    if ((XINT)pp == ERR)
		cl_error (E_UERR, e_pambig, p, pfp->pf_ltp->lt_lname);
	}

	paramset (pp, *f);
	if (pp->p_type & PT_PSET)
	    psetreload (pfp, pp);
	pp->p_flags |= P_CLSET;
}

/* <op1> <op2> . <op2 + op1>
 */
void
o_add (void)
{
	binop (OP_ADD);
}

/* <increment to be added to named parameter> .
 */
void
o_addassign (memel *argp)
{
	/* order of operands will be incorrect.
	 * strictly speaking, only strings are not commutative but we need
	 * to pop both operands anyway to check.
	 */
	char *pname = (char *) argp;
	char *pk, *t, *p, *f;
	struct param *pp;
	struct operand o1, o2;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);
	validparamget (pp, *f);
	o1 = popop();
	o2 = popop();

	if ((o2.o_type & OT_BASIC) == OT_STRING) {
	    /* copy o2 onto dictionary to avoid overwriting it on stack
	     * when o1 is pushed. we can get by with not worrying about o1
	     * as long as whatever code copies the string works when the
	     * strings overlap.
	     */
	    XINT oldtopd = topd;
	    char *s2 = memneed (btoi (strlen (o2.o_val.v_s) + 1));
	    strcpy (s2, o2.o_val.v_s);
	    o2.o_val.v_s = s2;
	    pushop (&o1);
	    pushop (&o2);
	    topd = oldtopd;		/* discard temp string area	*/

	} else {
	    pushop (&o1);
	    pushop (&o2);
	}

	binop (OP_ADD);
	paramset (pp, *f);
	pp->p_flags |= P_SET;
}

/* <name of file to be appended> .
 * includes stdout as well as stderr.
 */
void
o_allappend (void)
{
	struct	operand o;
	char	*fname, *mode;

	opcast (OT_STRING);
	o = popop();
	fname = o.o_val.v_s;

	if (newtask->t_flags & T_FOREIGN &&
	    newtask->t_stdout == stdout && newtask->t_stderr == stderr) {

	    /* If foreign task and i/o has not already been redirected by
	     * the parent, let ZOSCMD open the spool file.
	     */
	    newtask->ft_out = newtask->ft_err = comdstr (fname);
	    newtask->t_flags |= T_APPEND;

	} else {
	    mode = (newtask->t_flags & T_STDOUTB) ? "ab" : "a";

	    if ((newtask->t_stdout = fopen (fname, mode)) == NULL)
		cl_error (E_UERR, e_appopen, fname);

	    newtask->t_stderr = newtask->t_stdout;
	    newtask->t_flags |= (T_MYOUT|T_MYERR);
	}
}


/* <name of file to be used as stderr> .
 * redirect everything, including the stderr channel.
 */
void
o_allredir (void)
{
	struct	operand o;
	char	*fname, *mode;

	opcast (OT_STRING);
	o = popop();
	fname = (o.o_val.v_s);

	if (newtask->t_flags & T_FOREIGN &&
	    newtask->t_stdout == stdout && newtask->t_stderr == stderr) {

	    /* If foreign task and i/o has not already been redirected by
	     * the parent, let ZOSCMD open the spool file.
	     */
	    newtask->ft_out = newtask->ft_err = comdstr (fname);

	} else {
	    mode = (newtask->t_flags & T_STDOUTB) ? "wb" : "w";

	    if ((newtask->t_stderr = fopen (fname, mode)) == NULL)
		cl_error (E_UERR, e_wopen, fname);

	    newtask->t_stdout = newtask->t_stderr;
	    newtask->t_flags |= (T_MYOUT|T_MYERR);
	}
}


/* <op1> <op2> . <op1 && op2>
 */
void
o_and (void)
{
	binexp (OP_AND);
}

/* <name of file to be appended> .
 */
void
o_append (void)
{
	struct	operand o;
	char	*fname, *mode;

	opcast (OT_STRING);
	o = popop();
	fname = (o.o_val.v_s);

	if (newtask->t_flags & T_FOREIGN && newtask->t_stdout == stdout) {
	    /* If foreign task let ZOSCMD open the spool file.
	     */
	    newtask->ft_out = comdstr (fname);
	    newtask->t_flags |= T_APPEND;
	} else {
	    mode = (newtask->t_flags & T_STDOUTB) ? "ab" : "a";

	    if ((newtask->t_stdout = fopen (fname, mode)) == NULL)
		cl_error (E_UERR, e_appopen, fname);

	    newtask->t_flags |= T_MYOUT;
	}
}


/* <new value for named parameter> .
 */
void
o_assign (memel *argp)
{
	char *pname = (char *) argp;
	char *pk, *t, *p, *f;
	struct param *pp;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);
	paramset (pp, *f);
	pp->p_flags |= P_SET;
}

/* <truth value> .
 * branch if false (or INDEF).
 */
void
o_biff (memel *argp)
{
	extern XINT pc;
	struct operand o;

	opcast (OT_BOOL);
	o = popop();
	if (!o.o_val.v_i || opindef (&o))
	    pc += (int)*argp;
}

/* .
 * arrange to start a new task. set newtask.
 * see runtime.c
 */
void
o_call (memel *argp)
{
	callnewtask ((char *) argp);
}

/* <op> . <- op>
 */
void
o_chsign (void)
{
	unop (OP_MINUS);
}

/* <op> // <op>
 * string concatenation
 */
void
o_concat (void)
{
	binop (OP_CONCAT);
}

/* <op1> <op2> . <op1 / op2>
 */
void
o_div (void)
{
	binop (OP_DIV);
}

void
o_doend (void)
{
}

/* <value to be divided into named parameter> .
 */
void
o_divassign (memel *argp)
{
	char	*pname = (char *) argp;
	char	*pk, *t, *p, *f;
	struct	param *pp;
	struct	operand o1, o2;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);

	validparamget (pp, *f);		/* get param value on stack	*/
	o1 = popop();			/* swap operands		*/
	o2 = popop();
	pushop (&o1);
	pushop (&o2);
	binop (OP_DIV);			/* perform the division		*/
	paramset (pp, *f);
	pp->p_flags |= P_SET;
}

/* <value to be concatenated onto named parameter> .
 */
void
o_catassign (memel *argp)
{
	char	*pname = (char *) argp;
	char	*pk, *t, *p, *f;
	char	s1[1024+1];
	struct	operand o1, o2;
	struct	param *pp;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);
	paramget (pp, *f);

	/* If param value is undefined merely assign into it, otherwise
	 * concatenate operand to current value.
	 */
	o1 = popop();
	if (!opundef(&o1)) {
	    /* Must copy string value off of operand stack or the next
	     * pushop below will reuse the space!
	     */
	    o2 = popop();
	    strncpy (s1, o2.o_val.v_s, 1024);
	    s1[1024] = EOS;
	    o2.o_val.v_s = s1;

	    pushop (&o1);
	    pushop (&o2);
	    binop (OP_CONCAT);
	}

	paramset (pp, *f);
	pp->p_flags |= P_SET;
}

/* <op1> <op2> . <op1 == op2>
 */
void
o_eq (void)
{
	binexp (OP_EQ);
}

/* run the newtask. see exec.c.
 */
void
o_exec (void)
{
	execnewtask ();
}

/* <op1> <op2> . <op1 > op2>
 */
void
o_ge (void)
{
	binexp (OP_GE);
}

/* unconditional goto.
 * *argp is the SIGNED increment to be added to pc.
 */
void
o_dogoto (memel *argp)
{
	extern XINT pc;
	pc += (int)*argp;
	if (pc >= STACKSIZ)
	    cl_error (E_IERR, "pc set wildly to %d during goto", pc);
}

/* <op1> <op2> . <op1 > op2>
 */
void
o_gt (void)
{
	binexp (OP_GT);
}

/* <string operand> .
 * if argument to which we are assigning is a simple string or filename (or
 *   list, since assigning to a list sets a filename too), set it to o_val.v_s,
 * else use o_val.v_s as the name of a parameter and use its value as the name
 *   of the variable, that is, do an indirect through o_val.v_s.
 * compiled when the parser sees a simple identifier, not in an expression.
 *   this avoids quotes around simple strings and filenames.
 * if the parameter is to be fake, make it type string and do not do the
 *   indirection.
 */
void
o_indirabsset (memel *argp)
{
	char	*argname = (char *) argp;
	char	*pk, *t, *p, *f;
	struct	pfile *pfp;
	struct	param *pp;
	int	type, string_len;

	pfp = newtask->t_pfp;
	if (pfp->pf_flags & PF_FAKE) {
	    struct	operand o;
	    o = popop();
	    string_len = strlen (o.o_val.v_s);
	    pp = newfakeparam (pfp, argname, 0, OT_STRING, string_len);
	    f = argname;
	    *f = FN_NULL;
	    pushop (&o);

	} else {
	    breakout (argname, &pk, &t, &p, &f);
	    if (*pk)
		cl_error (E_UERR, e_simplep, p);
	    pp = ppfind (pfp, t, p, 0, NO);
	    if (pp == NULL)
		cl_error (E_UERR, e_pnonexist, p);
	    if ((XINT)pp == ERR)
		cl_error (E_UERR, e_pambig, p, pfp->pf_ltp->lt_lname);
	}

	/* lone identifiers are treated as strings, rather than variables,
	 * if the corresponding parameter is a simple string, filename or list.
	 * note that fakeparams are made as strings.
	 */
	type = pp->p_type;
	if (type & (PT_FILNAM|PT_LIST|PT_PSET)) {
	    struct operand o;
	    o = popop();
	    pushop (&o);
	} else if ((type & OT_BASIC) != OT_STRING ||
	    type & (PT_STRUCT|PT_IMCUR|PT_GCUR|PT_UKEY)) {

	    opindir(); /* replace top op with value of o_val.v_s */
	}

	paramset (pp, *f);
	if (pp->p_type & PT_PSET)
	    psetreload (pfp, pp);
	pp->p_flags |= P_CLSET;
}

/* <string operand> .
 * if argument to which we are assigning is a simple string or filename (or
 *   list, since assigning to a list sets a filename too), set it to o_val.v_s,
 * else use o_val.v_s as the name of a parameter and use its value as the name
 *   of the variable, that is, do an indirect through o_val.v_s.
 * compiled when the parser sees a simple identifier, not in an expression.
 *   this avoids quotes around simple strings and filenames.
 */
void
o_indirposset (memel *argp)
{
	int pos = (int) *argp;
	struct pfile *pfp;
	struct param *pp;
	int type, string_len;

	pfp = newtask->t_pfp;
	if (pfp->pf_flags & PF_FAKE) {
	    struct	operand o;
	    o = popop();
	    string_len = strlen (o.o_val.v_s);
	    pp = newfakeparam (pfp, (char *) NULL, pos, OT_STRING, string_len);
	    pushop (&o);
	} else {
	    pp = paramfind (pfp, (char *) NULL, pos, NO);
	    if (pp == NULL)
		cl_error (E_UERR, e_posargs, newtask->t_ltp->lt_lname);
	}

	/* lone identifiers are treated as strings, rather than variables,
	 * if the corresponding parameter is a simple string, filename or list.
	 * note that fakeparams are made as strings.
	 */
	type = pp->p_type;
	if (type & (PT_FILNAM|PT_LIST|PT_PSET)) {
	    struct operand o;
	    o = popop();
	    pushop (&o);
	} else if ((type & OT_BASIC) != OT_STRING ||
	    type & (PT_STRUCT|PT_IMCUR|PT_GCUR|PT_UKEY)) {

	    opindir(); /* replace top op with value of o_val.v_s */
	}

	paramset (pp, FN_NULL);
	pfp->pf_n++;
	pp->p_flags |= P_CLSET;
}

/* Increment the loop counters for an implicit loop.
 */
void
o_indxincr (memel *argp)
{
	int	i;
	i = 0;
	while (i < n_oarr) {
	    if (oarr_curr[i] < oarr_end[i] ) {
		oarr_curr[i] ++;
		i_oarr = 0;
		pc += argp[0];	/* Branch to beginning of statement. */
		return;
	    } else  {
		oarr_curr[i] = oarr_beg[i];
		i++;
	    }
	}

	/* Finished loop, branch around stored data. */
	pc += argp[1];

	/* Clear flag for next implicit loop. */
	imloopset = 0;
}


/* .
 * given the name of a parameter, print it on t_out, the task pipe channel.
 */
void
o_inspect (memel *argp)
{
	char *pname = (char *) argp;
	char *pk, *t, *p, *f;
	struct param *pp;
	struct operand o;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);

	if (*f == FN_NULL && (pp->p_type & PT_LIST)) {
	    /* Hitting EOF from a list is ok during an inspect stmt so
	     * avoid using paramget() with its EOF error.
	     * readlist() may set P_LEOF.
	     */
	    o = readlist (pp);
	    if ((pp->p_flags & P_LEOF) || inrange (pp, &o))
		pushop (&o);
	    else
		query (pp);
	} else
	    validparamget (pp, *f);

	o = popop();

	if (cldebug && (o.o_type & OT_BASIC) == OT_STRING)
	    eprintf ("Inspect--%s\n", o.o_val.v_s);

	prop (&o);
	tprintf ("\n");
}


/* [<op1> <op2> ... <opn>] <nops> . <result>
 * intrinsic functions, like sin, cos, mod, etc.
 * argp is the name of the function to run and the top operand (we guarantee
 * at least one) is the number of remaining operands to be used.
 * all the defines are in operand.h. the function names and running them is
 * done by intrfunc() in gram.c.
 */
void
o_intrinsic (memel *argp)
{
	char *funcname = (char *) argp;
	struct operand o;
	int nargs;

	o = popop();
	nargs = o.o_val.v_i;

	intrfunc (funcname, nargs);
}

/* <op1> <op2> . <op1 <= op2>
 */
void
o_le (void)
{
	binexp (OP_LE);
}

/* <op1> <op2> . <op1 < op2>
 */
void
o_lt (void)
{
	binexp (OP_LT);
}

/* <op1> <op2> . <op2 * op1>
 */
void
o_mul (void)
{
	binop (OP_MUL);
}

/* <value to be multiplied into named parameter> .
 */
void
o_mulassign (memel *argp)
{
	char	*pname = (char *) argp;
	char	*pk, *t, *p, *f;
	struct	param *pp;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);

	validparamget (pp, *f);
	binop (OP_MUL);
	paramset (pp, *f);
	pp->p_flags |= P_SET;
}

/* <op1> <op2> . <op1 != op2>
 */
void
o_ne (void)
{
	binexp (OP_NE);
}

/* <op> . <!op>
 */
void
o_not (void)
{
	unexp (OP_NOT);
}

/* <op1> <op2> . <op1 || op2>
 */
void
o_or (void)
{
	binexp (OP_OR);
}


/* OSESC -- Send a command to the host system.  Command is a string pointed
 * to by argp.  Try to run it so its stdout and stderr will go to out t_stdout
 * and t_stderr of the current task.
 */
void
o_osesc (memel *argp)
{
	char *command = (char *)argp;

	clsystem (command, currentask->t_stdout, currentask->t_stderr);
}


/* <new value for argument at command position *argp> .
 */
void
o_posargset (memel *argp)
{
	int	pos = (int) *argp;
	struct	pfile *pfp;
	struct	param *pp;
	struct	operand o;
	int	string_len=0;

	pfp = newtask->t_pfp;

	if (pos < 0) {
	    /* Lone comma in arg list, merely bump nargs counter */
	    pfp->pf_n++;
	    return;
	}

	if (pfp->pf_flags & PF_FAKE) {
	    o = popop();
	    if ((o.o_type & OT_BASIC) == OT_STRING)
		string_len = strlen (o.o_val.v_s);
	    pp = newfakeparam (pfp, (char *) NULL, pos, o.o_type, string_len);
	    pushop (&o);
	} else {
	    pp = paramfind (pfp, (char *) NULL, pos, NO);
	    if (pp == NULL)
		cl_error (E_UERR, e_posargs, newtask->t_ltp->lt_lname);
	}

	paramset (pp, FN_NULL);
	pfp->pf_n++;
	pp->p_flags |= P_CLSET;
}


/* <op1> <op2> . <op1 ** op2>
 */
void
o_dopow (void)
{
	binop (OP_POW);
}


/* <exprn-1> ... <expr1> <dest> <n> .
 * Do the print task.  First op on stack is number of operands to follow.
 * Next one is the name of the destination parameter, rest are values to
 * be printed.
 */
void
o_doprint (void)
{
	/* This is not used -- print is imp. as a builtin task.
	struct operand o;

	o = popop();
	print (o.o_val.v_i - 1);
	 */
}

/* <value to be printed> .
 * used to print an operand on the stack. not to be confused with doprint.
 */
void
o_immed (void)
{
	struct operand o;

	o = popop();
	prop (&o);
	tprintf ("\n");
}

/* . <new constant>
 * The "illegal constant" business comes from the possibility of syntactically
 * correct but valuely wrong sexagesimal constants, such as 1:222:1.
 * We don't want to abort in sexa() because it may be used to digest a query
 * response and producing a quiet undefined op there is correct.
 */
void
o_pushconst (memel *argp)
{
	/* argument is pointer to an operand */
	struct operand *op;

	op = (struct operand *) argp;
	if (opundef (op))
	    cl_error (E_UERR, "illegal constant");
	pushop (op);
}

/* Push an index value onto the control stack for later use
 * when the parameter is accessed.
 */
void
o_pushindex (int *mode)
{
	struct operand op;

	 if (cldebug)
	    printf ("PUSHINDEX: mode=%d loopset=%d\n", *mode, imloopset);

	if (*mode == 0) {	/* Normal array index reference. */
	    opcast(OT_INT);
	    op = popop();
	    push (op.o_val.v_i);
	} else if (*mode == -1  ||  imloopset) {
	    /* Array reference in implicit loop. */
	    push (oarr_curr[i_oarr]);
	    i_oarr++;
	    if (i_oarr >= n_oarr)
		i_oarr = 0;
	} else {
	    /* This is the first array reference in an implicit loop.
	     * It must initialize the loop parameters.  The argument
	     * is an offset to the initialization info.
	     */
	    int stk;

	    stk = pc + *mode;

	    n_oarr = stack[stk++];
	    for (i_oarr=0; i_oarr<n_oarr; i_oarr++) {
		oarr_beg[i_oarr] = stack[stk++];
		oarr_curr[i_oarr] = oarr_beg[i_oarr];
		oarr_end[i_oarr] = stack[stk++];
	    }
	    /* Set flag so that we don't do this again. */
	    imloopset++;

	    /* And we still have to push a value. */
	    push (oarr_curr[0]);
	    i_oarr = 1;
	    if (i_oarr >= n_oarr)
		i_oarr = 0;
	}

	/* Increment counter of number of indexes pushed.
	 */
	n_indexes++;
}

/* . <value of parameter>
 */
void
o_pushparam (memel *argp)
{
	char *pname = (char *) argp;
	char *pk, *t, *p, *f;
	struct param *pp;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);
	validparamget (pp, *f);
}


/* <name of file to be used as stdout> .
 */
void
o_redir (void)
{
	struct	operand o;
	char	*fname, *mode;

	opcast (OT_STRING);
	o = popop();
	fname = (o.o_val.v_s);

	if (newtask->t_flags & T_FOREIGN && newtask->t_stdout == stdout) {
	    /* If foreign task let ZOSCMD open the spool file.
	     */
	    newtask->ft_out = comdstr (fname);

	} else if (strcmp (fname, IPCOUT) == 0) {
	    /* Redirect the task stdout via IPC to a subprocess. */
	    newtask->t_stdout = newtask->t_out;
	    newtask->t_flags |= T_IPCIO;

	} else {
	    mode = (newtask->t_flags & T_STDOUTB) ? "wb" : "w";

	    if ((newtask->t_stdout = fopen (fname, mode)) == NULL)
		cl_error (E_UERR, e_wopen, fname);

	    newtask->t_flags |= T_MYOUT;
	}
}


/* <name of file to be used as stdin> .
 */
void
o_redirin (void)
{
	struct	operand o;
	char	*fname, *mode;

	opcast (OT_STRING);
	o = popop();
	fname = (o.o_val.v_s);

	if (newtask->t_flags & T_FOREIGN && newtask->t_stdin == stdin) {
	    /* If foreign task let ZOSCMD open the command file.
	     */
	    newtask->ft_in = comdstr (fname);
	} else {
	    mode = (newtask->t_flags & T_STDINB) ? "rb" : "r";

	    if ((newtask->t_stdin = fopen (fname, mode)) == NULL)
		cl_error (E_UERR, e_ropen, fname);

	    newtask->t_flags |= T_MYIN;
	}
}


/* GSREDIR -- Graphics stream redirection.
 * <filename> .
 */
void
o_gsredir (memel *argp)
{
	register char	*ip;
	register FILE	*fp;
	char	*streams = (char *)argp;
	struct	operand o;
	char	*fname;
	int	count;

	/* Get the filename.
	 */
	opcast (OT_STRING);
	o = popop();
	fname = o.o_val.v_s;

	/* Scan the redir token to determine the file access mode, e.g., if
	 * ">G", create a new file, and if ">>G", append to a file.
	 */
	for (count=0, ip=streams;  *ip;  ip++)
	    if (*ip == '>')
		count++;

	if ((fp = fopen (fname, count > 1 ? "ab" : "wb")) == NULL)
	    cl_error (E_UERR, e_wopen, fname);

	/* The first string operand on the stack is some combination of the
	 * characters GIP, listing the streams (stdgraph, stdimage, stdplot)
	 * to be redirected to the named file.  The lexical analyzer guarantees
	 * that we will not be called unless the string consists of some
	 * combination of the characters >GIP, hence error checking for other
	 * char, no chars, etc., is not needed.
	 */
	for (ip=streams;  *ip;  ip++)
	    if (*ip == 'G') {
		newtask->t_flags |= T_MYSTDGRAPH;
		newtask->t_stdgraph = fp;
	    } else if (*ip == 'I') {
		newtask->t_flags |= T_MYSTDIMAGE;
		newtask->t_stdimage = fp;
	    } else if (*ip == 'P') {
		newtask->t_flags |= T_MYSTDPLOT;
		newtask->t_stdplot = fp;
	    }
}

void
o_doaddpipe (memel *argp)
{
	XINT	getpipe_pc = *argp;
	char	*x1, *pk, *t, *x2;	
	char	*ltname;
	struct	operand	o;
	struct	ltask *ltp;
	char	*addpipe();

	/* ADDPIPE is called immediately before REDIR and before EXEC so we
	 * do not have to worry about storing the pipefile name in the dict.
	 * Our argument is the PC of the GETPIPE instruction, the args field
	 * of which is the taskname of the second task in the pipe.  If either
	 * the new task (first task in the pipe) or the second task is a
	 * FOREIGN task, the pipe must be created as a text file.
	 */
	ltname = (char *)&(coderef(getpipe_pc)->c_args);
	if (*ltname == '$')
	    ltname++;
	breakout (ltname, &x1, &pk, &t, &x2);
	ltp = cmdsrch (pk, t);

	binpipe = ((ltp == NULL || !(ltp->lt_flags & LT_FOREIGN)) &&
	    !(newtask->t_flags & T_FOREIGN));

	if (binpipe)
	    newtask->t_flags |= T_STDOUTB;

	o.o_type = OT_STRING;
	o.o_val.v_s = comdstr (addpipe());
	pushop (&o);
}

void
o_dogetpipe (
    memel *argp			/* name of ltask (not used) */
)
{
	struct	operand o;
	char	*getpipe(), *comdstr();

	/* GETPIPE is called immediately before REDIRIN and before EXEC so we
	 * do not have to worry about storing the pipefile name in the dict.
	 * The flag binpipe is set by the last ADDPIPE if the pipe is a binary
	 * file.
	 */
	if (binpipe)
	    newtask->t_flags |= T_STDINB;

	o.o_type = OT_STRING;
	o.o_val.v_s = comdstr (getpipe());
	pushop (&o);
}


void
o_rmpipes (memel *argp)
{
	delpipes ((int)*argp);
}


void
o_doreturn (void)
{
	eprintf ("return not implemented\n");
}

/* <paramn> ... <param1> <source> <n> .
 * do the scan function. first op on stack is number of string ops to
 * follow, rest are names of destination params.  SCAN scans the standard
 * input.
 */
void
o_doscan (void)
{
	struct operand o;

	o = popop();
	cl_scan (o.o_val.v_i - 1, "stdin");
}

void
o_doscanf (void)
{
	struct operand o;
	struct operand o_sv[64];
	char	format[SZ_LINE];
	int	nargs, i;

	/* Get number of arguments. */
	o = popop();
	nargs = o.o_val.v_i;

	/* Get scan format.  Unfortunately the way the parser works this
	 * is the last operand on the stack.  We need to pop and save the
	 * first nargs-1 operands and restore them when done.
	 */
	for (i=0;  i < nargs-1;  i++)
	    o_sv[i] = popop();

	o = popop();
	if ((o.o_type & OT_BASIC) != OT_STRING)
	    cl_error (E_UERR, "scanf: bad format string\n");
	strcpy (format, o.o_val.v_s);

	for (--i;  i >= 0;  i--)
	    pushop (&o_sv[i]);

	/* Do the scan. */
	cl_scanf (format, nargs-2, "stdin");
}

/* <paramn> ... <param1> <source> <n> .
 * Do the fscan function.  First op on stack is number of string ops to
 * follow.  Next one is the name of the source parameter, rest are names of
 * destination params.
 */
void
o_dofscan (void)
{
	struct operand o;

	o = popop();
	cl_scan (o.o_val.v_i - 1, "");
}

void
o_dofscanf (void)
{
	struct operand o, o_sv[64];
	char	format[SZ_LINE];
	char	pname[SZ_FNAME];
	int	nargs, i;

	/* Get number of arguments. */
	o = popop();
	nargs = o.o_val.v_i;

	/* Get scan format and input parameter name.  The arguments on the
	 * stack are pushed in the order input param name, format string,
	 * and then the output arguments.
	 */

	/* Get output arguments. */
	for (i=0;  i < nargs-2;  i++)
	    o_sv[i] = popop();

	/* Get format string. */
	o = popop();
	if ((o.o_type & OT_BASIC) != OT_STRING)
	    cl_error (E_UERR, "fscanf: bad format string\n");
	strcpy (format, o.o_val.v_s);

	/* Get parameter name. */
	o = popop();
	if ((o.o_type & OT_BASIC) != OT_STRING)
	    cl_error (E_UERR, "fscanf: bad input parameter specification\n");
	strcpy (pname, o.o_val.v_s);

	/* Restore the output argument operands. */
	for (--i;  i >= 0;  i--)
	    pushop (&o_sv[i]);

	/* Restore the input parameter name operand. */
	o.o_type = OT_STRING;
	o.o_val.v_s = pname;
	pushop (&o);

	/* Do the scan. */
	cl_scanf (format, nargs-2, "");
}

/* <op1> <op2> . <op1 - op2>
 */
void
o_sub (void)
{
	binop (OP_SUB);
}

/* <value to be subtracted from named parameter> .
 */
void
o_subassign (memel *argp)
{
	/* operands are backwards on stack, so negate and add. can get by
	 * with this as long as subtraction is never defined for strings.
	 * if it is someday, will have to do something like in addassign.
	 */
	char *pname = (char *) argp;
	char *pk, *t, *p, *f;
	struct param *pp;

	breakout (pname, &pk, &t, &p, &f);
	pp = paramsrch (pk, t, p);

	unop (OP_MINUS);
	validparamget (pp, *f);
	binop (OP_ADD);
	paramset (pp, *f);
	pp->p_flags |= P_SET;
}

/* Doswitch finds the appropriate location to jump to in the
 * jump table and goes there.
 */
void
o_doswitch (int *jmpdelta)
{
	int pdft, icase, jmptable;
	int value=0;
	struct operand o;
	memel delta;
	/* Remember to subtract SZ_CE 'cuz PC has already been incremented. */
	jmptable = *jmpdelta + pc - SZ_CE;

	o = popop();
	if (o.o_type == OT_INT)
	    value = o.o_val.v_i;
	else if (o.o_type == OT_STRING) {
	    if (*o.o_val.v_s != '\0'  &&  *(o.o_val.v_s+1) == '\0')
		value = (int) *o.o_val.v_s;
	    else
		cl_error(E_UERR, "Illegal switch value.");
	} else
	    cl_error (E_UERR, "Illegal switch value.");

	pdft = stack[jmptable];

	if (cldebug)
	    eprintf ("doswitch: pdft=%d\n", pdft);

	/* Loop over cases.
	 */
	for (icase= jmptable + 1; stack[icase] != 0; icase++) {
	    int nval, ival, pcase;
	    memel *val;

	    pcase = stack[icase] + pc - SZ_CE;
	    nval = coderef(pcase)->c_length - (SZ_CE - 1);
	    currentline = coderef(pcase)->c_scriptln;

	    /* Loop over all values for a particular case.
	     */
	    val = & (coderef(pcase)->c_args);
	    for (ival=0; ival<nval; ival++, val++) {
		if (*val == value) {
		    /* Remember to skip over the CASE operand itself. */
		    delta =  pcase + (nval+(SZ_CE-1)) - (pc-SZ_CE) - SZ_CE;
		    o_dogoto (&delta);
		    return;
		}
	    }
	}

	/* Default? */
	if (pdft != 0) {
	    pdft = pdft + pc - SZ_CE;
	    /* Skipping over DEFAULT block takes 2 ints.
	     */
	    delta =  (pdft+(SZ_CE-1)) - (pc-SZ_CE) - SZ_CE;
	    o_dogoto (&delta);
	    return;
	}

	/* If there is no default we just drop through to the
	 * next statement which is a branch beyond the SWITCH.
	 */
}

void
o_swoff (memel *argp)
{
	register char *pname = (char *)argp;
	register struct param *pp;
	struct	operand o;
	struct	pfile *pfp;
	char	*pk, *t, *p, *f;

	breakout (pname, &pk, &t, &p, &f);
	if (*pk)
	    cl_error (E_UERR, e_simplep, p);
	pfp = newtask->t_pfp;
	pp = ppfind (pfp, t, p, 0, NO);
	if (pp == NULL)
	    cl_error (E_UERR, e_pnonexist, p);
	if ((XINT)pp == ERR)
	    cl_error (E_UERR, e_pambig, p, newtask->t_ltp->lt_lname);

	o.o_type = OT_BOOL;
	o.o_val.v_i = NO;
	pushop (&o);
	paramset (pp, FN_VALUE);
	if (pp->p_type & PT_PSET)
	    psetreload (pfp, pp);

	pp->p_flags |= P_CLSET;
}

void
o_swon (memel *argp)
{
	register char *pname = (char *)argp;
	register struct param *pp;
	struct	pfile *pfp;
	struct	operand o;
	char	*pk, *t, *p, *f;

	breakout (pname, &pk, &t, &p, &f);
	if (*pk)
	    cl_error (E_UERR, e_simplep, p);

	pfp = newtask->t_pfp;
	pp = ppfind (pfp, t, p, 0, NO);
	if (pp == NULL)
	    cl_error (E_UERR, e_pnonexist, p);
	if ((XINT)pp == ERR)
	    cl_error (E_UERR, e_pambig, p, newtask->t_ltp->lt_lname);

	o.o_type = OT_BOOL;
	o.o_val.v_i = YES;
	pushop (&o);
	paramset (pp, FN_VALUE);
	if (pp->p_type & PT_PSET)
	    psetreload (pfp, pp);

	pp->p_flags |= P_CLSET;
}


/* FIXLANGUAGE -- Called only once, during startup after processing the
 * cl startup file (clpackage.cl) to set the PKCCL flag for task LANGUAGE
 * in the package CLPACKAGE.  Thereafter, when language is executed it
 * will merely cause the current package to be changed.  This cannot be
 * done in the conventional way since clpackage.language() is never
 * executed to load the language package, since it is the root package.
 */
void
o_fixlanguage (void)
{
	register struct ltask *ltp;

	ltp = ltasksrch (CLPACKAGE, ROOTPACKAGE);
	ltp->lt_flags |= (LT_PACCL|LT_CL);
	ltp->lt_pkp = pacfind (ROOTPACKAGE);
}


/* the opcode jump table.
 *
 * order of the entries here must agree with constants in opcodes.h.
 * if the name is a keyword in C or a common library entry point,
 * then precede it with "do" but alphabetize it according to its intended name.
 */

void (*opcodetbl[])() = {
/*  0 */	o_undefined,

/*  1 */	o_absargset,
/*  2 */	o_add,
/*  3 */	o_addassign,
/*  4 */	o_doaddpipe,
/*  5 */	o_allappend,

/*  6 */	o_allredir,
/*  7 */	o_and,
/*  8 */	o_append,
/*  9 */	o_assign,
/* 10 */	o_biff,

/* 11 */	o_call,
/* 12 */	0,		/* The CASE operand is never executed.*/
/* 13 */	o_chsign,
/* 14 */	o_concat,
/* 15 */	0,		/* The DEFAULT operand is never executed. */

/* 16 */	o_div,
/* 17 */	o_divassign,
/* 18 */	o_doend,
/* 19 */	o_eq,
/* 20 */	o_exec,

/* 21 */	o_dofscan,
/* 22 */	o_dofscanf,
/* 23 */	o_ge,
/* 24 */	o_dogoto,
/* 25 */	o_dogetpipe,

/* 26 */	o_gt,
/* 27 */	o_immed,
/* 28 */	o_indirabsset,
/* 29 */	o_indirposset,
/* 30 */	o_indxincr,

/* 31 */	o_inspect,
/* 32 */	o_intrinsic,
/* 33 */	o_le,
/* 34 */	o_lt,
/* 35 */	o_mul,

/* 36 */	o_mulassign,
/* 37 */	o_ne,
/* 38 */	o_not,
/* 39 */	o_or,
/* 40 */	o_osesc,

/* 41 */	o_posargset,
/* 42 */	o_dopow,
/* 43 */	o_doprint,
/* 44 */	o_pushconst,
/* 45 */	o_pushindex,

/* 46 */	o_pushparam,
/* 47 */	o_redir,
/* 48 */	o_redirin,
/* 49 */	o_rmpipes,
/* 50 */	o_doreturn,

/* 51 */	o_doscan,
/* 52 */	o_doscanf,
/* 53 */	o_sub,
/* 54 */	o_subassign,
/* 55 */	o_doswitch,

/* 56 */	o_swoff,
/* 57 */	o_swon,
/* 58 */	o_fixlanguage,
/* 59 */	o_gsredir,
/* 60 */	o_catassign,
};
