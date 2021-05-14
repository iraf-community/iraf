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
#include "task.h"
#include "errs.h"
#include "mem.h"
#include "proto.h"


/*
 * STACK -- "stack" is actually two stacks:
 * starting at the top and growing downwards is the "control stack",
 *   used for stacking compiler intermediates at compile time and the
 *   running and any pending task structs at runtime.
 * the other, called the "operand stack", starts at the bottom and grows up.
 *   compiled code is put at its base and basos and topos are set when
 *   compilation completes to just above the last instruction. at run-time,
 *   starting at basos and growing upwards, it contains struct operands,
 *   possibly a string if o_type == OT_STRING, and the index of the last
 *   operand in a linked-list fashion; see pushop(). when runtime completes, 
 *   its entire contents are disgarded by setting pc = bascode and starting new
 *   code compilation.
 *
 * in both cases, the respective "top" values are the indices into "stack" that
 *   were most recently last assigned. They are not related to the size of the
 *   object on the stack but always refer simply to the last integer index.
 *   valid topcs and topos always satisfy: 0 <= topos < topcs < STACKSIZ.
 */

memel stack[STACKSIZ];		/* control and operand stack combined	*/
XINT topcs = STACKSIZ;		/* index of last cstack; grows downward	*/
XINT topos = -1;		/* index of last ostack; grows upward	*/
XINT basos = -1;		/* lowest legal index of operand stack	*/

/* Push a memel value onto the control stack.  Return ERR if it would cause
 * overflow, else OK.  The control stack is used by the parser during
 * compilation.  If an error occurs during compilation, taskunwind() will
 * call poptask() to pop tasks off the control stack.  We must be careful
 * to avoid having the compiler temporaries interfere with task frames.
 */
void
pushmem (memel v)
{
	if (topcs - 1 > topos)
	    stack[--topcs] = v;
	else
	    eprintf ("control stack overflow; topcs/topos = %d/%d\n",
		    topcs, topos);
}


/* Pop top memory value off control stack and return it.
 * ==> no real err return, although it is checked.
 */
memel
popmem (void)
{
	if (topcs < STACKSIZ)
	    return (stack[topcs++]);
	else {
	    eprintf ("control stack underflow\n");
	    return ((memel) ERR);
	}
}

/* PPush pushes an element onto the stack, but leaves the top
 * of the stack untouched.
 */
void
ppushmem (memel p)
{
	register memel	q;

	q = popmem();
	pushmem(p);
	pushmem(q);
}


/* push operand *op, string storage if o_type == OT_STRING, and last topos 
 * onto operand stack.
 * return copy of new operand so that its o.o_val.v_s will point to the
 * stack-stored string; if not string, it will be same as the passed *op.
 * call error() if overflow and DO NOT RETURN.
 *
 * N.B. opcast() uses this layout intimately.
 *
 *                   --------------
 * (new) topos ->   | last topos   |
 *		    |--------------|
 *		    | possible     |
 *		    | string       |
 *		    | storage      |<-
 *	 	    |--------------|  |
 *		    |struct operand|  |
 *		    | (o.o_val.v_s)|--
 *		    |--------------|
 * (last topos ->)  | last topos   |
 *		    |--------------|
 *			...
 */
struct operand
pushop (struct operand *op)
{
	struct operand  junk;

	if (topos + OPSIZ+1 < topcs) {
	    int		lasttopos = topos;
	    struct	operand *dest;

	    dest = (struct operand *) &stack[topos+1];
	    *dest = *op;

	    if (op->o_type == OT_STRING) {
		int	len = btoi (strlen (op->o_val.v_s) + 1);
		if (topos + OPSIZ+1 + len >= topcs)
		    goto overflow;
		dest->o_val.v_s = (char *) &stack[topos+OPSIZ+1];
		strcpy (dest->o_val.v_s, op->o_val.v_s);
		topos += len;
	    }

	    topos += OPSIZ+1;
	    stack[topos] = lasttopos;

	    return (*dest);
	}

overflow:
	cl_error (E_IERR, e_soverflow, topcs, topos);
	/* NOTREACHED */
	return (junk);
}

/* pop top operand from stack and return copy of it. If type is string,
 * be sure to use it before the next pushop() or the string will get clobbered.
 * set topos to top of stack; see diagram with pushop().
 * call error() and do not return if underflow.
 */
struct operand
popop (void)
{
	struct operand  junk;

	if (topos > basos) {
	    struct	operand *op;

	    topos = stack[topos];
	    op = (struct operand *) &stack[topos+1];
	    return (*op);
	}
	cl_error (E_UERR, e_sunderflow);
	/* NOTREACHED */
	return (junk);
}


/* Create a new, uninitialized, task on the control stack.  Call error()
 * and don't return if overflow, else return pointer to new entry.  Save
 * index of new task frame so that we don't get confused by temporaries
 * left on the stack by the parser if error occurs during parsing.
 */
int last_task_frame;				/* for error recovery */

struct task *
pushtask (void)
{
	if (topcs - TASKSIZ  > topos) {
	    topcs -= TASKSIZ;
	    last_task_frame = topcs;
	    return ((struct task *) &stack[topcs]);
	} 
	cl_error (E_UERR, "task stack overflow");	/* does not return */
/* NOTREACHED */
	return ((struct task *) NULL);
}


/* Increment topcs and return pointer to next task struct on control stack.
 * (Top entry may be inspected with pushtask (poptask()) or with currentask.)
 * Call error() and do not return on underflow.
 */
struct task *
poptask (void)
{
	if (topcs <= STACKSIZ - TASKSIZ) {
	    if (topcs < last_task_frame) {
		/* If we get here, something has been pushed on the control
		 * stack by pop() since the last task frame, which did not
		 * get cleared off.  This may happen if error() is called
		 * during compilation.
		 */
		topcs = last_task_frame;
	    }
	    topcs += TASKSIZ;
	    last_task_frame = topcs;
	    return ((struct task *) &stack[topcs]);
	} 
	cl_error (E_IERR, "Control stack underflow: topcs = %d", topcs);
/* NOTREACHED */
	return ((struct task *) NULL);
}
