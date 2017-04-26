/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_fset
#define import_error
#define import_ctype
#define import_stdio
#define import_alloc
#define import_ttset
#define import_prstat
#define import_xwhen
#include <iraf.h>

/*
*/
#include "config.h"
#include "clmodes.h"
#include "mem.h"
#include "operand.h"
#include "opcodes.h"
#include "param.h"
#include "task.h"
#include "errs.h"


/*
 * BUILTIN_VO -- This file contains the functions that perform the built-in
 *   VO-related commands of the cl, such as coneCaller(), getRegistry(), etc.
 *
 * SetVOBuiltins() contains a table of functions and their user names; add
 *   to this table when adding new builtin functions.
 *
 * See builtins.c for addition notes and details.
 */

extern	int   cldebug, cltrace;		/* debug/trace flags		     */
extern	int   lastjobno;		/* last background job spawned	     */
extern	int   gologout;			/* flag to execute() to cause logout */
extern  int   logout_status;      	/* optional arg to logout()          */
extern  int   errorline;		/* error recover line		     */
extern  int   currentline;		/* line currently being executed     */
extern	char *findexe();

extern	int do_error;			/* for error recovery/trapping 	     */


extern int	VOClient_initialized;

typedef int (*PFI)();

static PFI	old_onipc;		/* X_IPC handler		     */
int		voc_onipc();


/* CL_VOCINIT --  Initialize the VO Client interface.
 */
void
cl_vocinit (void)
{
    register struct pfile *pfp;
    struct  operand o;
    int     n, status;

    if (VOClient_initialized == 0) {

	c_xwhen (X_IPC, voc_onipc, &old_onipc);

        pfp = newtask->t_pfp;
        if ((n = nargs (pfp)) > 1) {
            cl_error (E_UERR, e_posargs, "vocinit");
	    return;

        } else if (n < 1) {
            status = voc_initVOClient (envget("vo_runid"));

	} else {
            pushbparams (pfp->pf_pp);
            popop();                    /* discard fake name.           */
            opcast (OT_STRING);
            o = popop();                /* get ltask                    */

            status = voc_initVOClient (o.o_val.v_s);
        }

	if (status) {
            cl_error (E_UERR, "Can't init VOClient", "vocinit");
	} else
	    VOClient_initialized = 1;
    }
}


/* CL_VOCSTOP --  Stop the VO Client interface.
 */
void
cl_vocstop (void)
{
    register struct pfile *pfp;
    struct  operand o;
    int     n, status;

    if (VOClient_initialized == 1) {
	c_xwhen (X_IPC, old_onipc, &old_onipc);

        pfp = newtask->t_pfp;
        if ((n = nargs (pfp)) > 1) {
            cl_error (E_UERR, e_posargs, "vocstop");
	    return;

        } else if (n < 1) {
            voc_closeVOClient (1);

	} else {
            pushbparams (pfp->pf_pp);
            popop();                    /* discard fake name.           */
            opcast (OT_INT);
            o = popop();                /* get ltask                    */
            voc_closeVOClient (o.o_val.v_i);
	}
	VOClient_initialized = 0;
    }
}


/* CL_VOCRESET --  Restart the VO Client interface.
 */
void
cl_vocreset ()
{
    int status;

    voc_closeVOClient (1);
    status = voc_initVOClient (envget("vo_runid"));
}


/* ========================================================
 *
 * End of builtin functions.
 * What follows is their support code.
 *
 * ========================================================*/


/* VO_SETBUILTINS -- Add the builtin functions to package at pkp (this should
 * always just be clpackage).  To add more functions, write the support function
 * and enter it into the builtin table, btbl.  Reverse alpha due to lifo nature
 * of list.  Aliases can be made easily with multiple b_names using the same
 * b_f.  Setting LT_INVIS will keep it from being seen in the menu.
 */
void
vo_setbuiltins (
    struct package *pkp
)
{
    static struct builtin {
    	char	*b_name;
    	void	(*b_f)();
    	int	b_flags;
    } btbl[] = {
        { "vocinit",  cl_vocinit,  0}, /* initialize the VO Client	*/
        { "vocstop",  cl_vocstop,  0}, /* stop the VO Client	*/
        { "vocreset", cl_vocreset, 0}, /* reset the VO Client	*/
        NULL
    };

    register struct builtin *bp;

    for (bp = btbl; bp < &btbl[sizeof(btbl)/sizeof(struct builtin)]; bp++)
        newbuiltin (pkp, bp->b_name, bp->b_f, bp->b_flags, "", 0);
}


/* VOC_ONIPC -- Call this when get a signal that indicates a write to an IPC
 * channel with no reader.  We are called after the system X_IPC handler
 * has been called to cleanup the internal process tables and file system,
 * disabling any further output to the process.
 */         
/* ARGSUSED */
int
voc_onipc (
    int   *vex,                   /* virtual exception code       */
    PFI   *next_handler           /* next handler to be called    */
)
{       
    VOClient_initialized = 0;
    cl_vocreset();
    cl_error (E_UERR, "Abnormal termination of %s, resetting\n",
	"VOClientd");

    return (0);
}           
