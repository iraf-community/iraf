/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZCALL[0-10] -- Call the procedure whose entry point address is pointed to
 * by the first argument, which is the integer valued entry point address of
 * the procedure as returned by ZLOCPR.  Up to ten arguments are passed by
 * reference to the called subprocedure.
 */

typedef	int (*PFI)();		/* pointer to function returning an int	*/

ZCALL0 (proc)
XINT	*proc;
{
	(*(PFI)(*proc))();
}

ZCALL1 (proc, arg1)
XINT	*proc;
int	*arg1;
{
	(*(PFI)(*proc)) (arg1);
}


ZCALL2 (proc, arg1, arg2)
XINT	*proc;
int	*arg1, *arg2;
{
	(*(PFI)(*proc)) (arg1, arg2);
}


ZCALL3 (proc, arg1, arg2, arg3)
XINT	*proc;
int	*arg1, *arg2, *arg3;
{
	(*(PFI)(*proc)) (arg1, arg2, arg3);
}


ZCALL4 (proc, arg1, arg2, arg3, arg4)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4;
{
	(*(PFI)(*proc)) (arg1, arg2, arg3, arg4);
}


ZCALL5 (proc, arg1, arg2, arg3, arg4, arg5)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5;
{
	(*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}


ZCALL6 (proc, arg1, arg2, arg3, arg4, arg5, arg6)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6;
{
	(*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}


ZCALL7 (proc, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7;
{
	(*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}


ZCALL8 (proc, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8;
{
	(*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}


ZCALL9 (proc, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9;
{
	(*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}


ZCALLA (proc, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9, *arg10;
{
	(*(PFI)(*proc)) (arg1,
	    arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}
