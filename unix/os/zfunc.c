/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZFUNC[0-10] -- Call the function whose entry point address is pointed to
 * by the first argument, which is the integer valued entry point address of
 * the procedure as returned by ZLOCPR.  Up to ten arguments are passed by
 * reference to the called subprocedure.  The integer function value is
 * returned as the function value of the ZFUNC procedure (only integer
 * functions are supported).
 */

typedef	int (*PFI)();		/* pointer to function returning an int	*/

ZFUNC0 (proc)
XINT	*proc;
{
	return ((*(PFI)(*proc))());
}

ZFUNC1 (proc, arg1)
XINT	*proc;
int	*arg1;
{
	return ((*(PFI)(*proc)) (arg1));
}


ZFUNC2 (proc, arg1, arg2)
XINT	*proc;
int	*arg1, *arg2;
{
	return ((*(PFI)(*proc)) (arg1, arg2));
}


ZFUNC3 (proc, arg1, arg2, arg3)
XINT	*proc;
int	*arg1, *arg2, *arg3;
{
	return ((*(PFI)(*proc)) (arg1, arg2, arg3));
}


ZFUNC4 (proc, arg1, arg2, arg3, arg4)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4;
{
	return ((*(PFI)(*proc)) (arg1, arg2, arg3, arg4));
}


ZFUNC5 (proc, arg1, arg2, arg3, arg4, arg5)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5;
{
	return ((*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5));
}


ZFUNC6 (proc, arg1, arg2, arg3, arg4, arg5, arg6)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6;
{
	return ((*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6));
}


ZFUNC7 (proc, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7;
{
	return ((*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7));
}


ZFUNC8 (proc, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8;
{
	return ((*(PFI)(*proc))
	    (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8));
}


ZFUNC9 (proc, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9;
{
	return ((*(PFI)(*proc))
	    (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9));
}


ZFUNCA (proc, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
XINT	*proc;
int	*arg1, *arg2, *arg3, *arg4, *arg5, *arg6, *arg7, *arg8, *arg9, *arg10;
{
	return ((*(PFI)(*proc))
	    (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10));
}
