/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

/* ZCALL[0-10] -- Call the procedure whose entry point address is pointed to
 * by the first argument, which is the integer valued entry point address of
 * the procedure as returned by ZLOCPR.  Up to ten arguments are passed by
 * reference to the called subprocedure.
 */




int ZCALL0 (XINT *proc)
{
	return (*(PFI)(*proc))();
}

int ZCALL1 (XINT *proc, void *arg1)
{
	return (*(PFI)(*proc)) (arg1);
}


int ZCALL2 (XINT *proc, void *arg1, void *arg2)
{
	return (*(PFI)(*proc)) (arg1, arg2);
}


int ZCALL3 (XINT *proc, void *arg1, void *arg2, void *arg3)
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3);
}


int ZCALL4 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4)
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4);
}


int ZCALL5 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	     void *arg5)
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}


int ZCALL6 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6)
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}


int ZCALL7 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	     void *arg5, void *arg6, void *arg7)
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}


int ZCALL8 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8)
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}


int ZCALL9 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8, void *arg9)
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, 
				arg8, arg9);
}


int ZCALLA (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, 
	     void *arg10)
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, 
				arg8, arg9, arg10);
}
