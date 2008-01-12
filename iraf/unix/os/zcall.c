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

int ZCALL0 ( XINT *proc )
{
	return (*(PFU)(*proc))();
}

int ZCALL1 ( XINT *proc, XINT *arg1 )
{
	return (*(PFU)(*proc)) (arg1);
}


int ZCALL2 ( XINT *proc, XINT *arg1, XINT *arg2 )
{
	return (*(PFU)(*proc)) (arg1, arg2);
}


int ZCALL3 ( XINT *proc, XINT *arg1, XINT *arg2, XINT *arg3 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3);
}


int ZCALL4 ( XINT *proc, XINT *arg1, XINT *arg2, XINT *arg3, XINT *arg4 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4);
}


int ZCALL5 ( XINT *proc, XINT *arg1, XINT *arg2, XINT *arg3, XINT *arg4, 
	     XINT *arg5 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}


int ZCALL6 ( XINT *proc, XINT *arg1, XINT *arg2, XINT *arg3, XINT *arg4,
	     XINT *arg5, XINT *arg6 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}


int ZCALL7 ( XINT *proc, XINT *arg1, XINT *arg2, XINT *arg3, XINT *arg4, 
	     XINT *arg5, XINT *arg6, XINT *arg7 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}


int ZCALL8 ( XINT *proc, XINT *arg1, XINT *arg2, XINT *arg3, XINT *arg4,
	     XINT *arg5, XINT *arg6, XINT *arg7, XINT *arg8 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}


int ZCALL9 ( XINT *proc, XINT *arg1, XINT *arg2, XINT *arg3, XINT *arg4,
	     XINT *arg5, XINT *arg6, XINT *arg7, XINT *arg8, XINT *arg9 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}


int ZCALLA ( XINT *proc, XINT *arg1, XINT *arg2, XINT *arg3, XINT *arg4,
	     XINT *arg5, XINT *arg6, XINT *arg7, XINT *arg8, XINT *arg9, 
	     XINT *arg10 )
{
	return (*(PFU)(*proc)) (arg1,
	    arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}
