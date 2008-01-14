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

int ZCALL0 ( XPOINTER *proc )
{
	return (*(PFU)(*proc))();
}

int ZCALL1 ( XPOINTER *proc, void *arg1 )
{
	return (*(PFU)(*proc)) (arg1);
}


int ZCALL2 ( XPOINTER *proc, void *arg1, void *arg2 )
{
	return (*(PFU)(*proc)) (arg1, arg2);
}


int ZCALL3 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3);
}


int ZCALL4 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4);
}


int ZCALL5 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	     void *arg5 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}


int ZCALL6 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}


int ZCALL7 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	     void *arg5, void *arg6, void *arg7 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}


int ZCALL8 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}


int ZCALL9 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8, void *arg9 )
{
	return (*(PFU)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}


int ZCALLA ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, 
	     void *arg10 )
{
	return (*(PFU)(*proc)) (arg1,
	    arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}
