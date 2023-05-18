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


typedef int (*PFI_0)();
typedef int (*PFI_1)(void *);
typedef int (*PFI_2)(void *,void *);
typedef int (*PFI_3)(void *,void *,void *);
typedef int (*PFI_4)(void *,void *,void *,void *);
typedef int (*PFI_5)(void *,void *,void *,void *,void *);
typedef int (*PFI_6)(void *,void *,void *,void *,void *,void *);
typedef int (*PFI_7)(void *,void *,void *,void *,void *,void *,void *);
typedef int (*PFI_8)(void *,void *,void *,void *,void *,void *,void *,
                     void *);
typedef int (*PFI_9)(void *,void *,void *,void *,void *,void *,void *,
                     void *,void *);
typedef int (*PFI_A)(void *,void *,void *,void *,void *,void *,void *,
                     void *,void *,void *);



int ZCALL0 (XINT *proc)
{
	return (*(PFI_0)(*proc))();
}

int ZCALL1 (XINT *proc, void *arg1)
{
	return (*(PFI_1)(*proc)) (arg1);
}

int ZCALL2 (XINT *proc, void *arg1, void *arg2)
{
	return (*(PFI_2)(*proc)) (arg1, arg2);
}

int ZCALL3 (XINT *proc, void *arg1, void *arg2, void *arg3)
{
	return (*(PFI_3)(*proc)) (arg1, arg2, arg3);
}

int ZCALL4 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4)
{
	return (*(PFI_4)(*proc)) (arg1, arg2, arg3, arg4);
}

int ZCALL5 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	     void *arg5)
{
	return (*(PFI_5)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}

int ZCALL6 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6)
{
	return (*(PFI_6)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}

int ZCALL7 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	     void *arg5, void *arg6, void *arg7)
{
	return (*(PFI_7)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

int ZCALL8 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8)
{
	return (*(PFI_8)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7,
                                  arg8);
}

int ZCALL9 (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8, void *arg9)
{
	return (*(PFI_9)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, 
				  arg8, arg9);
}

int ZCALLA (XINT *proc, void *arg1, void *arg2, void *arg3, void *arg4,
	     void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, 
	     void *arg10)
{
	return (*(PFI_A)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, 
				  arg8, arg9, arg10);
}
