/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define import_spp
#define import_kernel
#define import_knames
#include <iraf.h>

/* ZFUNC[0-10] -- Call the function whose entry point address is pointed to
 * by the first argument, which is the integer valued entry point address of
 * the procedure as returned by ZLOCPR.  Up to ten arguments are passed by
 * reference to the called subprocedure.  The integer function value is
 * returned as the function value of the ZFUNC procedure (only integer
 * functions are supported).
 */

XINT ZFUNC0 ( XPOINTER *proc )
{
	return (*(PFI)(*proc))();
}
XINT ZFUNC1 ( XPOINTER *proc, void *arg1 )
{
	return (*(PFI)(*proc)) (arg1);
}

XINT ZFUNC2 ( XPOINTER *proc, void *arg1, void *arg2 )
{
	return (*(PFI)(*proc)) (arg1, arg2);
}

XINT ZFUNC3 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3 )
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3);
}

XINT ZFUNC4 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4 )
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4);
}

XINT ZFUNC5 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	      void *arg5 )
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}

XINT ZFUNC6 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	      void *arg5, void *arg6 )
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}

XINT ZFUNC7 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	      void *arg5, void *arg6, void *arg7 )
{
	return (*(PFI)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

XINT ZFUNC8 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	      void *arg5, void *arg6, void *arg7, void *arg8 )
{
	return (*(PFI)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

XINT ZFUNC9 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	      void *arg5, void *arg6, void *arg7, void *arg8, void *arg9 )
{
	return (*(PFI)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

XINT ZFUNCA ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	      void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, 
	      void *arg10 )
{
	return (*(PFI)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}


XLONG ZLFNC0 ( XPOINTER *proc )
{
	return (*(PFL)(*proc))();
}
XLONG ZLFNC1 ( XPOINTER *proc, void *arg1 )
{
	return (*(PFL)(*proc)) (arg1);
}

XLONG ZLFNC2 ( XPOINTER *proc, void *arg1, void *arg2 )
{
	return (*(PFL)(*proc)) (arg1, arg2);
}

XLONG ZLFNC3 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3);
}

XLONG ZLFNC4 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3, arg4);
}

XLONG ZLFNC5 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}

XLONG ZLFNC6 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}

XLONG ZLFNC7 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6, void *arg7 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

XLONG ZLFNC8 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6, void *arg7, void *arg8 )
{
	return (*(PFL)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

XLONG ZLFNC9 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6, void *arg7, void *arg8, void *arg9 )
{
	return (*(PFL)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

XLONG ZLFNCA ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, 
	       void *arg10 )
{
	return (*(PFL)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}


XPOINTER ZPFNC0 ( XPOINTER *proc )
{
	return (*(PFP)(*proc))();
}
XPOINTER ZPFNC1 ( XPOINTER *proc, void *arg1 )
{
	return (*(PFP)(*proc)) (arg1);
}

XPOINTER ZPFNC2 ( XPOINTER *proc, void *arg1, void *arg2 )
{
	return (*(PFP)(*proc)) (arg1, arg2);
}

XPOINTER ZPFNC3 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3);
}

XPOINTER ZPFNC4 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3, arg4);
}

XPOINTER ZPFNC5 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}

XPOINTER ZPFNC6 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}

XPOINTER ZPFNC7 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6, void *arg7 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

XPOINTER ZPFNC8 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6, void *arg7, void *arg8 )
{
	return (*(PFP)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

XPOINTER ZPFNC9 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6, void *arg7, void *arg8, void *arg9 )
{
	return (*(PFP)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

XPOINTER ZPFNCA ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, 
		  void *arg10 )
{
	return (*(PFP)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}
