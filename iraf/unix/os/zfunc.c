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


XLONG ZFNCL0 ( XPOINTER *proc )
{
	return (*(PFL)(*proc))();
}
XLONG ZFNCL1 ( XPOINTER *proc, void *arg1 )
{
	return (*(PFL)(*proc)) (arg1);
}

XLONG ZFNCL2 ( XPOINTER *proc, void *arg1, void *arg2 )
{
	return (*(PFL)(*proc)) (arg1, arg2);
}

XLONG ZFNCL3 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3);
}

XLONG ZFNCL4 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3, arg4);
}

XLONG ZFNCL5 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}

XLONG ZFNCL6 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}

XLONG ZFNCL7 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6, void *arg7 )
{
	return (*(PFL)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

XLONG ZFNCL8 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6, void *arg7, void *arg8 )
{
	return (*(PFL)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

XLONG ZFNCL9 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6, void *arg7, void *arg8, void *arg9 )
{
	return (*(PFL)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

XLONG ZFNCLA ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
	       void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, 
	       void *arg10 )
{
	return (*(PFL)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}


XPOINTER ZFNCP0 ( XPOINTER *proc )
{
	return (*(PFP)(*proc))();
}
XPOINTER ZFNCP1 ( XPOINTER *proc, void *arg1 )
{
	return (*(PFP)(*proc)) (arg1);
}

XPOINTER ZFNCP2 ( XPOINTER *proc, void *arg1, void *arg2 )
{
	return (*(PFP)(*proc)) (arg1, arg2);
}

XPOINTER ZFNCP3 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3);
}

XPOINTER ZFNCP4 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3, arg4);
}

XPOINTER ZFNCP5 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3, arg4, arg5);
}

XPOINTER ZFNCP6 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6);
}

XPOINTER ZFNCP7 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6, void *arg7 )
{
	return (*(PFP)(*proc)) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

XPOINTER ZFNCP8 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6, void *arg7, void *arg8 )
{
	return (*(PFP)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

XPOINTER ZFNCP9 ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6, void *arg7, void *arg8, void *arg9 )
{
	return (*(PFP)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

XPOINTER ZFNCPA ( XPOINTER *proc, void *arg1, void *arg2, void *arg3, void *arg4, 
		  void *arg5, void *arg6, void *arg7, void *arg8, void *arg9, 
		  void *arg10 )
{
	return (*(PFP)(*proc))(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}
