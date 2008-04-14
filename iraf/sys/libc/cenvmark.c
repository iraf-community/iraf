/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/* ENVMARK -- Mark the position of the "stack pointer" of the environment
 * list.  A subsequent call to ENVFREE will unset all set environment
 * operations since the corresponding mark.
 */
/* envp : storage for saved stack pointer */
void c_envmark ( int *envp )
{
	XINT x_envp = *envp;	/* OK ??? */
	ENVMARK (&x_envp);	/* OK ??? */
	*envp = x_envp;		/* OK ??? */
}


/* ENVFREE -- Free or unset all environment variables set since the matching
 * call to ENVMARK.  The number of redefined variables uncovered by the free
 * operation is returned as the function value.
 */
/* envp    : marker returned by envmark      */
/* userfcn : epa of user function for redefs */
int c_envfree ( int envp, PFU userfcn )
{
	XINT x_envp = envp;
	XPOINTER x_userfcn = (XPOINTER)userfcn;
	return (ENVFREE (&x_envp, &x_userfcn));
}


/* PRENVFREE -- Free or unset all environment variables set since the matching
 * call to ENVMARK.  The number of redefined variables uncovered by the free
 * operation is returned as the function value.  This call is equivalent to
 * envfree except that it also updates the values of any uncovered redefinitions
 * in the specified connected subprocesses.
 */
/* pid  : process pid, or 0 for all subprocs */
/* envp : marker returned by envmark         */
int c_prenvfree ( int pid, int envp )
{
	XINT x_pid = pid;
	XINT x_envp = envp;
	return (PRENVFREE (&x_pid, &x_envp));
}
