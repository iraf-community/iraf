/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/* ENVMARK -- Mark the position of the "stack pointer" of the environment
 * list.  A subsequent call to ENVFREE will unset all set environment
 * operations since the corresponding mark.
 */
c_envmark (envp)
int	*envp;			/* storage for saved stack pointer	*/
{
	ENVMARK (envp);
}


/* ENVFREE -- Free or unset all environment variables set since the matching
 * call to ENVMARK.  The number of redefined variables uncovered by the free
 * operation is returned as the function value.
 */
c_envfree (envp, userfcn)
int	envp;			/* marker returned by envmark		*/
int	userfcn;		/* epa of user function for redefs	*/
{
	return (ENVFREE (&envp, &userfcn));
}


/* PRENVFREE -- Free or unset all environment variables set since the matching
 * call to ENVMARK.  The number of redefined variables uncovered by the free
 * operation is returned as the function value.  This call is equivalent to
 * envfree except that it also updates the values of any uncovered redefinitions
 * in the specified connected subprocesses.
 */
c_prenvfree (pid, envp)
int	pid;			/* process pid, or 0 for all subprocs	*/
int	envp;			/* marker returned by envmark		*/
{
	return (PRENVFREE (&pid, &envp));
}
