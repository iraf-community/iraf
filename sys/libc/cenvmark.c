/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* ENVMARK -- Mark the position of the "stack pointer" of the environment
** list.  A subsequent call to ENVFREE will unset all set environment
** operations since the corresponding mark.
*/
void
c_envmark (
  XINT	*envp			/* storage for saved stack pointer	*/
)
{
        int  ENVMARK (XINT *old_top);

	ENVMARK (envp);
}


/* ENVFREE -- Free or unset all environment variables set since the matching
** call to ENVMARK.  The number of redefined variables uncovered by the free
** operation is returned as the function value.
*/
int
c_envfree (
  int	envp,			/* marker returned by envmark		*/
  int	userfcn			/* epa of user function for redefs	*/
)
{
	XINT  x_envp = envp;
        XINT  ENVFREE (XINT *old_top, XINT *userfcn);

	return (ENVFREE (&x_envp, (XINT *)&userfcn));
}


/* PRENVFREE -- Free or unset all environment variables set since the matching
** call to ENVMARK.  The number of redefined variables uncovered by the free
** operation is returned as the function value.  This call is equivalent to
** envfree except that it also updates the values of any uncovered redefinitions
** in the specified connected subprocesses.
*/
int
c_prenvfree (
  int	pid,			/* process pid, or 0 for all subprocs	*/
  int	envp			/* marker returned by envmark		*/
)
{
	XINT  x_pid = pid,  x_envp = envp;
        XINT  PRENVFREE (XINT *pid, XINT *marker);

	return (PRENVFREE (&x_pid, &x_envp));
}
