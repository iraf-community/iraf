/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

extern	unsigned VSHLIB[], VSHEND;	/* shared library descriptor */

/* ZLOCPR -- Return the entry point address of a procedure as a magic
 * integer value.  A subsequent call to one of the ZCALL primitives is used
 * to call the procedure.
 */
int
ZLOCPR (
  PFI	proc,			/* procedure for which we desire address */
  XINT	*o_epa			/* entry point address */
)
{
	register unsigned *epa = (unsigned *) proc;
	*o_epa = (XINT) epa;
	return (XOK);
}
