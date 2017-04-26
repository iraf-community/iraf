/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

#ifdef SOLARIS
#define SUNOS
#endif

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

#ifdef SUNOS
	/* Return immediately if the shared library is not in use. */
	if (VSHLIB[0] == 0)
	    return (XOK);

	/* If the shared library is in use and the reference procedure is
	 * a transfer vector, return the address of the actual function.
	 * This is necessary to permit equality comparisons when ZLOCPR
	 * is called to reference the same procedure in both the shared
	 * library image and the client process.
	 */
	if (epa < VSHLIB || epa >= (unsigned *)&VSHEND)
	    return (XOK);

	/* Disassemble the JMP instruction in the transfer vector to get the
	 * address of the referenced procedure in the shared library. [MACHDEP]
	 */
#ifdef i386
	*o_epa = (XINT)((unsigned)epa + *((unsigned *)((char *)epa + 1)) + 5);
#else
#ifdef mc68000
	*o_epa = (XINT)(*((unsigned *)((char *)epa + 2)));
#else
#ifdef sparc
	*o_epa = (XINT)(((*epa & 0x3fffff) << 10) | (*(epa+1) & 0x3ff));
#endif
#endif
#endif

#endif

	return (XOK);
}
