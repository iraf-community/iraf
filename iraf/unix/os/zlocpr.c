/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#define import_kernel
#define import_knames
#define import_spp
#include <iraf.h>

#ifdef SOLARIS
#define SUNOS
#endif

/* ZLOCPR -- Return the entry point address of a procedure as a magic
 * integer value.  A subsequent call to one of the ZCALL primitives is used
 * to call the procedure.
 */
/* proc  : procedure for which we desire address */
/* o_epa : entry point address */
int ZLOCPR ( PFU proc, XPOINTER *o_epa )
{
	void *epa = (void *) proc;

	*o_epa = (XPOINTER) epa;

#ifdef SUNOS
	/* Return immediately if the shared library is not in use. */
	if (VSHLIB[0] == 0)
	    return XOK;

	/* If the shared library is in use and the reference procedure is
	 * a transfer vector, return the address of the actual function.
	 * This is necessary to permit equality comparisons when ZLOCPR
	 * is called to reference the same procedure in both the shared
	 * library image and the client process.
	 */
	if ( (unsigned XPOINTER *)epa < VSHLIB || (unsigned XPOINTER *)epa >= (unsigned XPOINTER *)&VSHEND)
	    return XOK;

	/* Disassemble the JMP instruction in the transfer vector to get the
	 * address of the referenced procedure in the shared library. [MACHDEP]
	 */
#ifdef mc68000
	*o_epa = (XPOINTER)(*((unsigned XPOINTER *)((char *)epa + 2)));
#else
#ifdef sparc
	*o_epa = (XPOINTER)(((*epa & 0x3fffff) << 10) | (*((unsigned XPOINTER *)epa+1) & 0x3ff));
#endif
#endif

#endif
	return XOK;
}
