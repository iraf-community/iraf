#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZGTPID -- Get process id number (used for process control and to make
 * unique file names).
 */
ZGTPID (pid)
XINT	*pid;
{
	*pid = getpid();
}
