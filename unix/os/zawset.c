/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <sys/time.h>
#include <sys/resource.h>
#include <stdio.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZAWSET -- Adjust the "working set", i.e., the maximum amount of physical
 * memory allocated to the process.  4.[23]BSD UNIX implements a "soft" working
 * set limit as the resource limit RLIMIT_RSS.  Both soft and and hard limits
 * are provided; we use only the soft limit, which tells the system to steal
 * (or don't steal) memory from a particular process if memory gets tight.
 * The soft limit is initialized to the IRAF default by zmain at process
 * starup time, if not already set by the parent process when the child was
 * forked.
 */
ZAWSET (best_size, new_size, old_size, max_size)
XINT	*best_size;		/* requested working set size, bytes.	*/
XINT	*new_size, *old_size;	/* actual new and old sizes, bytes.	*/
XINT	*max_size;		/* max working set size, bytes		*/
{
	static	int initialized;
	int	working_set_size;
	struct	rlimit rlp;

	getrlimit (RLIMIT_RSS,  &rlp); working_set_size = rlp.rlim_cur;

	/* The hard limit value rlim_max can greatly exceed the physical
	 * memory on some systems, so we provide a defined parameter in
	 * kernel.h to set a more realistic upper limit.
	 */
	(*max_size) = min (rlp.rlim_max, SZ_MAXWORKSET);

	/* We are called by zmain during process startup to set the initial
	 * default working set.
	 */
	if (!initialized) {
	    rlp.rlim_cur = SZ_DEFWORKSET;
	    setrlimit (RLIMIT_RSS, &rlp);
	    working_set_size = SZ_DEFWORKSET;
	    initialized++;
	}

	/* Now try to set the size requested by our caller.  If bestsize was
	 * given as zero, merely return the status values.
	 */
	if (*best_size == 0)
	    *old_size = *new_size = working_set_size;
	else {
	    rlp.rlim_cur = min (*best_size, rlp.rlim_max);
	    setrlimit (RLIMIT_RSS, &rlp);
	    getrlimit (RLIMIT_RSS, &rlp);

	    *old_size = working_set_size;
	    *new_size = min (*best_size, rlp.rlim_cur);
	}
}
