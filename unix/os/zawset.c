/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#ifdef SYSV
#define NORLIMIT
#endif

#ifdef LINUX
#undef NORLIMIT
#endif

#ifdef SOLARIS
#define RLIMIT_RSS RLIMIT_VMEM
#undef NORLIMIT
#endif

#include <stdio.h>
#ifndef NORLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#endif

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* Kernel default working set values in bytes. */
int defworkset = SZ_DEFWORKSET;
int maxworkset = SZ_MAXWORKSET;
static int max_wss = 0;


/* ZAWSET -- Adjust or query the "working set", i.e., the maximum amount of
 * physical memory allocated to the process.
 */
ZAWSET (best_size, new_size, old_size, max_size)
XINT	*best_size;		/* requested working set size, bytes.	*/
XINT	*new_size, *old_size;	/* actual new and old sizes, bytes.	*/
XINT	*max_size;		/* max working set size, bytes		*/
{
	char *s, *getenv();
#ifndef NORLIMIT
	int working_set_size;
	struct rlimit rlp;
#endif

	/* The hard upper limit on memory utilization defined by the unix
	 * kernel can be limited either by the value compiled into the IRAF
	 * kernel, or by the value set in the user environment variable
	 * MAXWORKSET, given in units of Mb.
	 */
	if (!max_wss)
	    if (s = getenv ("MAXWORKSET")) {
		max_wss = atoi(s) * 1024*1024;
		if (max_wss < 1024*1024)
		    max_wss = maxworkset;
	    } else
		max_wss = maxworkset;

#ifdef NORLIMIT
	if (*best_size == 0)
	    *old_size = *new_size = defworkset;
	else
	    *new_size = *old_size = min (max_wss, *best_size);
	*max_size = max_wss;
#else
	getrlimit (RLIMIT_RSS, &rlp);
	working_set_size = min (max_wss, rlp.rlim_cur);

	/* Now try to set the size requested by our caller.  If bestsize was
	 * given as zero, merely return the status values.
	 */
	(*max_size) = min (max_wss, rlp.rlim_max);
	if (*best_size <= 0)
	    *new_size = *old_size = working_set_size;
	else {
	    rlp.rlim_cur = min (*best_size, *max_size);
	    if (rlp.rlim_cur > working_set_size)
		setrlimit (RLIMIT_RSS, &rlp);
	    getrlimit (RLIMIT_RSS, &rlp);
	    *old_size = working_set_size;
	    *new_size = min(*best_size, min(max_wss, rlp.rlim_cur));
	}
#endif
}
