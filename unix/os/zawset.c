/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#ifdef SYSV
#define NORLIMIT
#endif

#ifndef CYGWIN
#ifdef LINUX
#undef NORLIMIT
#endif
#endif

#ifdef SOLARIS
#define RLIMIT_RSS RLIMIT_VMEM
#undef NORLIMIT
#endif

#include <stdio.h>
#include <unistd.h>
#ifndef NORLIMIT
#include <sys/time.h>
#include <sys/resource.h>
#endif

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

#define	PCT_RESERVE	10	 /* percent */
#define	MIN_RESERVE	50	 /* megabytes */
#define	MIN_WORKSET	32	 /* megabytes */

#define	ENV_DEBUG	"ZAWSET_DEBUG"
#define	MB		(1024*1024)
#define	KB		1024

/* Kernel default working set values in bytes. */
unsigned int defworkset = SZ_DEFWORKSET;
unsigned int maxworkset = SZ_MAXWORKSET;
static unsigned int max_wss = 0;
extern char *getenv();


/* ZAWSET -- Adjust or query the "working set", i.e., the maximum amount of
 * physical memory allocated to the process.
 */
int
ZAWSET (
  XINT	*best_size,		/* requested working set size, bytes.	*/
  XINT	*new_size, 
  XINT  *old_size,		/* actual new and old sizes, bytes.	*/
  XINT	*max_size 		/* max working set size, bytes		*/
)
{
	int physmem=0, kb_page;
	int debug = (getenv(ENV_DEBUG) != NULL);
	char *s, *getenv();

#ifndef NORLIMIT
	unsigned int working_set_size;
	struct rlimit rlp;
#endif


	/* Get the page size in kilobytes. */
	kb_page = getpagesize() / KB;

#ifdef _SC_PHYS_PAGES
	/* On recent POSIX systems (including Solaris, Linux, and maybe
	 * others) we can use sysconf to get the actual system memory size.
	 * The computation is done in KB to avoid integer overflow.
	 */
	physmem = sysconf(_SC_PHYS_PAGES) * kb_page;
	if (physmem > 0) {
	    maxworkset = min (MAX_LONG / KB, physmem);

	    /* Don't try to use all of physical memory. */
	    if (maxworkset == physmem) {
		maxworkset -= (max ((MIN_RESERVE*MB)/KB,
		    physmem * PCT_RESERVE / 100));
		if (maxworkset <= 0)
		    maxworkset = (MIN_WORKSET * MB) / KB;
	    }

	    /* Now convert back to bytes. */
	    maxworkset *= 1024;
	}
#endif

	/* The hard upper limit on memory utilization defined by the unix
	 * kernel can be limited either by the value compiled into the IRAF
	 * kernel, or by the value set in the user environment variable
	 * MAXWORKSET, given in units of Mb.
	 */
	if (!max_wss) {
	    if ( (s = getenv ("MAXWORKSET")) ) {
		max_wss = atoi(s) * 1024*1024;
		if (max_wss < 1024*1024)
		    max_wss = maxworkset;
	    } else
		max_wss = maxworkset;
	}

	if (debug)
	    fprintf(stderr,"zawset: physmem=%dm, maxworkset=%dm max_wss=%dm\n",
		physmem / KB, maxworkset / MB, max_wss / MB);

#ifdef NORLIMIT
	if (*best_size == 0)
	    *old_size = *new_size = defworkset;
	else
	    *new_size = *old_size = min (max_wss, *best_size);
	*max_size = max_wss;
#else
	getrlimit (RLIMIT_RSS, &rlp);
	if (debug)
	    fprintf (stderr, "zawset: starting rlimit cur=%ldm, max=%ldm\n",
		(long)(rlp.rlim_cur == RLIM_INFINITY ? 0 : rlp.rlim_cur) / MB,
		(long)(rlp.rlim_max == RLIM_INFINITY ? 0 : rlp.rlim_max) / MB);

	working_set_size = min (max_wss,
	    rlp.rlim_cur == RLIM_INFINITY ? max_wss : rlp.rlim_cur);

	/* Now try to set the size requested by our caller.  If bestsize was
	 * given as zero, merely return the status values.
	 */
	(*max_size) = min (max_wss,
	    rlp.rlim_max == RLIM_INFINITY ? max_wss : rlp.rlim_max);

	if (*best_size <= 0)
	    *new_size = *old_size = working_set_size;
	else {
	    rlp.rlim_cur = min (*best_size, *max_size);
	    if (rlp.rlim_cur > working_set_size)
		setrlimit (RLIMIT_RSS, &rlp);
	    getrlimit (RLIMIT_RSS, &rlp);
	    *old_size = working_set_size;
	    *new_size = min(*best_size, min(max_wss,
		rlp.rlim_cur == RLIM_INFINITY ? max_wss : rlp.rlim_cur));
	}
	if (debug)
	    fprintf (stderr, "zawset: adjusted rlimit cur=%ldm, max=%ldm\n",
		(long)(rlp.rlim_cur == RLIM_INFINITY ? 0 : rlp.rlim_cur) / MB,
		(long)(rlp.rlim_max == RLIM_INFINITY ? 0 : rlp.rlim_max) / MB);
#endif
	if (debug)
	    fprintf (stderr, "zawset: best=%ldm old=%ldm new=%ldm max=%ldm\n",
		(long)*best_size/MB, (long)*old_size/MB, 
		(long)*new_size/MB, (long)*max_size/MB);

	return (XOK);
}
