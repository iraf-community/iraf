#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <ctype.h>
#include <fcntl.h>
#include "vmcache.h"

#ifdef sun
#ifndef MS_SYNC
#define MS_SYNC 0  /* SunOS */
#else
#include <sys/systeminfo.h>
#endif
#endif

/*
 * Virtual Memory Cache Controller
 *
 * The VM cache controller manages a region of physical memory in the host
 * computer.  Entire files or file segments are loaded into the cache (into
 * memory).  Space to store such files is made available by the cache
 * controller by freeing the least recently used file segments.  This explicit
 * freeing of space immediately before it is reused for new data prevents
 * (in most cases) the kernel reclaim page daemon from running, causing cached
 * data to remain in memory until freed, and preventing the flow of data
 * through the cache from causing the system to page heavily and steal pages
 * away from the region of memory outside the cache.
 *
 *	      vm = vm_initcache (vm|NULL, initstr)
 *		      vm_status (vm, outbuf, maxch, flags)
 *		  vm_closecache (vm)
 *
 *		      vm_access (vm, fname, mode, flags)
 *		    vm_statfile (vm, fname, flags)
 * 	         vm_setpriority (vm, fname, priority)
 *		   vm_cachefile (vm, fname, flags)
 *		 vm_uncachefile (vm, fname, flags)
 *		 vm_refreshfile (vm, fname, flags)
 *		     vm_cachefd (vm, fd, acmode, flags)
 *		   vm_uncachefd (vm, fd, flags)
 *		   vm_refreshfd (vm, fd, flags)
 *
 *		vm_reservespace (vm, nbytes)
 * 	  addr = vm_cacheregion (vm, fname, fd, offset, nbytes, acmode, flags)
 *	       vm_uncacheregion (vm, fd, offset, nbytes, flags)
 * 	       vm_refreshregion (vm, fd, offset, nbytes)
 *
 *		        vm_sync (vm, fd, offset, nbytes, flags)
 *		       vm_msync (vm, addr, nbytes, flags)
 *
 * Before the VM cache is used it should be initialized with vm_initcache.
 * The string "initstr" may be used to set the size of the cache, enable
 * or disable it (e.g. for performance tests), and set other options.
 * A summary of the VMcache configuration and contents can be generated
 * with vm_status.
 *
 * Files or file segments are loaded into the cache with routines such as
 * vm_cachefile and vm_cacheregion.  Normally, cached files or file segments
 * are reused on a least-recently-used basis.  A file can be locked in the
 * cache by setting the VM_LOCKFILE flag when the file is cached.  This is
 * automatic for vm_cacheregion since the address at which the file is
 * mapped is returned to the caller and hence the file is assumed to be in
 * use.  When a file or region which is locked in the cache is no longer
 * needed one of the "uncache" routines should be called to make the space
 * used by the cached file data available for reuse.  Note that "uncaching"
 * a file or file segment does not immediately remove the data from the
 * cache.  Any "uncached" data normally remains in the cache until the
 * space it uses is needed to load other data.
 *
 * VMcache is a library which is compiled into a process.  This can be
 * incorportated into a server process to manage the VM cache for a
 * group of cooperating processes running on the same computer.  The
 * vmcached program (VMcache daemon) is one such program.
 */


#define	DEF_CACHESIZE	"50%"
#define	DEF_PHYSPAGES	32768
#define READAHEAD	32768
#define	DEF_PRIORITY	1
#define	DEF_REFBASE	1
#define	DEF_TOCK	600
#define	SZ_HASHTBL	16384
#define	SZ_NAME		64
#define	SZ_VALSTR	64
#define SZ_PATHNAME	1024
#define	SZ_LINE		4096

/* Solaris and FreeBSD have a madvise() system call. */
#define HAVE_MADVISE	1

/* Linux provides a madvise call, but it is not implemented and produces
 * a linker warning message.  The madvise call will always fail, but this
 * is harmless (it just means that the cache fails to control paging and
 * everything operates "normally".
 */
#ifdef linux
#undef  HAVE_MADVISE
#define MADV_WILLNEED   3               /* will need these pages */
#define MADV_DONTNEED   4               /* don't need these page */
#endif

#define	isfile(sp,st)	(sp->device == st.st_dev && sp->inode == st.st_ino)


/* Segment descriptor. */
struct segment {
	struct segment *next;
	struct segment *prev;
	struct segment *nexthash;
	int priority;
	int userpri;
	int refcnt;
	int nrefs;
	time_t atime;
	time_t ptime;
	void *addr;
	int fd;
	int acmode;
	unsigned long inode;
	unsigned long device;
	unsigned long offset;
	unsigned long nbytes;
	char *fname;
}; typedef struct segment Segment;

/* Main VMcache descriptor. */
struct vmcache {
	Segment *segment_head, *last_mapped, *segment_tail;
	int cache_initialized;
	int cache_enabled;
	int cachelen;
	unsigned long cacheused;
	unsigned long cachesize;
	unsigned long physmem;
	int lockpages;
	int pagesize;
	int defuserpri;
	int refbase;
	int tock;
}; typedef struct vmcache VMcache;

static debug = 0;
static VMcache vmcache;
static Segment *hashtbl[SZ_HASHTBL];

static int primes[] = {
	101,103,107,109,113,127,131,137,139,
	149,151,157,163,167,173,179,181,191,
};

static vm_readahead();
static vm_uncache();
static Segment *vm_locate();
static int vm_cachepriority();
static int hashint();


/* VM_INITCACHE -- Initialize the VM cache.  A pointer to the cache 
 * descriptor is returned as the function value, or NULL if the cache cannot
 * be initialized.  The argument VM may point to an existing cache which
 * is to be reinitialized, or may be NULL if the cache is being initialized
 * for the first time.
 *
 * The INITSTR argument is used to control all init-time cache options.
 * INITSTR is a sequence of keyword=value substrings.  The recognized options
 * are as follows:
 *
 *	cachesize	total cache size
 *	lockpages	lock pages in memory
 *	enable		enable the cache
 *	debug		turn on debug messages
 *	defpri		default file priority
 *	refbase		number of file references before file is cached
 *	tock		interval (seconds) at which file references degrade
 *	
 * Other options may be added in the future.
 *
 * Keywords which take a size type value (e.g. cachesize) permit values
 * such as "x" (size in bytes), "x%" (X percent of physical memory), "xK"
 * (X kilobytes), or "xM" (X megabytes).  The "x%" notation may not work
 * correctly on all systems as it is not always easy to determine the total
 * physical memory.
 *
 * If the cache is initialized with "enable=no" then all the cache routines
 * will still be called, the cache controller will be disabled.
 */
void *
vm_initcache (vm, initstr)
register VMcache *vm;
char *initstr;
{
	register char *ip, *op;
	char keyword[SZ_NAME], valstr[SZ_NAME];
	char cachesize[SZ_VALSTR], *modchar;
	int percent, enable = 1, lockpages = 0;
	int defuserpri, refbase, tock;
	unsigned long physpages;

	if (debug)
	    fprintf (stderr, "vm_initcache (0x%x, \"%s\")\n", vm, initstr);

	strcpy (cachesize, DEF_CACHESIZE);
	defuserpri = DEF_PRIORITY;
	refbase = DEF_REFBASE;
	tock = DEF_TOCK;

	/* Scan the initialization string.  Initstr may be NULL or the empty
	 * string, if only the defaults are desired.
	 */
	for (ip=initstr;  ip && *ip;  ) {
	    /* Advance to the next keyword=value pair. */
	    while (*ip && (isspace(*ip) || *ip == ','))
		ip++;

	    /* Extract the keyword. */
	    for (op=keyword;  *ip && isalnum(*ip);  )
		*op++ = *ip++;
	    *op = '\0';

	    while (*ip && (isspace(*ip) || *ip == '='))
		ip++;

	    /* Extract the value string. */
	    for (op=valstr;  *ip && (isalnum(*ip) || *ip == '%');  )
		*op++ = *ip++;
	    *op = '\0';

	    if (strcmp (keyword, "cachesize") == 0) {
		strcpy (cachesize, valstr);
	    } else if (strcmp (keyword, "defpri") == 0) {
		defuserpri = atoi (valstr);
	    } else if (strcmp (keyword, "refbase") == 0) {
		refbase = atoi (valstr);
	    } else if (strcmp (keyword, "tock") == 0) {
		tock = atoi (valstr);
	    } else if (strcmp (keyword, "lockpages") == 0) {
		int ch = valstr[0];
		lockpages = (ch == 'y' || ch == 'Y');
	    } else if (strcmp (keyword, "enable") == 0) {
		int ch = valstr[0];
		enable = (ch == 'y' || ch == 'Y');
	    } else if (strcmp (keyword, "debug") == 0) {
		int ch = valstr[0];
		debug = (ch == 'y' || ch == 'Y');
	    }
	}

	/* The VM cache needs to be global for a given host, so we just
	 * use a statically allocated cache descriptor here.  In the most
	 * general case the whole VMcache interface needs to be split into
	 * a client-server configuration, with the cache server managing
	 * virtual memory for a collection of processes.
	 */
	if (!vm)
	    vm = &vmcache;

	/* Shut down the old cache if already enabled. */
	vm_closecache (vm);

	/* There is no good way to guess the total physical memory if this
	 * is not available from the system.  But in such a case the user
	 * can just set the value of the cachesize explicitly in the initstr.
	 */
#ifdef _SC_PHYS_PAGES
	physpages = sysconf (_SC_PHYS_PAGES);
	if (debug) {
	    fprintf (stderr, "total physical memory %d (%dm)\n",
		physpages * getpagesize(),
		physpages * getpagesize() / (1024 * 1024));
	}
#else
	physpages = DEF_PHYSPAGES;
#endif

	vm->cachelen = 0;
	vm->cacheused = 0;
	vm->cache_enabled = enable;
	vm->cache_initialized = 1;
	vm->segment_head = NULL;
	vm->segment_tail = NULL;
	vm->pagesize = getpagesize();
	vm->physmem = physpages * vm->pagesize;
	vm->lockpages = lockpages;
	vm->defuserpri = defuserpri;
	vm->refbase = refbase;
	vm->tock = tock;

	vm->cachesize = percent = strtol (cachesize, &modchar, 10);
	if (modchar == cachesize)
	    vm->cachesize = physpages / 2 * vm->pagesize;
	else if (*modchar == '%')
	    vm->cachesize = physpages * percent / 100 * vm->pagesize;
	else if (*modchar == 'k' || *modchar == 'K')
	    vm->cachesize *= 1024;
	else if (*modchar == 'm' || *modchar == 'M')
	    vm->cachesize *= (1024 * 1024);
	else if (*modchar == 'g' || *modchar == 'G')
	    vm->cachesize *= (1024 * 1024 * 1024);

	return ((void *)vm);
}


/* VM_CLOSECACHE -- Forcibly shutdown a cache if it is already open.
 * All segments are freed and returned to the system.  An attempt is made
 * to close any open files (this is the only case where the VM cache code
 * closes files opened by the caller).
 */
vm_closecache (vm)
register VMcache *vm;
{
	register Segment *sp;
	struct stat st;

	if (debug)
	    fprintf (stderr, "vm_closecache (0x%x)\n", vm);
	if (!vm->cache_initialized)
	    return;

	/* Free successive segments at the head of the cache list until the
	 * list is empty.
	 */
	while (sp = vm->segment_head) {
	    vm_uncache (vm, sp, VM_DESTROYREGION | VM_CANCELREFCNT);

	    /* Since we are closing the cache attempt to forcibly close the
	     * associated file descriptor if it refers to an open file.
	     * Make sure that FD refers to the correct file.
	     */
	    if (fstat (sp->fd, &st) == 0)
		if (isfile(sp,st))
		    close (sp->fd);
	}

	vm->cache_initialized = 0;
}


/* VM_ACCESS -- Access the named file and determine if it is in the cache.
 * Accessing a file via vm_access may cause the file to be loaded into the
 * cache, depending upon the cache tuning parameters and per-file statistics
 * such as the number of past references to the file and how recently they
 * occurred.  A return value of -1 indicates that the named file does not
 * exist or could not be physically accessed.  A value of zero indicates
 * that the file is not cached (is not being managed by the cache).  A value
 * of 1 indicates that the file is being managed by the cache.  Accessing
 * a file updates the reference count and time of last access of the file.
 * and increases the probability that it will be cached in memory.
 *
 * Applications which use VMcache should call vm_access whenever a file is
 * opened or otherwise accessed so that VMcache can keep statistics on file
 * accesses and optimize use of the cache.  If vm_access returns 1 the client
 * should use normal i/o to access the file (normal VM-based file i/o or 
 * mmap).  If vm_access returns 0 VMcache has determined that the file is
 * not worth caching in memory, and some form of direct i/o (bypassing
 * system virtual memory) should be used to access the file.
 *
 * The file must exist at the time that vm_access is called.  If the file
 * already exists and has changed size (e.g., data was appended to the file
 * since the last access) then vm_access will add or remove VM segments to
 * adjust to the new size of the file.  If a new file is being created and
 * it is desired to reserve VM space for the file, two approaches are
 * possible: 1) use seek,write to write a byte where the EOF of the new
 * file will be when all data has been written, so that vm_access will 
 * reserve space for the new file pages; 2) access the short or zero-length
 * file, explicitly reserve unallocated VM space with vm_reservespace,
 * and rely upon vm_access to adjust to the new file size the next time
 * the file is accessed.  Option 1) is the best technique for reserving VM
 * space for large new files which may subsequently be shared by other
 * applications.
 */
vm_access (vm, fname, mode, flags)
register VMcache *vm;
char *fname, *mode;
int flags;
{
	register Segment *sp, *xp;
	Segment *first=NULL, *last=NULL;
	unsigned long offset, x0, x1, vm_offset, vm_nbytes;
	int spaceused, map, n, status=0, fd;
	struct stat st;

	if (debug)
	    fprintf (stderr, "vm_access (0x%x, \"%s\", 0%o)\n",
		vm, fname, flags);
	if (!vm->cache_enabled)
	    return (0);

	if ((fd = open (fname, O_RDONLY)) < 0)
	    return (-1);
	if (fstat (fd, &st) < 0) {
abort:	    close (fd);
	    return (-1);
	}

	/* Align offset,nbytes to map the full file. */
	x0 = offset = 0;
	x0 = (x0 - (x0 % vm->pagesize));
	x1 = offset + st.st_size - 1;
	x1 = (x1 - (x1 % vm->pagesize)) + vm->pagesize - 1;
	vm_offset = x0;
	vm_nbytes = x1 - x0 + 1;

again:
	/* See if the file is already in the cache list. */
	first = last = vm_locate (vm, st.st_ino, st.st_dev);
	for (sp = first;  sp;  sp = sp->nexthash)
	    if (isfile(sp,st))
		last = sp;

	/* If the file is already in the cache check whether it has changed
	 * size and adjust the segment descriptors until they agree with the
	 * current file size before we proceed further.
	 */
	if (last) {
	    if (vm_nbytes < (last->offset + last->nbytes)) {
		/* If the file has gotten smaller uncache the last segment
		 * and start over.  Repeat until the last segment includes EOF.
		 */
		vm_uncache (vm, last, VM_DESTROYREGION|VM_CANCELREFCNT);
		goto again;

	    } else if (vm_nbytes > (last->offset + last->nbytes)) {
		/* If the file has gotten larger cache the new data as a new
		 * file segment.
		 */
		unsigned long offset, nbytes;
		void *addr;

		offset = last->offset + last->nbytes;
		nbytes = vm_nbytes - offset;
		addr = vm_cacheregion (vm, fname, fd,
		    offset, nbytes, last->acmode, VM_DONTMAP);
		if (!addr)
		    goto abort;
		goto again;
	    }
	    /* else fall through */
	} else {
	    /* File is not currently in the cache.  Create a new segment
	     * encompassing the entire file, but don't map it in yet.
	     */
	    void *addr;
	    addr = vm_cacheregion (vm, fname, fd,
		vm_offset, vm_nbytes, VM_READONLY, VM_DONTMAP);
	    if (!addr)
		goto abort;
	    goto again;
	}

	/*
	 * If we get here we have one or more file segments in the cache.
	 * The segments may or may not be mapped and they can be anywhere
	 * in the cache list.  We need to compute the new priority for the
	 * file, relocate the segments in the cache, determine whether or
	 * not the file will be mapped, and adjust the contents of the 
	 * cache accordingly.
	 */

	/* Update the priority of the current file and give all cached file
	 * segments the same reference attributes, since we treating the
	 * entire file as a whole here.
	 */
	first = vm_locate (vm, st.st_ino, st.st_dev);
	first->nrefs++;
	first->atime = time(0);
	first->priority = vm_cachepriority (vm, first);

	for (sp = first;  sp;  sp = sp->nexthash)
	    if (isfile(sp,st)) {
		sp->nrefs = first->nrefs;
		sp->atime = first->atime;
		sp->priority = first->priority;
	    }

	/* Recompute the priorities of all other segments in the head or
	 * "active" area of the cache list.
	 */
	for (sp = vm->segment_head, n=0;  sp;  sp = sp->next, n++) {
	    if (!isfile(sp,st))
		sp->priority = vm_cachepriority (vm, sp);
	    if (sp == vm->last_mapped)
		break;
	}
	for (sp = vm->last_mapped->next;  --n >= 0 && sp;  sp = sp->next)
	    if (!isfile(sp,st))
		sp->priority = vm_cachepriority (vm, sp);

	/* Scan the cache list and determine where in priority order to place
	 * the accessed segment.  Since manually cached segments are always
	 * placed at the head of the list there is no guarantee that the cache
	 * list will be in strict priority order, but this doesn't matter.
	 */
	for (xp = vm->segment_head;  xp;  xp = xp->next)
	    if (first->priority >= xp->priority)
		break;

	/* Relink each segment of the accessed file in just before the lower
	 * priority segment pointed to by XP.  This collects all the file
	 * segments in allocation order within the list.
	 */
	for (sp=first;  sp;  sp = sp->nexthash)
	    if (isfile(sp,st)) {
		/* Unlink segment SP. */
		if (sp->next)
		    sp->next->prev = sp->prev;
		else
		    vm->segment_tail = sp->prev;

		if (sp->prev)
		    sp->prev->next = sp->next;
		else
		    vm->segment_head = sp->next;

		/* Link segment SP in just before XP. */
		sp->next = xp;
		if (xp) {
		    sp->prev = xp->prev;
		    sp->prev->next = sp;
		} else {
		    /* XP is NULL; SP will be the new segment_tail. */
		    sp->prev = vm->segment_tail;
		    vm->segment_tail = sp;
		}

		/* If XP is at the list head SP replaces it at the head. */
		if (vm->segment_head == xp)
		    vm->segment_head = sp;
	    }

	/* Scan the new cache list to see if the accessed file is in the
	 * allocated portion of the list.
	 */
	for (sp = vm->segment_head, spaceused=map=0;  sp;  sp = sp->next) {
	    if (sp == first) {
		map = (spaceused + vm_nbytes <= vm->cachesize);
		break;
	    } else if (sp->addr && !isfile(sp,st)) {
		spaceused += sp->nbytes;
		if (spaceused >= vm->cachesize)
		    break;
	    }
	}

	/* Map the file if it lies above the cutoff point. */
	if (map) {
	    /* Free sufficient memory pages for the new region.  If space
	     * is already allocated to this file don't free it unnecessarily.
	     */
	    for (sp = first, n=vm_nbytes;  sp;  sp = sp->nexthash)
		if (isfile(sp,st) && sp->addr)
		    n -= sp->nbytes;

	    if (n > 0)
		vm_reservespace (vm, n);

	    /* Map the accessed file segments. */
	    for (sp = first, n=vm_nbytes;  sp;  sp = sp->nexthash) {
		if (!isfile(sp,st))
		    continue;

		if (!sp->addr) {
		    void *addr;

		    addr = mmap (NULL, (size_t)sp->nbytes,
			sp->acmode, MAP_SHARED, fd, (off_t)sp->offset);
		    if (!addr) {
			map = 0;
			break;
		    }

		    /* Lock segment in memory if indicated. */
		    if (vm->lockpages && vm->cache_enabled)
			mlock (addr, (size_t) sp->nbytes);

		    vm->cacheused += sp->nbytes;
		    sp->addr = addr;
		}

		/* Preload the accessed file segment. */
		vm_readahead (vm, sp->addr, sp->nbytes);
	    }

	    status = 1;
	}

	close (fd);
	return (status);
}


/* VM_STATFILE -- Determine if the named file is in the cache.  A return
 * value of -1 indicates that the named file does not exist or could not
 * be accessed.  A value of zero indicates that the file is not cached.
 * A value of 1 or more indicates the number of file segments in the cache.
 */
vm_statfile (vm, fname)
register VMcache *vm;
char *fname;
{
	register Segment *sp;
	struct stat st;
	int status=0;

	if (debug)
	    fprintf (stderr, "vm_statfile (0x%x, \"%s\")\n", vm, fname);
	if (!vm->cache_enabled)
	    return (0);

	if (stat (fname, &st) < 0)
	    return (-1);

	for (sp = vm_locate(vm,st.st_ino,st.st_dev);  sp;  sp = sp->nexthash)
	    if (isfile(sp,st))
		status++;

	return (status);
}


/* VM_SETPRIORITY -- Set the user-defined priority of a file already in the
 * cache list from a prior access or cache call.  If the file priority is
 * zero it will never be cached in memory.  A priority of 1 is neutral;
 * higher values increase the cache priority of the file.
 */
vm_setpriority (vm, fname, priority)
register VMcache *vm;
char *fname;
int priority;
{
	register Segment *sp;
	struct stat st;
	int status=0;

	if (priority < 0)
	    priority = 0;

	if (debug)
	    fprintf (stderr, "vm_setpriority (0x%x, \"%s\", %d)\n",
		vm, fname, priority);
	if (!vm->cache_enabled)
	    return (0);

	if (stat (fname, &st) < 0)
	    return (-1);

	for (sp = vm_locate(vm,st.st_ino,st.st_dev);  sp;  sp = sp->nexthash)
	    if (isfile(sp,st))
		sp->userpri = priority;

	return (status);
}


/* VM_CACHEFILE -- Cache an entire named file in the VM cache.
 */
vm_cachefile (vm, fname, flags)
register VMcache *vm;
char *fname;
int flags;
{
	struct stat st;
	int fd;

	if (debug)
	    fprintf (stderr, "vm_cachefile (0x%x, \"%s\", 0%o)\n",
		vm, fname, flags);
	if (!vm->cache_enabled)
	    return (0);

	if ((fd = open (fname, O_RDONLY)) < 0)
	    return (-1);
	if (fstat (fd, &st) < 0)
	    return (-1);

	if (!vm_cacheregion (vm, fname, fd, 0L, st.st_size, VM_READONLY, 0)) {
	    close (fd);
	    return (-1);
	}

	close (fd);
	if (!(flags & VM_LOCKFILE))
	    vm_uncachefile (vm, fname, 0);

	return (0);
}


/* VM_CACHEFD -- Cache an already open file in the VM cache.
 */
vm_cachefd (vm, fd, acmode, flags)
register VMcache *vm;
int acmode;
int flags;
{
	struct stat st;

	if (debug)
	    fprintf (stderr, "vm_cachefd (0x%x, %d, 0%o, 0%o)\n",
		vm, fd, acmode, flags);
	if (!vm->cache_enabled)
	    return (0);

	if (fstat (fd, &st) < 0)
	    return (-1);

	if (!vm_cacheregion (vm, NULL, fd, 0L, st.st_size, acmode, flags))
	    return (-1);

	if (!(flags & VM_LOCKFILE))
	    vm_uncachefd (vm, fd, 0);

	return (0);
}


/* VM_UNCACHEFILE -- Identify a cached file as ready for reuse.  The file
 * remains in the cache, but its space is available for reuse on a least
 * recently used basis.  If it is desired to immediately free the space used
 * by cached file immediately the VM_DESTROYREGION flag may be set in FLAGS.
 */
vm_uncachefile (vm, fname, flags)
register VMcache *vm;
char *fname;
int flags;
{
	register Segment *sp;
	struct stat st;
	int status = 0;

	if (debug)
	    fprintf (stderr, "vm_uncachefile (0x%x, \"%s\", 0%o)\n",
		vm, fname, flags);
	if (!vm->cache_enabled)
	    return (0);

	if (stat (fname, &st) < 0)
	    return (-1);

	for (sp = vm_locate(vm,st.st_ino,st.st_dev);  sp;  sp = sp->nexthash) {
	    if (!isfile(sp,st))
		continue;
	    if (vm_uncache (vm, sp, flags) < 0)
		status = -1;
	}

	return (status);
}


/* VM_UNCACHEFD -- Uncache an entire file identified by its file descriptor.
 * The file remains in the cache, but its space is available for reuse on a
 * least recently used basis.  If it is desired to immediately free the space
 * used by cached file immediately the VM_DESTROYREGION flag may be set in
 * FLAGS.
 */
vm_uncachefd (vm, fd, flags)
register VMcache *vm;
int fd;
int flags;
{
	register Segment *sp;
	struct stat st;
	int status = 0;

	if (debug)
	    fprintf (stderr, "vm_uncachefd (0x%x, %d, 0%o)\n",
		vm, fd, flags);
	if (!vm->cache_enabled)
	    return (0);

	if (fstat (fd, &st) < 0)
	    return (-1);

	for (sp = vm_locate(vm,st.st_ino,st.st_dev);  sp;  sp = sp->nexthash) {
	    if (!isfile(sp,st))
		continue;
	    if (vm_uncache (vm, sp, flags) < 0)
		status = -1;
	}

	return (status);
}


/* VM_REFRESHFILE -- Refresh an entire named file in the VM cache.
 * If the file is not in the cache nothing is done and -1 is returned.
 * If the file is cached it is refreshed, i.e., moved to the head of
 * the cache, reloading any pages not already present in memory.
 */
vm_refreshfile (vm, fname, flags)
register VMcache *vm;
char *fname;
int flags;
{
	struct stat st;
	int fd;

	if (debug)
	    fprintf (stderr, "vm_refreshfile (0x%x, \"%s\", 0%o)\n",
		vm, fname, flags);
	if (!vm->cache_enabled)
	    return (0);

	if ((fd = open (fname, O_RDONLY)) < 0)
	    return (-1);
	if (fstat (fd, &st) < 0)
	    return (-1);

	if (!vm_refreshregion (vm, fd, 0L, st.st_size)) {
	    close (fd);
	    return (-1);
	}

	close (fd);
	return (0);
}


/* VM_REFRESHFD -- Refresh an already open file in the VM cache.
 */
vm_refreshfd (vm, fd, flags)
register VMcache *vm;
int fd;
int flags;
{
	struct stat st;

	if (debug)
	    fprintf (stderr, "vm_refreshfd (0x%x, %d, 0%o)\n",
		vm, fd, flags);
	if (!vm->cache_enabled)
	    return (0);

	if (fstat (fd, &st) < 0)
	    return (-1);

	if (!vm_refreshregion (vm, fd, 0L, st.st_size))
	    return (-1);

	return (0);
}


/* VM_CACHEREGION -- Cache a region or segment of a file.  File segments are
 * removed from the tail of the LRU cache list until sufficient space is 
 * available for the new segment.  The new file segment is then mapped and a
 * request is issued to asynchronously read in the file data.  The virtual
 * memory address of the cached and mapped region is returned.
 *
 * File segments may be redundantly cached in which case the existing
 * mapping is refreshed and the segment is moved to the head of the cache.
 * Each cache operation increments the reference count of the region and
 * a matching uncache is required to eventually return the reference count
 * to zero allowing the space to be reused.  vm_refreshregion can be called
 * instead of cacheregion if all that is desired is to refresh the mapping
 * and move the cached region to the head of the cache.  A single file may
 * be cached as multiple segments but the segments must be page aligned
 * and must not overlap.  The virtual memory addresses of independent segments
 * may not be contiguous in virtual memory even though the corresponding
 * file regions are.  If a new segment overlaps an existing segment it must
 * fall within the existing segment as the size of a segment cannot be changed
 * once it is created.  If a file is expected to grow in size after it is
 * cached, the size of the cached region must be at least as large as the
 * expected size of the file.
 *
 * vm_cacheregion can (should) be used instead of MMAP to map files into
 * memory, if the files will be managed by the VM cache controller.  Otherwise
 * the same file may be mapped twice by the same process, which may use
 * extra virtual memory.  Only files can be mapped using vm_cacheregion, and
 * all mappings are for shared data.
 *
 * If the cache is disabled vm_cacheregion will still map file segments into
 * memory, and vm_uncacheregion will unmap them when the reference count goes
 * to zero (regardless of whether the VM_DESTROYREGION flag is set if the
 * cache is disabled).
 *
 * If write access to a segment is desired the file referenced by FD must
 * have already been opened with write permission.
 */
void *
vm_cacheregion (vm, fname, fd, offset, nbytes, acmode, flags)
register VMcache *vm;
char *fname;
int fd;
unsigned long offset;
unsigned long nbytes;
int acmode, flags;
{
	register Segment *sp, *xp;
	unsigned long x0, x1, vm_offset, vm_nbytes;
	struct stat st;
	int mode;
	void *addr;

	if (debug)
	    fprintf (stderr,
		"vm_cacheregion (0x%x, \"%s\", %d, %d, %d, 0%o, 0%o)\n",
		vm, fname, fd, offset, nbytes, acmode, flags);
	if (fstat (fd, &st) < 0)
	    return (NULL);

	/* Align offset,nbytes to fill the referenced memory pages.
	 */
	x0 = offset;
	x0 = (x0 - (x0 % vm->pagesize));

	x1 = offset + nbytes - 1;
	x1 = (x1 - (x1 % vm->pagesize)) + vm->pagesize - 1;

	vm_offset = x0;
	vm_nbytes = x1 - x0 + 1;

	/* Is this a reference to an already cached segment?
	 */
	for (sp = vm_locate(vm,st.st_ino,st.st_dev);  sp;  sp = sp->nexthash) {
	    if (!isfile(sp,st))
		continue;

	    if (x0 >= sp->offset && x0 < (sp->offset + sp->nbytes))
		if (x1 >= sp->offset && x1 < (sp->offset + sp->nbytes)) {
		    /* New segment lies entirely within an existing one. */
		    vm_offset = sp->offset;
		    vm_nbytes = sp->nbytes;
		    goto refresh;
		} else {
		    /* New segment extends an existing one. */
		    return (NULL);
		}
	}

	mode = PROT_READ;
	if (acmode == VM_READWRITE)
	    mode |= PROT_WRITE;

	if (flags & VM_DONTMAP)
	    addr = NULL;
	else {
	    /* Free sufficient memory pages for the new region.  */
	    vm_reservespace (vm, vm_nbytes);

	    /* Map the new segment, reusing the VM pages freed above. */
	    addr = mmap (NULL,
		(size_t)vm_nbytes, mode, MAP_SHARED, fd, (off_t)vm_offset);
	    if (!addr)
		return (NULL);

	    /* Lock segment in memory if indicated. */
	    if (vm->lockpages && vm->cache_enabled)
		mlock (addr, (size_t) vm_nbytes);

	    vm->cacheused += vm_nbytes;
	}

	/* Get a segment descriptor for the new segment. */
	if (!(sp = (Segment *) calloc (1, sizeof(Segment)))) {
	    if (addr)
		munmap (addr, vm_nbytes);
	    return (NULL);
	}

	vm->cachelen++;
	sp->fd = fd;
	sp->acmode = acmode;
	sp->inode = st.st_ino;
	sp->device = st.st_dev;
	sp->offset = vm_offset;
	sp->nbytes = vm_nbytes;
	sp->addr = addr;
	sp->ptime = time(0);
	sp->userpri = vm->defuserpri;
	if (fname) {
	    sp->fname = (char *) malloc (strlen(fname)+1);
	    strcpy (sp->fname, fname);
	}

	/* Set up the new segment at the head of the cache. */
	sp->next = vm->segment_head;
	sp->prev = NULL;
	if (vm->segment_head)
	    vm->segment_head->prev = sp;
	vm->segment_head = sp;

	/* If there is nothing at the tail of the cache yet this element
	 * becomes the tail of the cache list.
	 */
	if (!vm->segment_tail)
	    vm->segment_tail = sp;
	if (!vm->last_mapped)
	    vm->last_mapped = sp;

	/* Add the segment to the global file hash table.
	 */
	if (xp = vm_locate(vm,st.st_dev,st.st_ino)) {
	    /* The file is already in the hash table.  Add the new segment
	     * to the tail of the file segment list.
	     */
	    while (xp->nexthash)
		xp = xp->nexthash;
	    xp->nexthash = sp;

	} else {
	    /* Add initial file segment to hash table. */
	    int hashval;

	    hashval = hashint (SZ_HASHTBL, (int)st.st_dev, (int)st.st_ino);
	    if (xp = hashtbl[hashval]) {
		while (xp->nexthash)
		    xp = xp->nexthash;
		xp->nexthash = sp;
	    } else
		hashtbl[hashval] = sp;
	}

refresh:
	/* Move a new or existing segment to the head of the cache and
	 * increment the reference count.  Refresh the segment pages if
	 * indicated.
	 */
	if (vm->segment_head != sp) {
	    /* Unlink the list element. */
	    if (sp->next)
		sp->next->prev = sp->prev;
	    if (sp->prev)
		sp->prev->next = sp->next;

	    /* Link current segment at head of cache. */
	    sp->next = vm->segment_head;
	    sp->prev = NULL;
	    if (vm->segment_head)
		vm->segment_head->prev = sp;
	    vm->segment_head = sp;

	    if (!vm->segment_tail)
		vm->segment_tail = sp;
	}

	/* Preload the referenced segment if indicated. */
	if (vm->cache_enabled && !(flags & VM_DONTMAP))
	    vm_readahead (vm, addr, vm_nbytes);

	sp->refcnt++;
	sp->nrefs++;
	sp->atime = time(0);
	sp->priority = vm_cachepriority (vm, sp);

	return ((void *)((char *)addr + (offset - vm_offset)));
}


/* VM_UNCACHEREGION -- Called after a vm_cacheregion to indicate that the
 * cached region is available for reuse.  For every call to vm_cacheregion
 * there must be a corresponding call to vm_uncacheregion before the space
 * used by the region can be reused.  Uncaching a region does not immediately
 * free the space used by the region, it merely decrements a reference 
 * count so that the region can later be freed and reused if its space is
 * needed.  The region remains in the cache and can be immediately reclaimed
 * by a subequent vm_cacheregion.  If it is known that the space will not
 * be reused, it can be freed immediately by setting the VM_DESTROYREGION
 * flag in FLAGS.
 */
vm_uncacheregion (vm, fd, offset, nbytes, flags)
register VMcache *vm;
int fd;
unsigned long offset;
unsigned long nbytes;
int flags;
{
	register Segment *sp;
	unsigned long x0, x1, vm_offset, vm_nbytes;
	struct stat st;
	int mode;

	if (debug)
	    fprintf (stderr, "vm_uncacheregion (0x%x, %d, %d, %d, 0%o)\n",
		vm, fd, offset, nbytes, flags);

	/* Map offset,nbytes to a range of memory pages.
	 */
	x0 = offset;
	x0 = (x0 - (x0 % vm->pagesize));

	x1 = offset + nbytes - 1;
	x1 = (x1 - (x1 % vm->pagesize)) + vm->pagesize - 1;

	vm_offset = x0;
	vm_nbytes = x1 - x0 + 1;

	if (fstat (fd, &st) < 0)
	    return (-1);

	/* Locate the referenced segment.  */
	for (sp = vm_locate(vm,st.st_ino,st.st_dev);  sp;  sp = sp->nexthash)
	    if (isfile(sp,st) && (sp->offset == vm_offset))
		break;
	if (!sp)
	    return (-1);   /* not found */

	return (vm_uncache (vm, sp, flags));
}


/* VM_REFRESHREGION -- Refresh an already cached file region.  The region is
 * moved to the head of the cache and preloading of any non-memory resident
 * pages is initiated.
 */
vm_refreshregion (vm, fd, offset, nbytes)
register VMcache *vm;
int fd;
unsigned long offset;
unsigned long nbytes;
{
	register Segment *sp;
	unsigned long x0, x1, vm_offset, vm_nbytes;
	struct stat st;
	int mode;
	void *addr;

	if (debug)
	    fprintf (stderr, "vm_refreshregion (0x%x, %d, %d, %d)\n",
		vm, fd, offset, nbytes);

	if (!vm->cache_enabled)
	    return (0);

	/* Map offset,nbytes to a range of memory pages.
	 */
	x0 = offset;
	x0 = (x0 - (x0 % vm->pagesize));

	x1 = offset + nbytes - 1;
	x1 = (x1 - (x1 % vm->pagesize)) + vm->pagesize - 1;

	vm_offset = x0;
	vm_nbytes = x1 - x0 + 1;

	if (fstat (fd, &st) < 0)
	    return (-1);

	/* Locate the referenced segment.  */
	for (sp = vm_locate(vm,st.st_ino,st.st_dev);  sp;  sp = sp->nexthash)
	    if (isfile(sp,st) && (sp->offset == vm_offset))
		break;
	if (!sp)
	    return (-1);   /* not found */

	/* Relink the segment at the head of the cache.
	 */
	if (vm->last_mapped == sp && sp->prev)
	    vm->last_mapped = sp->prev;

	if (vm->segment_head != sp) {
	    /* Unlink the list element. */
	    if (sp->next)
		sp->next->prev = sp->prev;
	    if (sp->prev)
		sp->prev->next = sp->next;

	    /* Link current segment at head of cache. */
	    sp->next = vm->segment_head;
	    sp->prev = NULL;
	    if (vm->segment_head)
		vm->segment_head->prev = sp;
	    vm->segment_head = sp;
	}

	sp->nrefs++;
	sp->atime = time(0);
	sp->priority = vm_cachepriority (vm, sp);

	/* Preload any missing pages from the referenced segment. */
	madvise (addr, vm_nbytes, MADV_WILLNEED);

	return (0);
}


/* VM_UNCACHE -- Internal routine to free a cache segment.
 */
static
vm_uncache (vm, sp, flags)
register VMcache *vm;
register Segment *sp;
int flags;
{
	register Segment *xp;
	Segment *first, *last;
	int hashval, status=0, mode;

	if (debug)
	    fprintf (stderr, "vm_uncache (0x%x, 0x%x, 0%o)\n", vm, sp, flags);

	/* Decrement the reference count.  Setting VM_CANCELREFCNT (as in
	 * closecache) causes any references to be ignored.
	 */
	if (--sp->refcnt < 0 || (flags & VM_CANCELREFCNT))
	    sp->refcnt = 0;

	/* If the reference count is zero and the VM_DESTROYREGION flag is
	 * set, try to free up the pages immediately, otherwise merely
	 * decrement the reference count so that it can be reused if it is
	 * referenced before the space it uses is reclaimed by another cache
	 * load.
	 */
	if (!sp->refcnt && ((flags & VM_DESTROYREGION) || !vm->cache_enabled)) {
	    if (vm->cache_enabled)
		madvise (sp->addr, sp->nbytes, MADV_DONTNEED);
	    if (munmap (sp->addr, sp->nbytes) < 0)
		status = -1;
	    vm->cacheused -= sp->nbytes;

	    /* Remove the segment from the file hash table. */
	    first = vm_locate (vm, sp->device, sp->inode);
	    hashval = hashint (SZ_HASHTBL, sp->device, sp->inode);

	    for (xp=first, last=NULL;  xp;  last=xp, xp=xp->nexthash)
		if (xp == sp) {
		    if (last)
			last->nexthash = sp->nexthash;
		    if (hashtbl[hashval] == sp)
			hashtbl[hashval] = sp->nexthash;
		    break;
		}

	    /* Update last_mapped if it points to this segment. */
	    if (vm->last_mapped == sp && sp->prev)
		vm->last_mapped = sp->prev;

	    /* Unlink and free the segment descriptor. */
	    if (sp->next)
		sp->next->prev = sp->prev;
	    if (sp->prev)
		sp->prev->next = sp->next;
	    if (vm->segment_head == sp)
		vm->segment_head = sp->next;
	    if (vm->segment_tail == sp)
		vm->segment_tail = sp->prev;

	    if (sp->fname)
		free (sp->fname);
	    free ((void *)sp);
	    vm->cachelen--;
	}

	return (status);
}


/* VM_RESERVESPACE -- Free space in the cache, e.g. to create space to cache
 * a new file or file segment.  File segments are freed at the tail of the
 * cache list until the requested space is available.  Only segments which 
 * have a reference count of zero are freed.  We do not actually remove
 * segments from the cache here, we just free any mapped pages.
 */
vm_reservespace (vm, nbytes)
register VMcache *vm;
unsigned long nbytes;
{
	register Segment *sp;
	unsigned long freespace = vm->cachesize - vm->cacheused;
	int locked_segment_seen = 0;

	if (debug)
	    fprintf (stderr, "vm_reservespace (0x%x, %d)\n", vm, nbytes);

	if (!vm->cache_enabled)
	    return (0);

	for (sp = vm->last_mapped;  sp;  sp = sp->prev) {
	    freespace = vm->cachesize - vm->cacheused;
	    if (freespace > nbytes)
		break;

	    if (sp->refcnt) {
		locked_segment_seen++;
		continue;
	    } else if (!sp->addr)
		continue;

	    if (debug)
		fprintf (stderr, "vm_reservespace: free %d bytes at 0x%x\n",
		    sp->nbytes, sp->addr);

	    madvise (sp->addr, sp->nbytes, MADV_DONTNEED);
	    munmap (sp->addr, sp->nbytes);
	    vm->cacheused -= sp->nbytes;
	    sp->addr = NULL;

	    if (sp == vm->last_mapped && !locked_segment_seen)
		vm->last_mapped = sp->prev;
	}

	return ((freespace >= nbytes) ? 0 : -1);
}


/* VM_STATUS -- Return a description of the status and contents of the VM
 * cache.  The output is written to the supplied text buffer.
 */
vm_status (vm, outbuf, maxch, flags)
register VMcache *vm;
char *outbuf;
int maxch, flags;
{
	register Segment *sp;
	register char *op = outbuf;
	char buf[SZ_LINE];
	int seg, nseg;

	sprintf (buf, "initialized %d\n", vm->cache_initialized);
	strcpy (op, buf);  op += strlen (buf);

	sprintf (buf, "enabled %d\n", vm->cache_enabled);
	strcpy (op, buf);  op += strlen (buf);

	sprintf (buf, "lockpages %d\n", vm->lockpages);
	strcpy (op, buf);  op += strlen (buf);

	sprintf (buf, "physmem %d\n", vm->physmem);
	strcpy (op, buf);  op += strlen (buf);

	sprintf (buf, "cachesize %d\n", vm->cachesize);
	strcpy (op, buf);  op += strlen (buf);

	sprintf (buf, "cacheused %d\n", vm->cacheused);
	strcpy (op, buf);  op += strlen (buf);

	sprintf (buf, "pagesize %d\n", vm->pagesize);
	strcpy (op, buf);  op += strlen (buf);

	for (nseg=0, sp = vm->segment_head;  sp;  sp = sp->next)
	    nseg++;
	sprintf (buf, "nsegments %d\n", nseg);
	strcpy (op, buf);  op += strlen (buf);

	for (seg=0, sp = vm->segment_head;  sp;  sp = sp->next, seg++) {
	    sprintf (buf, "segment %d inode %d device %d ",
		seg, sp->inode, sp->device);
	    sprintf (buf+strlen(buf), "offset %d nbytes %d refcnt %d %s\n",
		sp->offset, sp->nbytes, sp->refcnt,
		sp->fname ? sp->fname : "[done]");
	    if (op-outbuf+strlen(buf) >= maxch)
		break;
	    strcpy (op, buf);  op += strlen (buf);
	}

	return (op - outbuf);
}


/* VM_LOCATE -- Internal routine to locate the initial segment of a cached
 * file given its device and inode.  NULL is returned if the referenced file
 * has no segments in the cache.
 */
static Segment *
vm_locate (vm, device, inode)
VMcache *vm;
register dev_t device;
register ino_t inode;
{
	register Segment *sp;
	int hashval;

	hashval = hashint (SZ_HASHTBL, device, inode);
	for (sp = hashtbl[hashval];  sp;  sp = sp->nexthash)
	    if (sp->device == device && sp->inode == inode)
		return (sp);

	return (NULL);
}


/* HASHINT -- Hash a pair of integer values.  An integer hash value in the
 * range 0-nthreads is returned.
 */
static int
hashint (nthreads, w1, w2)
int nthreads;
register int w1, w2;
{
	unsigned int h1, h2;
	register int i=0;

	h1 = (((w1 >> 16) * primes[i++]) ^ (w1 * primes[i++]));
	h2 = (((w2 >> 16) * primes[i++]) ^ (w2 * primes[i++]));

	return ((h1 ^ h2) % nthreads);
}


/* VM_CACHEPRIORITY -- Compute the cache priority of a file segment.  Various
 * heuristics are possible for computing the cache priority of a segment.
 * The one used here assigns a priority which scales with a user defined
 * per-file priority, and which is a function of the number of recent
 * references to the file.  The USERPRI, REFBASE, and TOCK parameters can
 * be used (possibly in combination with manual cache control commands) to
 * tune the algorithm for the expected file activity.
 */
static int
vm_cachepriority (vm, sp)
register VMcache *vm;
register Segment *sp;
{
	register int priority = 0;
	time_t curtime = time(NULL);

	/* A user-specified priority of zero overrides. */
	if (sp->userpri <= 0)
	    return (0);

	/* Compute the cache priority for the segment. */
	priority = (sp->nrefs - vm->refbase) -
	    ((curtime - sp->atime) / vm->tock);
	if (priority < 0)
	    priority = 0;
	priority *= sp->userpri;

	/* Degrade nrefs every tock seconds if the file is not being
	 * accessed.
	 */
	if (sp->atime > sp->ptime)
	    sp->ptime = sp->atime;
	else if ((curtime - sp->ptime) > vm->tock) {
	    sp->nrefs -= ((curtime - sp->ptime) / vm->tock);
	    if (sp->nrefs < 0)
		sp->nrefs = 0;
	    sp->ptime = curtime;
	}

	return (priority);
}


/* VM_SYNC -- Sync (update on disk) any pages of virtual memory mapped to
 * the given region of the given file.  If nbytes=0, any mapped regions of
 * the given file are synced.  If the VM_ASYNC flag is set the sync operation
 * will be performed asynchronously and vm_sync will return immediately,
 * otherwise vm_sync waits for the synchronization operation to complete.
 */
vm_sync (vm, fd, offset, nbytes, flags)
register VMcache *vm;
int fd;
unsigned long offset;
unsigned long nbytes;
int flags;
{
	register Segment *sp;
	unsigned long x0, x1, vm_offset, vm_nbytes;
	int syncflag, status = 0;
	struct stat st;

	if (debug)
	    fprintf (stderr, "vm_sync (0x%x, %d, %d, %d, 0%o)\n",
		vm, fd, offset, nbytes, flags);
	if (!vm->cache_enabled)
	    return (0);

	/* Map offset,nbytes to a range of memory pages.
	 */
	x0 = offset;
	x0 = (x0 - (x0 % vm->pagesize));

	x1 = offset + nbytes - 1;
	x1 = (x1 - (x1 % vm->pagesize)) + vm->pagesize - 1;

	vm_offset = x0;
	vm_nbytes = x1 - x0 + 1;

#ifdef sun
#ifdef _SYS_SYSTEMINFO_H
	/* This is a mess.  The values of MS_SYNC,MS_ASYNC changed between
	 * Solaris 2.6 and 2.7.  This code assumes that the system is
	 * being built on a Solaris 2.7 or greater system, but the wired-in
	 * values below allow the executable to be run on earlier versions.
	 */
	{
	    char buf[SZ_NAME];   /* e.g. "5.7" */

	    sysinfo (SI_RELEASE, buf, SZ_NAME);
	    if (buf[0] >= '5' && buf[2] >= '7')
		syncflag = (flags & VM_ASYNC) ? MS_ASYNC : MS_SYNC;
	    else
		syncflag = (flags & VM_ASYNC) ? 0x1 : 0x0;
	}
#else
	syncflag = (flags & VM_ASYNC) ? MS_ASYNC : MS_SYNC;
#endif
#else
	syncflag = (flags & VM_ASYNC) ? MS_ASYNC : MS_SYNC;
#endif

	if (fstat (fd, &st) < 0)
	    return (-1);

	/* Locate the referenced segment.  */
	for (sp = vm->segment_head;  sp;  sp = sp->next) {
	    if (!isfile(sp,st))
		continue;

	    if (!nbytes || sp->offset == vm_offset)
		if (msync (sp->addr, sp->nbytes, syncflag))
		    status = -1;
	}

	return (status);
}


/* VM_MSYNC -- Sync the given region of virtual memory.  This routine does
 * not require that the caller know the file to which the memory is mapped.
 * If the VM_ASYNC flag is set the sync operation will be performed
 * asynchronously and vm_sync will return immediately, therwise vm_sync waits
 * for the synchronization operation to complete.
 */
vm_msync (vm, addr, nbytes, flags)
register VMcache *vm;
void *addr;
unsigned long nbytes;
int flags;
{
	register Segment *sp;
	unsigned long addr1, addr2;
	int syncflag;

	if (debug)
	    fprintf (stderr, "vm_msync (0x%x, 0x%x, %d, 0%o)\n",
		vm, addr, nbytes, flags);

	/* Align the given address region to the page boundaries.
	 */
	addr1 = ((long)addr - ((long)addr % vm->pagesize));
	addr2 = (long)addr + nbytes - 1;
	addr2 = (addr2 - (addr2 % vm->pagesize)) + vm->pagesize - 1;
	syncflag = (flags & VM_ASYNC) ? MS_ASYNC : MS_SYNC;

	return (msync ((void *)addr1, addr2 - addr1 + 1, syncflag));
}


/* VM_READAHEAD -- Internal routine used to request that a segment of file
 * data be preloaded.
 */
static
vm_readahead (vm, addr, nbytes)
register VMcache *vm;
void *addr;
unsigned long nbytes;
{
	register int n, nb;
	int chunk = READAHEAD * vm->pagesize;
	unsigned long buf = (unsigned long) addr;

	/* Break large reads into chunks of READAHEAD memory pages.  This
	 * increases the chance that file access and computation can overlap
	 * the readahead i/o.
	 */
	for (n=0;  n < nbytes;  n += chunk) {
	    nb = nbytes - n;
	    if (nb > chunk)
		nb = chunk;
	    madvise ((void *)(buf + n), nb, MADV_WILLNEED);
	}
}
