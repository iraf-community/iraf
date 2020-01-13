/*
 * VMCACHE.H -- Public definitions for the VMcache interface.
 */

#define DEF_VMSOCK      	8677
#define ENV_VMSOCK		"VMPORT"

#define	VM_READONLY		0001
#define	VM_READWRITE		0002
#define	VM_WRITEONLY		0004
#define	VM_ASYNC		0010
#define	VM_SYNC			0020
#define	VM_LOCKFILE		0040
#define	VM_DESTROYREGION	0100
#define	VM_CANCELREFCNT		0200
#define	VM_DONTMAP		0400


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


void	*vm_initcache (register VMcache *vm, char *initstr);
void	*vm_cacheregion (register VMcache *vm, char *fname, int fd,
            unsigned long offset, unsigned long nbytes, int acmode, int flags);
