#include <stdio.h>
#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

/* ZMALOC -- Allocate space on the heap.  NULL is returned if the buffer
 * cannot be allocated, otherwise the address of the buffer is returned
 * in "buf".
 */
ZMALOC (buf, nbytes, status)
XINT	*buf;			/* receives address of buffer		*/
XINT	*nbytes;		/* buffer size, machine bytes		*/
XINT	*status;		/* status return: XOK or XERR		*/
{
	register char *bufptr;
	char    *malloc();

	bufptr = malloc ((int)*nbytes);
	if (bufptr != NULL) {
	    *buf = ADDR_TO_LOC (bufptr);
	    *status = XOK;
	} else
	    *status = XERR;
}

#ifdef SKIPTHISFORCONVEX
#ifndef DEBUGMEM

/* We do not use the memory allocator used in 4.2BSD and 4.3BSD UNIX because
 * the storage allocation scheme used with this allocator (buffer sizes are
 * restricted to 2**N in size) is extremely inefficient for very large buffers.
 */
#ifdef BSD42
#define	DEBUGMEM
#endif
#ifdef BSD43
#define	DEBUGMEM
#endif

#endif

#ifdef DEBUGMEM

/* The source for the memory allocation routine is explicitly included
 * here because we need to access storage local to the allocator for
 * debugging purposes.  The 4.1BSD malloc.c source is unchanged.
 *
 * IMPORTANT NOTE -- Use of this memory allocator may cause conflicts on some
 * systems when linking with host libraries which assume the existence of a
 * host system memory allocator with different properties and possibly
 * additional entry points.
 */
/* --------------------------- malloc.c ------------------------------- */
#define INT int
#define ALIGN int
#define NALIGN 1
#define WORD sizeof(union store)
#define BLOCK 1024	/* a multiple of WORD*/
#define BUSY 1
#define testbusy(p) ((INT)(p)&BUSY)
#define setbusy(p) (union store *)((INT)(p)|BUSY)
#define clearbusy(p) (union store *)((INT)(p)&~BUSY)

union store { union store *ptr;
	      ALIGN dummy[NALIGN];
	      int calloc;	/*calloc clears an array of integers*/
};

static	union store allocs[2];	/*initial arena*/
static	union store *allocp;	/*search ptr*/
static	union store *alloct;	/*arena top*/
static	union store *allocx;	/*for benefit of realloc*/
char	*sbrk();
/* --------------------------- end malloc.c ------------------------------- */


/* ZMEMCK -- This is a very highly system dependent debugging routine
 *   provided to check the 4.1BSD UNIX memory allocator buffer list for
 *   logical consistency.  The buffer list is a circular list of pointers to
 *   pointers.  Each pointer is stored in the physical integer location 
 *   immediately preceeding the buffer itself.  The first bit of the pointer
 *   is set if the buffer is "busy" (in use).  Memory which is not used by
 *   the allocator is always marked busy, so that it cannot be allocated.
 *   The head of the list is given by the external variable allocs[0].
 *   The external variable allocp cycles around the list as malloc performs
 *   a circular search for an unused buffer.
 * Our function is to traverse the list starting at allocs[0], printing the
 *   location and status of each buffer on the standard error output.
 * This routine is not required to run IRAF; if it is difficult to implement,
 *   merely leave the entry point in as a nop.
 */

#define	SZB_WORD	4
#define	SZ_NUMBUF	20
#define	BASE		8

ZMEMCK()
{
	register union store *p;

	for (p = allocs;  clearbusy(p->ptr) > p;  p = clearbusy(p->ptr)) {
	    /* Encode the buffer pointer in octal and print on stderr,
	     * followed by the buffer length in bytes and the buffer
	     * status (busy or not busy).
	     */
	    putc ('\t', stderr);
	    zmem_putint ((int)p + SZB_WORD, stderr);
	    putc ('\t', stderr);
	    zmem_putint (clearbusy(p->ptr) - p - SZB_WORD, stderr);
	    fputs (" bytes, ", stderr);
	    putc ('\t', stderr);
	    if (testbusy (p->ptr))
		fputs ("busy\n", stderr);
	    else
		fputs ("not busy\n", stderr);
	}

	fputs ("\thigh: ", stderr);
	zmem_putint ((int)p + SZB_WORD, stderr);
	putc ('\n', stderr);
	p = clearbusy(p->ptr);
	fputs ("\tstop: ", stderr);
	zmem_putint ((int)p + SZB_WORD, stderr);
	putc ('\n', stderr);
}



/* ZMEM_PUTINT -- Encode a number in octal and write to the output file.
 */
zmem_putint (num, fp)
register unsigned int num;
FILE	*fp;
{
	register char *op, *ip;
	char	numbuf[SZ_NUMBUF];

	for (op=numbuf;  num != 0;  num /= BASE)
	    *op++ = (num % BASE) + '0';
	*op = EOS;

	for (ip = op-1;  ip >= numbuf;  --ip)
	    putc (*ip, fp);
}

/* --------------------------- malloc.c ----------------------------------- */
#ifdef debug
#define ASSERT(p) if(!(p))botch("p");else
botch(s)
char *s;
{
	printf("assertion botched: %s\n",s);
	abort();
}
#else
#define ASSERT(p)
#endif

/*	avoid break bug */
#ifdef pdp11
#define GRANULE 64
#else
#define GRANULE 0
#endif

/* C storage allocator
 * circular first-fit strategy
 * works with noncontiguous, but monotonically linked, arena
 * each block is preceded by a ptr to the (pointer of) 
 * the next following block
 * blocks are exact number of words long 
 * aligned to the data type requirements of ALIGN
 * pointers to blocks must have BUSY bit 0
 * bit in ptr is 1 for busy, 0 for idle
 * gaps in arena are merely noted as busy blocks
 * last block of arena (pointed to by alloct) is empty and
 * has a pointer to first
 * idle blocks are coalesced during space search
 *
 * a different implementation may need to redefine
 * ALIGN, NALIGN, BLOCK, BUSY, INT
 * where INT is integer type to which a pointer can be cast
 */
char *
malloc(nbytes)
unsigned nbytes;
{
	register union store *p, *q;
	register nw;
	static temp;	/*coroutines assume no auto*/

	if(allocs[0].ptr==0) {	/*first time*/
		allocs[0].ptr = setbusy(&allocs[1]);
		allocs[1].ptr = setbusy(&allocs[0]);
		alloct = &allocs[1];
		allocp = &allocs[0];
	}
	nw = (nbytes+WORD+WORD-1)/WORD;
	ASSERT(allocp>=allocs && allocp<=alloct);
	ASSERT(allock());
	for(p=allocp; ; ) {
		for(temp=0; ; ) {
			if(!testbusy(p->ptr)) {
				while(!testbusy((q=p->ptr)->ptr)) {
					ASSERT(q>p&&q<alloct);
					p->ptr = q->ptr;
				}
				if(q>=p+nw && p+nw>=p)
					goto found;
			}
			q = p;
			p = clearbusy(p->ptr);
			if(p>q)
				ASSERT(p<=alloct);
			else if(q!=alloct || p!=allocs) {
				ASSERT(q==alloct&&p==allocs);
				return(NULL);
			} else if(++temp>1)
				break;
		}
		temp = ((nw+BLOCK/WORD)/(BLOCK/WORD))*(BLOCK/WORD);
		q = (union store *)sbrk(0);
		if(q+temp+GRANULE < q) {
			return(NULL);
		}
		q = (union store *)sbrk(temp*WORD);
		if((INT)q == -1) {
			return(NULL);
		}
		ASSERT(q>alloct);
		alloct->ptr = q;
		if(q!=alloct+1)
			alloct->ptr = setbusy(alloct->ptr);
		alloct = q->ptr = q+temp-1;
		alloct->ptr = setbusy(allocs);
	}
found:
	allocp = p + nw;
	ASSERT(allocp<=alloct);
	if(q>allocp) {
		allocx = allocp->ptr;
		allocp->ptr = p->ptr;
	}
	p->ptr = setbusy(allocp);
	return((char *)(p+1));
}

/*	freeing strategy tuned for LIFO allocation
*/
free(ap)
register char *ap;
{
	register union store *p = (union store *)ap;

	ASSERT(p>clearbusy(allocs[1].ptr)&&p<=alloct);
	ASSERT(allock());
	allocp = --p;
	ASSERT(testbusy(p->ptr));
	p->ptr = clearbusy(p->ptr);
	ASSERT(p->ptr > allocp && p->ptr <= alloct);
}

/*	realloc(p, nbytes) reallocates a block obtained from malloc()
 *	and freed since last call of malloc()
 *	to have new size nbytes, and old content
 *	returns new location, or 0 on failure
*/

char *
realloc(p, nbytes)
register union store *p;
unsigned nbytes;
{
	register union store *q;
	union store *s, *t;
	register unsigned nw;
	unsigned onw;

	if(testbusy(p[-1].ptr))
		free((char *)p);
	onw = p[-1].ptr - p;
	q = (union store *)malloc(nbytes);
	if(q==NULL || q==p)
		return((char *)q);
	s = p;
	t = q;
	nw = (nbytes+WORD-1)/WORD;
	if(nw<onw)
		onw = nw;
	while(onw--!=0)
		*t++ = *s++;
	if(q<p && q+nw>=p)
		(q+(q+nw-p))->ptr = allocx;
	return((char *)q);
}

#ifdef debug
allock()
{
#ifdef longdebug
	register union store *p;
	int x;
	x = 0;
	for(p= &allocs[0]; clearbusy(p->ptr) > p; p=clearbusy(p->ptr)) {
		if(p==allocp)
			x++;
	}
	ASSERT(p==alloct);
	return(x==1|p==allocp);
#else
	return(1);
#endif
}
#endif
#endif
#endif
