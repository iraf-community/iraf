/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>

/*
 * CSALLOC -- Dynamic memory allocation on the SPP stack.
 *
 *			c_smark (&sp)
 *			c_sfree (sp)
 *		 ptr = c_salloc (nbytes)
 *
 * A contiguous segment of dynamic storage may be allocated with C_SALLOC,
 * much as is done with the UNIX emulation procedure MALLOC.  All buffer
 * space allocated on the stack since a call to C_SMARK to mark the position
 * of the stack pointer may be freed in a single call to C_SFREE.
 *
 * The stack is implemented as a linked list of a few large buffers allocated
 * on the heap with MALLOC, each of which normally contains many small buffers
 * allocated with individual C_SALLOC calls.  Stack allocation is very
 * efficient for buffers small enough to fit into a stack segment.  If it is
 * necessary to add a new segment to accomodate a large buffer, the expense is
 * about the same as for a buffer allocation with MALLOC.
 */


/* C_SALLOC -- Allocate a contiguous segment of memory on the stack.  The
 * contents of the buffer will be uninitialized.  The buffer is guaranteed to
 * have at least XDOUBLE alignment with the Mem common.  One extra XCHAR
 * of storage is automatically allocated for the EOS delimiter in the event
 * that the buffer is used to hold a character string (thus it is not necessary
 * to be forever adding +1 in calls to the memory allocator).
 *
 * N.B.: it is a fatal error if storage cannot be allocated on the stack,
 * hence error checking is not necessary.
 */
char *
c_salloc (nbytes)
unsigned nbytes;		/* nbytes of storage to be allocated	*/
{
	XINT	buf;
	XINT	nchars, dtype = TY_CHAR;

	nchars = (nbytes + sizeof(XCHAR)-1) / sizeof(XCHAR);
	SALLOC (&buf, &nchars, &dtype);
	return ((char *)&Memc[buf]);
}


/* C_SMARK -- Mark the position of the stack pointer.
 */
c_smark (sp)
int	*sp;			/* stack pointer is saved here		*/
{
	SMARK (sp);
}


/* C_SFREE -- Free all stack storage allocated since the stack pointer passed as
 * the sole argument was marked by C_SMARK.
 */
c_sfree (sp)
int	sp;			/* saved stack pointer			*/
{
	SFREE (&sp);
}
