# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<syserr.h>

# KREALLOC -- Change the size of a previously allocated buffer, moving the
# buffer if necessary.  If there is no old buffer (NULL pointer) simply
# allocate a new buffer.  This routine is equivalent to REALLOC except that it
# merely returns ERR as the function value if an error occurs.
# 
# Buffer reallocation or resizing can always be implemented by allocating a new
# buffer, copying the contents of the old buffer to the new buffer, and then
# deleting the old buffer.  Nonetheless we use a OS entry point to do the actual
# reallocation, because often it will be possible to change the size of a buffer
# without moving it, particularly when decreasing the size of the buffer.
# 
# Allowing the OS to move a buffer causes problems due to the difference in
# alignment criteria imposed by the IRAF pointer scheme, which enforces
# stringent alignment criteria, versus OS memory allocation schemes which
# typically only align on word or longword boundaries.  Therefore we must
# check the offset of the data area after reallocation, possibly shifting
# the contents of data area up or down a few chars to reestablish alignment
# with Mem.

int procedure krealloc (ptr, a_nelems, a_dtype)

pointer	ptr			# buffer to be reallocated
int	a_nelems		# new size of buffer
int	a_dtype			# buffer datatype

pointer	dataptr
int	nelems, dtype, nchars, old_fwa, new_fwa
int	char_shift, old_char_offset, new_char_offset
int	status, locbuf, loc_Mem

int	mgtfwa(), sizeof(), kmalloc()
pointer	mgdptr(), msvfwa(), coerce()
data	loc_Mem /NULL/
int	zrtadr()
include	"memdbg.com"

begin
	# Copy over the number of elements and the data type in case they are
	# located in the block of memory we are reallocating.

	nelems = a_nelems
	dtype  = a_dtype

	if (ptr == NULL) {
	    return (kmalloc (ptr, nelems, dtype))

	} else {
	    if (dtype == TY_CHAR)
		nchars = nelems + 1 + SZ_INT + SZ_MEMALIGN
	    else
		nchars = nelems * sizeof(dtype) + SZ_INT + SZ_MEMALIGN
	    old_fwa = mgtfwa (ptr, dtype)
	    new_fwa = old_fwa

	    # Change the buffer size; any error is fatal.
	    call zraloc (new_fwa, nchars * SZB_CHAR, status)
	    if (status == ERR) {
		ptr = NULL
		return (ERR)
	    }

	    if (retaddr == 0)
		retaddr = zrtadr()
	    if (new_fwa != old_fwa) {
		call zmemlg (old_fwa, retaddr, 'F', 1,
		    "realloc frees old buf", 0, 0)
		call zmemlg (new_fwa, retaddr, 'A', 1,
		    "realloc allocs new buf", nelems * sizeof(dtype), 0)
	    } else {
		call zmemlg (old_fwa, retaddr, 'R', 1,
		    "realloc %d", nelems * sizeof(dtype), 0)
	    }
	    retaddr = 0

	    # Compute the char offset of the old data area within the original
	    # buffer; zraloc() guarantees that the old data will have the same
	    # offset in the new buffer.  Compute the char offset of the new
	    # data area.  These need not be the same due to the OS allocating
	    # the new buffer to alignment criteria less than those required
	    # by MEMIO.

	    call zlocva (Memc[coerce(ptr,dtype,TY_CHAR)], locbuf)
	    old_char_offset = (locbuf - old_fwa)

	    # We must compute a pointer to the data area within the new
	    # buffer before we can compute the char offset of the new data
	    # area within the new buffer.

	    if (loc_Mem == NULL)
		call zlocva (Memc, loc_Mem)

	    dataptr = mgdptr (new_fwa, TY_CHAR, SZ_MEMALIGN, loc_Mem)
	    call zlocva (Memc[dataptr], locbuf)
	    new_char_offset = (locbuf - new_fwa)

	    # Shift the old data to satisfy the new alignment criteria,
	    # if necessary.

	    char_shift = (new_char_offset - old_char_offset)
	    if (char_shift != 0) {
		call amovc (Memc[dataptr - char_shift], Memc[dataptr],
		    nelems * sizeof(dtype))
	    }

	    # Save the fwa of the OS buffer in the buffer header, and return
	    # new pointer to user.

	    ptr = msvfwa (new_fwa, dtype, SZ_MEMALIGN, loc_Mem)
	}

	return (OK)
end
