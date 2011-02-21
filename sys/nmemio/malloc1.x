# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>

.help malloc1
.nf -------------------------------------------------------------------------
MEMIO -- Internal data structures.

If "p" is the pointer returned by malloc, the first element of storage is
referenced by the expression "Mem_[p]", where the underscore is replaced
by the appropriate type suffix.  A pointer to an object of one data type
is NOT equivalent to a pointer to another data type, even if both pointers
reference the same physical address.

The actual physical address of the physical buffer area allocated is stored
in the integer cell immediately preceeding the buffer returned to the user.
If this cell is corrupted, the condition will later be detected, and a fatal
error ("memory corrupted") will result.

For example, for a machine with a 4 byte integer, the first part of the
buffer area might appear as follows (the first few unused cells may or
may not be needed to satisfy the alignment criteria):

      offset			allocation
	
	0		start of the physical buffer (from zmaloc)
	1
	2
	3
	4		byte 1 of saved fwa (address of cell 0)
	5		byte 2	"   "    "
	6		byte 3	"   "    "
	7		byte 4	"   "    "
	8		first cell available to user (maximum alignment)


New Scheme allowing for 64-bit architectures(10/15/2009):

      offset			allocation
	
	 0		start of the physical buffer (from zmaloc)
	 0-7		alignment space
	 8-15		bytes 1-8 of saved fwa (address of cell 0)
	16-23		Bytes 1-8 of upper sentinal location
	24-31		Bytes 1-8 of pointer type
	32-39		Bytes 1-8 of nbytes of storage
	40-47		Bytes 1-8 of lower sentinal value
	48		first cell available to user (maximum alignment)
	N+1		Bytes 1-8 of upper sentinal value

   Total storage required is

      [ ((nelems + 1) * sizeof(dtype)) + sz-align + (5 * SZ_INT) ] * SZB_CHAR
	

MALLOC, given the CHAR address of the buffer allocated by the z-routine,
adds space for the saved fwa (an integer), and determines the address of the
next cell which is sufficiently aligned, relative to the Mem common.  This
cell marks the start of the user buffer area.  The buffer fwa is saved in an
integer location preceding the "first cell".

MFREE, called with a pointer to the buffer to be returned, fetches the location
of the physical buffer from the save area.  If this does not agree with the
buffer pointer, either (1) the buffer pointer is invalid or of the wrong
datatype, or (2), the save area has been overwritten (memory has been
corrupted).  If everything checks out, the buffer fwa is passed to a z-routine
to free the physical buffer space.

TODO:	- Add debugging routine to summarize allocated buffer space and
		check for buffer overruns (add sentinel at end of buffer).
	- Keep track of buffers allocated while a program is running and
		return at program termination, like closing open files.
.endhelp ---------------------------------------------------------------------



# MALLOC1 -- Low level procedure which does the actual buffer allocation.

int procedure malloc1 (output_pointer, nelems, dtype, sz_align, fwa_align)

pointer	output_pointer		# buffer pointer (output)
int	nelems			# number of elements of storage required
int	dtype			# datatype of the storage elements
int	sz_align		# number of chars of alignment required
int	fwa_align		# address to which buffer is to be aligned

int	fwa, nchars, nuser, status
pointer	cp

int	sizeof()
pointer	msvfwa(), coerce()

include "nmemio.com"

begin
	if (dtype == TY_CHAR)
	    nuser = nelems + 1  			# add space for EOS
	else
	    nuser = nelems * sizeof (dtype) + 1
	nchars = nuser + (8 * SZ_INT) + sz_align

	call zmaloc (fwa, (nchars * SZB_CHAR), status)

	if (status == ERR)
	    return (ERR)

	else {
	    output_pointer = msvfwa (fwa, dtype, nelems, sz_align, fwa_align)

	    if (mclear > 0) {
		# Clear the user area only.
	        cp = coerce (output_pointer, dtype, TY_CHAR)
                call aclrc (Memc[cp], (nuser * SZB_CHAR))
	    }

	    # Update usage stats.
	    if (mreport > 0) {
	        nalloc = nalloc + 1
	        mem_used = mem_used + (nchars * SZB_CHAR)
	        if ((nchars * SZB_CHAR) > max_alloc)
		    max_alloc = (nchars * SZB_CHAR)
	    }

	    # Save the ptr in the GC buffer.
	    if (mcollect > 0)
		call mgc_save (output_pointer, dtype)

	    return (OK)
	}
end
