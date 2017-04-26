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

MALLOC, given the CHAR address of the buffer allocated by the z-routine,
adds space for the saved fwa (an integer), and determines the address of the
next cell which is sufficiently aligned, relative to the Mem common.  This
cell marks the start of the user buffer area.  The buffer fwa is saved in the
integer location immediately preceding the "first cell".

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

int	fwa, nchars, status
int	sizeof()
pointer	msvfwa()

begin
	if (dtype == TY_CHAR)
	    nchars = nelems + 1 + SZ_INT + sz_align	# add space for EOS
	else
	    nchars = nelems * sizeof (dtype) + SZ_INT + sz_align

	call zmaloc (fwa, nchars * SZB_CHAR, status)

	if (status == ERR)
	    return (ERR)
	else {
	    output_pointer = msvfwa (fwa, dtype, sz_align, fwa_align)
	    return (OK)
	}
end
