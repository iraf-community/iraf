include <imhdr.h>

# PV_GMEM -- Determine how much memory we can get and actually use for the
# volume rotation sequence.  We only allocate actual memory temporarily in
# order to see how much is really available; IMIO will later take care of
# the actual io buffer allocation.

define	DECR_MEM		0.8	# decrement mem by magic factor


procedure pv_gmem (im1, im2, use_both, verbose, max_ws, len_x, oldsize)
pointer	im1			# Input 3d image.
pointer	im2			# Output projected image(s)
bool	use_both		# Use both opacity and intensity voxels
int	verbose			# Report memory usage?
int	max_ws			# Maximum working set to allocate
int	len_x			# (output) safe amount of memory to use
int	oldsize			# (output) old memory to be reset at termination

int	intype, outtype, reqmem, gotmem, needmem, maxsize
int	yzslice_pix, yzreq
pointer	buf_in
bool	topped_out

int	begmem(), sizeof()
errchk	begmem(), malloc()

begin
	# See how much memory we can get; if we cannot get whole input image
	# into memory, do it in chunks of yz slices.
	intype = IM_PIXTYPE(im1)
	outtype = IM_PIXTYPE(im2)
	reqmem = IM_LEN(im1,1) * IM_LEN(im1,2) * IM_LEN(im1,3)
	reqmem = reqmem * sizeof (intype)
	if (use_both)
	    reqmem = 2 * reqmem

	# Add output buffer.
	reqmem = reqmem + IM_LEN(im2,1) * sizeof (outtype)

	# Decrease to max_ws (a task parameter in CHAR units).
	reqmem = min (reqmem, max_ws)

	repeat {
	    iferr (gotmem = begmem (reqmem, oldsize, maxsize)) {
		reqmem = reqmem * DECR_MEM
		if (verbose == YES) {
		    call eprintf ("ERR gotmem=begmem(); retrying at size %d\n")
			call pargi (reqmem)
		}
	    } else {
		if (verbose == YES) {
		    call eprintf ("gotmem=%d, oldsize=%d, maxsize=%d\n")
		    call pargi (gotmem)
		    call pargi (oldsize)
		    call pargi (maxsize)
		}
		break
	    }
	}
	
	# Make sure it is really available, and if not, decrement to largest
	# number of yz slices possible.
	needmem = gotmem
	yzslice_pix = IM_LEN(im1,2) * IM_LEN(im1,3)
	yzreq = yzslice_pix * sizeof(intype)
	if (yzreq - IM_LEN(im1,1) * sizeof(TY_REAL) > needmem) {
	    call eprintf ("Not enough memory for 1 yz slice of input image\n")
	    call error (0, "Out of memory")
	}
	topped_out = false
	repeat {
	    iferr (call malloc (buf_in, needmem, intype)) {
		needmem = needmem - yzreq
		if (needmem < yzreq) {
		    call eprintf ("Had to decrease memory too much")
		    call error (0, "Memory allocation error (yzslice_pix)")
		}
		topped_out = true
	    } else {
		call mfree (buf_in, intype)
		break
	    }
	}

	# Experiments show that horrible things happen if we actually use
	# this much memory.  Decrease by magic factor.
	if (topped_out) {
	    call fixmem (max (needmem, oldsize))
	    if (verbose == YES) {
		call eprintf ("Had to decrease memory for malloc().")
		call eprintf ("  Working set now %d\n")
		    call pargi (needmem)
	    }
	    needmem = needmem * DECR_MEM
	    if (verbose == YES) {
		call eprintf ("Remaining memory for image buffers = %d\n")
		    call pargi (needmem)
	    }
	}
	if (needmem < yzreq) {
	    call eprintf ("Not enough memory for 1 yz slice of input image\n")
	    call error (0, "Out of memory")
	}

	# We return the number of columns to gulp from the input image at one
	# time and oldmem so the task can release its memory on completion.
	len_x = needmem / yzreq
end
