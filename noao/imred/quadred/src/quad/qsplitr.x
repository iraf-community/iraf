include <imhdr.h>
include "quadgeom.h"

# QSPLITx -- Split multi-readout image into separate images one for each 
# readout.

procedure qsplitr (in, out, qg)

pointer	in			#I Image pointer for input image
pointer	out[ARB]		#I Image pointer for output images
pointer	qg			#I pointer to quadgeom structure

long	ivec[IM_MAXDIM], ovec[IM_MAXDIM, QG_MAXAMPS]
int	amp, amp2, x, y, line, junk
pointer	ibuf, obuf, ptr
bool	all_phantom

int	imgnlr(), impnlr()

begin
	# Setup start vectors for sequential reads ...
	call amovkl (long(1), ivec, IM_MAXDIM)
	# ... and writes
	do amp = 1, QG_NAMPS (qg)
	    call amovkl (long(1), ovec[1, amp], IM_MAXDIM)

	do y = 1, QG_NAMPSY(qg) {
	    amp2 = QG_AMP(qg, 1, y)

	    # Check to see if there are any non phantom regions in this tier
	    all_phantom = true
	    do x = 1, QG_NAMPSX(qg) {
		amp = QG_AMP(qg, x, y)
		if (QG_PHANTOM (qg, amp) == NO) {
		    all_phantom = false 
		    break
		}
	    }

	    if (all_phantom) {

		# Reset start vector for reads to skip phantom data
		ivec[2] = ivec[2] + long (QG_NY (qg, amp2))

	    } else {

		do line = 1, QG_NY(qg, amp2) {
		    junk = imgnlr (in, ibuf, ivec)
		    ptr = ibuf
		    do x = 1, QG_NAMPSX(qg) {
			amp = QG_AMP(qg, x, y)
			if (QG_PHANTOM (qg, amp) == NO) {
			    junk = impnlr (out[amp], obuf, ovec[1, amp])
			    call amovr (Memr[ptr], Memr[obuf], QG_NX(qg, amp))
			}
			ptr  = ptr + QG_NX(qg, amp)
		    }
		}
	    }
	}
end

# QJOINx -- Join multi-readout sub-images into a single image.

procedure qjoinr (in, out, qg)

pointer	in[ARB]			#I Image pointer for input images.
pointer	out			#I Image pointer for output image.
pointer	qg			#I pointer to quadgeom structure.

long	ivec[IM_MAXDIM, QG_MAXAMPS], ovec[IM_MAXDIM]
int	amp, amp2, x, y, line, junk
pointer	ibuf, obuf, ptr

int	imgnlr(), impnlr()

begin
	# Setup start vectors for sequential reads ...
	do amp = 1, QG_NAMPS (qg)
	    call amovkl (long(1), ivec[1, amp], IM_MAXDIM)
	# ... and writes
	call amovkl (long(1), ovec, IM_MAXDIM)

	do y = 1, QG_NAMPSY(qg) {
	    amp2 = QG_AMP(qg, 1, y)
	    do line = 1, QG_NY(qg, amp2) {
		junk = impnlr (out, obuf, ovec)
		ptr = obuf
		do x = 1, QG_NAMPSX(qg) {
		    amp = QG_AMP(qg, x, y)
		    junk = imgnlr (in[amp], ibuf, ivec[1, amp])
		    call amovr (Memr[ibuf], Memr[ptr], QG_NX(qg, amp))
		    ptr  = ptr + QG_NX(qg, amp)
		}
	    }
	}
end
