include	"../mwcs.h"

# MW_V1TRAN -- Optimized 1D coordinate transformation for an array of points.

procedure mw_v1tranr (a_ct, x1, x2, npts)

pointer	a_ct			#I pointer to CTRAN descriptor
real	x1[ARB]			#I coordinates in input system
real	x2[ARB]			#O coordinates in output system
int	npts

int	i
pointer	ct
real	scale, offset
errchk	mw_ctranr

begin
	# Get real or double version of descriptor.
	ct = CT_R(a_ct)

	scale  = Memr[CT_LTM(ct)]
	offset = Memr[CT_LTV(ct)]

	# Perform the transformation; case LNR is a simple linear transform.
	if (CT_TYPE(ct) == LNR) {
	    do i = 1, npts
		x2[i] = scale * x1[i] + offset
	} else {
	    do i = 1, npts
		call mw_ctranr (a_ct, x1[i], x2[i], 1)
	}
end
