include	"../mwcs.h"

# MW_V1TRAN -- Optimized 1D coordinate transformation for an array of points.

procedure mw_v1trand (a_ct, x1, x2, npts)

pointer	a_ct			#I pointer to CTRAN descriptor
double	x1[ARB]			#I coordinates in input system
double	x2[ARB]			#O coordinates in output system
int	npts

int	i
pointer	ct
double	scale, offset
errchk	mw_ctrand

begin
	# Get real or double version of descriptor.
	ct = CT_D(a_ct)

	scale  = Memd[CT_LTM(ct)]
	offset = Memd[CT_LTV(ct)]

	# Perform the transformation; case LNR is a simple linear transform.
	if (CT_TYPE(ct) == LNR) {
	    do i = 1, npts
		x2[i] = scale * x1[i] + offset
	} else {
	    do i = 1, npts
		call mw_ctrand (a_ct, x1[i], x2[i], 1)
	}
end
