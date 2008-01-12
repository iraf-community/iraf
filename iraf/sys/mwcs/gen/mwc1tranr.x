include	"../mwcs.h"

# MW_C1TRAN -- Optimized 1D coordinate transformation.

real procedure mw_c1tranr (a_ct, x)

pointer	a_ct			#I pointer to CTRAN descriptor
real	x			#I coordinates in input system

real	y
pointer	ct

begin
	# Get real or double version of descriptor.
	ct = CT_R(a_ct)

	# Perform the transformation; LNR is a simple linear transformation.
	if (CT_TYPE(ct) == LNR) {
	    return (Memr[CT_LTM(ct)] * x + Memr[CT_LTV(ct)])
	} else {
	    call mw_ctranr (a_ct, x, y, 1)
	    return (y)
	}
end
