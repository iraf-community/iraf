include	"../mwcs.h"

# MW_C1TRAN -- Optimized 1D coordinate transformation.

double procedure mw_c1trand (a_ct, x)

pointer	a_ct			#I pointer to CTRAN descriptor
double	x			#I coordinates in input system

double	y
pointer	ct

begin
	# Get real or double version of descriptor.
	ct = CT_D(a_ct)

	# Perform the transformation; LNR is a simple linear transformation.
	if (CT_TYPE(ct) == LNR) {
	    return (Memd[CT_LTM(ct)] * x + Memd[CT_LTV(ct)])
	} else {
	    call mw_ctrand (a_ct, x, y, 1)
	    return (y)
	}
end
