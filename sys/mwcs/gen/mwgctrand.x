# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../mwcs.h"

# MW_GCTRAN -- Get a coordinate transformation compiled in a previous call
# to mw_sctran.  When the transformation is compiled, it is specified by
# naming the input and output systems and the axes over which the transform
# is to be performed.  Rather than return this information, which the
# application already knows, we return the actual transform, i.e., the
# linear transformation matrix and translation vector comprising the linear
# portion of the transform, and axis class arrays for the input and output
# systems defining the axis types.  If the axis types are all zero, there
# are no WCS function calls for any axis in either system, and the
# transformation is completely linear (hence computable by the application
# if desired, e.g., with mw_ltr).

int procedure mw_gctrand (a_ct, o_ltm, o_ltv, axtype1, axtype2, maxdim)

pointer	a_ct			#I pointer to CTRAN descriptor
double	o_ltm[ARB]		#O linear tranformation matrix
double	o_ltv[ARB]		#O translation matrix
int	axtype1[ARB]		#O axis types for input system
int	axtype2[ARB]		#O axis types for output system
int	maxdim			#I how much stuff to return

pointer	ct
int	pdim, ndim, i, j

begin
	ct = CT_D(a_ct)
	pdim = CT_NDIM(ct)
	ndim = min (pdim, maxdim)

	# Output the goods.
	do j = 1, ndim {
	    axtype1[j] = WCS_AXCLASS(CT_WCSI(ct),j)
	    axtype2[j] = WCS_AXCLASS(CT_WCSO(ct),j)
	    o_ltv[j] = Memd[CT_LTV(ct)+(j-1)]
	    do i = 1, ndim
		o_ltm[(j-1)*ndim+i] = Memd[CT_LTM(ct)+(j-1)*pdim+(i-1)]
	}

	return (pdim)
end
