include	"../mwcs.h"

# MW_CTRAN -- Transform a single N-dimensional point, using the optimized
# transformation set up by a prior call to MW_SCTRAN.

procedure mw_ctranr (a_ct, p1, p2, ndim)

pointer	a_ct			#I pointer to CTRAN descriptor
real	p1[ndim]		#I coordinates of point in input system
real	p2[ndim]		#O coordinates of point in output system
int	ndim			#I dimensionality of point

int	naxes, i, j
pointer	ct, fc, ltm, ltv, d_ct
double	v1[MAX_DIM], v2[MAX_DIM], iv[MAX_DIM], ov[MAX_DIM]
errchk	zcall3

begin
	# Get real or double version of descriptor.
	ct = CT_R(a_ct)

	ltm = CT_LTM(ct)
	ltv = CT_LTV(ct)

	# Specially optimized cases.
	if (CT_TYPE(ct) == LNR) {
	    # Simple linear, nonrotated transformation.
	    do i = 1, ndim
		p2[i] = Memr[ltm+(i-1)*(ndim+1)] * p1[i] + Memr[ltv+i-1]
	    return
	} else if (CT_TYPE(ct) == LRO) {
	    # Simple linear, rotated transformation.
	    call mw_ltranr (p1, p2, Memr[ltm], Memr[ltv], ndim)
	    return
	}

	# If we get here the transformation involves a call to one or more
	# WCS functions.  In this general case, the transformation consists
	# of zero or more calls to WCS functions to transform the input
	# world coordinates to the linear input system, followed by a general
	# linear transformation to the linear output system, followed by zero
	# or more calls to WCS functions to do the forward transformation
	# to generate the final output world coordinates.  The WCS function
	# calls are always evaluated in double precision.

	# Make zero or more WCS function calls for the different axes of the
	# input system (inverse transform).

	call achtrd (p1, iv, ndim)
	do j = 1, CT_NCALLI(ct) {
	    # Get pointer to function call descriptor.
	    fc = CT_FCI(ct,j)
	    naxes = FC_NAXES(fc)

	    # Extract the coordinate vector for the function call.
	    do i = 1, naxes
		v1[i] = p1[FC_AXIS(fc,i)]

	    # Call the WCS function.
	    call zcall3 (FC_FCN(fc), fc, v1, v2)

	    # Edit the vector IV, replacing the entries associated with
	    # the WCS function by the transformed values.

	    do i = 1, naxes
		iv[FC_AXIS(fc,i)] = v2[i]
	}

	# Apply the general linear transformation.  We may as well do this in
	# double since we already have to use double for the function calls.

	d_ct = CT_D(a_ct)
	call mw_ltrand (iv, ov, Memd[CT_LTM(d_ct)], Memd[CT_LTV(d_ct)], ndim)

	# Make zero or more WCS function calls for the different axes of the
	# output system (forward transform to final world system).

	call achtdr (ov, p2, ndim)
	do j = 1, CT_NCALLO(ct) {
	    # Get pointer to function call descriptor.
	    fc = CT_FCO(ct,j)
	    naxes = FC_NAXES(fc)

	    # Extract the coordinate vector for the function call.
	    do i = 1, naxes
		v1[i] = ov[FC_AXIS(fc,i)]

	    # Call the WCS function.
	    call zcall3 (FC_FCN(fc), fc, v1, v2)

	    # Edit the final output vector, replacing the entries for the
	    # function axes by their transformed values.

	    do i = 1, naxes
		p2[FC_AXIS(fc,i)] = v2[i]
	}
end
