# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"mwcs.h"

.help WFSAMP
.nf -------------------------------------------------------------------------
WFSAMP -- WCS function driver for the one dimensional sampled wcs function.
For this driver, the function P<->W (physical to/from world) is defined by
a sampled WCS curve.

Driver routines:

	FN_INIT		    wf_smp_init (fc, dir)
	FN_DESTROY			(none)
	FN_FWD		   wf_smp_ctran (fc, v1, v2)
	FN_INV				(same)

In this initial implementation, only linear interpolation of the sampled
curve is provided, but the driver is easily extended to provide additional
interpolators.  NOTE that this entire driver assumes that the sampled function
is monotonic.
.endhelp --------------------------------------------------------------------

# Driver specific fields of function call (FC) descriptor.
define	FC_NPTS		Memi[$1+FCU]		# number of points in curve
define	FC_LOC		Memi[$1+FCU+1]		# location in IN vector
define	FC_V1		Memi[$1+FCU+2]		# pointer to IN vector
define	FC_V2		Memi[$1+FCU+3]		# pointer to OUT vector
define	FC_W		Memd[P2D($1+FCU+4)]	# W value (CRVAL)
define	FC_DIR		Memi[$1+FCU+6]		# direction of transform


# WF_SMP_INIT -- Initialize the function call descriptor for the indicated
# type of transform (forward or inverse).

procedure wf_smp_init (fc, dir)

pointer	fc			#I pointer to FC descriptor
int	dir			#I type of transformation

int	axis, npts
pointer	wp, mw, sp, emsg, pv, wv

begin
	# Enforce the current restriction to 1-dim sampled functions.
	if (FC_NAXES(fc) != 1)
	    call error (1, "Sampled wcs functions must be 1-dimensional")

	wp = FC_WCS(fc)
	mw = CT_MW(FC_CT(fc))
	axis = CT_AXIS(FC_CT(fc),1)

	# Get pointers to the input and output sample vectors.  For our
	# purposes there is no difference between the forward and inverse
	# transform; we just swap the vectors for the inverse transform.
	# The use of direct pointers here assumes that the DBUF is not
	# reallocated while the CTRAN is being used.

	npts = WCS_NPTS(wp,axis)
	pv   = WCS_PV(wp,axis)
	wv   = WCS_WV(wp,axis)

	# Verify that we have a sampled WCS for this axis.
	if (npts <= 0 || pv == NULL || wv == NULL) {
	    call smark (sp)
	    call salloc (emsg, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[emsg], SZ_LINE,
		"No sampled wcs entered for axis %d")
		call pargi (axis)
	    call error (2, Memc[emsg])
	    call sfree (sp)
	}

	if (dir == FORWARD) {
	    FC_V1(fc) = MI_DBUF(mw) + pv - 1
	    FC_V2(fc) = MI_DBUF(mw) + wv - 1
	} else {
	    FC_V1(fc) = MI_DBUF(mw) + wv - 1
	    FC_V2(fc) = MI_DBUF(mw) + pv - 1
	}

	FC_NPTS(fc) = npts
	if (WCS_W(wp) == NULL)
	    FC_W(fc) = 0.0
	else
	    FC_W(fc) = D(mw,WCS_W(wp)+axis-1)

	FC_LOC(fc) = 1
	FC_DIR(fc) = dir
end


# WF_SMP_CTRAN -- Given the coordinates of a point X in the input curve,
# locate the sample interval containing the point, and return the coordinate
# of the same point in the output curve using simple linear interpolation
# (currently) to evaluate the WCS function value.

procedure wf_smp_ctran (fc, a_x, a_y)

pointer	fc			#I pointer to FC descriptor
double	a_x			#I point to sample WCS at
double	a_y			#O value of WCS at that point

int	index, i
double	frac, x, y
pointer	ip, op, i1, i2
int	wf_smp_binsearch()
define	sample_ 91
define	oor_ 92

begin
	i1 = FC_V1(fc)
	ip = FC_LOC(fc) + i1 - 1

	# Get the input X value.
	if (FC_DIR(fc) == FORWARD)
	    x = a_x
	else
	    x = a_x - FC_W(fc)

	# Always check the current inverval first, to optimize the case of
	# repeated samplings of the same region of the curve.

	if (Memd[ip] <= x && x <= Memd[ip+1])
	    goto sample_

	i2 = i1 + FC_NPTS(fc) - 1

	# Next check several intervals to either side.
	if (x < Memd[ip]) {
	    do i = 1, 5 {
		ip = ip - 1
		if (ip < i1)
		    goto oor_
		if (Memd[ip] <= x)
		    goto sample_
	    }
	} else {
	    do i = 1, 5 {
		if (Memd[ip+1] >= x)
		    goto sample_
		ip = ip + 1
		if (ip+1 > i2)
		    goto oor_
	    }
	}

	# Give up and do a full binary search!
	index = wf_smp_binsearch (x, Memd[i1], FC_NPTS(fc))
	if (i == 0)
	    goto oor_
	else
	    ip = i1 + index - 1

	# Having found the proper interval, compute the function value by
	# interpolating the output vector.
sample_
	op = FC_V2(fc) + ip-i1
	frac = (Memd[ip+1] - x) / (Memd[ip+1] - Memd[ip])
	y = (Memd[op+1] - Memd[op]) * frac + Memd[op]

	# Get the output Y value.
	if (FC_DIR(fc) == FORWARD)
	    a_y = y
	else
	    a_y = y + FC_W(fc)

	return
oor_
	# Given X value is not in the region covered by the sampled curve,
	# or at least we couldn't find it with a binary search.

	call error (2, "Out of bounds reference on sampled WCS curve")
end


# WF_SMP_BINSEARCH -- Perform a binary search of a sorted array for the
# interval containing the given point.

int procedure wf_smp_binsearch (x, v, npts)

double	x				#I point we want interval for
double	v[ARB]				#I array to be searched
int	npts				#I number of points in array

int	low, high, pos, i

begin
	low = 1
	high = max (1, npts - 1)
	pos = 0

	# Cut range of search in half until interval is found, or until range
	# vanishes (high - low <= 1).

	do i = 1, npts {
	    pos = (high - low) / 2 + low
	    if (pos == low)
		return (0)				# not found
	    else if (v[pos] <= x && x <= v[pos+1])
		return (pos)
	    else if (x < v[pos])
		high = pos
	    else
		low = pos
	}
end
