# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <plset.h>
include "pllseg.h"
include	<plio.h>

# PL_SECTNOTEMPTY -- Test whether the indicated mask image section is empty.

bool procedure pl_sectnotempty (pl_src, v1, v2, ndim)

pointer	pl_src			#I mask descriptor
long	v1[PL_MAXDIM]		#I starting coordinates of section
long	v2[PL_MAXDIM]		#I ending coordinates of section
int	ndim

pointer	lp
int	ll_src, i
long	v[PL_MAXDIM], vs[PL_MAXDIM], ve[PL_MAXDIM], vn[PL_MAXDIM]

bool	pll_empty()
int	pl_reference(), plloop()
errchk	plvalid, plsslv, pl_reference

begin
	call plvalid (pl_src)

	# Initialize the N-dimensional loop counters.
	do i = 1, PL_MAXDIM
	    if (i <= ndim) {
		if (v1[i] <= v2[i]) {
		    vs[i] = v1[i]
		    vn[i] = v2[i] - v1[i] + 1
		} else {
		    vs[i] = v2[i]
		    vn[i] = v1[i] - v2[i] + 1
		}
	    } else {
		vs[i] = 1
		vn[i] = 1
	    }

	call plsslv (pl_src, vs, vn, v, ve)

	# Test each line segment in the section.
	repeat {
	    ll_src = pl_reference (pl_src, v)
	    if (ll_src != PL_EMPTYLINE) {
		lp = Ref (pl_src, ll_src)
		if (!pll_empty (Mems[lp], vs[1], vn[1]))
		    return (true)
	    }
	} until (plloop (v, vs, ve, PL_NAXES(pl_src)) == LOOP_DONE)

	return (false)
end


# PLL_EMPTY -- Test whether a section of a line list is empty.

bool procedure pll_empty (ll_src, xs, npix)

short	ll_src[ARB]		#I input line list
int	xs			#I first pixel to test
int	npix			#I length of region to be tested

int	nleft, x1, np, v_src, i
int	d_src[LEN_PLLDES]

begin
	# Advance to the indicated position in the source list.
	x1 = 1
	pll_init (ll_src, d_src)
	do i = 1, ARB {
	    np = min (pll_nleft(d_src), xs - x1)
	    pll_getseg (ll_src, d_src, np, v_src)
	    x1 = x1 + np
	    if (x1 >= xs || np == 0)
		break
	}

	# Test if the next npix pixels are zero.
	if (pll_nleft(d_src) < npix)
	    return (false)
	else {
	    pll_getseg (ll_src, d_src, npix, v_src)
	    return (v_src == 0)
	}

	# Test if the next npix pixels are zero.  Note the line list is
	# segmented and we have to read segments until we have examined NPIX
	# pixels, or until a nonzero mask pixel is encountered.

	for (nleft=npix;  nleft > 0;  nleft = nleft - np) {
	    np = min (pll_nleft(d_src), nleft)
	    pll_getseg (ll_src, d_src, np, v_src)
	    if (v_src != 0)
		return (false)
	}

	return (true)
end
