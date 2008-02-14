include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	"xtfixpix.h"

# This version uses an internal copy of the input mask rather than modifying
# the input mask.


# XT_FPINIT -- Initialize FIXPIX data structure.
# If the mask is null or empty a null pointer is returned.
# If the mask is not empty the mask is examined for bad pixels requiring
# column interpolation.  The columns and interpolation endpoints are
# recorded.  Note that line interpolation does not need to be mapped since
# this can be done efficiently as the reference image is accessed line by
# line.

pointer procedure yt_fpinit (pmin, lvalin, cvalin)

pointer	pmin			#I Pixel mask
int	lvalin			#I Input line interpolation code
int	cvalin			#I Input column interpolation code

int	i, j, k, l, n, nc, nl, l1, l2, lmin, lmax, ncols, lval, cval, ncompress
short	val
long	v[IM_MAXDIM]
pointer	pm, fp, ptr, col, pl1, pl2
pointer	sp, buf, cols

bool	pm_empty()
pointer	pm_newcopy()
errchk	pmglrs, pmplrs

begin
	# Check for empty mask.
	if (pmin == NULL)
	    return (NULL)
	if (pm_empty (pmin))
	    return (NULL)

	# Make an internal copy of the mask.
	pm = pm_newcopy (pmin)

	# Get mask size.
	call pm_gsize (pm, i, v, j)
	nc = v[1]
	nl = v[2]

	# Allocate memory and data structure.
	call smark (sp)
	call salloc (buf, 3*max(nc, nl), TY_SHORT) 
	call salloc (cols, nc, TY_SHORT)
	call calloc (fp, FP_LEN, TY_STRUCT)

	# Set the mask codes.  Go through the mask and change any mask codes
	# that match the input mask code to the output mask code (if they are
	# different).  This is done to move the mask codes to a range that
	# won't conflict with the length values.  For any other code replace
	# the value by the length of the bad region along the line.  This
	# value will be used in comparison to the length along the column for
	# setting the interpolation for the narrower dimension.

	if ((IS_INDEFI(lvalin)||lvalin<1) && (IS_INDEFI(cvalin)||cvalin<1)) {
	    lval = FP_LDEF
	    cval = FP_CDEF
	} else if (IS_INDEFI(lvalin) || lvalin < 1) {
	    lval = FP_LDEF
	    cval = mod (cvalin - 1, nc) + 1
	    if (lval == cval)
		lval = FP_CDEF
	} else if (IS_INDEFI(cvalin) || cvalin < 1) {
	    lval = mod (lvalin - 1, nc) + 1
	    cval = FP_CDEF
	    if (cval == lval)
		cval = FP_LDEF
	} else if (lvalin != cvalin) {
	    lval = mod (lvalin - 1, nc) + 1
	    cval = mod (cvalin - 1, nc) + 1
	} else {
	    call mfree (fp, TY_STRUCT)
	    call sfree (sp)
	    call error (1, "Interpolation codes cannot be the same")
	}
	call yt_fpsinterp (pmin, pm, nc, nl, v, Mems[buf], lvalin, cvalin,
	    lval, cval)

	# Go through and check if there is any need for column interpolation;
	# i.e. are there any mask values different from the line interpolation.

	call aclrs (Mems[cols], nc)
	call amovkl (long(1), v, IM_MAXDIM)
	do l = 1, nl {
	    v[2] = l
	    call pmglrs (pm, v, Mems[buf], 0, nc, 0)
	    ptr = buf + 3
	    do i = 2, Mems[buf] {
		val = Mems[ptr+2]
		if (val != lval) {
		    val = 1
		    n = Mems[ptr+1] 
		    call amovks (val, Mems[cols+Mems[ptr]-1], n)
		}
		ptr = ptr + 3
	    }
	}
	n = 0
	do i = 1, nc
	    if (Mems[cols+i-1] != 0)
		n = n + 1

	# If there are mask codes for either column interpolation or
	# interpolation lengths along lines to compare against column
	# interpolation check the interpolation length against the
	# column and set the line interpolation endpoints to use.
	# compute the minimum and maximum lines that are endpoints
	# to restrict the random access pass that will be needed to
	# get the endpoint values.

	if (n > 0) {
	    n = n + 10
	    call malloc (col, n, TY_INT)
	    call malloc (pl1, n, TY_INT)
	    call malloc (pl2, n, TY_INT)
	    ncols = 0
	    lmin = nl
	    lmax = 0
	    ncompress = 0
	    do i = 1, nc {
		if (Mems[cols+i-1] == 0)
		    next
		v[1] = i
		do l = 1, nl {
		    v[2] = l
		    call pmglps (pm, v, Mems[buf+l-1], 0, 1, 0)
		}
		for (l1=1; l1<=nl && Mems[buf+l1-1]==0; l1=l1+1)
		    ;
		while (l1 <= nl) {
		    l1 = l1 - 1
		    for (l2=l1+1; l2<=nl && Mems[buf+l2-1]!=0; l2=l2+1)
			;
		    j = 0
		    k = nc + l2 - l1 - 1
		    do l = l1+1, l2-1 {
			val = Mems[buf+l-1]
			if (val == cval)
			    j = j + 1
			else if (val > nc) {
			    if (val > k) {
				j = j + 1
				val = cval
			    } else
				val = lval
			    v[2] = l
			    call pmplps (pm, v, val, 0, 1, PIX_SRC)
			    ncompress = ncompress + 1
			}
		    }
		    if (ncompress > 100) {
			call pm_compress (pm)
			ncompress = 0
		    }
		    if (j > 0) {
			if (ncols == n) {
			    n = n + 10
			    call realloc (col, n, TY_INT)
			    call realloc (pl1, n, TY_INT)
			    call realloc (pl2, n, TY_INT)
			}
			j = 1 + l1 - 1
			k = 1 + l2 - 1
			lmin = min (lmin, j, k)
			lmax = max (lmax, j, k)
			Memi[col+ncols] = i
			Memi[pl1+ncols] = j
			Memi[pl2+ncols] = k
			ncols = ncols + 1
		    }
		    for (l1=l2+1; l1<=nl && Mems[buf+l1-1]==0; l1=l1+1)
			;
		}
	    }

	    FP_LMIN(fp) = lmin
	    FP_LMAX(fp) = lmax
	    FP_NCOLS(fp) = ncols
	    FP_PCOL(fp) = col
	    FP_PL1(fp) = pl1
	    FP_PL2(fp) = pl2
	}

	FP_PM(fp) = pm
	FP_LVAL(fp) = lval
	FP_CVAL(fp) = cval

	call sfree (sp)
	return (fp)
end


# XT_SINTERP -- Set length of line interpolation regions.
# The mask values are set to the length of any column interpolation
# plus an offset leaving any line and column interpolation codes
# unchanged.  These values will be used in a second pass to compare
# to the lengths of line interpolation and then the mask values will
# be reset to one of the line or column interpolation codes based on
# the minimum distance.

procedure yt_fpsinterp (pmin, pm, nc, nl, v, data, lvalin, cvalin,
	lvalout, cvalout)

pointer	pmin		#I Input pixel mask
pointer	pm		#I Modified pixel mask
int	nc, nl		#I Mask size
long	v[ARB]		#I Coordinate vector
short	data[ARB]	#I Data buffer
int	lvalin		#I Input line interpolation code
int	cvalin		#I Input column interpolation code
int	lvalout		#I Output line interpolation code
int	cvalout		#I Output column interpolation code

int	c, l, c1, c2, val
bool	pm_linenotempty()

begin
	call amovkl (long(1), v, IM_MAXDIM)
	do l = 1, nl {
	    v[2] = l
	    if (!pm_linenotempty (pmin, v))
		next
	    
	    call pmglps (pmin, v, data, 0, nc, 0)

	    for (c1=1; c1<=nc && data[c1]==0; c1=c1+1)
		;
	    while (c1 <= nc) {
		for (c2=c1+1; c2<=nc && data[c2]!=0; c2=c2+1)
		    ;
		c2 = c2 - 1
		do c = c1, c2 {
		    val = data[c]
		    if (val == lvalin) {
			if (lvalin != lvalout)
			    data[c] = lvalout
		    } else if (val == cvalin) {
			if (cvalin != cvalout)
			    data[c] = cvalout
		    } else {
			data[c] = nc + c2 - c1 + 1
		    }
		}
		for (c1=c2+2; c1<=nc && data[c1]==0; c1=c1+1)
		    ;
	    }

	    call pmplps (pm, v, data, 0, nc, PIX_SRC)
	}
end


# XT_FPFREE -- Free FIXPIX data structures.

procedure yt_fpfree (fp)

pointer	fp		#U FIXPIX data structure

begin
	if (fp == NULL)
	    return
	call mfree (FP_PCOL(fp), TY_INT)
	call mfree (FP_PL1(fp), TY_INT)
	call mfree (FP_PL2(fp), TY_INT)
	if (FP_PV1(fp) != NULL)
	    call mfree (FP_PV1(fp), FP_PIXTYPE(fp))
	if (FP_PV2(fp) != NULL)
	    call mfree (FP_PV2(fp), FP_PIXTYPE(fp))
	if (FP_DATA(fp) != NULL)
	    call mfree (FP_DATA(fp), FP_PIXTYPE(fp))
	call pm_close (FP_PM(fp))
	call mfree (fp, TY_STRUCT)
end
