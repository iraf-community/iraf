include <gset.h>
include <mach.h>
include "linmatch.h"

# RG_LFIND -- Find the point nearest the cursor regardless of whether it
# has been deleted or not.

int procedure rg_lfind (gd, ls, wcs, wx, wy, bscale, bzero, plot_type)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	wcs			#I the wcs of the point
real	wx			#I the x coordinate of point to be deleted
real	wy			#I the y coordinate of point to be deleted
real	bscale			#I the computed bscale value
real	bzero			#I the computed bzero value
int	plot_type		#I the current plot type

int	region
int	rg_mmffind(), rg_mmrfind(), rg_bzffind(), rg_bzrfind()
int	rg_msffind(), rg_msrfind()

begin
	switch (plot_type) {
	case LS_MMFIT:
	    region = rg_mmffind (gd, ls, wx, wy)
	case LS_MMRESID:
	    region = rg_mmrfind (gd, ls, wx, wy, bscale, bzero)
	case LS_BSZFIT:
	    region = rg_bzffind (gd, ls, wcs, wx, wy)
	case LS_BSZRESID:
	    region = rg_bzrfind (gd, ls, wcs, wx, wy, bscale, bzero)
	case LS_MAGSKYFIT:
	    region = rg_msffind (gd, ls, wcs, wx, wy)
	case LS_MAGSKYRESID:
	    region = rg_msrfind (gd, ls, wcs, wx, wy, bscale, bzero)
	default:
	    region = 0
	}

	return (region)
end


# RG_LDELETE -- Delete or undelete regions from the data.

int procedure rg_ldelete (gd, ls, udelete, wcs, wx, wy, bscale, bzero,
	plot_type, delete) 

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I/O the user deletions array
int	wcs			#I the wcs for multi-wcs plots
real	wx			#I the x coordinate of point to be deleted
real	wy			#I the y coordinate of point to be deleted
real	bscale			#I the computed bscale value
real	bzero			#I the computed bzero value
int	plot_type		#I the current plot type
int	delete			#I delete the point

int	region
int	rg_rdelete(), rg_mmfdelete(), rg_mmrdelete(), rg_bzfdelete()
int	rg_bzrdelete(), rg_msfdelete(), rg_msrdelete()

begin
	switch (plot_type) {
	case LS_MMHIST:
	    region = rg_rdelete (gd, ls, udelete, delete)
	case LS_MMFIT:
	    region = rg_mmfdelete (gd, ls, udelete, wx, wy, delete)
	case LS_MMRESID:
	    region = rg_mmrdelete (gd, ls, udelete, wx, wy, bscale,
	        bzero, delete)
	case LS_RIFIT:
	    region = rg_rdelete (gd, ls, udelete, delete)
	case LS_RIRESID:
	    region = rg_rdelete (gd, ls, udelete, delete)
	case LS_BSZFIT:
	    region = rg_bzfdelete (gd, ls, udelete, wcs, wx, wy, delete)
	case LS_BSZRESID:
	    region = rg_bzrdelete (gd, ls, udelete, wcs, wx, wy, bscale,
		bzero, delete)
	case LS_MAGSKYFIT:
	    region = rg_msfdelete (gd, ls, udelete, wcs, wx, wy, delete)
	case LS_MAGSKYRESID:
	    region = rg_msrdelete (gd, ls, udelete, wcs, wx, wy, bscale,
		bzero, delete)
	default:
	    region = 0
	}

	return (region)
end


# RG_RDELETE -- Delete or undelete a particular region from the data using
# a histogram or fit plot.

int procedure rg_rdelete (gd, ls, udelete, delete)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I/O the user deletions array
int	delete			#I delete the point

int	region
int	rg_lstati()
pointer	rg_lstatp()

begin
	# Get the current region.
	region = rg_lstati (ls, CNREGION)
	if (region < 1 || region > rg_lstati (ls, NREGIONS))
	    return (0)

	# Delete or undelete the region.
	if (delete == YES) {
	    if (Memi[rg_lstatp(ls,RDELETE)+region-1] == LS_NO) {
		udelete[region] = YES
		return (region)
	    } else
		return (0)
	} else {
	    if (Memi[rg_lstatp(ls,RDELETE)+region-1] != LS_NO) {
		udelete[region] = NO
		return (region)
	    } else
		return (0)
	}
end


# RG_MMFDELETE -- Delete or undelete a point computed from the mean, median,
# or mode.

int procedure rg_mmfdelete (gd, ls, udelete, wx, wy, delete)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I/O the user deletions array
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
int	delete			#I delete the input object

int	nregions, region, mtype
pointer	sp, xdata, ydata
int	rg_lstati(), rg_lpdelete(), rg_lpundelete()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	# Determine the type of data to plot.
	mtype = 0
	switch (rg_lstati(ls, BSALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	switch (rg_lstati(ls, BZALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	if (mtype <= 0)
	    return (0)

	# Allocate working space.
	call smark (sp)
	call salloc (xdata, nregions, TY_REAL)
	call salloc (ydata, nregions, TY_REAL)

	# Get the data.
	switch (mtype) {
	case LS_MEAN:
	    call amovr (Memr[rg_lstatp(ls,IMEAN)], Memr[xdata], nregions)
	    call amovr (Memr[rg_lstatp(ls,RMEAN)], Memr[ydata], nregions)
	case LS_MEDIAN:
	    call amovr (Memr[rg_lstatp(ls,IMEDIAN)], Memr[xdata], nregions)
	    call amovr (Memr[rg_lstatp(ls,RMEDIAN)], Memr[ydata], nregions)
	case LS_MODE:
	    call amovr (Memr[rg_lstatp(ls,IMODE)], Memr[xdata], nregions)
	    call amovr (Memr[rg_lstatp(ls,RMODE)], Memr[ydata], nregions)
	}

	# Delete or undelete the point.
	if (delete == YES)
	    region = rg_lpdelete (gd, 1, wx, wy, Memr[xdata], Memr[ydata],
		Memi[rg_lstatp(ls,RDELETE)], udelete, nregions)
	else
	    region = rg_lpundelete (gd, 1, wx, wy, Memr[xdata], Memr[ydata],
		Memi[rg_lstatp(ls,RDELETE)], udelete, nregions)

	call sfree (sp)

	return (region)
end


# RG_MMRDELETE -- Delete or undelete a point computed from the mean, median,
# or mode residuals plots.

int procedure rg_mmrdelete (gd, ls, udelete, wx, wy, bscale, bzero, delete)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I/O the user deletions array
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
real	bscale			#I the computed bscale factor
real	bzero			#I the computed bzero factor
int	delete			#I delete the input object

int	nregions, region, mtype
pointer	sp, xdata, ydata
int	rg_lstati(), rg_lpdelete(), rg_lpundelete()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	# Determine the type of data to plot.
	mtype = 0
	switch (rg_lstati(ls, BSALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	switch (rg_lstati(ls, BZALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	if (mtype <= 0)
	    return (0)

	# Allocate working space.
	call smark (sp)
	call salloc (xdata, nregions, TY_REAL)
	call salloc (ydata, nregions, TY_REAL)

	switch (mtype) {
	case LS_MEAN:
	    call amovr (Memr[rg_lstatp(ls,IMEAN)], Memr[xdata], nregions)
	    call altmr (Memr[rg_lstatp(ls,IMEAN)], Memr[ydata], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMEAN)], Memr[ydata], Memr[ydata],
	        nregions)
	case LS_MEDIAN:
	    call amovr (Memr[rg_lstatp(ls,IMEDIAN)], Memr[xdata], nregions)
	    call altmr (Memr[rg_lstatp(ls,IMEDIAN)], Memr[ydata], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMEDIAN)], Memr[ydata], Memr[ydata],
	        nregions)
	case LS_MODE:
	    call amovr (Memr[rg_lstatp(ls,IMODE)], Memr[xdata], nregions)
	    call altmr (Memr[rg_lstatp(ls,IMODE)], Memr[ydata], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMODE)], Memr[ydata], Memr[ydata],
	        nregions)
	}

	# Delete or undelete the point.
	if (delete == YES)
	    region = rg_lpdelete (gd, 1, wx, wy, Memr[xdata], Memr[ydata],
		Memi[rg_lstatp(ls,RDELETE)], udelete, nregions)
	else
	    region = rg_lpundelete (gd, 1, wx, wy, Memr[xdata], Memr[ydata],
		Memi[rg_lstatp(ls,RDELETE)], udelete, nregions)

	call sfree (sp)

	return (region)
end


# RG_BZFDELETE -- Delete or undelete a point computed from the  average
# of the fitted bscale or bzeros.

int procedure rg_bzfdelete (gd, ls, udelete, wcs, wx, wy, delete)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I/O the user deletions array
int	wcs			#I the wcs number
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
int	delete			#I delete the input object

int	i, nregions, region
pointer	sp, xreg
int	rg_lstati(), rg_lpdelete(), rg_lpundelete()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	call smark (sp)
	call salloc (xreg, nregions, TY_REAL)
	do i = 1, nregions
	    Memr[xreg+i-1] = i

	# Delete or undelete the point.
	if (delete == YES) {
	    if (wcs == 1)
	        region = rg_lpdelete (gd, wcs, wx, wy, Memr[xreg],
	            Memr[rg_lstatp(ls,RBSCALE)], Memi[rg_lstatp(ls,RDELETE)],
		    udelete, nregions)
	    else if (wcs == 2)
	        region = rg_lpdelete (gd, wcs, wx, wy, Memr[xreg],
	            Memr[rg_lstatp(ls,RBZERO)], Memi[rg_lstatp(ls,RDELETE)],
		    udelete, nregions)
	    else
		region = 0
	} else {
	    if (wcs == 1)
	        region = rg_lpundelete (gd, wcs, wx, wy, Memr[xreg],
		    Memr[rg_lstatp(ls,RBSCALE)], Memi[rg_lstatp(ls,RDELETE)],
		    udelete, nregions)
	    else if (wcs == 2)
	        region = rg_lpundelete (gd, wcs, wx, wy, Memr[xreg],
		    Memr[rg_lstatp(ls,RBZERO)], Memi[rg_lstatp(ls,RDELETE)],
		    udelete, nregions)
	    else
		region = 0
	}

	call sfree (sp)

	return (region)
end


# RG_BZRDELETE -- Delete or undelete a point computed from the  average
# of the fitted bscale or bzero residuals.

int procedure rg_bzrdelete (gd, ls, udelete, wcs, wx, wy, bscale, bzero,
	delete)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I/O the user deletions array
int	wcs			#I the wcs number
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
real	bscale			#I the input bscale value
real	bzero			#I the input bzero value
int	delete			#I delete the input object

int	i, nregions, region
pointer	sp, xreg, yreg
int	rg_lstati(), rg_lpdelete(), rg_lpundelete()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	call smark (sp)
	call salloc (xreg, nregions, TY_REAL)
	call salloc (yreg, nregions, TY_REAL)
	do i = 1, nregions
	    Memr[xreg+i-1] = i

	# Delete or undelete the point.
	if (delete == YES) {
	    if (wcs == 1) {
		call asubkr (Memr[rg_lstatp(ls,RBSCALE)], bscale, Memr[yreg],
		    nregions)
	        region = rg_lpdelete (gd, wcs, wx, wy, Memr[xreg], Memr[yreg],
		    Memi[rg_lstatp(ls,RDELETE)], udelete, nregions)
	    } else if (wcs == 2) {
		call asubkr (Memr[rg_lstatp(ls,RBZERO)], bzero, Memr[yreg],
		    nregions)
	        region = rg_lpdelete (gd, wcs, wx, wy, Memr[xreg],
	            Memr[yreg], Memi[rg_lstatp(ls,RDELETE)], udelete, nregions)
	    } else
		region = 0
	} else {
	    if (wcs == 1) {
		call asubkr (Memr[rg_lstatp(ls,RBSCALE)], bscale, Memr[yreg],
		    nregions)
	        region = rg_lpundelete (gd, wcs, wx, wy, Memr[xreg],
		    Memr[yreg], Memi[rg_lstatp(ls,RDELETE)], udelete, nregions)
	    } else if (wcs == 2) {
		call asubkr (Memr[rg_lstatp(ls,RBZERO)], bzero, Memr[yreg],
		    nregions)
	        region = rg_lpundelete (gd, wcs, wx, wy, Memr[xreg],
		    Memr[yreg], Memi[rg_lstatp(ls,RDELETE)], udelete, nregions)
	    } else
		region = 0
	}

	call sfree (sp)

	return (region)
end


# RG_MSFDELETE -- Delete or undelete a point computed from the  average
# of the fitted bscale or bzeros.

int procedure rg_msfdelete (gd, ls, udelete, wcs, wx, wy, delete)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I/O the user deletions array
int	wcs			#I the wcs number
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
int	delete			#I delete the input object

int	nregions, region
int	rg_lstati(), rg_lpdelete(), rg_lpundelete()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	# Delete or undelete the point.
	if (delete == YES) {
	    if (wcs == 1)
	        region = rg_lpdelete (gd, wcs, wx, wy, Memr[rg_lstatp(ls,
		    IMAG)], Memr[rg_lstatp(ls,RMAG)], Memi[rg_lstatp(ls,
		    RDELETE)], udelete, nregions)
	    else if (wcs == 2)
	        region = rg_lpdelete (gd, wcs, wx, wy, Memr[rg_lstatp(ls,
		    ISKY)], Memr[rg_lstatp(ls,RSKY)], Memi[rg_lstatp(ls,
		    RDELETE)], udelete, nregions)
	    else
		region = 0
	} else {
	    if (wcs == 1)
	        region = rg_lpundelete (gd, wcs, wx, wy, Memr[rg_lstatp(ls,
		    IMAG)], Memr[rg_lstatp(ls,RMAG)], Memi[rg_lstatp(ls,
		    RDELETE)], udelete, nregions)
	    else if (wcs == 2)
	        region = rg_lpundelete (gd, wcs, wx, wy, Memr[rg_lstatp(ls,
		    ISKY)], Memr[rg_lstatp(ls,RSKY)], Memi[rg_lstatp(ls,
		    RDELETE)], udelete, nregions)
	    else
		region = 0
	}

	return (region)
end


# RG_MSRDELETE -- Delete or undelete a point computed from the  average
# of the fitted bscale or bzeros.

int procedure rg_msrdelete (gd, ls, udelete, wcs, wx, wy, bscale, bzero, delete)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	udelete[ARB]		#I/O the user deletions array
int	wcs			#I the wcs number
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
real	bscale			#I the input bscale value
real	bzero			#I the input bzero value
int	delete			#I delete the input object

int	nregions, region
pointer	sp, resid
int	rg_lstati(), rg_lpdelete(), rg_lpundelete()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	call smark (sp)
	call salloc (resid, nregions, TY_REAL)

	if (wcs == 1) {
	    if (bscale > 0.0) {
	        call aaddkr (Memr[rg_lstatp(ls,IMAG)], -2.5*log10(bscale),
		    Memr[resid], nregions)
	        call asubr (Memr[rg_lstatp(ls,RMAG)], Memr[resid],
		    Memr[resid], nregions)
	    } else
	        call asubr (Memr[rg_lstatp(ls,RMAG)], Memr[rg_lstatp(ls,
		    IMAG)], Memr[resid], nregions)
	} else {
	    call altmr (Memr[rg_lstatp(ls,ISKY)], Memr[resid], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RSKY)], Memr[resid], Memr[resid],
	        nregions)
	}

	# Delete or undelete the point.
	if (delete == YES) {
	    if (wcs == 1)
	        region = rg_lpdelete (gd, wcs, wx, wy, Memr[rg_lstatp(ls,
		    IMAG)], Memr[resid], Memi[rg_lstatp(ls,RDELETE)],
		    udelete, nregions)
	    else if (wcs == 2)
	        region = rg_lpdelete (gd, wcs, wx, wy, Memr[rg_lstatp(ls,
		    ISKY)], Memr[resid], Memi[rg_lstatp(ls,RDELETE)],
		    udelete, nregions)
	    else
		region = 0
	} else {
	    if (wcs == 1)
	        region = rg_lpundelete (gd, wcs, wx, wy, Memr[rg_lstatp(ls,
		    IMAG)], Memr[resid], Memi[rg_lstatp(ls,RDELETE)],
		    udelete, nregions)
	    else if (wcs == 2)
	        region = rg_lpundelete (gd, wcs, wx, wy, Memr[rg_lstatp(ls,
		    ISKY)], Memr[resid], Memi[rg_lstatp(ls,RDELETE)],
		    udelete, nregions)
	    else
		region = 0
	}

	call sfree (sp)

	return (region)
end

# RG_MMFFIND -- Find a point computed from the mean, median, or mode.

int procedure rg_mmffind (gd, ls, wx, wy)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate

int	nregions, mtype, region
pointer	sp, xdata, ydata
int	rg_lstati(), rg_lpfind()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	# Determine the type of data to plot.
	mtype = 0
	switch (rg_lstati(ls, BSALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	switch (rg_lstati(ls, BZALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	if (mtype <= 0)
	    return (0)

	# Allocate working space.
	call smark (sp)
	call salloc (xdata, nregions, TY_REAL)
	call salloc (ydata, nregions, TY_REAL)

	# Get the data.
	switch (mtype) {
	case LS_MEAN:
	    call amovr (Memr[rg_lstatp(ls,IMEAN)], Memr[xdata], nregions)
	    call amovr (Memr[rg_lstatp(ls,RMEAN)], Memr[ydata], nregions)
	case LS_MEDIAN:
	    call amovr (Memr[rg_lstatp(ls,IMEDIAN)], Memr[xdata], nregions)
	    call amovr (Memr[rg_lstatp(ls,RMEDIAN)], Memr[ydata], nregions)
	case LS_MODE:
	    call amovr (Memr[rg_lstatp(ls,IMODE)], Memr[xdata], nregions)
	    call amovr (Memr[rg_lstatp(ls,RMODE)], Memr[ydata], nregions)
	}

	region = rg_lpfind (gd, 1, wx, wy, Memr[xdata], Memr[ydata], nregions)

	call sfree (sp)

	return (region)
end


# RG_MMRFIND -- Find a point computed from the mean, median, or mode.

int procedure rg_mmrfind (gd, ls, wx, wy, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
real	bscale			#I the input bscale factor
real	bzero			#I the input bzero factor

int	nregions, mtype, region
pointer	sp, xdata, ydata
int	rg_lstati(), rg_lpfind()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	# Determine the type of data to plot.
	mtype = 0
	switch (rg_lstati(ls, BSALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	switch (rg_lstati(ls, BZALGORITHM)) {
	case LS_MEAN:
	    mtype = LS_MEAN
	case LS_MEDIAN:
	    mtype = LS_MEDIAN
	case LS_MODE:
	    mtype = LS_MODE
	default:
	}
	if (mtype <= 0)
	    return (0)

	# Allocate working space.
	call smark (sp)
	call salloc (xdata, nregions, TY_REAL)
	call salloc (ydata, nregions, TY_REAL)

	switch (mtype) {
	case LS_MEAN:
	    call amovr (Memr[rg_lstatp(ls,IMEAN)], Memr[xdata], nregions)
	    call altmr (Memr[rg_lstatp(ls,IMEAN)], Memr[ydata], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMEAN)], Memr[ydata], Memr[ydata],
	        nregions)
	case LS_MEDIAN:
	    call amovr (Memr[rg_lstatp(ls,IMEDIAN)], Memr[xdata], nregions)
	    call altmr (Memr[rg_lstatp(ls,IMEDIAN)], Memr[ydata], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMEDIAN)], Memr[ydata], Memr[ydata],
	        nregions)
	case LS_MODE:
	    call amovr (Memr[rg_lstatp(ls,IMODE)], Memr[xdata], nregions)
	    call altmr (Memr[rg_lstatp(ls,IMODE)], Memr[ydata], nregions,
	        bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RMODE)], Memr[ydata], Memr[ydata],
	        nregions)
	}

	region = rg_lpfind (gd, 1, wx, wy, Memr[xdata], Memr[ydata], nregions)

	call sfree (sp)

	return (region)
end


# RG_BZFFIND -- Find a point computed from the bscale and bzero fits
# to all the regions.

int procedure rg_bzffind (gd, ls, wcs, wx, wy)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	wcs			#I the input wcs
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate

int	i, nregions, region
pointer	sp, xreg
int	rg_lstati(), rg_lpfind()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	call smark (sp)
	call salloc (xreg, nregions, TY_REAL)
	do i = 1, nregions
	    Memr[xreg+i-1] = i

	if (wcs == 1)
	    region = rg_lpfind (gd, 1, wx, wy, Memr[xreg], Memr[rg_lstatp(ls,
	        RBSCALE)], nregions)
	else if (wcs == 2)
	    region = rg_lpfind (gd, 2, wx, wy, Memr[xreg], Memr[rg_lstatp(ls,
	        RBZERO)], nregions)
	else
	    region = 0

	call sfree (sp)

	return (region)
end


# RG_BZRFIND -- Find a point computed from the bscale and bzero fit
# residuals to all the regions.

int procedure rg_bzrfind (gd, ls, wcs, wx, wy, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	wcs			#I the input wcs
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
real	bscale			#I the input bscale value
real	bzero			#I the input bscale value

int	i, nregions, region
pointer	sp, xreg, yreg
int	rg_lstati(), rg_lpfind()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	call smark (sp)
	call salloc (xreg, nregions, TY_REAL)
	call salloc (yreg, nregions, TY_REAL)

	do i = 1, nregions
	    Memr[xreg+i-1] = i

	if (wcs == 1) {
	    call asubkr (Memr[rg_lstatp(ls,RBSCALE)], bscale, Memr[yreg],
	        nregions)
	    region = rg_lpfind (gd, 1, wx, wy, Memr[xreg], Memr[yreg],
	        nregions)
	} else if (wcs == 2) {
	    call asubkr (Memr[rg_lstatp(ls,RBZERO)], bzero, Memr[yreg],
	        nregions)
	    region = rg_lpfind (gd, 2, wx, wy, Memr[xreg], Memr[yreg],
	        nregions)
	} else
	    region = 0

	call sfree (sp)

	return (region)
end


# RG_MSFFIND -- Find a point computed from the bscale and bzero fits
# to all the regions.

int procedure rg_msffind (gd, ls, wcs, wx, wy)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	wcs			#I the input wcs
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate

int	nregions, region
int	rg_lstati(), rg_lpfind()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	if (wcs == 1)
	    region = rg_lpfind (gd, 1, wx, wy, Memr[rg_lstatp(ls,IMAG)],
	        Memr[rg_lstatp(ls,RMAG)], nregions)
	else if (wcs == 2)
	    region = rg_lpfind (gd, 2, wx, wy, Memr[rg_lstatp(ls,ISKY)],
	        Memr[rg_lstatp(ls,RSKY)], nregions)
	else
	    region = 0

	return (region)
end


# RG_MSRFIND -- Find a point computed from the bscale and bzero fits
# to all the regions.

int procedure rg_msrfind (gd, ls, wcs, wx, wy, bscale, bzero)

pointer	gd			#I pointer to the graphics stream
pointer	ls			#I pointer to the linmatch structure
int	wcs			#I the input wcs
real	wx			#I the input x coordinate
real	wy			#I the input y coordinate
real	bscale			#I the input bscale value
real	bzero			#I the input bzero value

int	nregions, region
pointer	sp, resid
int	rg_lstati(), rg_lpfind()
pointer	rg_lstatp()

begin
	nregions = rg_lstati (ls, NREGIONS)
	if (nregions <= 1)
	    return (0)

	call smark (sp)
	call salloc (resid, nregions, TY_REAL)

	if (wcs == 1) {
	    if (bscale > 0.0) {
		call aaddkr (Memr[rg_lstatp(ls,IMAG)], -2.5*log10(bscale),
		    Memr[resid], nregions)
		call asubr (Memr[rg_lstatp(ls,RMAG)], Memr[resid], Memr[resid],
		    nregions)
	    } else
		call asubr (Memr[rg_lstatp(ls,RMAG)], Memr[rg_lstatp(ls,IMAG)],
		    Memr[resid], nregions)
	    region = rg_lpfind (gd, 1, wx, wy, Memr[rg_lstatp(ls,IMAG)],
	        Memr[resid], nregions)
	} else if (wcs == 2) {
	    call altmr (Memr[rg_lstatp(ls,ISKY)], Memr[resid], nregions,
		bscale, bzero)
	    call asubr (Memr[rg_lstatp(ls,RSKY)], Memr[resid], Memr[resid],
		nregions)
	    region = rg_lpfind (gd, 2, wx, wy, Memr[rg_lstatp(ls,ISKY)],
	        Memr[resid], nregions)
	} else
	    region = 0

	call sfree (sp)

	return (region)
end


# RG_LPDELETE -- Delete a point from the plot.

int procedure rg_lpdelete (gd, wcs, wx, wy, xdata, ydata, delete, udelete, npts)

pointer	gd			#I the graphics stream descriptor
int	wcs			#I the input wcs
real	wx, wy			#I the point to be deleted.
real	xdata[ARB]		#I the input x data array
real	ydata[ARB]		#I the input y data array
int	delete[ARB]		#I the deletions array
int	udelete[ARB]		#I/O the user deletions array
int	npts			#I the number of points

int	i, region
real	wx0, wy0, r2min, r2, x0, y0

begin
	call gctran (gd, wx, wy, wx0, wy0, wcs, 0)
	r2min = MAX_REAL
	region = 0

	# Find the point to be deleted.
	do i = 1, npts {
	    if (delete[i] != LS_NO)
		next
	    call gctran (gd, xdata[i], ydata[i], x0, y0, wcs, 0)
	    r2 = (x0 - wx0) ** 2 + (y0 - wy0) ** 2
	    if (r2 < r2min) {
		r2min = r2
		region = i
	    }
	}

	if (region > 0) {
	    call gseti (gd, G_WCS, wcs)
	    call gscur (gd, xdata[region], ydata[region])
	    call gmark (gd, xdata[region], ydata[region], GM_CROSS, 2.0, 2.0)
	    udelete[region] = YES
	}

	return (region)
end


# RG_LPUNDELETE -- Undelete a point from the plot.

int procedure rg_lpundelete (gd, wcs, wx, wy, xdata, ydata, delete,
	udelete, npts)

pointer	gd			#I the graphics stream descriptor
int	wcs			#I the input wcs
real	wx, wy			#I the point to be deleted.
real	xdata[ARB]		#I the input x data array
real	ydata[ARB]		#I the input y data array
int	delete[ARB]		#I the deletions array
int	udelete[ARB]		#I/O the user deletions array
int	npts			#I the number of points

int	i, region
real	wx0, wy0, r2min, r2, x0, y0

begin
	call gctran (gd, wx, wy, wx0, wy0, wcs, 0)
	r2min = MAX_REAL
	region = 0

	# Find the point to be deleted.
	do i = 1, npts {
	    if (udelete[i] == NO)
		next
	    call gctran (gd, xdata[i], ydata[i], x0, y0, wcs, 0)
	    r2 = (x0 - wx0) ** 2 + (y0 - wy0) ** 2
	    if (r2 < r2min) {
		r2min = r2
		region = i
	    }
	}

	if (region > 0) {
	    call gseti (gd, G_WCS, wcs)
	    call gscur (gd, xdata[region], ydata[region])
	    call gseti (gd, G_PMLTYPE, GL_CLEAR)
	    call gmark (gd, xdata[region], ydata[region], GM_CROSS, 2.0, 2.0)
	    call gseti (gd, G_PMLTYPE, GL_SOLID)
	    call gmark (gd, xdata[region], ydata[region], GM_BOX, 2.0, 2.0)
	    udelete[region] = NO
	}

	return (region)
end


# RG_LPFIND -- Find a point in the plot.

int procedure rg_lpfind (gd, wcs, wx, wy, xdata, ydata, npts)

pointer	gd			#I the graphics stream descriptor
int	wcs			#I the input wcs
real	wx, wy			#I the point to be deleted.
real	xdata[ARB]		#I the input x data array
real	ydata[ARB]		#I the input y data array
int	npts			#I the number of points

int	i, region
real	wx0, wy0, r2min, x0, y0, r2

begin
	call gctran (gd, wx, wy, wx0, wy0, wcs, 0)
	r2min = MAX_REAL
	region = 0

	# Find the point to be deleted.
	do i = 1, npts {
	    call gctran (gd, xdata[i], ydata[i], x0, y0, wcs, 0)
	    r2 = (x0 - wx0) ** 2 + (y0 - wy0) ** 2
	    if (r2 < r2min) {
		r2min = r2
		region = i
	    }
	}

	return (region)
end

