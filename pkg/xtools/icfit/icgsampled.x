# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include	<pkg/rg.h>
include	<pkg/gtools.h>
include	"icfit.h"

# ICG_SAMPLE -- Mark sample.

procedure icg_sampled (ic, gp, gt, x, npts, pltype)

pointer	ic				# ICFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
double	x[npts]				# Ordinates of graph
int	npts				# Number of data points
int	pltype				# Plot line type

pointer	rg
int	i, axis, pltype1
real	xl, xr, yb, yt, dy
real	x1, x2, y1, y2, y3

int	gstati(), stridxs(), gt_geti()
pointer	rg_xrangesd()

begin
	if (stridxs ("*", Memc[IC_SAMPLE(ic)]) > 0)
	    return

	# Find axis along which the independent data is plotted.
	if (IC_AXES(ic,IC_GKEY(ic),1) == 'x')
	    axis = 1
	else if (IC_AXES(ic,IC_GKEY(ic),2) == 'x')
	    axis = 2
	else
	    return

	if (gt_geti (gt, GTTRANSPOSE) == YES)
	    axis = mod (axis, 2) + 1

	pltype1 = gstati (gp, G_PLTYPE)
	call gseti (gp, G_PLTYPE, pltype)
	rg = rg_xrangesd (Memc[IC_SAMPLE(ic)], x, npts)

	switch (axis) {
	case 1:
	    call ggwind (gp, xl, xr, yb, yt)

	    dy = yt - yb
	    y1 = yb + dy / 100
	    y2 = y1 + dy / 20
	    y3 = (y1 + y2) / 2

	    do i = 1, RG_NRGS(rg) {
	        x1 = x[RG_X1(rg, i)]
	        x2 = x[RG_X2(rg, i)]
	        if ((x1 > xl) && (x1 < xr))
	            call gline (gp, x1, y1, x1, y2)
	        if ((x2 > xl) && (x2 < xr))
	            call gline (gp, x2, y1, x2, y2)
	        call gline (gp, x1, y3, x2, y3)
	    }
	case 2:
	    call ggwind (gp, yb, yt, xl, xr)

	    dy = yt - yb
	    y1 = yb + dy / 100
	    y2 = y1 + dy / 20
	    y3 = (y1 + y2) / 2

	    do i = 1, RG_NRGS(rg) {
	        x1 = x[RG_X1(rg, i)]
	        x2 = x[RG_X2(rg, i)]
	        if ((x1 > xl) && (x1 < xr))
	            call gline (gp, y1, x1, y2, x1)
	        if ((x2 > xl) && (x2 < xr))
	            call gline (gp, y1, x2, y2, x2)
	        call gline (gp, y3, x1, y3, x2)
	    }
	}

	call gseti (gp, G_PLTYPE, pltype1)
	call rg_free (rg)
end


# ICG_DSAMPLE -- Delete sample region.

procedure icg_dsampled (wx, wy, ic, gp, gt, x, npts)

real	wx, wy				# Region to be deleted
pointer	ic				# ICFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
double	x[npts]				# Ordinates of graph
int	npts				# Number of data points

pointer	sp, str, rg
int	i,  j, axis, pltype1
real	w, diff
real	xl, xr, yb, yt, dy
real	x1, x2, y1, y2, y3

int	gstati(), stridxs(), gt_geti()
pointer	rg_xrangesd()

begin
	if (stridxs ("*", Memc[IC_SAMPLE(ic)]) > 0)
	    return

	# Find axis along which the independent data is plotted.
	if (IC_AXES(ic,IC_GKEY(ic),1) == 'x')
	    axis = 1
	else if (IC_AXES(ic,IC_GKEY(ic),2) == 'x')
	    axis = 2
	else
	    return

	if (gt_geti (gt, GTTRANSPOSE) == YES)
	    axis = mod (axis, 2) + 1

	# Initialize
	pltype1 = gstati (gp, G_PLTYPE)
	call gseti (gp, G_PLTYPE, 0)
	rg = rg_xrangesd (Memc[IC_SAMPLE(ic)], x, npts)

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	Memc[IC_SAMPLE(ic)] = EOS

	# Find nearest sample region
	if (axis == 1)
	    w = wx
	else
	    w = wy

	j = 1
	diff = MAX_REAL
	do i = 1, RG_NRGS(rg) {
	    x1 = x[RG_X1(rg, i)]
	    x2 = x[RG_X2(rg, i)]
	    if (w < x1) {
		if (x1 - w < diff) {
		    diff = x1 - wx
		    j = i
		}
	    } else if (wx > x2) {
		if (wx - x2 < diff) {
		    diff = x1 - wx
		    j = i
		}
	    } else {
		diff = 0.
		j = i
	    }
	}

	# Erase sample region and reset sample string
	switch (axis) {
	case 1:
	    call ggwind (gp, xl, xr, yb, yt)

	    dy = yt - yb
	    y1 = yb + dy / 100
	    y2 = y1 + dy / 20
	    y3 = (y1 + y2) / 2

	    do i = 1, RG_NRGS(rg) {
	        x1 = x[RG_X1(rg, i)]
	        x2 = x[RG_X2(rg, i)]
		if (i != j) {
		    if (x1 == int(x1) && x2 == int(x2))
			call sprintf (Memc[str], SZ_FNAME, " %d:%d")
		    else
			call sprintf (Memc[str], SZ_FNAME, " %g:%g")
			call pargr (x1)
			call pargr (x2)
		    call strcat (Memc[str], Memc[IC_SAMPLE(ic)], IC_SZSAMPLE)
		} else {
		    if ((x1 > xl) && (x1 < xr))
			call gline (gp, x1, y1, x1, y2)
		    if ((x2 > xl) && (x2 < xr))
			call gline (gp, x2, y1, x2, y2)
		    call gline (gp, x1, y3, x2, y3)
		    IC_NEWX(ic) = YES
		}
	    }
	case 2:
	    call ggwind (gp, yb, yt, xl, xr)

	    dy = yt - yb
	    y1 = yb + dy / 100
	    y2 = y1 + dy / 20
	    y3 = (y1 + y2) / 2

	    do i = 1, RG_NRGS(rg) {
	        x1 = x[RG_X1(rg, i)]
	        x2 = x[RG_X2(rg, i)]
		if (i != j) {
		    if (x1 == int(x1) && x2 == int(x2))
			call sprintf (Memc[str], SZ_FNAME, " %d:%d")
		    else
			call sprintf (Memc[str], SZ_FNAME, " %g:%g")
			call pargr (x1)
			call pargr (x2)
		    call strcat (Memc[str], Memc[IC_SAMPLE(ic)], IC_SZSAMPLE)
		} else {
		    if ((x1 > xl) && (x1 < xr))
			call gline (gp, y1, x1, y2, x1)
		    if ((x2 > xl) && (x2 < xr))
			call gline (gp, y1, x2, y2, x2)
		    call gline (gp, y3, x1, y3, x2)
		    IC_NEWX(ic) = YES
		}
	    }
	}

	if (Memc[IC_SAMPLE(ic)] == EOS)
	    call strcat ("*", Memc[IC_SAMPLE(ic)], IC_SZSAMPLE)

	call gseti (gp, G_PLTYPE, pltype1)
	call rg_free (rg)
	call sfree (sp)
end
