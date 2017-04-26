include	<gset.h>
include	<pkg/gtools.h>
include	<math/nlfit.h>
include	<pkg/inlfit.h>

define	NGRAPH		100		# Number of fit points to graph
define	MSIZE		3.0		# mark size for rejected points (real)


# ING_GRAPH -- Graph data and fit. First plot the data marking deleted
# points, then overplot rejected points, and finally overplot the fit.

procedure ing_graphd (in, gp, gt, nl, x, y, wts, npts, nvars)

pointer	in				# INLFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointers
pointer	nl				# NLFIT pointer
double	x[ARB]				# Independent variables (npts * nvars)
double	y[npts]				# Dependent variables
double	wts[npts]			# Weights
int	npts				# Number of points
int	nvars				# Number of variables

pointer	xout, yout
pointer	sp

begin
	# Alloacate axes data memory.
	call smark (sp)
	call salloc (xout, npts, TY_DOUBLE)
	call salloc (yout, npts, TY_DOUBLE)

	# Set axes data.
	call ing_axesd (in, gt, nl, 1, x, y, Memd[xout], npts, nvars)
	call ing_axesd (in, gt, nl, 2, x, y, Memd[yout], npts, nvars)

	# Set graphic parameters.
	call ing_paramsd (in, nl, x, y, wts, npts, nvars, gt)

	# Plot data and deleted points.
	call ing_g1d (in, gp, gt, Memd[xout], Memd[yout], wts, npts)

	# Overplot rejected points.
	call ing_g2d (in, gp, gt, Memd[xout], Memd[yout], npts)

	# Overplot the fit.
	call ing_gfd (in, gp, gt, nl, x, wts, npts, nvars)

	# Free memory
	call sfree (sp)
end


# ING_G1 - Plot data and deleted points (weight = 0.0).

procedure ing_g1d (in, gp, gt, x, y, wts, npts)

pointer	in				# INLFIT pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
double	x[npts]				# Ordinates
double	y[npts]				# Abscissas
double	wts[npts]			# Weights
int	npts				# Number of points

int	i
pointer	sp, xr, yr, xr1, yr1, gt1

int	in_geti()

begin
	# Allocate memory.
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (xr1, 2, TY_REAL)
	call salloc (yr1, 2, TY_REAL)

	# Change type to real for plotting.
	call achtdr (x, Memr[xr], npts)
	call achtdr (y, Memr[yr], npts)

	# Start new graph if not overplotting.
	if (in_geti (in, INLOVERPLOT) == NO) {
	    call gclear (gp)
	    call gascale (gp, Memr[xr], npts, 1)
	    call gascale (gp, Memr[yr], npts, 2)
	    call gt_swind (gp, gt)
	    call gt_labax (gp, gt)
	}

	# Initialize auxiliaray GTOOLS descriptor for deleted points.
	call gt_copy (gt, gt1)
	call gt_sets (gt1, GTTYPE, "mark")
	call gt_sets (gt1, GTMARK, "cross")

	# Plot data points marking deleted points with other symbol.
	Memr[xr1] = Memr[xr]
	Memr[yr1] = Memr[yr]
	do i = 1, npts {
	    if (wts[i] == double (0.0)) {
		call gt_plot (gp, gt1, Memr[xr+i-1], Memr[yr+i-1], 1) 
	    } else {
#		Memr[xr1+1] = Memr[xr+i-1]
#		Memr[yr1+1] = Memr[yr+i-1]
#		call gt_plot (gp, gt, Memr[xr1], Memr[yr1], 2)
#		Memr[xr1] = Memr[xr1+1]
#		Memr[yr1] = Memr[yr1+1]

		call gt_plot (gp, gt, Memr[xr+i-1], Memr[yr+i-1], 1)
	    }
	}

	# Reset overplot flag.
	call in_puti (in, INLOVERPLOT, NO)

	# Free memory and auxiliary GTOOLS descriptor.
	call sfree (sp)
	call gt_free (gt1)
end


# ING_G2 - Overplot rejected points.

procedure ing_g2d (in, gp, gt, x, y, npts)

pointer	in			# INLFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
double	x[npts], y[npts]	# Data points
int	npts			# Number of data points

int	i
pointer	sp, xr, yr, gt1
pointer	rejpts

int	in_geti()
int	in_getp()

begin
	# Don't plot if there are no rejected points
	if (in_geti (in, INLNREJPTS) == 0)
	    return

	# Allocate axes memory.
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)

	# Change type to real for plotting.
	call achtdr (x, Memr[xr], npts)
	call achtdr (y, Memr[yr], npts)

	# Initialize auxiliary GTOOLS descriptor
	# for rejected points.
	call gt_copy (gt, gt1)
	call gt_sets (gt1, GTTYPE, "mark")
	call gt_sets (gt1, GTMARK, "diamond")
	call gt_setr (gt1, GTXSIZE, MSIZE)
	call gt_setr (gt1, GTYSIZE, MSIZE)

	# Plot rejected points if there are any.
	rejpts = in_getp (in, INLREJPTS)
	do i = 1, npts {
	    if (Memi[rejpts + i - 1] == YES)
		call gt_plot (gp, gt1, Memr[xr + i - 1], Memr[yr + i - 1], 1)
	}

	# Free memory and auxiliary GTOOLS descriptor.
	call gt_free (gt1)
	call sfree (sp)
end


# ING_GF - Overplot the fit using dashed lines.

procedure ing_gfd (in, gp, gt, nl, xin, wts, npts, nvars)

pointer	in			# INLFIT pointer
pointer	gp			# GIO pointer
pointer	gt			# GTOOL pointer
pointer	nl			# NLFIT pointer
double	xin[ARB]		# Independent variables
double	wts[npts]		# weights
int	npts			# Number of points to plot
int	nvars			# Number of variables

int	i
pointer	sp, xr, yr, x, y, xo, yo, gt1

int	in_geti()

begin
	# Don't plot if there is a fit error.
	if (in_geti (in, INLFITERROR) != DONE ||
	    in_geti (in, INLPLOTFIT) == NO)
	    return

	# Allocate memory.
	call smark (sp)
	call salloc (xr, npts, TY_REAL)
	call salloc (yr, npts, TY_REAL)
	call salloc (x,  npts * nvars, TY_DOUBLE)
	call salloc (y,  npts, TY_DOUBLE)
	call salloc (xo, npts, TY_DOUBLE)
	call salloc (yo, npts, TY_DOUBLE)

	# Move input data into vector.
	call amovd (xin, Memd[x], npts * nvars)

	# Calculate vector of fit values.
	call nlvectord (nl, Memd[x], Memd[y], npts, nvars)

	# Set axes data.
	call ing_axesd (in, gt, nl, 1, Memd[x], Memd[y], Memd[xo],
			 npts, nvars)
	call ing_axesd (in, gt, nl, 2, Memd[x], Memd[y], Memd[yo],
			 npts, nvars)

	# Convert to real for plotting.
	call achtdr (Memd[xo], Memr[xr], npts)
	call achtdr (Memd[yo], Memr[yr], npts)

	# Initialize auxiliary GTOOLS descriptor, plot the
	# fit and free the auxiliary descriptor.
	call gt_copy (gt, gt1)
	call gt_sets (gt1, GTTYPE, "mark")
	call gt_sets (gt1, GTMARK, "box")
	call gt_setr (gt1, GTXSIZE, MSIZE)
	call gt_setr (gt1, GTXSIZE, MSIZE)
	do i = 1, npts {
	    if (wts[i] != double (0.0))
		call gt_plot (gp, gt1, Memr[xr+i-1], Memr[yr+i-1], 1) 
	}
	call gt_free (gt1)

	# Free memory.
	call sfree (sp)
end
