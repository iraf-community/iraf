include	<pkg/gtools.h>
include	"apertures.h"


# AP_GRAPH -- Graph the image data and call ap_gmark to mark the apertures.

procedure ap_graph (gp, gt, imdata, npts, imvec, aps, naps)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer
real	imdata[npts]		# Image data
int	npts			# Number points in image data
int	imvec			# Image vector
pointer	aps[ARB]		# Aperture pointers
int	naps			# Number of apertures

real	x1, x2

begin
	call gclear (gp)

	x1 = 1.
	x2 = npts
	call gswind (gp, x1, x2, INDEF, INDEF)
	call gascale (gp, imdata, npts, 2)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)
	call gvline (gp, imdata, npts, x1, x2)

	call ap_gmark (gp, imvec, aps, naps)
	if (naps == 1)
	    call ap_gmarkb (gp, imvec, aps, naps)
end


# AP_PLOT -- Make a plot of the apertures if plot output is defined.

procedure ap_plot (image, line, nsum, aps, naps)

char	image[SZ_FNAME]		# Image to be edited
int	line			# Dispersion line
int	nsum			# Number of dispersion lines to sum

pointer	aps[ARB]		# Aperture pointers
int	naps			# Number of apertures

int	npts, apaxis, fd
pointer	im, imdata, title, gp, gt, gt_init()
errchk	ap_getdata, ap_popen

begin
	call ap_popen (gp, fd, "aps")
	if (gp == NULL)
	    return

	# Map the image and get the image data.
	call ap_getdata (image, line, nsum, im, imdata, npts, apaxis, title)

	gt = gt_init()
	call gt_sets (gt, GTTITLE, Memc[title])
	call gt_sets (gt, GTPARAMS, "")
	call gt_setr (gt, GTXMIN, INDEF)
	call gt_setr (gt, GTXMAX, INDEF)
	call gt_setr (gt, GTYMIN, INDEF)
	call gt_setr (gt, GTYMAX, INDEF)

	call ap_graph (gp, gt, Memr[imdata], npts, line, aps, naps)

	call mfree (imdata, TY_REAL)
	call mfree (title, TY_CHAR)
	call ap_pclose (gp, fd)
	call gt_free (gt)
	call imunmap (im)
end


# AP_GRAPH1 -- Make a graph of the extracted 1D spectrum.

procedure ap_graph1 (gt, bufout, npts, nspec)

pointer	gt			# GTOOLS pointer
real	bufout[npts, nspec]	# Data
int	npts			# Number of data points
int	nspec			# Number of spectra

real	wx, wy
int	i, wcs, key, gt_gcur()
pointer	sp, str, gp
errchk	ap_gopen

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call ap_gopen (gp)
	call gclear (gp)
	call gswind (gp, 1., real (npts), INDEF, INDEF)
	call gascale (gp, bufout, npts * nspec, 2)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)
	do i = 1, nspec
	    call gvline (gp, bufout[1,i], npts, 1., real (npts))
	call gflush (gp)

	while (gt_gcur ("gcur", wx, wy, wcs, key, Memc[str],
	    SZ_LINE) != EOF) {
	    switch (key) {
	    case 'I':
		call fatal (0, "Interrupt")
	    }
	}

	call sfree (sp)
end


# AP_PLOT1 -- Make a plot of the extracted 1D spectrum.

procedure ap_plot1 (gt, bufout, npts, nspec)

pointer	gt			# GTOOLS pointer
real	bufout[npts,nspec]	# Data
int	npts			# Number of data points
int	nspec			# Number of spectra

int	i, fd
pointer	gp
errchk	ap_popen

begin
	call ap_popen (gp, fd, "spec")
	if (gp == NULL)
	    return

	call gclear (gp)
	call gswind (gp, 1., real (npts), INDEF, INDEF)
	call gascale (gp, bufout, npts * nspec, 2)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)
	do i = 1, nspec
	    call gvline (gp, bufout[1,i], npts, 1., real (npts))
	call gflush (gp)

	call ap_pclose (gp, fd)
end
