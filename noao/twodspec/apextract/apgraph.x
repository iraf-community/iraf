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
end
