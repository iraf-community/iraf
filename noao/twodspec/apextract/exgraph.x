# EX_GRAPH -- Make a graph of the extracted 1D spectrum.

procedure ex_graph (gt, bufout, npts)

pointer	gt		# GTOOLS pointer
real	bufout[npts]	# Data
int	npts		# Number of data points

real	wx, wy
int	wcs, key, gt_gcur()
pointer	sp, str, gp
errchk	ap_gopen

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call ap_gopen (gp)
	call gclear (gp)
	call gswind (gp, 1., real (npts), INDEF, INDEF)
	call gascale (gp, bufout, npts, 2)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)
	call gvline (gp, bufout, npts, 1., real (npts))
	call gflush (gp)

	while (gt_gcur ("apio.cursor", wx, wy, wcs, key, Memc[str],
	    SZ_LINE) != EOF) {
	    switch (key) {
	    case 'I':
		call fatal (0, "Interrupt")
	    }
	}

	call sfree (sp)
end


# EX_PLOT -- Make a plot of the extracted 1D spectrum.

procedure ex_plot (gt, bufout, npts)

pointer	gt		# GTOOLS pointer
real	bufout[npts]	# Data
int	npts		# Number of data points

int	fd
pointer	gp
errchk	ap_popen

begin
	call ap_popen (gp, fd)
	if (gp == NULL)
	    return

	call gclear (gp)
	call gswind (gp, 1., real (npts), INDEF, INDEF)
	call gascale (gp, bufout, npts, 2)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)
	call gvline (gp, bufout, npts, 1., real (npts))
	call gflush (gp)

	call ap_pclose (gp, fd)
end
