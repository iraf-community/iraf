include	"sensfunc.h"


# SF_STATS -- Print basic statistics about the stars and the fit.

procedure sf_stats (fd, stds, nstds, function, order, npts, rms)

int	fd			# Output file descriptor (may be STDOUT)
pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
char	function[ARB]		# Fitted function
int	order			# Order of function
int	npts			# Number of points in fit
real	rms			# RMS of fit

int	i, j, n
real	rms1, dev1, dev2, dev3
pointer	sp, str, wts

begin
	# Start with system ID.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sysid (Memc[str], SZ_LINE)

	# Determine beam from first standard star not excluded.
	for (i=1; (i<nstds) && (STD_FLAG(stds[i])==SF_EXCLUDE); i=i+1)
	    ;
	call fprintf (fd, "%s\n")
	    call pargstr (Memc[str])
	call fprintf (fd, "Sensitivity function for aperture %d:\n")
	    call pargi (STD_BEAM(stds[i]))
	call fprintf (fd,
        "Fitting function is %s of order %d with %d points and RMS of %6.4f.\n")
	    call pargstr (function)
	    call pargi (order)
	    call pargi (npts)
	    call pargr (rms)

	call fprintf (fd, "%12s %7s %7s %7s %7s %7s %7s %7s\n")
	    call pargstr ("Image")
	    call pargstr ("Airmass")
	    call pargstr ("Points")
	    call pargstr ("Shift")
	    call pargstr ("RMS Fit")
	    call pargstr ("Dev 1")
	    call pargstr ("Dev 2")
	    call pargstr ("Dev 3")

	do i = 1, nstds {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next

	    n = 0
	    wts = STD_WTS(stds[i]) - 1
	    for (j=1; j<=STD_NWAVES(stds[i]); j=j+1)
		if (Memr[wts+j] != 0.)
		    n = n + 1
	    if ((i == nstds-1) && (n == 0))
		next

	    call sf_devs (stds[i], rms1, dev1, dev2, dev3)

	    call fprintf (fd, "%12s %7.3f %7d %7.4f %7.4f %7.4f %7.4f %7.4f")
		call pargstr (STD_IMAGE(stds[i]))
		call pargr (STD_AIRMASS(stds[i]))
		call pargi (n)
		call pargr (STD_SHIFT(stds[i]))
		call pargr (rms1)
		call pargr (dev1)
		call pargr (dev2)
		call pargr (dev3)

	    if (n == 0) {
	        call fprintf (fd, "%s")
		    call pargstr (" <-- deleted")
	    }
	    call fprintf (fd, "\n")
	}

	# Trailing spacer
	call fprintf (fd, "\n")
end


# SF_DEVS - Compute rms and mean deviations from the fit.
# The deviations are computed in three segments.

procedure  sf_devs (std, rms, dev1, dev2, dev3)

pointer	std		# Standard star data
real	rms		# RMS about fit
real	dev1		# Average deviation in first third of data
real	dev2		# Average deviation in second third of data
real	dev3		# Average deviation in last third of data

int	i, ndev1, ndev2, ndev3, nrms, nbin, nwaves
real	dev
pointer	sens, fit, wts

begin
	# Get elements froms standard star structure.
	nwaves = STD_NWAVES(std)
	sens = STD_SENS(std)
	fit = STD_FIT(std)
	wts = STD_WTS(std)

	# Divide into thirds.
	rms = 0.
	ndev1 = 0
	dev1 = 0.
	nbin = nwaves / 3
	for (i=1; i<= nbin; i=i+1)
	    if (Memr[wts+i-1] != 0.) {
		dev = Memr[sens+i-1] - Memr[fit+i-1]
		dev1 = dev1 + dev
		rms = rms + dev ** 2
		ndev1 = ndev1 + 1
	    }
	if (ndev1 > 0)
	    dev1 = dev1 / ndev1

	ndev2 = 0
	dev2 = 0.
	nbin = 2 * nwaves / 3
	for (; i<=nbin; i=i+1)
	    if (Memr[wts+i-1] != 0.) {
		dev = Memr[sens+i-1] - Memr[fit+i-1]
		dev2 = dev2 + dev
		rms = rms + dev ** 2
		ndev2 = ndev2 + 1
	    }
	if (ndev2 > 0)
	    dev2 = dev2 / ndev2

	ndev3 = 0
	dev3 = 0.
	nbin = nwaves
	for (; i<=nbin; i=i+1)
	    if (Memr[wts+i-1] != 0.) {
		dev = Memr[sens+i-1] - Memr[fit+i-1]
		dev3 = dev3 + dev
		rms = rms + dev ** 2
		ndev3 = ndev3 + 1
	    }
	if (ndev3 > 0)
	    dev3 = dev3 / ndev3

	nrms = ndev1 + ndev2 + ndev3
	if (nrms > 0)
	    rms = sqrt (rms / nrms)
end
