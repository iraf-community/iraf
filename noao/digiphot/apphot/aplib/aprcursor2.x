include "../lib/apphot.h"
include "../lib/radprof.h"
include "../lib/fitpsf.h"

# AP_CRPROF -- Read the radial profile size off the radial profile plot.

real procedure ap_crprof (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# output file sequence number
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, radius, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vrpradius()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Estimate the minimum (maximum) data level.
	# Mark maximum radius of the radial profile.
	call printf ("Mark maximum radius for profile (%g) pixels:")
	    call pargr (apstatr (ap, RPRADIUS) * scale)
	call gscur (gd, apstatr (ap, RPRADIUS) * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk < 0.0 || xjunk > rmax)
	    radius = apstatr (ap, RPRADIUS)
	else
	    radius = xjunk / scale

	# Verify the results.
	call apsetr (ap, RPRADIUS, radius)
	radius = ap_vrpradius (ap)

	# Store the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_RPRADIUS, radius, UN_RSCALEUNIT,
	        "fitting radius")

	call sfree (sp)

	return (radius)
end


# AP_CRPSTEP -- Read the radial profile size off the radial profile plot.

real procedure ap_crpstep (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# output file sequence number
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, step, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vstep()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the radial profile step size.
	call printf ("Mark step size (%g) pixels:")
	    call pargr (apstatr (ap, RPSTEP) * scale)
	call gscur (gd, apstatr (ap, RPSTEP) * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk < 0.0 || xjunk > rmax)
	    step = apstatr (ap, RPSTEP)
	else
	    step = xjunk / scale

	# Verify the results.
	call apsetr (ap, RPSTEP, step)
	step = ap_vstep (ap)

	# Store the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_RPSTEP, step, UN_RSCALEUNIT,
	        "step size in pixels")

	call sfree (sp)

	return (step)
end


# AP_CPAPERT -- Read the fitting radius on the radial profile plot.

real procedure ap_cpapert (ap, gd, out, stid, rmin, rmax, imin, imax)

pointer	ap		# pointer to the apphot structure
pointer	gd		# pointer to the grapics stream
int	out		# output file descriptor
int	stid		# output file sequence number
real	rmin, rmax	# x axis limits
real	imin, imax	# y axis limits

int	wcs, key, stat
pointer	sp, cmd
real	scale, psfapert, xjunk, yjunk
int	clgcur()
real	apstatr(), ap_vpsfapert()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	scale = apstatr (ap, SCALE)

	# Mark the fitting radius on the plot.
	call printf ("Mark fitting box half width (%g) pixels:")
	    call pargr (apstatr (ap, PSFAPERT) * scale)
	call gscur (gd, apstatr (ap, PSFAPERT) * scale, (imin + imax) / 2.0)
	stat = clgcur ("gcommands", xjunk, yjunk, wcs, key, Memc[cmd], SZ_LINE)
	if (stat == EOF || xjunk <= 0.0 || xjunk > rmax)
	    psfapert = apstatr (ap, PSFAPERT)
	else
	    psfapert = xjunk / scale

	# Verify the results.
	call apsetr (ap, PSFAPERT, psfapert)
	psfapert = ap_vpsfapert (ap)

	# Store the results.
	if (out != NULL && stid > 1)
	    call ap_rparam (out, KY_PSFAPERT, 2.0 * psfapert, UN_PSFSCALEUNIT,
	        "width of the fitting box")

	call sfree (sp)

	return (psfapert)
end
