include "../lib/daophot.h"

# DP_GPPARS -- Procedure to fetch the daophot task parameters.

procedure dp_gppars (dao, im)

pointer	dao		# pointer to daophot structure
pointer	im		# input image descriptor

int	dp, dap
pointer	mp, str
real	scale, psfrad, fitrad, matchrad

bool	clgetb(), clgpsetb()
int	clgpseti(), btoi()
pointer	clopset()
real	clgpsetr()

begin
	# Allocate working space.
	call smark (mp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Open the daophot structure.
	call dp_init (dao)

	# Set the package parameter text and initialize the verbose switch.
	call dp_seti (dao, TEXT, btoi (clgetb ("text")))
	call dp_seti (dao, VERBOSE, btoi (false))

	# Open the datapars parameter set.
	dp = clopset ("datapars")

	# Set the datapars parameters.
	scale = clgpsetr (dp, "scale")
	call dp_setr (dao, SCALE, scale)
	call dp_setr (dao, MAXGDATA, clgpsetr (dp, "datamax"))
	call dp_setr (dao, MINGDATA, clgpsetr (dp, "datamin"))

	# Initialize the observing parameters.
	call clgpset (dp, "ccdread", Memc[str], SZ_FNAME)
	call dp_sets (dao, CCDREAD, Memc[str])
	call dp_setr (dao, READ_NOISE, clgpsetr (dp, "readnoise"))
	call clgpset (dp, "gain", Memc[str], SZ_FNAME)
	call dp_sets (dao, CCDGAIN, Memc[str])
	call dp_setr (dao, PHOT_ADC, clgpsetr (dp, "epadu"))
	call dp_setr (dao, ITIME, 1.0)
	call clgpset (dp, "airmass", Memc[str], SZ_FNAME)
	call dp_sets (dao, AIRMASS, Memc[str])
	call dp_setr (dao, XAIRMASS, clgpsetr (dp, "xairmass"))
	call clgpset (dp, "filter", Memc[str], SZ_FNAME)
	call dp_sets (dao, FILTER, Memc[str])
	call clgpset (dp, "ifilter", Memc[str], SZ_FNAME)
	call dp_rmwhite (Memc[str], Memc[str], SZ_FNAME)
	call dp_sets (dao, IFILTER, Memc[str])
	call clgpset (dp, "obstime", Memc[str], SZ_FNAME)
	call dp_sets (dao, OBSTIME, Memc[str])
	call clgpset (dp, "otime", Memc[str], SZ_FNAME)
	call dp_sets (dao, OTIME, Memc[str])


	# Close the datapars parameter set.
	call clcpset (dp)

	# Open the daopars parameter set.
	dap = clopset ("daopars")

	# Set the daopars parameters.
	call dp_seti (dao, VARPSF, btoi (clgpsetb (dap, "varpsf")))
	psfrad = clgpsetr (dap, "psfrad")
	call dp_setr (dao, RPSFRAD, psfrad)
	fitrad = clgpsetr (dap, "fitrad")
	call dp_setr (dao, SFITRAD, fitrad)
	matchrad = clgpsetr (dap, "matchrad")
	call dp_setr (dao, SMATCHRAD, matchrad)
	call dp_setr (dao, CRITOVLAP, clgpsetr (dap, "critoverlap"))
	call dp_seti (dao, MAXITER, clgpseti (dap, "maxiter"))
	call dp_seti (dao, MAXGROUP, clgpseti (dap, "maxgroup"))
	call dp_seti (dao, MAXSTAR, clgpseti (dap, "maxnstar"))
	call dp_seti (dao, RECENTER, btoi (clgpsetb (dap, "recenter")))
	call dp_setr (dao, CLIPRANGE, clgpsetr (dap, "cliprange"))
	call dp_seti (dao, CLIPEXP, clgpseti (dap, "clipexp"))

	# Close the datapars and daopars pset files.
	call clcpset (dap)

	# Compute the psf radius, fitting radius and matching radius in
	# pixels.

	call dp_setr (dao, SPSFRAD, psfrad)
	call dp_setr (dao, PSFRAD, psfrad / scale)
	call dp_setr (dao, FITRAD, fitrad / scale)
	call dp_setr (dao, MATCHRAD, matchrad / scale)

	# Get the proper values from the image header if an image is defined.
	if (im != NULL) {
	    call dp_padu (im, dao)
	    call dp_rdnoise (im, dao)
	    call dp_filter (im, dao)
	    call dp_airmass (im, dao)
	    call dp_otime (im, dao)
	}

	call sfree (mp)
end
