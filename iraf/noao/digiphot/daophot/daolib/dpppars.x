include "../lib/daophotdef.h"

# DP_PPPARS -- Store the daophot package parameters in the pset files.

procedure dp_pppars (dao)

pointer	dao		# pointer to daophot structure

pointer	sp, str, fstr, dap
bool	itob()
int	dp_stati(), strlen()
pointer	clopset()
real	dp_statr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (fstr, SZ_FNAME, TY_CHAR)

	# Set the package parameter text.
	call clputb ("text", itob (dp_stati (dao, TEXT)))

	# Get and set the datapars parameter sets.
	dap = clopset ("datapars")

	# Store the data dependent parameters.
	call clppsetr (dap, "scale", dp_statr (dao, SCALE))
	call clppsetr (dap, "fwhmpsf", dp_statr (dao, SFWHMPSF))
	call clppsetr (dap, "datamin", dp_statr (dao, MINGDATA))
	call clppsetr (dap, "datamax", dp_statr (dao, MAXGDATA))

	# Store the noise parameters.
	call dp_stats (dao, CCDGAIN, Memc[str], SZ_FNAME)
	call clppset (dap, "gain", Memc[str])
	call clppsetr (dap, "epadu", dp_statr (dao, PHOTADU))
	call dp_stats (dao, CCDREAD, Memc[str], SZ_FNAME)
	call clppset (dap, "ccdread", Memc[str])
	call clppsetr (dap, "readnoise", dp_statr (dao, READNOISE))

	# Store the observing parameters.
	call dp_stats (dao, EXPTIME, Memc[str], SZ_FNAME)
	call clppset (dap, "exposure", Memc[str])
	call clppsetr (dap, "itime", dp_statr (dao, ITIME))
	call dp_stats (dao, AIRMASS, Memc[str], SZ_FNAME)
	call clppset (dap, "airmass", Memc[str])
	call clppsetr (dap, "xairmass", dp_statr (dao, XAIRMASS))
	call dp_stats (dao, FILTER, Memc[str], SZ_FNAME)
	call clppset (dap, "filter", Memc[str])
	call dp_stats (dao, IFILTER, Memc[str], SZ_FNAME)
	call clppset (dap, "ifilter", Memc[str])
	call dp_stats (dao, OBSTIME, Memc[str], SZ_FNAME)
	call clppset (dap, "obstime", Memc[str])
	call dp_stats (dao, OTIME, Memc[str], SZ_FNAME)
	call clppset (dap, "otime", Memc[str])

	# Close the datapars parameter set.
	call clcpset (dap)

	# Open the daopars parameter set.
	dap = clopset ("daopars")

	# Store the psf function parameters.
	call dp_stats (dao, FUNCLIST, Memc[fstr], SZ_FNAME)
	call strcpy (Memc[fstr+1], Memc[str], strlen(Memc[fstr+1]) - 1)
	call clppset (dap, "function", Memc[str])
	call clppseti (dap, "varorder", dp_stati (dao, VARORDER))
	#call clppsetb (dap, "fexpand", itob (dp_stati (dao, FEXPAND)))
	call clppseti (dap, "nclean", dp_stati (dao, NCLEAN))
	call clppsetb (dap, "saturated", itob (dp_stati (dao, SATURATED)))
	call clppsetr (dap, "psfrad", dp_statr (dao, SPSFRAD))
	call clppsetr (dap, "matchrad", dp_statr (dao, SMATCHRAD))

	# Store the fitting algorithm parameters.
	call clppsetr (dap, "fitrad", dp_statr (dao, SFITRAD))
	call clppsetr (dap, "sannulus", dp_statr (dao, SANNULUS))
	call clppsetr (dap, "wsannulus", dp_statr (dao, SDANNULUS))
	call clppsetr (dap, "critsnratio", dp_statr (dao, CRITSNRATIO))
	call clppseti (dap, "maxiter", dp_stati (dao, MAXITER))
	call clppseti (dap, "maxgroup", dp_stati (dao, MAXGROUP))
	call clppseti (dap, "maxnstar", dp_stati (dao, MAXNSTAR))
	call clppsetb (dap, "recenter", itob (dp_stati (dao, RECENTER)))
	call clppsetb (dap, "fitsky", itob (dp_stati (dao, FITSKY)))
	call clppsetb (dap, "groupsky", itob (dp_stati (dao, GROUPSKY)))
	call clppsetr (dap, "flaterr", dp_statr (dao, FLATERR))
	call clppsetr (dap, "proferr", dp_statr (dao, PROFERR))
	call clppsetr (dap, "cliprange", dp_statr (dao, CLIPRANGE))
	call clppseti (dap, "clipexp", dp_stati (dao, CLIPEXP))
	call clppsetr (dap, "mergerad", dp_statr (dao, SMERGERAD))

	# Close the daopars parameter set.
	call clcpset (dap)

	call sfree (sp)
end
