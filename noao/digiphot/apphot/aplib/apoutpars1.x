include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/find.h"


# AP_DAPARS -- Procedure to write out the current DATAPARS parameters
# to the current DATAPARS parameter file.

procedure ap_dapars (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str, np
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Open the parameter sets.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	np = clopset ("datapars")

	# Set the noise model.
	call apstats (ap, NSTRING, Memc[str], SZ_FNAME)
	call clppset (np, "noise", Memc[str])

	# Get the rest of the data dependent parameters.
	call clppsetr (np, "fwhmpsf", apstatr (ap, FWHMPSF))
	call clppsetr (np, "scale", 1.0 / apstatr (ap, SCALE))
	call clppsetb (np, "emission", itob (apstati (ap, POSITIVE)))
	call clppsetr (np, "datamin", apstatr (ap, DATAMIN))
	call clppsetr (np, "datamax", apstatr (ap, DATAMAX))

	call clppsetr (np, "sigma", apstatr (ap, SKYSIGMA))
	call apstats (ap, GAIN, Memc[str], SZ_LINE)
	call clppset (np, "gain", Memc[str])
	call clppsetr (np, "epadu", apstatr (ap, EPADU))
	call apstats (ap, CCDREAD, Memc[str], SZ_LINE)
	call clppset (np, "ccdread", Memc[str])
	call clppsetr (np, "readnoise", apstatr (ap, READNOISE))

	call apstats (ap, EXPOSURE, Memc[str], SZ_LINE)
	call clppset (np, "exposure", Memc[str])
	call clppsetr (np, "itime", apstatr (ap, ITIME))

	call apstats (ap, AIRMASS, Memc[str], SZ_LINE)
	call clppset (np, "airmass", Memc[str])
	call clppsetr (np, "xairmass", apstatr (ap, XAIRMASS))

	call apstats (ap, FILTER, Memc[str], SZ_LINE)
	call clppset (np, "filter", Memc[str])
	call apstats (ap, FILTERID, Memc[str], SZ_LINE)
	call clppset (np, "ifilter", Memc[str])

	call apstats (ap, OBSTIME, Memc[str], SZ_LINE)
	call clppset (np, "obstime", Memc[str])
	call apstats (ap, OTIME, Memc[str], SZ_LINE)
	call clppset (np, "otime", Memc[str])

	# Close the pset files.
	call clcpset (np)
	call sfree (sp)
end


# AP_FIPARS -- Procedure to write out the current FINDPARS parameters
# to the current FINDPARS parameter file.

procedure ap_fipars (ap)

pointer	ap		# pointer to apphot structure

pointer	pp
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	pp = clopset ("findpars")

	call clppsetr (pp, "nsigma", apstatr (ap, NSIGMA) )
	call clppsetr (pp, "ratio", apstatr (ap, RATIO))
	call clppsetr (pp, "theta", apstatr (ap, THETA))

	call clppsetr (pp, "threshold", apstatr (ap, THRESHOLD))
	call clppsetr (pp, "sharplo", apstatr (ap, SHARPLO))
	call clppsetr (pp, "sharphi", apstatr (ap, SHARPHI))
	call clppsetr (pp, "roundlo", apstatr (ap, ROUNDLO))
	call clppsetr (pp, "roundhi", apstatr (ap, ROUNDHI))

	call clppsetb (pp, "mkdetections", itob (apstati (ap, MKDETECTIONS)))

	call clcpset (pp)
end
