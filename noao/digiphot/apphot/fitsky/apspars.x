include "../lib/apphot.h"
include "../lib/fitsky.h"
include "../lib/display.h"
include "../lib/noise.h"

# AP_SPPARS -- Procedure to write out the current sky fitting parameters
# to the parameter files.

procedure ap_sppars (ap)

pointer	ap		# pointer to apphot structure

pointer	mp, str, sp, np
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Open the parameter sets.
	call smark (mp)
	call salloc (str, SZ_LINE, TY_CHAR)
	sp = clopset ("fitskypars")
	np = clopset ("datapars")

	# Set the sky fitting algorithm.
	call apstats (ap, SSTRING, Memc[str], SZ_FNAME)
	call clppset (sp, "salgorithm", Memc[str], SZ_LINE)

	# Set the remainder of the sky fitting parameters.
	call clppsetr (sp, "annulus", apstatr (ap, ANNULUS))
	call clppsetr (sp, "dannulus", apstatr (ap, DANNULUS))
	call clppsetr (sp, "skyvalue", apstatr (ap, SKY_BACKGROUND))
	call clppseti (sp, "smaxiter", apstati (ap, SMAXITER))
	call clppsetr (sp, "skreject", apstatr (ap, K2))
	call clppseti (sp, "snreject", apstati (ap, SNREJECT))
	call clppsetr (sp, "khist", apstatr (ap, K1))
	call clppsetr (sp, "binsize", apstatr (ap, BINSIZE))
	call clppsetb (sp, "smooth", itob (apstati (ap, SMOOTH))) 
	call clppsetr (sp, "rgrow", apstatr (ap, RGROW))
	call clppsetb (sp, "mksky", itob (apstati (ap, MKSKY)))

	# Set the noise model.
	call apstats (ap, NSTRING, Memc[str], SZ_LINE)
	call clppset (np, "noise", Memc[str], SZ_LINE)

	# Set the rest of the data dependent parameters.
	call clppsetr (np, "fwhmpsf", apstatr (ap, FWHMPSF))
	call clppsetr (np, "scale", 1.0 / apstatr (ap, SCALE))
	call clppsetb (np, "emission", itob (apstati (ap, POSITIVE)))
	call clppsetr (np, "datamin", apstatr (ap, DATAMIN))
	call clppsetr (np, "datamax", apstatr (ap, DATAMAX))
	call clppsetr (np, "threshold", apstatr (ap, THRESHOLD))
	call clppsetr (np, "cthreshold", apstatr (ap, CTHRESHOLD))
	call clppsetr (np, "sigma", apstatr (ap, SKYSIGMA))
	call apstats (ap, GAIN, Memc[str], SZ_LINE)
	call clppset (np, "gain", Memc[str], SZ_LINE)
	call clppsetr (np, "epadu", apstatr (ap, EPADU))
	call apstats (ap, CCDREAD, Memc[str], SZ_LINE)
	call clppset (np, "ccdread", Memc[str], SZ_LINE)
	call clppsetr (np, "readnoise", apstatr (ap, READNOISE))
	call apstats (ap, EXPOSURE, Memc[str], SZ_LINE)
	call clppset (np, "exposure", Memc[str], SZ_LINE)
	call clppsetr (np, "itime", apstatr (ap, ITIME))

	# Radial profile plots
	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))

	# Close up the pset files.
	call clcpset (sp)
	call clcpset (np)
	call sfree (mp)
end
