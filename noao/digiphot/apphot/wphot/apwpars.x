include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/noise.h"
include "../lib/display.h"

# AP_WPARS -- Procedure to write out the wphot task parameters.

procedure ap_wpars (ap)

pointer	ap		# pointer to apphot structure

pointer	mp, str, np, cp, sp, pp
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	call smark (mp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the parameter sets.
	np = clopset ("datapars")
	cp = clopset ("centerpars")
	sp = clopset ("fitskypars")
	pp = clopset ("photpars")

	# Set the noise model.
	call apstats (ap, NSTRING, Memc[str], SZ_FNAME)
	call clppset (np, "noise", Memc[str], SZ_LINE)

	# Set the rest of the noise parameters.
	call clppsetr (np, "scale", 1.0 / apstatr (ap, SCALE))
	call clppsetr (np, "fwhmpsf", apstatr (ap, FWHMPSF))
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

	# Set the centering function.
	call apstats (ap, CSTRING, Memc[str], SZ_FNAME)
	call clppset (cp, "calgorithm", Memc[str], SZ_LINE)

	# Set the weighting function
	call apstats (ap, PWSTRING, Memc[str], SZ_FNAME)
	call clppset (pp, "weighting", Memc[str], SZ_LINE)

	# Set the rest of the centering parameters.
	call clppsetr (cp, "cbox", 2.0 * apstatr (ap, CAPERT))
	call clppsetr (cp, "maxshift", apstatr (ap, MAXSHIFT))
	call clppsetr (cp, "minsnratio", apstatr (ap, MINSNRATIO))
	call clppseti (cp, "cmaxiter", apstati (ap, CMAXITER))
	call clppsetb (cp, "clean", itob (apstati (ap, CLEAN)))
	call clppsetr (cp, "rclean", apstatr (ap, RCLEAN))
	call clppsetr (cp, "rclip", apstatr (ap, RCLIP))
	call clppsetr (cp, "kclean", apstatr (ap, SIGMACLEAN))
	call clppsetb (cp, "mkcenter", itob (apstati (ap, MKCENTER)))

	# Set the sky fitting algorithm.
	call apstats (ap, SSTRING, Memc[str], SZ_FNAME)
	call clppset (sp, "salgorithm", Memc[str], SZ_LINE)

	# Set the remainder of the sky fitting parameters
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

	# Set the photometry parameters.
	call apstats (ap, APERTS, Memc[str], SZ_LINE)
	call clppset (pp, "apertures", Memc[str], SZ_LINE)
	call clppsetr (pp, "zmag", apstatr (ap, ZMAG))
	call clppsetb (pp, "mkapert", itob (apstati (ap, MKAPERT)))

	# Set the radplots parameter.
	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))

	# Close the pset files.
	call clcpset (np)
	call clcpset (cp)
	call clcpset (sp)
	call clcpset (pp)
	call sfree (mp)
end
