include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

# AP_PYPARS -- Procedure to write the current polyphot parameters to the
# output file.

procedure ap_pypars (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str, sky, cp, np, pp
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Allocate the space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the psets.
	cp = clopset ("centerpars")
	sky = clopset ("fitskypars")
	np = clopset ("datapars")
	pp = clopset ("polypars")

	# Set the centering algorithm parameter.
	call apstats (ap, CSTRING, Memc[str], SZ_FNAME)
	call clppset (cp, "calgorithm", Memc[str], SZ_LINE)

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

	# Set the sky fitting algorithm parameter.
	call apstats (ap, SSTRING, Memc[str], SZ_LINE)
	call clppset (sky, "salgorithm", Memc[str], SZ_FNAME)

	# Set the remainder of the sky fitting parameters.
	call clppsetr (sky, "annulus", apstatr (ap, ANNULUS))
	call clppsetr (sky, "dannulus", apstatr (ap, DANNULUS))
	call clppsetr (sky, "skyvalue", apstatr (ap, SKY_BACKGROUND))
	call clppseti (sky, "smaxiter", apstati (ap, SMAXITER))
	call clppsetr (sky, "skreject", apstatr (ap, K2))
	call clppseti (sky, "snreject", apstati (ap, SNREJECT))
	call clppsetr (sky, "khist", apstatr (ap, K1))
	call clppsetr (sky, "binsize", apstatr (ap, BINSIZE))
	call clppsetb (sky, "smooth", itob (apstati (ap, SMOOTH))) 
	call clppsetr (sky, "rgrow", apstatr (ap, RGROW))
	call clppsetb (sky, "mksky", itob (apstati (ap, MKSKY)))

	# Set the data noise model.
	call apstats (ap, NSTRING, Memc[str], SZ_FNAME)
	call clppset (np, "noise", Memc[str], SZ_LINE)

	# Set the rest of the data dependent parameters.
	call clppsetr (np, "fwhmpsf", apstatr (ap, FWHMPSF))
	call clppsetr (np, "scale", 1.0 / apstatr (ap, SCALE))
	call clppsetr (np, "datamin", apstatr (ap, DATAMIN))
	call clppsetr (np, "datamax", apstatr (ap, DATAMAX))
	call clppsetb (np, "emission", itob (apstati (ap, POSITIVE)))
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

	# Set the photometry parameters.
	call clppsetr (pp, "zmag", apstatr (ap, PYZMAG))
	call clppsetb (pp, "mkpolygon", itob (apstati (ap, MKPOLYGON)))

	# Close the pset files.
	call clcpset (cp)
	call clcpset (sky)
	call clcpset (np)
	call clcpset (pp)
	call sfree (sp)
end
