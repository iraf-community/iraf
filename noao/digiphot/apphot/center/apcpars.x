include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/display.h"
include "../lib/noise.h"

define	MAX_NAPERTS	15

# AP_PCPARS -- Procedure to write out the current centering parameters
# to the current parameter files.

procedure ap_pcpars (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str, cp, np
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Open the parameter sets.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	cp = clopset ("centerpars")
	np = clopset ("datapars")

	# Get the centering function.
	call apstats (ap, CSTRING, Memc[str], SZ_FNAME)
	call clppset (cp, "calgorithm", Memc[str], SZ_FNAME)

	# Get the rest of the centering parameters.
	call clppsetr (cp, "cbox", 2.0 * apstatr (ap, CAPERT))
	call clppsetr (cp, "maxshift", apstatr (ap, MAXSHIFT))
	call clppsetr (cp, "minsnratio", apstatr (ap, MINSNRATIO))
	call clppseti (cp, "cmaxiter", apstati (ap, CMAXITER))
	call clppsetb (cp, "clean", itob (apstati (ap, CLEAN)))
	call clppsetr (cp, "rclean", apstatr (ap, RCLEAN))
	call clppsetr (cp, "rclip", apstatr (ap, RCLIP))
	call clppsetr (cp, "kclean", apstatr (ap, SIGMACLEAN))
	call clppsetb (cp, "mkcenter", itob (apstati (ap, MKCENTER)))

	# Get the noise model.
	call apstats (ap, NSTRING, Memc[str], SZ_FNAME)
	call clppset (np, "noise", Memc[str], SZ_FNAME)

	# Get the rest of the data dependent parameters.
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

	# Set the plotting command.
	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))

	# Close the pset files.
	call clcpset (cp)
	call clcpset (np)
	call sfree (sp)
end
