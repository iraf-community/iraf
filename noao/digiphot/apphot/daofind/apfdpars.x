include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/find.h"


# AP_FDPARS -- Procedure to write out the current centering parameters
# to the current parameter files.

procedure ap_fdpars (ap)

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
	call clppset (np, "noise", Memc[str], SZ_FNAME)

	# Get the rest of the data dependent parameters.
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

	# Get the daofind parameters.
	call clputr ("daofind.nsigma", apstatr (ap, NSIGMA) )
	call clputr ("daofind.ratio", apstatr (ap, RATIO))
	call clputr ("daofind.theta", apstatr (ap, THETA))
	call clputr ("daofind.sharplo", apstatr (ap, SHARPLO))
	call clputr ("daofind.sharphi", apstatr (ap, SHARPHI))
	call clputr ("daofind.roundlo", apstatr (ap, ROUNDLO))
	call clputr ("daofind.roundhi", apstatr (ap, ROUNDHI))

	# Close the pset files.
	call clcpset (np)
	call sfree (sp)
end
