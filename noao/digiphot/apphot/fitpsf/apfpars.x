include "../lib/apphot.h"
include "../lib/fitpsf.h"
include "../lib/noise.h"
include "../lib/display.h"

# AP_PPFPARS -- Procedure to write the fitpsf parameters to a parameter file.

procedure ap_ppfpars (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str, np
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Initialize and open psets.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	np =  clopset ("datapars")

	# Store the psf fitting parameters.
	call clputr ("box", 2.0 * apstatr (ap, PSFAPERT))
	call apstats (ap, PSFSTRING, Memc[str], SZ_FNAME)
	call clpstr ("function", Memc[str], SZ_FNAME)
	call clputi ("maxiter", apstati (ap, PMAXITER))
	call clputr ("kreject", apstatr (ap, PK2))
	call clputi ("nreject", apstati (ap, PNREJECT))
	call clputb ("mkbox", itob (apstati (ap, MKPSFBOX)))

	# Store the data dependent parameters.
	call apstats (ap, NSTRING, Memc[str], SZ_FNAME)
	call clppset (np, "noise", Memc[str], SZ_LINE)
	call clppsetr (np, "scale", 1.0 / apstatr (ap, SCALE))
	call clppsetr (np, "fwhmpsf", apstatr (ap, FWHMPSF))
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

	# Close the psets.
	call clcpset (np)
	call sfree (sp)
end
