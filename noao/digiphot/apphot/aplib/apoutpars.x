include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/polyphot.h"


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
	call apstats (ap, AIRMASS, Memc[str], SZ_LINE)
	call clppset (np, "airmass", Memc[str], SZ_LINE)
	call clppsetr (np, "xairmass", apstatr (ap, XAIRMASS))
	call apstats (ap, FILTER, Memc[str], SZ_LINE)
	call clppset (np, "filter", Memc[str], SZ_LINE)
	call apstats (ap, FILTERID, Memc[str], SZ_LINE)
	call clppset (np, "ifilter", Memc[str], SZ_LINE)

	# Close the pset files.
	call clcpset (np)
	call sfree (sp)
end


# AP_CEPARS -- Procedure to write out the current CENTERPARS parameters
# to the current CENTERPARS parameter file.

procedure ap_cepars (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str, cp
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Open the parameter set.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	cp = clopset ("centerpars")

	# Write the centering parameters.
	call apstats (ap, CSTRING, Memc[str], SZ_FNAME)
	call clppset (cp, "calgorithm", Memc[str], SZ_FNAME)
	call clppsetr (cp, "cbox", 2.0 * apstatr (ap, CAPERT))
	call clppsetr (cp, "maxshift", apstatr (ap, MAXSHIFT))
	call clppsetr (cp, "minsnratio", apstatr (ap, MINSNRATIO))
	call clppseti (cp, "cmaxiter", apstati (ap, CMAXITER))
	call clppsetb (cp, "clean", itob (apstati (ap, CLEAN)))
	call clppsetr (cp, "rclean", apstatr (ap, RCLEAN))
	call clppsetr (cp, "rclip", apstatr (ap, RCLIP))
	call clppsetr (cp, "kclean", apstatr (ap, SIGMACLEAN))
	call clppsetb (cp, "mkcenter", itob (apstati (ap, MKCENTER)))

	# Close the pset file.
	call clcpset (cp)
	call sfree (sp)
end


# AP_SAPARS -- Procedure to write out the current FITSKYPARS parameters
# to the FITSKYPARS file.

procedure ap_sapars (ap)

pointer	ap		# pointer to apphot structure

pointer	mp, str, sp
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Open the parameter sets.
	call smark (mp)
	call salloc (str, SZ_LINE, TY_CHAR)
	sp = clopset ("fitskypars")

	# Set the sky fitting parameters.
	call apstats (ap, SSTRING, Memc[str], SZ_FNAME)
	call clppset (sp, "salgorithm", Memc[str], SZ_LINE)
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

	# Close up the pset files.
	call clcpset (sp)
	call sfree (mp)
end


# AP_PHPARS -- Procedure to write out the PHOTPARS parameters to the
# PHOTPARS task.

procedure ap_phpars (ap)

pointer	ap		# pointer to apphot structure

pointer	mp, str, pp
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Open the parameter set.
	call smark (mp)
	call salloc (str, SZ_LINE, TY_CHAR)
	pp = clopset ("photpars")

	# Set the photometry parameters.
	call apstats (ap, APERTS, Memc[str], SZ_LINE)
	call clppset (pp, "apertures", Memc[str], SZ_LINE)
	call clppsetr (pp, "zmag", apstatr (ap, ZMAG))
	call apstats (ap, PWSTRING, Memc[str], SZ_FNAME)
	call clppset (pp, "weighting", Memc[str], SZ_LINE)
	call clppsetb (pp, "mkapert", itob (apstati (ap, MKAPERT)))

	# Close the pset file.
	call clcpset (pp)
	call sfree (mp)
end


# AP_POPARS -- Procedure to write the current POLYPARS parameters to the
# current POLYPARS parameter file.

procedure ap_popars (ap)

pointer	ap		# pointer to apphot structure

pointer	pp
bool	itob()
int	apstati()
pointer	clopset()
real	apstatr()

begin
	# Open the psets.
	pp = clopset ("polypars")

	# Set the photometry parameters.
	call clppsetr (pp, "zmag", apstatr (ap, PYZMAG))
	call clppsetb (pp, "mkpolygon", itob (apstati (ap, MKPOLYGON)))

	# Close the pset files.
	call clcpset (pp)
end
