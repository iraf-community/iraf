include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/fitsky.h"

# AP_SGPARS -- Procedure to fetch the parameters for the fitsky task.

procedure ap_sgpars (ap)

pointer	ap		# pointer to apphot structure

real	fwhmpsf
int	function, nfunction
pointer	sp, pp, np, str
real	annulus, dannulus

bool	clgetb(), clgpsetb()
int	strdic(), clgpseti(), btoi()
pointer	clopset()
real	clgpsetr()

begin
	# Open pset files.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	np = clopset ("datapars")
	pp = clopset ("fitskypars")

	# Get the sky fitting algorithm and annulus parameters
	call clgpset (pp, "salgorithm", Memc[str], SZ_LINE)
	function = strdic (Memc[str], Memc[str], SZ_LINE, SFUNCS)
	annulus = clgpsetr (pp, "annulus")
	dannulus = clgpsetr (pp, "dannulus")

	# Get the full width half maximum and the noise parameters.
	fwhmpsf = clgpsetr (np, "fwhmpsf")
	call clgpset (np, "noise", Memc[str], SZ_LINE)
	nfunction = strdic (Memc[str], Memc[str], SZ_LINE, NFUNCS)

	# Open the sky fitting structure and set the remaining parameters.
	call apsinit (ap, function, annulus, dannulus, fwhmpsf, nfunction)
	call apsetr (ap, K1, clgpsetr (pp, "khist"))
	call apsetr (ap, K2, clgpsetr (pp, "skreject"))
	call apsetr (ap, SKY_BACKGROUND, clgpsetr (pp, "skyvalue"))
	call apseti (ap, SMAXITER, clgpseti (pp, "smaxiter"))
	call apseti (ap, SNREJECT, clgpseti (pp, "snreject"))
	call apsetr (ap, BINSIZE, clgpsetr (pp, "binsize"))
	call apseti (ap, SMOOTH, btoi (clgpsetb (pp, "smooth")))
	call apsetr (ap, RGROW, clgpsetr (pp, "rgrow"))
	call apseti (ap, MKSKY, btoi (clgpsetb (pp, "mksky")))

	# Set the remaining data dependent parameters.
	call apsetr (ap, SCALE, 1.0 / clgpsetr (np, "scale"))
	call apsetr (ap, DATAMIN, clgpsetr (np, "datamin"))
	call apsetr (ap, DATAMAX, clgpsetr (np, "datamax"))
	call apseti (ap, POSITIVE, btoi (clgpsetb (np, "emission")))
	call apsetr (ap, THRESHOLD, clgpsetr (np, "threshold"))
	call apsetr (ap, CTHRESHOLD, clgpsetr (np, "cthreshold"))
	call apsetr (ap, SKYSIGMA, clgpsetr (np, "sigma"))
	call clgpset (np, "gain", Memc[str], SZ_LINE)
	call apsets (ap, GAIN, Memc[str])
	call apsetr (ap, EPADU, clgpsetr (np, "epadu"))
	call clgpset (np, "ccdread", Memc[str], SZ_LINE)
	call apsets (ap, CCDREAD, Memc[str])
	call apsetr (ap, READNOISE, clgpsetr (np, "readnoise"))

	call clgpset (np, "exposure", Memc[str], SZ_LINE)
	call apsets (ap, EXPOSURE, Memc[str])
	call apsetr (ap, ITIME, clgpsetr (np, "itime"))
	call clgpset (np, "airmass", Memc[str], SZ_LINE)
	call apsets (ap, AIRMASS, Memc[str])
	call apsetr (ap, XAIRMASS, clgpsetr (np, "xairmass"))
	call clgpset (np, "filter", Memc[str], SZ_LINE)
	call apsets (ap, FILTER, Memc[str])
	call clgpset (np, "ifilter", Memc[str], SZ_LINE)
	call apsets (ap, FILTERID, Memc[str])

	# Get radial plots.
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))

	# Close up the pset files.
	call clcpset (pp)
	call clcpset (np)
	call sfree (sp)
end
