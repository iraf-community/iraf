include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/fitpsf.h"

# AP_GPFPARS -- Procedure to fetch the fitpsf parameters.

procedure ap_gpfpars (ap)

pointer	ap		# pointer to the apphot structure

int	noise, function
pointer	sp, str, np
real	box, fwhmpsf
bool	clgetb(), clgpsetb()
int	clgwrd(), clgeti(), strdic(), btoi()
pointer	clopset()
real	clgetr(), clgpsetr()

begin
	# Initialize and open psets.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	np = clopset ("datapars")

	# Open the PSF fitting structure.
	function = clgwrd ("function", Memc[str], SZ_LINE, PSFFUNCS)
	box = clgetr ("box") / 2.0
	fwhmpsf = clgpsetr (np, "fwhmpsf")
	call clgpset (np, "noise", Memc[str], SZ_LINE)
	noise = strdic (Memc[str], Memc[str], SZ_LINE, NFUNCS)
	call apsfinit (ap, function, box, fwhmpsf, noise)

	# Get the rest of the PSF fitting parameters.
	call apseti (ap, PMAXITER, clgeti ("maxiter"))
	call apseti (ap, PNREJECT, clgeti ("nreject"))
	call apsetr (ap, PK2, clgetr ("kreject"))
	call apseti (ap, MKPSFBOX, btoi (clgetb ("mkbox")))

	# Get the rest of noise model parameters.
	call apsetr (ap, SCALE, 1.0 / clgpsetr (np, "scale"))
	call apseti (ap, POSITIVE, btoi (clgpsetb (np, "emission")))
	call apsetr (ap, DATAMIN, clgpsetr (np, "datamin"))
	call apsetr (ap, DATAMAX, clgpsetr (np, "datamax"))
	call apsetr (ap, THRESHOLD, clgpsetr (np, "threshold"))
	call apsetr (ap, CTHRESHOLD, clgpsetr (np, "cthreshold"))
	call apsetr (ap, SKYSIGMA, clgpsetr (np, "sigma"))
	call clgpset (np, "gain", Memc[str], SZ_LINE)
	call apsets (ap, GAIN, Memc[str], SZ_LINE)
	call apsetr (ap, EPADU, clgpsetr (np, "epadu"))
	call clgpset (np, "ccdread", Memc[str], SZ_LINE)
	call apsets (ap, CCDREAD, Memc[str], SZ_LINE)
	call apsetr (ap, READNOISE, clgpsetr (np, "readnoise"))

	call clgpset (np, "exposure", Memc[str], SZ_LINE)
	call apsets (ap, EXPOSURE, Memc[str], SZ_LINE)
	call apsetr (ap, ITIME, clgpsetr (np, "itime"))
	call clgpset (np, "airmass", Memc[str], SZ_LINE)
	call apsets (ap, AIRMASS, Memc[str], SZ_LINE)
	call apsetr (ap, XAIRMASS, clgpsetr (np, "xairmass"))
	call clgpset (np, "filter", Memc[str], SZ_LINE)
	call apsets (ap, FILTER, Memc[str], SZ_LINE)
	call clgpset (np, "ifilter", Memc[str], SZ_LINE)
	call apsets (ap, FILTERID, Memc[str], SZ_LINE)

	# Close up the psets.
	call clcpset (np)
	call sfree (sp)
end
