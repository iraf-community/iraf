include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/find.h"

# AP_FDGPARS -- Open up the apphot data structure and get the input
# parameters.

procedure ap_fdgpars (ap)

pointer	ap			# pointer to the apphot structure

int	noise
pointer	sp, str, np
real	fwhmpsf

bool	clgetb(), clgpsetb()
int	strdic(), btoi()
pointer	clopset()
real	clgpsetr(), clgetr()

begin

	# Open the pset parameter files.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	np = clopset ("datapars")

	# Get the fwhmpsf and noise function.
	call clgpset (np, "noise", Memc[str], SZ_LINE)
	fwhmpsf = clgpsetr (np, "fwhmpsf")
	noise = strdic (Memc[str], Memc[str], SZ_LINE, NFUNCS)

	# Open the apphot structure.
	call ap_fdinit (ap, fwhmpsf, noise)

	# Set the remaining noise parameters.
	call apsetr (ap, SCALE, 1.0 / clgpsetr (np, "scale"))
	call apseti (ap, POSITIVE, btoi (clgpsetb (np, "emission")))
	call apsetr (ap, DATAMIN, clgpsetr (np, "datamin"))
	call apsetr (ap, DATAMAX, clgpsetr (np, "datamax"))
	call clgpset (np, "gain", Memc[str], SZ_LINE)
	call apsets (ap, GAIN, Memc[str])
	call apsetr (ap, THRESHOLD, clgpsetr (np, "threshold"))
	call apsetr (ap, CTHRESHOLD, clgpsetr (np, "cthreshold"))
	call apsetr (ap, SKYSIGMA, clgpsetr (np, "sigma"))
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

	# Get the remaining kernel statistics.
	call apsetr (ap, RATIO, clgetr ("ratio"))
	call apsetr (ap, THETA, clgetr ("theta"))
	call apsetr (ap, NSIGMA, clgetr ("nsigma"))

	# Get the image detection characteristics.
	call apsetr (ap, SHARPLO, clgetr ("sharplo"))
	call apsetr (ap, SHARPHI, clgetr ("sharphi"))
	call apsetr (ap, ROUNDLO, clgetr ("roundlo"))
	call apsetr (ap, ROUNDHI, clgetr ("roundhi"))

	# Set the marking command.
	call apseti (ap, MKDETECTIONS, btoi (clgetb ("mkdetections")))

	# Close the parameter set files.
	call clcpset (np)
	call sfree (sp)
end
