include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/find.h"

# AP_GDAPARS-- Read in the data dependent parameters from the datapars file.

procedure ap_gdapars (ap)

pointer	ap			# pointer to the apphot structure

int	noise
pointer	sp, str, np
bool	clgpsetb()
int	strdic(), btoi()
pointer	clopset()
real	clgpsetr()

begin
	# Allocate workin space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the pset parameter file.
	np = clopset ("datapars")

	# Get the data dependent parameters.
	call apsetr (ap, FWHMPSF, clgpsetr (np, "fwhmpsf"))
	call apsetr (ap, SCALE, 1.0 / clgpsetr (np, "scale"))
	call apseti (ap, POSITIVE, btoi (clgpsetb (np, "emission")))
	call apsetr (ap, DATAMIN, clgpsetr (np, "datamin"))
	call apsetr (ap, DATAMAX, clgpsetr (np, "datamax"))
	call apsetr (ap, SKYSIGMA, clgpsetr (np, "sigma"))

	# Get the noise function parameters.
	call clgpset (np, "noise", Memc[str], SZ_LINE)
	noise = strdic (Memc[str], Memc[str], SZ_LINE, NFUNCS)
	call apsets (ap, NSTRING, Memc[str])
	call apseti (ap, NOISEFUNCTION, noise)
	call clgpset (np, "gain", Memc[str], SZ_LINE)
	call apsets (ap, GAIN, Memc[str])
	call apsetr (ap, EPADU, clgpsetr (np, "epadu"))
	call clgpset (np, "ccdread", Memc[str], SZ_LINE)
	call apsets (ap, CCDREAD, Memc[str])
	call apsetr (ap, READNOISE, clgpsetr (np, "readnoise"))

	# Get the image header parameters.
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
	call clgpset (np, "obstime", Memc[str], SZ_LINE)
	call apsets (ap, OBSTIME, Memc[str])
	call clgpset (np, "otime", Memc[str], SZ_LINE)
	call apsets (ap, OTIME, Memc[str])

	# Close the parameter set files.
	call clcpset (np)

	call sfree (sp)
end


# AP_GFIPARS -- Read in the object finding parametes from the findpars
# parameter file.

procedure ap_gfipars (ap)

pointer	ap			# pointer to the apphot structure

pointer	pp
bool	clgpsetb()
int	btoi()
pointer	clopset()
real	clgpsetr()

begin
	# Open the pset parameter file.
	pp = clopset ("findpars")

	# Get the kernel statistics.
	call apsetr (ap, NSIGMA, clgpsetr (pp, "nsigma"))
	call apsetr (ap, RATIO, clgpsetr (pp, "ratio"))
	call apsetr (ap, THETA, clgpsetr (pp, "theta"))

	# Get the image detection characteristics.
	call apsetr (ap, THRESHOLD, clgpsetr (pp, "threshold"))
	call apsetr (ap, SHARPLO, clgpsetr (pp, "sharplo"))
	call apsetr (ap, SHARPHI, clgpsetr (pp, "sharphi"))
	call apsetr (ap, ROUNDLO, clgpsetr (pp, "roundlo"))
	call apsetr (ap, ROUNDHI, clgpsetr (pp, "roundhi"))

	# Set the marking parameter.
	call apseti (ap, MKDETECTIONS, btoi (clgpsetb (pp, "mkdetections")))

	# Close the parameter set file.
	call clcpset (pp)
end
