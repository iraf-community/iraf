include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"

# AP_GCPARS -- Procedure to read in the centering parameters from the
# appropriate parameters files.

procedure ap_gcpars (ap)

pointer	ap			# pointer to centering structure

int	function, noise
pointer	sp, str, pp, np
real	cbox, fwhmpsf
bool	clgetb(), clgpsetb()
int	strdic(), clgpseti(), btoi()
pointer	clopset()
real	clgpsetr()

begin
	# Open the pset parameter files.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	pp = clopset ("centerpars")
	np = clopset ("datapars")

	# Get the centering algorithm and box size.
	call clgpset (pp, "calgorithm", Memc[str], SZ_LINE)
	function = strdic (Memc[str], Memc[str], SZ_LINE, CFUNCS)
	cbox = clgpsetr (pp, "cbox") / 2.0

	# Get the noise function.
	call clgpset (np, "noise", Memc[str], SZ_LINE)
	fwhmpsf = clgpsetr (np, "fwhmpsf")
	noise = strdic (Memc[str], Memc[str], SZ_LINE, NFUNCS)

	# Set the remaining centering parameters.
	call apcinit (ap, function, cbox, fwhmpsf, noise)
	call apsetr (ap, MAXSHIFT, clgpsetr (pp, "maxshift"))
	call apsetr (ap, MINSNRATIO, clgpsetr (pp, "minsnratio"))
	call apseti (ap, CMAXITER, clgpseti (pp, "cmaxiter"))
	call apseti (ap, CLEAN, btoi (clgpsetb (pp, "clean")))
	call apsetr (ap, RCLEAN, clgpsetr (pp, "rclean"))
	call apsetr (ap, RCLIP, clgpsetr (pp, "rclip"))
	call apsetr (ap, SIGMACLEAN, clgpsetr (pp, "kclean"))
	call apseti (ap, MKCENTER, btoi (clgpsetb (pp, "mkcenter")))

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

	# Make radial plots on stdgraph.
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))

	# Close the parameter set files.
	call clcpset (pp)
	call clcpset (np)
	call sfree (sp)
end
