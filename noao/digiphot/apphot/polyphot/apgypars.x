include "../lib/display.h"
include "../lib/fitsky.h"
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/polyphot.h"

# AP_GYPARS -- Procedure to fetch the polyphot task parameters.

procedure ap_gypars (ap)

pointer	ap		# pointer to apphot fitting structure

int	cfunction, sfunction, noise
pointer	mp, str, np, cp, sp, pp
real	fwhmpsf, cbox, annulus, dannulus
bool	clgpsetb()
int	strdic(), clgpseti(), btoi()
pointer	clopset()
real	clgpsetr()

begin
	call smark (mp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open psets.
	np = clopset ("datapars")
	cp = clopset ("centerpars")
	sp = clopset ("fitskypars")
	pp = clopset ("polypars")

	# Get the noise model, sky algorithm and open the polyphot structure.
	fwhmpsf = clgpsetr (np, "fwhmpsf")
	call clgpset (np, "noise", Memc[str], SZ_LINE)
	noise = strdic (Memc[str], Memc[str], SZ_LINE, NFUNCS)
	call clgpset (cp, "calgorithm", Memc[str], SZ_LINE)
	cfunction = strdic (Memc[str], Memc[str], SZ_LINE, CFUNCS)
	cbox = clgpsetr (cp, "cbox") / 2.0
	call clgpset (sp, "salgorithm", Memc[str], SZ_LINE)
	sfunction = strdic (Memc[str], Memc[str], SZ_LINE, SFUNCS)
	annulus = clgpsetr (sp, "annulus")
	dannulus = clgpsetr (sp, "dannulus")
	call apyinit (ap, cfunction, cbox, sfunction, annulus, dannulus,
	    fwhmpsf, noise)

	# Set remaining noise parameters.
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
	call apsets (ap, EXPOSURE, Memc[str])
	call apsetr (ap, ITIME, clgpsetr (np, "itime"))
	call clgpset (np, "airmass", Memc[str], SZ_LINE)
	call apsets (ap, AIRMASS, Memc[str])
	call apsetr (ap, XAIRMASS, clgpsetr (np, "xairmass"))
	call clgpset (np, "filter", Memc[str], SZ_LINE)
	call apsets (ap, FILTER, Memc[str])
	call clgpset (np, "ifilter", Memc[str], SZ_LINE)
	call apsets (ap, FILTERID, Memc[str])

	# Set the remaining centering parameters
	call apsetr (ap, MAXSHIFT, clgpsetr (cp, "maxshift"))
	call apsetr (ap, MINSNRATIO, clgpsetr (cp, "minsnratio"))
	call apseti (ap, CMAXITER, clgpseti (cp, "cmaxiter"))
	call apseti (ap, CLEAN, btoi (clgpsetb (cp, "clean")))
	call apsetr (ap, RCLEAN, clgpsetr (cp, "rclean"))
	call apsetr (ap, RCLIP, clgpsetr (cp, "rclip"))
	call apsetr (ap, SIGMACLEAN, clgpsetr (cp, "kclean"))
	call apseti (ap, MKCENTER, btoi (clgpsetb (cp, "mkcenter")))

	# Set remaining sky fitting parameters.
	call apsetr (ap, K1, clgpsetr (sp, "khist"))
	call apsetr (ap, K2, clgpsetr (sp, "skreject"))
	call apseti (ap, SMAXITER, clgpseti (sp, "smaxiter"))
	call apseti (ap, SNREJECT, clgpseti (sp, "snreject"))
	call apsetr (ap, BINSIZE, clgpsetr (sp, "binsize"))
	call apsetr (ap, SKY_BACKGROUND, clgpsetr (sp, "skyvalue"))
	call apseti (ap, SMOOTH, btoi (clgpsetb (sp, "smooth")))
	call apsetr (ap, RGROW, clgpsetr (sp, "rgrow"))
	call apseti (ap, MKSKY, btoi (clgpsetb (sp, "mksky")))

	# Set the polyphot parameters.
	call apsetr (ap, PYZMAG, clgpsetr (pp, "zmag"))
	call apseti (ap, MKPOLYGON, btoi (clgpsetb (pp, "mkpolygon")))

	# Close the psets.
	call clcpset (np)
	call clcpset (cp)
	call clcpset (sp)
	call clcpset (pp)
	call sfree (mp)
end
