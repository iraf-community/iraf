include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_GPPARS -- Procedure to fetch the phot task parameters.

procedure ap_gppars (ap)

pointer	ap		# pointer to apphot structure

int	noise, cfunction, sfunction, naperts
pointer	mp, aperts, str, np, cp, sp, pp, apstr
real	fwhmpsf, cbox, annulus, dannulus
bool	clgetb(), clgpsetb()
int	clgpseti(), strdic(), ap_getaperts(), btoi()
pointer	clopset()
real	clgpsetr()

begin
	call smark (mp)
	call salloc (aperts, MAX_NAPERTS, TY_REAL)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (apstr, SZ_LINE, TY_CHAR)

	# Open the parameter sets.
	np = clopset ("datapars")
	cp = clopset ("centerpars")
	sp = clopset ("fitskypars")
	pp = clopset ("photpars")

	# Get the major parameters.
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
	call clgpset (pp, "apertures", Memc[apstr], SZ_LINE)
	naperts = ap_getaperts (Memc[apstr], Memr[aperts], MAX_NAPERTS)

	# Open the apphot structure.
	if (naperts <= 0.0)
	    call appinit (ap, cfunction, cbox, sfunction, annulus,
	        dannulus, 0.0, 1, AP_PWCONSTANT, fwhmpsf, noise) 
	else
	    call appinit (ap, cfunction, cbox, sfunction, annulus,
	        dannulus, Memr[aperts], naperts, AP_PWCONSTANT, fwhmpsf, noise) 

	# Set the remaining noise parameters.
	call apsetr (ap, SCALE, 1.0 / clgpsetr (np, "scale"))
	call apseti (ap, POSITIVE, btoi (clgpsetb (np, "emission")))
	call apsetr (ap, DATAMIN, clgpsetr (np, "datamin"))
	call apsetr (ap, DATAMAX, clgpsetr (np, "datamax"))
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

	# Set the remainder of the centering parameters.
	call apsetr (ap, MAXSHIFT, clgpsetr (cp, "maxshift"))
	call apsetr (ap, MINSNRATIO, clgpsetr (cp, "minsnratio"))
	call apseti (ap, CMAXITER, clgpseti (cp, "cmaxiter"))
	call apseti (ap, CLEAN, btoi (clgpsetb (cp, "clean")))
	call apsetr (ap, RCLEAN, clgpsetr (cp, "rclean"))
	call apsetr (ap, RCLIP, clgpsetr (cp, "rclip"))
	call apsetr (ap, SIGMACLEAN, clgpsetr (cp, "kclean"))
	call apseti (ap, MKCENTER, btoi (clgpsetb (cp, "mkcenter")))

	# Set the remainder of the sky fitting parameters.
	call apsetr (ap, SKY_BACKGROUND, clgpsetr (sp, "skyvalue")) 
	call apseti (ap, SMAXITER, clgpseti (sp, "smaxiter"))
	call apsetr (ap, K2, clgpsetr (sp, "skreject"))
	call apseti (ap, SNREJECT, clgpseti (sp, "snreject"))
	call apsetr (ap, K1, clgpsetr (sp, "khist"))
	call apsetr (ap, BINSIZE, clgpsetr (sp, "binsize"))
	call apseti (ap, SMOOTH, btoi (clgpsetb (sp, "smooth")))
	call apsetr (ap, RGROW, clgpsetr (sp, "rgrow"))
	call apseti (ap, MKSKY, btoi (clgpsetb (sp, "mksky")))

	# Set the remaining photometry parameters.
	if (naperts > 0)
	    call apsets (ap, APSTRING, Memc[apstr])
	call apsetr (ap, ZMAG, clgpsetr (pp, "zmag"))
	call apseti (ap, MKAPERT, btoi (clgpsetb (pp, "mkapert")))
	call apsets (ap, PWSTRING, "constant")
	call apseti (ap, PWEIGHTS, AP_PWCONSTANT)

	# Radial profile plots
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))

	# Close the pset files.
	call clcpset (np)
	call clcpset (cp)
	call clcpset (sp)
	call clcpset (pp)
	call sfree (mp)
end
