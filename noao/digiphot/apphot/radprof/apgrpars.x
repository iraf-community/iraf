include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/radprof.h"

# AP_GRPPARS -- Procedure to fetch the radprof parameters.

procedure ap_grppars (ap)

pointer	ap		# pointer to apphot structure

int	cfunction, sfunction, nfunction, naperts
pointer	cp, sp, np, pp, mp, str, aperts
real	fwhmpsf, cbox, annulus, dannulus, radius, step
bool	clgetb(), clgpsetb()
int	clgpseti(), clgeti(), strdic(), btoi(), ap_getaperts()
pointer	clopset()
real	clgetr(), clgpsetr()

begin
	# Open the parameter sets.
	call smark (mp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (aperts, MAX_NAPERTS, TY_REAL)
	np = clopset ("datapars")
	cp = clopset ("centerpars")
	sp = clopset ("fitskypars")
	pp = clopset ("photpars")

	# Get the centering algorithm and aperture.
	call clgpset (cp, "calgorithm", Memc[str], SZ_FNAME)
	cfunction = strdic (Memc[str], Memc[str], SZ_FNAME, CFUNCS)
	cbox = clgpsetr (cp, "cbox") / 2.0

	# Get the sky fitting algorithm and annuli.
	call clgpset (sp, "salgorithm", Memc[str], SZ_FNAME)
	sfunction = strdic (Memc[str], Memc[str], SZ_LINE, SFUNCS)
	annulus = clgpsetr (sp, "annulus")
	dannulus = clgpsetr (sp, "dannulus")

	# Get the data dependent parameters.
	fwhmpsf = clgpsetr (np, "fwhmpsf")
	call clgpset (np, "noise", Memc[str], SZ_FNAME)
	nfunction = strdic (Memc[str], Memc[str], SZ_LINE, NFUNCS)

	# Get the photometry parameters.
	call clgpset (pp, "apertures", Memc[str], SZ_FNAME)
	naperts = ap_getaperts (Memc[str], Memr[aperts], MAX_NAPERTS)

	# Get the radial profile specs.
	radius = clgetr ("radius")
	step = clgetr ("step")

	# Open the radprof structure.
	if (naperts <= 0)
	    call ap_rpinit (ap, cfunction, cbox, sfunction, annulus, dannulus,
	        0.0, 1, radius, step, fwhmpsf, nfunction) 
	else {
	    call ap_rpinit (ap, cfunction, cbox, sfunction, annulus, dannulus,
	        Memr[aperts], naperts, radius, step, fwhmpsf, nfunction) 
	    call apsets (ap, APSTRING, Memc[str])
	}

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

	# Set the remainder of the noise model parameters.
	call apsetr (ap, SCALE, 1.0 / clgpsetr (np, "scale"))
	call apseti (ap, POSITIVE, btoi (clgpsetb (np, "emission")))
	call apsetr (ap, DATAMIN, clgpsetr (np, "datamin"))
	call apsetr (ap, DATAMAX, clgpsetr (np, "datamax"))
	call clgpset (np, "gain", Memc[str], SZ_FNAME)
	call apsets (ap, GAIN, Memc[str])
	call apsetr (ap, THRESHOLD, clgpsetr (np, "threshold"))
	call apsetr (ap, CTHRESHOLD, clgpsetr (np, "cthreshold"))
	call apsetr (ap, SKYSIGMA, clgpsetr (np, "sigma"))
	call apsetr (ap, EPADU, clgpsetr (np, "epadu"))
	call clgpset (np, "ccdread", Memc[str], SZ_FNAME)
	call apsets (ap, CCDREAD, Memc[str])
	call apsetr (ap, READNOISE, clgpsetr (np, "readnoise"))
	call clgpset (np, "exposure", Memc[str], SZ_FNAME)
	call apsets (ap, EXPOSURE, Memc[str])  
	call apsetr (ap, ITIME, clgpsetr (np, "itime"))
	call clgpset (np, "airmass", Memc[str], SZ_FNAME)
	call apsets (ap, AIRMASS, Memc[str])  
	call apsetr (ap, XAIRMASS, clgpsetr (np, "xairmass"))
	call clgpset (np, "filter", Memc[str], SZ_FNAME)
	call apsets (ap, FILTER, Memc[str])  
	call clgpset (np, "ifilter", Memc[str], SZ_FNAME)
	call apsets (ap, FILTERID, Memc[str])  

	# Set remainder of photometry parameters.
	call apsetr (ap, ZMAG, clgpsetr (pp, "zmag"))
	call apseti (ap, MKAPERT, btoi (clgpsetb (pp, "mkapert")))

	# Set remainder of the radprof parameters.
	call apsetr (ap, RPKSIGMA, clgetr ("kreject"))
	call apseti (ap, RPNREJECT, clgeti ("nreject"))
	call apseti (ap, RPORDER, clgeti ("order"))

	# Radial profile plots.
	call apseti (ap, RADPLOTS, clgetb ("radplots"))

	# Close the pset files.
	call clcpset (cp)
	call clcpset (sp)
	call clcpset (np)
	call sfree (mp)
end
