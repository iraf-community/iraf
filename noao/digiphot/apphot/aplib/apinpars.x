include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/polyphot.h"

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
	call apsetr (ap, THRESHOLD, clgpsetr (np, "threshold"))
	call apsetr (ap, CTHRESHOLD, clgpsetr (np, "cthreshold"))
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


# AP_GCEPARS -- Read in the centering algorithm parameters from the 
# centerpars parameter file.

procedure ap_gcepars (ap)

pointer	ap			# pointer to the apphot structure

int	function
pointer	sp, str, pp
bool	clgpsetb()
int	strdic(), btoi(), clgpseti()
pointer	clopset()
real	clgpsetr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the pset parameter file.
	pp = clopset ("centerpars")

	# Get the centering parameters.
	call clgpset (pp, "calgorithm", Memc[str], SZ_LINE)
	function = strdic (Memc[str], Memc[str], SZ_LINE, CFUNCS)
	call apsets (ap, CSTRING, Memc[str])
	call apseti (ap, CENTERFUNCTION, function)
	call apsetr (ap, CAPERT, clgpsetr (pp, "cbox") / 2.0)
	call apsetr (ap, MAXSHIFT, clgpsetr (pp, "maxshift"))
	call apsetr (ap, MINSNRATIO, clgpsetr (pp, "minsnratio"))
	call apseti (ap, CMAXITER, clgpseti (pp, "cmaxiter"))
	call apseti (ap, CLEAN, btoi (clgpsetb (pp, "clean")))
	call apsetr (ap, RCLEAN, clgpsetr (pp, "rclean"))
	call apsetr (ap, RCLIP, clgpsetr (pp, "rclip"))
	call apsetr (ap, SIGMACLEAN, clgpsetr (pp, "kclean"))

	call apseti (ap, MKCENTER, btoi (clgpsetb (pp, "mkcenter")))


	# Close the parameter set file.
	call clcpset (pp)

	call sfree (sp)
end


# AP_GSAPARS -- Read in the sky fitting parameters from the fitskypars
# parameter file.

procedure ap_gsapars (ap)

pointer	ap		# pointer to the apphot strucuture

int	function
pointer	sp, str, pp
bool	clgpsetb()
int	strdic(), clgpseti(), btoi()
pointer	clopset()
real	clgpsetr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the pset parameter file.
	pp = clopset ("fitskypars")

	# Get the sky fitting algorithm parameters.
	call clgpset (pp, "salgorithm", Memc[str], SZ_LINE)
	function = strdic (Memc[str], Memc[str], SZ_LINE, SFUNCS)
	call apsets (ap, SSTRING, Memc[str])
	call apseti (ap, SKYFUNCTION, function)
	call apsetr (ap, ANNULUS, clgpsetr (pp, "annulus"))
	call apsetr (ap, DANNULUS, clgpsetr (pp, "dannulus"))
	call apsetr (ap, K1, clgpsetr (pp, "khist"))
	call apsetr (ap, K2, clgpsetr (pp, "skreject"))
	call apsetr (ap, SKY_BACKGROUND, clgpsetr (pp, "skyvalue"))
	call apseti (ap, SMAXITER, clgpseti (pp, "smaxiter"))
	call apseti (ap, SNREJECT, clgpseti (pp, "snreject"))
	call apsetr (ap, BINSIZE, clgpsetr (pp, "binsize"))
	call apseti (ap, SMOOTH, btoi (clgpsetb (pp, "smooth")))
	call apsetr (ap, RGROW, clgpsetr (pp, "rgrow"))

	# Get the marking parameter.
	call apseti (ap, MKSKY, btoi (clgpsetb (pp, "mksky")))

	# Close the parameter set file.
	call clcpset (pp)

	call sfree (sp)
end


# AP_GPHPARS -- Get the photometry algorithm parameters from the photometry
# file.

procedure ap_gphars (ap)

pointer	ap		# pointer to the apphot strucuture

pointer	sp, str, pp
bool	clgpsetb()
int	btoi()
pointer	clopset()
real	clgpsetr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the pset parameter file.
	pp = clopset ("photpars")

	# Get the photometry parameters.
	call clgpset (pp, "apertures", Memc[str], SZ_LINE)
	call apsets (ap, APERTS, Memc[str])
	call apsetr (ap, ZMAG, clgpsetr (pp, "zmag"))
	call apseti (ap, MKAPERT, btoi (clgpsetb (pp, "mkapert")))
	call apsets (ap, PWSTRING, "constant")
	call apseti (ap, PWEIGHTS, AP_PWCONSTANT)

	# Close the parameter set file.
	call clcpset (pp)

	call sfree (sp)
end


# AP_GPOPARS -- Get the polygonal aperture photometry parameters.

procedure ap_gpopars (ap)

pointer	ap		# pointer to the apphot strucuture

pointer	sp, str, pp
bool	clgpsetb()
int	btoi()
pointer	clopset()
real	clgpsetr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the pset parameter file.
	pp = clopset ("polypars")

	# Get the parameters.
	call apsetr (ap, PYZMAG, clgpsetr (pp, "zmag"))
	call apseti (ap, MKPOLYGON, btoi (clgpsetb (pp, "mkpolygon")))

	# Close the parameter set file.
	call clcpset (pp)

	call sfree (sp)
end


# AP_GWHPARS -- Get the photometry algorithm parameters from the photometry
# file.

procedure ap_gwhars (ap)

pointer	ap		# pointer to the apphot strucuture

int	weight
pointer	sp, str, pp
bool	clgpsetb()
int	btoi(), strdic()
pointer	clopset()
real	clgpsetr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the pset parameter file.
	pp = clopset ("photpars")

	# Get the photometry parameters.
	call clgpset (pp, "apertures", Memc[str], SZ_LINE)
	call apsets (ap, APERTS, Memc[str])
	call apsetr (ap, ZMAG, clgpsetr (pp, "zmag"))
	call apseti (ap, MKAPERT, btoi (clgpsetb (pp, "mkapert")))
	call apsets (ap, PWSTRING, "constant")
	call apseti (ap, PWEIGHTS, AP_PWCONSTANT)

	# Get the major parameters.
	call clgpset (pp, "weighting", Memc[str], SZ_LINE)
	weight = strdic (Memc[str], Memc[str], SZ_LINE, PWFUNCS)
	call apsets (ap, PWSTRING, Memc[str])
	call apseti (ap, PWEIGHTS, weight)

	# Close the parameter set file.
	call clcpset (pp)

	call sfree (sp)
end
