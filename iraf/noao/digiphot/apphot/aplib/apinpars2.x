include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/polyphot.h"

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
	call apsetr (ap, CTHRESHOLD, clgpsetr (pp, "cthreshold"))
	call apsetr (ap, MINSNRATIO, clgpsetr (pp, "minsnratio"))
	call apseti (ap, CMAXITER, clgpseti (pp, "cmaxiter"))
	call apsetr (ap, MAXSHIFT, clgpsetr (pp, "maxshift"))
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
	call apsetr (ap, SKY_BACKGROUND, clgpsetr (pp, "skyvalue"))
	call apsetr (ap, ANNULUS, clgpsetr (pp, "annulus"))
	call apsetr (ap, DANNULUS, clgpsetr (pp, "dannulus"))
	call apsetr (ap, K1, clgpsetr (pp, "khist"))
	call apsetr (ap, BINSIZE, clgpsetr (pp, "binsize"))
	call apseti (ap, SMOOTH, btoi (clgpsetb (pp, "smooth")))
	call apseti (ap, SMAXITER, clgpseti (pp, "smaxiter"))
	call apsetr (ap, SLOCLIP, clgpsetr (pp, "sloclip"))
	call apsetr (ap, SHICLIP, clgpsetr (pp, "shiclip"))
	call apseti (ap, SNREJECT, clgpseti (pp, "snreject"))
	call apsetr (ap, SLOREJECT, clgpsetr (pp, "sloreject"))
	call apsetr (ap, SHIREJECT, clgpsetr (pp, "shireject"))
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
