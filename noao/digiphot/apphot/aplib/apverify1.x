include <math.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_GVRAD -- Verify or get the radius of the extraction box.

int procedure ap_gvrad (defradius, radius)

real	defradius		# the default radius
real	radius			# the output radius

int	lenbuf
int	scan(), nscan()

begin
	call printf (
	    "Radius of extraction box in pixels (%4.1f) (CR  or value): ")
	    call pargr (defradius)
	call flush (STDOUT)

	if (scan () == EOF)
	    radius = defradius
	else {
	    call gargr (radius)
	    if (nscan () < 1)
	        radius = defradius
	}
	lenbuf = PI * radius * (radius + 1.0)

	return (lenbuf)
end


# AP_VFWHMPSF -- Verify the full width maximum of the psf.

real procedure ap_vfwhmpsf (ap)

pointer	ap		# pointer to the apphot structure

real	scale, fwhmpsf
int	scan(), nscan()
real	apstatr()

begin
	scale = apstatr (ap, SCALE) 
	call printf ("FWHM of features in scale units (%g) (CR or value): ")
	    call pargr (apstatr (ap, FWHMPSF))
	call flush (STDOUT)

	# Confirm the fwhmpsf.
	if (scan() == EOF)
	    fwhmpsf = apstatr (ap, FWHMPSF)
	else {
	    call gargr (fwhmpsf)
	    if (nscan () != 1)
	        fwhmpsf = apstatr (ap, FWHMPSF)
	}

	call printf ("\tNew FWHM of features: %g scale units  %g pixels\n")
	    call pargr (fwhmpsf)
	    call pargr (scale * fwhmpsf)
	call apsetr (ap, FWHMPSF, fwhmpsf)

	return (fwhmpsf)
end


# AP_VDATAMIN-- Verify the minimum good data value.

real procedure ap_vdatamin (ap)

pointer	ap		# pointer to the apphot structure

real	datamin
int	scan(), nscan()
real	apstatr()

begin
	# Confirm the threshold parameter.
	call printf ("Minimum good data value (%g) (CR or value): ")
	    call pargr (apstatr (ap, DATAMIN))
	call flush (STDOUT)
	if (scan() == EOF)
	    datamin = apstatr (ap, DATAMIN)
	else {
	    call gargr (datamin)
	    if (nscan () != 1)
	        datamin = apstatr (ap, DATAMIN)
	}

	call printf ("\tNew minimum good data value: %g counts\n")
	    call pargr (datamin)
	call apsetr (ap, DATAMIN, datamin)

	return (datamin)
end


# AP_VDATAMAX-- Verify the maximum good data value.

real procedure ap_vdatamax (ap)

pointer	ap		# pointer to the apphot structure

real	datamax
int	scan(), nscan()
real	apstatr()

begin
	# Confirm the threshold parameter.
	call printf ("Maximum good data value (%g) (CR or value): ")
	    call pargr (apstatr (ap, DATAMAX))
	call flush (STDOUT)
	if (scan() == EOF)
	    datamax = apstatr (ap, DATAMAX)
	else {
	    call gargr (datamax)
	    if (nscan () != 1)
	        datamax = apstatr (ap, DATAMAX)
	}

	call printf ("\tNew maximum good data value: %g counts\n")
	    call pargr (datamax)
	call apsetr (ap, DATAMAX, datamax)

	return (datamax)
end


# AP_VSIGMA -- Verify the standard deviation of the sky

real procedure ap_vsigma (ap)

pointer	ap		# pointer to the apphot structure

real	skysigma
int	scan(), nscan()
real	apstatr()

begin
	# Confirm the sky sigma parameter.
	call printf (
	    "Standard deviation of background in counts (%g) (CR or value): ")
	    call pargr (apstatr (ap, SKYSIGMA))
	call flush (STDOUT)
	if (scan() == EOF)
	    skysigma = apstatr (ap, SKYSIGMA)
	else {
	    call gargr (skysigma)
	    if (nscan () != 1)
	        skysigma = apstatr (ap, SKYSIGMA)
	}

	call printf ("\tNew standard deviation of background: %g counts\n")
	    call pargr (skysigma)
	call apsetr (ap, SKYSIGMA, skysigma)

	return (skysigma)
end


# AP_VCSTRING -- Verify the centering string.

procedure ap_vcstring (ap, str, maxch)

pointer	ap		# pointer to the apphot strucuture
char	str[ARB]	# output string
int	maxch		# maximum number of characters

int	cfunc
int	scan(), strdic(), nscan()

begin
	# Print the old centering algorithm.
	call apstats (ap, CSTRING, str, maxch)
	call printf ("Centering algorithm (%s) (CR or value): ")
	    call pargstr (str)
	call flush (STDOUT)

	# Confirm the centering algorithm.
	if (scan() != EOF) {
	    call gargwrd (str, maxch)
	    cfunc = strdic (str, str, maxch, CFUNCS)
	    if (nscan () == 1 && cfunc > 0) {
		call apseti (ap, CENTERFUNCTION, cfunc)
		call apsets (ap, CSTRING, str)
	    }
	}

	# Print the new result.
	call apstats (ap, CSTRING, str, maxch)
	call printf ("\tNew centering algorithm: %s\n")
	    call pargstr (str)
end


# AP_VCAPERT -- Verify the centering aperture.

real procedure ap_vcapert (ap)

pointer	ap		# pointert to the apphot strucuture

real	scale, capert
int	scan(), nscan()
real	apstatr()

begin
	# Get the apphot scale factor.
	scale = apstatr (ap, SCALE)

	# Print the old centering value.
	call printf ("Centering box width in scale units (%g) (CR or value): ")
	    call pargr (2.0 * apstatr (ap, CAPERT))
	call flush (STDOUT)

	# Get the new centering value.
	if (scan() == EOF)
	    capert = 2.0 * apstatr (ap, CAPERT)
	else {
	    call gargr (capert)
	    if (nscan () != 1)
	        capert = 2.0 * apstatr (ap, CAPERT)
	}

	# Type the new value.
	call apsetr (ap, CAPERT, capert / 2.0) 
	call printf ("\tNew centering box width: %g scale units  %g pixels\n")
	    call pargr (capert)
	    call pargr (scale * capert)

	return (capert / 2.0)
end


# AP_VCTHRESHOLD -- Verify the centering threshold parameter.

real procedure ap_vcthreshold (ap)

pointer	ap		# pointer to the apphot structure

real	skysigma, cthreshold
int	scan(), nscan()
real	apstatr()

begin
	# Get the sky sigma.
	skysigma = apstatr (ap, SKYSIGMA)

	# Print the old centering threshold.
	call printf (
	"Centering threshold in sigma above data minimum (%g) (CR or value): ")
	    call pargr (apstatr (ap, CTHRESHOLD))
	call flush (STDOUT)

	# Confirm the centering threshold parameter.
	if (scan() == EOF)
	    cthreshold = apstatr (ap, CTHRESHOLD)
	else {
	    call gargr (cthreshold)
	    if (nscan () != 1)
	        cthreshold = apstatr (ap, CTHRESHOLD)
	}

	# Print the new centering threshold.
	call apsetr (ap, CTHRESHOLD, cthreshold) 
	call printf ("\tNew centering threshold: %g skysigma  %g counts\n")
	    call pargr (cthreshold)
	if (IS_INDEFR(skysigma))
	    call pargr (INDEFR)
	else
	    call pargr (cthreshold * skysigma)

	return (cthreshold)
end


# AP_VRCLEAN -- Verify the cleaning radius

real procedure ap_vrclean (ap)

pointer	ap		# pointer to the apphot structure

real	scale, rclean
int	scan(), nscan()
real	apstatr()

begin
	# Confirm the cleaning radius.
	scale = apstatr (ap, SCALE) 
	call printf ("Cleaning radius in scale units (%g) (CR or value): ")
	    call pargr (apstatr (ap, RCLEAN))
	call flush (STDOUT)
	if (scan() == EOF)
	    rclean = apstatr (ap, RCLEAN)
	else {
	    call gargr (rclean)
	    if (nscan () != 1)
	        rclean = apstatr (ap, RCLEAN)
	}

	call apsetr (ap, RCLEAN, rclean)
	call printf ("\tNew cleaning radius: %g scale units  %g pixels\n")
	    call pargr (rclean)
	    call pargr (scale * rclean)

	return (rclean)
end


# AP_VRCLIP -- Verify the clipping radius.

real procedure ap_vrclip (ap)

pointer	ap		# pointer to the apphot structure

real	scale, rclip
int	scan(), nscan()
real	apstatr()

begin
	# Confirm the clipping radius.
	scale = apstatr (ap, SCALE) 
	call printf ("Clipping radius in scale units (%g) (CR or value): ")
	    call pargr (apstatr (ap, RCLIP))
	call flush (STDOUT)
	if (scan() == EOF)
	    rclip = apstatr (ap, RCLIP)
	else {
	    call gargr (rclip)
	    if (nscan () != 1)
	        rclip = apstatr (ap, RCLIP)
	}

	call apsetr (ap, RCLIP, rclip)
	call printf ("\tNew FWHM clipping radius: %g scale units  %g pixels\n")
	    call pargr (rclip)
	    call pargr (scale * rclip)

	return (rclip)
end


# AP_VSSTRING -- Verify the sky fitting algorithm string.

procedure ap_vsstring (ap, str, maxch)

pointer	ap		# pointer to the apphot structure
char	str[ARB]	# output string
int	maxch		# maximum number of characteristics

int	sfunc
int	scan(), nscan(), strdic()

begin
	# Print the old salgorithm string.
	call apstats (ap, SSTRING, str, maxch)
	call printf ("Sky fitting algorithm (%s) (CR or value): ")
	    call pargstr (str)
	call flush (STDOUT)

	# Confirm the sky fitting algorithm.
	if (scan() != EOF) {
	    call gargwrd (str, maxch)
	    sfunc = strdic (str, str, maxch, SFUNCS)
	    if (nscan () == 1 && sfunc > 0) {
		call apseti (ap, SKYFUNCTION, sfunc)
		call apsets (ap, SSTRING, str)
	    }
	}

	# Print the new salgorithm string.
	call apstats (ap, SSTRING, str, maxch)
	call printf ("\tSky fitting algorithm: %s\n")
	    call pargstr (str)
end


# AP_VANNULUS -- Verify the inner radius of sky annulus.

real procedure ap_vannulus (ap)

pointer	ap		# pointer to the apphot structure

real	scale, annulus
int	scan(), nscan()
real	apstatr()

begin
	scale = apstatr (ap, SCALE)

	# Print the old inner sky radius value.
	call printf (
	"Inner radius of sky annulus in scale units (%g) (CR or value): ")
            call pargr (apstatr (ap, ANNULUS))
	call flush (STDOUT)

	# Verify the new value.
	if (scan () == EOF)
	    annulus = apstatr (ap, ANNULUS)
	else {
	    call gargr (annulus)
	    if (nscan () != 1)
	        annulus = apstatr (ap, ANNULUS)
	}

	# Print the old inner sky radius value.
	call apsetr (ap, ANNULUS, annulus)
	call printf (
	    "\tNew inner radius of sky annulus: %g scale units %g pixels\n")
	    call pargr (annulus)
	    call pargr (scale * annulus)

	return (annulus)
end


# AP_VDANNULUS -- Verify the width of the sky annulus.

real procedure ap_vdannulus (ap)

pointer	ap		# pointer to the apphot structure

real	scale, dannulus
int	scan(), nscan()
real	apstatr()

begin
	scale = apstatr (ap, SCALE)

	# Print the old sky annulus width.
	call printf (
	    "Width of the sky annulus in scale units (%g) (CR or value): ")
	    call pargr (apstatr (ap, DANNULUS))
	call flush (STDOUT)

	# Confirm the width of the sky annulus.
	if (scan() == EOF)
	    dannulus = apstatr (ap, DANNULUS)
	else {
	    call gargr (dannulus)
	    if (nscan () != 1)
	        dannulus = apstatr (ap, DANNULUS)
	}

	# Print the new sky annulus width.
	call apsetr (ap, DANNULUS, dannulus)
	call printf (
	    "\tNew width of the sky annulus: %g scale units %g pixels\n")
	    call pargr (dannulus)
	    call pargr (scale * dannulus)

	return (dannulus)
end


# AP_VRGROW -- Verify the region growing radius

real procedure ap_vrgrow (ap)

pointer	ap		# pointer to the apphot structure

real	scale, rgrow
int	scan(), nscan()
real	apstatr()

begin
	scale = apstatr (ap, SCALE)

	# Print the old region growing radius.
	call printf (
	    "Region growing radius in scale units (%g) (CR or value): ")
	    call pargr (apstatr (ap, RGROW))
	call flush (STDOUT)

	# Confirm the region growing radius.
	if (scan() == EOF)
	    rgrow = apstatr (ap, RGROW)
	else {
	    call gargr (rgrow)
	    if (nscan () != 1)
	        rgrow = apstatr (ap, RGROW)
	}

	# Print the region growing radius.
	call apsetr (ap, RGROW, rgrow)
	call printf (
	    "\tNew region growing radius: %g scale units %g pixels\n")
	    call pargr (rgrow)
	    call pargr (scale * rgrow)

	return (rgrow)
end


# AP_VAPERTS -- Verify the photometric apertures.

procedure ap_vaperts (ap, str, maxch)

pointer	ap		# pointer to the apphot structure
char	str[ARB]	# output string
int	maxch		# maximum number of characters

int	i, naperts
pointer	sp, aperts
real	scale
int	scan(), nscan(), ap_getaperts()
real	apstatr()

begin
	call smark (sp)
	call salloc (aperts, MAX_NAPERTS, TY_REAL)

	scale = apstatr (ap, SCALE)

	# Print the old aperture string.
	call apstats (ap, APERTS, str, maxch)
	call printf (
	"File/list of aperture radii in scale units (%s) (CR or value): ")
	    call pargstr (str)
	call flush (STDOUT)

	# Get the new apertures.
	if (scan() == EOF)
	    call apstats (ap, APERTS, str, maxch)
	else {
	    call gargwrd (str, maxch)
	    if (nscan () != 1)
	        call apstats (ap, APERTS, str, maxch)
	}

	# Print the new apertures.
	naperts = ap_getaperts (str, Memr[aperts], MAX_NAPERTS)
	do i = 1, naperts {
	    call printf ("\tAperture radius %d: %g scale units %g pixels\n")
		call pargi (i)
		call pargr (Memr[aperts+i-1])
		call pargr (scale * Memr[aperts+i-1])
	}

	call apsets (ap, APERTS, str)
	call sfree (sp)
end


# AP_VPWSTRING -- Verify the weighting string.

procedure ap_vpwstring (ap, str, maxch)

pointer	ap		# pointer to the apphot structure
char	str[ARB]	# output string
int	maxch		# maximum number of characters

int	wfunc
int	scan(), nscan(), strdic()

begin
	# Print the old string.
	call apstats (ap, PWSTRING, str, maxch)
	call printf ("Weighting algorithm (%s) (CR or value): ")
	    call pargstr (str)
	call flush (STDOUT)

	# Get the new value.
	if (scan() != EOF) {
	    call gargwrd (str, maxch)
	    wfunc = strdic (str, str, maxch, PWFUNCS)
	    if (nscan () == 1 && wfunc > 0) {
		call apseti (ap, PWEIGHTS, wfunc)
		call apsets (ap, PWSTRING, str)
	    }
	}

	# Print the new value.
	call apstats (ap, PWSTRING, str, maxch)
	call printf ("\tNew weighting algorithm: %s\n")
	    call pargstr (str)

	call apsets (ap, PWSTRING, str)
end
