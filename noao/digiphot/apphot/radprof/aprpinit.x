include "../lib/apphotdef.h"
include "../lib/radprofdef.h"
include "../lib/phot.h"

# AP_RPINIT - Procedure to initialize the radial profile fitting structure.

procedure ap_rpinit (ap, cfunction, cbox, sfunction, annulus, dannulus,
        aperts, napert, radius, step, fwhmpsf, noise)

pointer	ap		# pointer to the apphot structure
int	cfunction	# centering algorithm
real	cbox		# half width of the centering box
int	sfunction	# sky fitting algorithm
real	annulus		# radius of sky annulus
real	dannulus	# width of sky annulus
real	aperts[ARB]	# array of apertures
int	napert		# number of apertures
real	radius		# radius of fitting region
real	step		# step size of output
real	fwhmpsf		# FWHM of the PSF
int	noise		# Noise model

begin
	# Set the image dependent parameters.
	call malloc (ap, LEN_APSTRUCT, TY_STRUCT)

	# Set up the global apphot package defaults.
	call ap_defsetup (ap, fwhmpsf)

	# Set up the noise model parameters.
	call ap_noisesetup (ap, noise)

	# Set up the centering algorithm parameters.
	call ap_ctrsetup (ap, cfunction, cbox)
	    
	# Set up the sky fitting parameters.
	call ap_skysetup (ap, sfunction, annulus, dannulus)

	# Set up the photometry parameters.
	call ap_photsetup (ap, aperts, napert, AP_PWCONSTANT)

	# Set up the radial profile fitting parameters.
	call ap_rpsetup (ap, radius, step)

	# Set up the display options.
	call ap_dispsetup (ap)

	# Set psf fitting and polyphot structures to null.
	AP_PPSF(ap) = NULL
	AP_POLY(ap) = NULL
end


# AP_RPSETUP -- Procedure to set up the radial profle fitting parameters.

procedure ap_rpsetup (ap, radius, step)

pointer	ap		# pointer to apphot structure
real	radius		# radius of psf to be fit
real	step		# step size

pointer	rprof

begin
	call malloc (AP_RPROF(ap), LEN_RPSTRUCT, TY_STRUCT)
	rprof = AP_RPROF(ap)
	AP_RPXCUR(rprof) = INDEFR
	AP_RPYCUR(rprof) = INDEFR
	AP_RPRADIUS(rprof) = radius
	AP_RPSTEP(rprof) = step
	AP_RPIX(rprof) = NULL
	AP_RPNPTS(rprof) = int (AP_RPRADIUS(rprof) / AP_RPSTEP(rprof)) + 1
	call malloc (AP_RPDIST(rprof), AP_RPNPTS(rprof), TY_REAL)
	call malloc (AP_INTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
	call malloc (AP_DINTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
	call malloc (AP_TINTENSITY(rprof), AP_RPNPTS(rprof), TY_REAL)
end
