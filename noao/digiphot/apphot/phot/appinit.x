include "../lib/apphotdef.h"
include "../lib/photdef.h"
include "../lib/phot.h"

# APPINIT - Procedure to initialize apphot structure.

procedure appinit (ap, cfunction, cbox, sfunction, annulus, dannulus,
        aperts, napert, weight, fwhmpsf, noise)

pointer	ap		# pointer to the apphot structure
int	cfunction	# centering algorithm
real	cbox		# half width of the centering box
int	sfunction	# sky fitting algorithm
real	annulus		# radius of sky annulus
real	dannulus	# width of sky annulus
real	aperts[ARB]	# apertures
int	napert		# number of apertures
int	weight		# weight for photometry
real	fwhmpsf		# FWHM of the PSF
int	noise		# noise model

begin
	# Set the image parameters.
	call malloc (ap, LEN_APSTRUCT, TY_STRUCT)

	# Set up the apphot package defaults.
	call ap_defsetup (ap, fwhmpsf)

	# Set up noise structure.
	call ap_noisesetup (ap, noise)

	# Set up centering structure.
	call ap_ctrsetup (ap, cfunction, cbox)

	# Set up sky fitting structure.
	call ap_skysetup (ap, sfunction, annulus, dannulus)

	# Set up photometry structure.
	call ap_photsetup (ap, aperts, napert, weight)
	    
	# Set the display options.
	call ap_dispsetup (ap)

	# Unused structures are set to null.
	AP_PPSF(ap) = NULL
	AP_POLY(ap) = NULL
	AP_RPROF(ap) = NULL
end


# AP_PHOTSETUP -- Procedure to set up the photometry parameters.

procedure ap_photsetup (ap, aperts, napert, weight)

pointer	ap		# pointer to apphot structure
real	aperts[ARB]	# array of apertures
int	napert		# number of apertures
int	weight		# weighting function for photometry

pointer	phot

begin
	# phot structure
	call malloc (AP_PPHOT(ap), LEN_PHOTSTRUCT, TY_STRUCT)
	phot = AP_PPHOT(ap)

	# Set the default values forthe photometry parameters.
	AP_PXCUR(phot) = INDEFR
	AP_PYCUR(phot) = INDEFR
	AP_NAPERTS(phot) = napert
	AP_ZMAG(phot) = DEF_ZMAG
	AP_PWEIGHTS(phot) = weight
	AP_APSTRING(phot) = EOS
	switch (weight) {
	case AP_PWCONSTANT:
	    call strcpy ("constant", AP_PWSTRING(phot), SZ_FNAME)
	case AP_PWCONE:
	    call strcpy ("cone", AP_PWSTRING(phot), SZ_FNAME)
	case AP_PWGAUSS:
	    call strcpy ("gauss", AP_PWSTRING(phot), SZ_FNAME)
	default:
	    call strcpy ("constant", AP_PWSTRING(phot), SZ_FNAME)
	}

	# Initialize buffers.
	AP_LENABUF(phot) = 0
	AP_NAPIX(phot) = 0
	AP_APIX(phot) = NULL
	AP_XAPIX(phot) = NULL
	AP_YAPIX(phot) = NULL

	# Allocate the buffers to hold the answers.
	call malloc (AP_APERTS(phot), napert, TY_REAL)
	call malloc (AP_MAGS(phot), napert, TY_REAL)
	call malloc (AP_MAGERRS(phot), napert, TY_REAL)
	call malloc (AP_AREA(phot), napert, TY_DOUBLE)
	call malloc (AP_SUMS(phot), napert, TY_DOUBLE)
	call amovr (aperts, Memr[AP_APERTS(phot)], napert)
	call asrtr (Memr[AP_APERTS(phot)], Memr[AP_APERTS(phot)], napert)
end
