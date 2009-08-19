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

size_t	sz_val

begin
	# Set the image parameters.
	sz_val = LEN_APSTRUCT
	call malloc (ap, sz_val, TY_STRUCT)

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

size_t	sz_val
pointer	phot

begin
	# phot structure
	sz_val = LEN_PHOTSTRUCT
	call malloc (AP_PPHOT(ap), sz_val, TY_STRUCT)
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
	sz_val = napert
	call malloc (AP_APERTS(phot), sz_val, TY_REAL)
	call malloc (AP_MAGS(phot), sz_val, TY_REAL)
	call malloc (AP_MAGERRS(phot), sz_val, TY_REAL)
	call malloc (AP_AREA(phot), sz_val, TY_DOUBLE)
	call malloc (AP_SUMS(phot), sz_val, TY_DOUBLE)
	sz_val = napert
	call amovr (aperts, Memr[AP_APERTS(phot)], sz_val)
	sz_val = napert
	call asrtr (Memr[AP_APERTS(phot)], Memr[AP_APERTS(phot)], sz_val)
end
