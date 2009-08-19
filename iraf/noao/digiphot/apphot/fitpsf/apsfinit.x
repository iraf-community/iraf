include "../lib/apphotdef.h"
include "../lib/fitpsfdef.h"
include "../lib/fitpsf.h"

# APSFINIT - Procedure to initialize the point spread modelling structure.

procedure apsfinit (ap, function, rbox, fwhmpsf, noise)

pointer	ap		# pointer to the apphot structure
int	function	# fitting function
real	rbox		# fitting radius
real	fwhmpsf		# full width half max of psf
int	noise		# noise model

size_t	sz_val

begin
	# Initialize the image parameters.
	sz_val = LEN_APSTRUCT
	call malloc (ap, sz_val, TY_STRUCT)

	# Set up the global apphot package parameters.
	call ap_defsetup (ap, fwhmpsf)

	# Setup noise model.
	call ap_noisesetup (ap, noise)

	# Set up the point spread fitting function.
	call ap_psfsetup (ap, function, rbox)

	# Set display options.
	call ap_dispsetup (ap)

	# Set remaining unused structure pointers to NULL.
	AP_PCENTER(ap) = NULL
	AP_PSKY(ap) = NULL
	AP_PPHOT(ap) = NULL
	AP_POLY(ap) = NULL
	AP_RPROF(ap) = NULL
end


# AP_PSFSETUP -- Procedure to define the PSF fitting parameters.

procedure ap_psfsetup (ap, function, rbox)

pointer	ap 		# pointer to apphot structure
int	function	# fitting function
real	rbox		# fitting aperture

size_t	sz_val
pointer	psf

begin
	sz_val = LEN_PSFSTRUCT
	call malloc (AP_PPSF(ap), sz_val, TY_STRUCT)
	psf = AP_PPSF(ap)

	# Set PSF fitting function.
	AP_PSFUNCTION(psf) = function
	switch (function) {
	case AP_RADGAUSS:
	    call strcpy ("radgauss", AP_PSFSTRING(psf), SZ_FNAME)
	case AP_ELLGAUSS:
	    call strcpy ("elgauss", AP_PSFSTRING(psf), SZ_FNAME)
	case AP_MOMENTS:
	    call strcpy ("moments", AP_PSFSTRING(psf), SZ_FNAME)
	default:
	    call strcpy ("radgauss", AP_PSFSTRING(psf), SZ_FNAME)
	}
	AP_PFXCUR(psf) = INDEFR
	AP_PFYCUR(psf) = INDEFR
	switch (function) {
	case AP_RADGAUSS:
	    AP_PSFNPARS(psf) = 5
	case AP_ELLGAUSS:
	    AP_PSFNPARS(psf) = 7
	case AP_MOMENTS:
	    AP_PSFNPARS(psf) = 7
	}

	# Set remaining PSF parameters.
	AP_PSFAPERT(psf) = rbox
	AP_MAXNPARS(psf) = DEF_MAXNPARS
	AP_PK2(psf) = DEF_PK2
	AP_PMAXITER(psf) = DEF_PMAXITER
	AP_PNREJECT(psf) = DEF_PNREJECT

	# Initialize buffers.
	AP_LENPSFBUF(psf) = 0
	AP_NPSFPIX(psf) = 0
	AP_PSFPIX(psf) = NULL
	AP_PSFXPIX(psf) = NULL
	AP_PSFYPIX(psf) = NULL

	# Allocate space for computed parameters.
	sz_val = AP_MAXNPARS(psf)
	call calloc (AP_PPARS(psf), sz_val, TY_REAL)
	call calloc (AP_PPERRS(psf), sz_val, TY_REAL)
end
