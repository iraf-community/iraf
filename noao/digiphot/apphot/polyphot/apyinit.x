include "../lib/apphotdef.h"
include "../lib/polyphotdef.h"

# AP_YINIT - Initialize the polyphot structure.

procedure ap_yinit (ap, cfunction, cbox, sfunction, annulus, dannulus, fwhmpsf,
    noise)

pointer	ap		# pointer to the apphot structure
int	cfunction	# centering algorithm
real	cbox		# centering box half-width
int	sfunction	# sky fitting algorithm
real	annulus		# inner radius of sky annulus
real	dannulus	# width of sky annulus
real	fwhmpsf		# fwhmpsf
int	noise		# Noise function

begin
	call malloc (ap, LEN_APSTRUCT, TY_STRUCT)

	# Set the global apphot package parameters.
	call ap_defsetup (ap, fwhmpsf)

	# Setup noise parameters.
	call ap_noisesetup (ap, noise)

	# Set display options.
	call ap_dispsetup (ap)

	# Setup the centering parameters.
	call ap_ctrsetup (ap, cfunction, cbox)

	# Set up the sky fitting parameters.
	call ap_skysetup (ap, sfunction, annulus, dannulus)

	# Setup the polyphot parameters.
	call ap_ysetup (ap)

	# Set unused structure pointers to null.
	AP_PPSF(ap) = NULL
	AP_PPHOT(ap) = NULL
	AP_RPROF(ap) = NULL
end


# AP_YSETUP -- Procedure to set parameters for polygonal aperture  photometry.

procedure ap_ysetup (ap)

pointer	ap		# pointer to apphot strucuture

pointer	ply

begin
	call malloc (AP_POLY(ap), LEN_PYSTRUCT, TY_STRUCT)
	ply = AP_POLY(ap)
	AP_PYCX(ply) = INDEFR
	AP_PYCY(ply) = INDEFR
	AP_PYX(ply) = INDEFR
	AP_PYY(ply) = INDEFR
	AP_PYZMAG(ply) = DEF_PYZMAG
	AP_PYNAME(ply) = EOS
	AP_PYROOT(ply) = EOS
end
