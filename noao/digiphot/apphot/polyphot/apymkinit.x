include "../lib/apphotdef.h"
include "../lib/noise.h"

# AP_YMKINIT - Procedure to initialize the polymark structure.

procedure ap_ymkinit (ap)

pointer	ap		# pointer to the apphot structure

begin
	call calloc (ap, LEN_APSTRUCT, TY_STRUCT)

	# Set the main structure parameters.
	call ap_defsetup (ap, 2.5)

	# Set the noise options.
	call ap_noisesetup (ap, AP_NPOISSON)

	# Set display options.
	call ap_dispsetup (ap)

	# Setup the polyphot parameters.
	call ap_ysetup (ap)

	# Set unused structure pointers to null.
	AP_PCENTER(ap) = NULL
	AP_PSKY(ap) = NULL
	AP_PPHOT(ap) = NULL
	AP_RPROF(ap) = NULL
	AP_PPSF(ap) = NULL
end
