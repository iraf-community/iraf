include "../lib/apphotdef.h"

# AP_YMKINIT - Procedure to initialize the polymark structure.

procedure ap_ymkinit (ap)

pointer	ap		# pointer to the apphot structure

begin
	call malloc (ap, LEN_APSTRUCT, TY_STRUCT)

	# Set the data dependent parameters.
	AP_IMNAME(ap) = EOS
	AP_CWX(ap) = INDEFR
	AP_CWY(ap) = INDEFR
	AP_WX(ap) = INDEFR
	AP_WY(ap) = INDEFR
	AP_SCALE(ap) = 1.0
	AP_FWHMPSF(ap) = 1.0
	AP_POSITIVE(ap) = DEF_POSITIVE
	AP_DATAMIN(ap) = INDEFR
	AP_DATAMAX(ap) = INDEFR
	AP_EXPOSURE(ap) = EOS
	AP_ITIME(ap) = DEF_ITIME

	# Set display options.
	call ap_dispsetup (ap)

	# Setup the polyphot parameters.
	call ap_ysetup (ap)

	# Set unused structure pointers to null.
	AP_NOISE(ap) = NULL
	AP_PCENTER(ap) = NULL
	AP_PSKY(ap) = NULL
	AP_PPHOT(ap) = NULL
	AP_RPROF(ap) = NULL
	AP_PPSF(ap) = NULL
end
