include "../lib/apphotdef.h"

# AP_IMBUF -- Set the parameters for the image buffer.

procedure ap_imbuf (ap, hwidth, sequential)

pointer	ap		# pointer to the apphot structure
int	hwidth		# halfwidth of the line buffer
int	sequential	# optimize for sequntial i/o

begin
	AP_SEQUENTIAL(ap) = sequential
	AP_HWIDTH(ap) = hwidth
	if (AP_IMBUF(ap) != NULL)
	    call mfree (AP_IMBUF(ap), TY_REAL)
	AP_IMBUF(ap) = NULL
end
