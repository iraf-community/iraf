include	"apertures.h"

# AP_PRINT -- Print the parameters of the indexed aperture.

procedure ap_print (index, line, all, aps)

int	index			# Index of aperture
int	line			# Dispersion line
int	all			# All flag
pointer	aps[ARB]		# Apertures

int	apaxis
pointer	ap
real	ap_cveval()

begin
	if (index < 1)
	    return

	if (all == YES)
	    call printf ("ALL: ")
	else
	    call printf ("     ")

	ap = aps[index]
	apaxis = AP_AXIS(ap)
	call printf (
"aperture = %d  beam = %d  center = %.2f  low = %.2f  upper = %.2f\n")
	    call pargi (AP_ID(ap))
	    call pargi (AP_BEAM(ap))
	    call pargr (AP_CEN(ap, apaxis)+ap_cveval (AP_CV(ap), real (line)))
	    call pargr (AP_LOW(ap, apaxis))
	    call pargr (AP_HIGH(ap, apaxis))
end
