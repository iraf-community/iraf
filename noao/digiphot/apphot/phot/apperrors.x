include "../lib/phot.h"

# AP_PERRORS -- Print the photometry task errors.

procedure ap_perrors (ap, cier, sier, pier)

pointer	ap		# apphot structure
int	cier		# centering error code
int	sier		# sky fitting error code
int	pier		# photometry error code

begin
	# Print the centering error message.
	call ap_cerrors (ap, cier)

	# Print the sky fitting error message.
	call ap_serrors (ap, sier)

	# Print the photometry error message.
	call ap_merrors (ap, pier)

end


# AP_MERRORS -- Print the photometry errors.

procedure ap_merrors (ap, pier)

pointer	ap			# pointer to the apphot structure (unused)
int	pier			# photometry error

begin
	# Print the photometry error message.
	switch (pier) {
	case AP_APERT_NOAPERT:
	    call printf ("Photometry apertures are outside of the image.\n")
	case AP_APERT_OUTOFBOUNDS:
	 call printf ("Photometry apertures are partially outside the image.\n")
	case AP_APERT_NOSKYMODE:
	    call printf ("The sky value is undefined.\n")
	case AP_APERT_NEGMAG:
	    call printf ("The total flux inside the aperture is negative.\n")
	case AP_APERT_BADDATA:
	    call printf ("Bad data in the aperture(s).\n")
	default:
	    call printf ("")
	}
end
