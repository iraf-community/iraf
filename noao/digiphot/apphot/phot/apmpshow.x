include "../lib/display.h"
include "../lib/phot.h"

# AP_MPSHOW -- Procedure to print the photometry parameters on the standard
# output.

procedure ap_mpshow (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	# Write out the image and cursor position.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Write out the phot parameters.
	call printf ("Photometry Parameters\n")
	call apstats (ap, PWSTRING, Memc[str], SZ_LINE)
	call printf ("    %s = %s %s\n")
	    call pargstr (KY_PWSTRING)
	    call pargstr (Memc[str])
	    call pargstr (UN_PWSTRING)
	call apstats (ap, APERTS, Memc[str], SZ_LINE)
	call printf ("    %s = %s %s\n")
	    call pargstr (KY_APERTS)
	    call pargstr (Memc[str])
	    call pargstr (UN_APERTS)
	call printf ("    %s = %g\n")
	    call pargstr (KY_ZMAG)
	    call pargr (apstatr (ap, ZMAG))
	call printf ("    %s = %b\n")
	    call pargstr (KY_MKAPERT)
	    call pargb (itob (apstati (ap, MKAPERT)))

	call sfree (sp)
end
