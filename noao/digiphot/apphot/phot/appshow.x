include "../lib/display.h"
include "../lib/phot.h"

# AP_PSHOW -- Procedure to print the photometry parameters.

procedure ap_pshow (ap)

pointer	ap	# pointer to the apphot strucuture

bool	itob()
int	apstati()

begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_cpshow (ap)
	call printf ("\n")
	call ap_spshow (ap)
	call printf ("\n")
	call ap_mpshow (ap)
	call printf ("    %s = %b\n")
	    call pargstr (KY_RADPLOTS)
	    call pargb (itob (apstati (ap, RADPLOTS)))
end


# AP_MSHOW -- Procedure to print the photometry parameters on the standard
# output.

procedure ap_mshow (ap)

pointer	ap		# pointer to apphot structure

bool	itob()
int	apstati()
begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_mpshow (ap)
	call printf ("    %s = %b\n")
	    call pargstr (KY_RADPLOTS)
	    call pargb (itob (apstati (ap, RADPLOTS)))
end


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
	    call pargstr (UN_PMODEL)
	call apstats (ap, APERTS, Memc[str], SZ_LINE)
	call printf ("    %s = %s %s\n")
	    call pargstr (KY_APERTS)
	    call pargstr (Memc[str])
	    call pargstr (UN_PSCALEUNIT)
	call printf ("    %s = %g\n")
	    call pargstr (KY_ZMAG)
	    call pargr (apstatr (ap, ZMAG))
	call printf ("    %s = %b\n")
	    call pargstr (KY_MKAPERT)
	    call pargb (itob (apstati (ap, MKAPERT)))

	call sfree (sp)
end
