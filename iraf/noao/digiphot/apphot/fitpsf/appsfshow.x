include "../lib/display.h"
include "../lib/fitpsf.h"

# AP_PSFSHOW -- Procedure to print the fitpsf parameters on the standard output.

procedure ap_psfshow (ap)

pointer	ap		# pointer to the apphot structure

begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_pfshow (ap)
end


# AP_PFSHOW -- Procedure to print out the fitting parameters on the standard
# output.

procedure ap_pfshow (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	# Print the PSF fitting parameters.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call printf ("Psf Modelling Parameters\n")
	call apstats (ap, PSFSTRING, Memc[str], SZ_FNAME)
	call printf ("    %s = %s\n")
	    call pargstr (KY_PSFSTRING)
	    call pargstr (Memc[str])

	call printf ("    %s = %g %s    %s = %d\n")
	    call pargstr (KY_PSFAPERT)
	    call pargr (2.0 * apstatr (ap, PSFAPERT))
	    call pargstr (UN_PSFSCALEUNIT)
	    call pargstr (KY_PMAXITER)
	    call pargi (apstati (ap, PMAXITER))

	call printf ("    %s = %g %s    %s = %d\n")
	    call pargstr (KY_PK2)
	    call pargr (apstatr (ap, PK2))
	    call pargstr (UN_PSFSIGMA)
	    call pargstr (KY_PNREJECT)
	    call pargi (apstati (ap, PNREJECT))

	call printf ("    %s = %b\n")
	    call pargstr (KY_MKPSFBOX)
	    call pargb (itob (apstati (ap, MKPSFBOX)))

	call sfree (sp)
end
