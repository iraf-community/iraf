include "../lib/display.h"
include "../lib/polyphot.h"

# AP_YSHOW -- Print the all the polyphot parameters on the standard output.

procedure ap_yshow (ap)

pointer	ap	# pointer to the apphot strucuture

begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_cpshow (ap)
	call printf ("\n")
	call ap_spshow (ap)
	call printf ("\n")
	call ap_yyshow (ap)
end


# AP_YPSHOW -- Print the noise and polypars parameters on the standard output.

procedure ap_ypshow (ap)

pointer	ap		# pointer to apphot structure

begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_yyshow (ap)
end


# AP_YYSHOW --  Print the polypars parameters on the standard output.

procedure ap_yyshow (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call printf ("Photometry Parameters\n")

	call apstats (ap, PYNAME, Memc[str], SZ_LINE)
	call printf ("    %s = %s\n")
	    call pargstr (KY_PYNAME)
	    call pargstr (Memc[str])

	call printf ("    %s = %g\n")
	    call pargstr (KY_PYZMAG)
	    call pargr (apstatr (ap, PYZMAG))

	call printf ("    %s = %b\n")
	    call pargstr (KY_MKPOLYGON)
	    call pargb (itob (apstati (ap, MKPOLYGON)))

	call sfree (sp)
end
