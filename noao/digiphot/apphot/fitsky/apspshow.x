include "../lib/fitsky.h"
include "../lib/display.h"

# AP_SPSHOW -- Procedure to print sky fitting parameters on the terminal.

procedure ap_spshow (ap)

pointer	ap	# pointer to the apphot strucuture

pointer	sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	# Print the image characteristics
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print the sky fitting parameters.
	call printf ("Sky Fitting Parameters\n")
	call apstats (ap, SSTRING, Memc[str], SZ_FNAME)
	call printf ("    %s = %s %s    %s = %g %s\n")
	    call pargstr (KY_SSTRING)
	    call pargstr (Memc[str])
	    call pargstr (UN_SSTRING)
	    call pargstr (KY_SKY_BACKGROUND)
	    call pargr (apstatr (ap, SKY_BACKGROUND))
	    call pargstr (UN_SKY_BACKGROUND)

	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_ANNULUS)
	    call pargr (apstatr (ap, ANNULUS))
	    call pargstr (UN_ANNULUS)
	    call pargstr (KY_DANNULUS)
	    call pargr (apstatr (ap, DANNULUS))
	    call pargstr (UN_DANNULUS)

	call printf ("    %s = %g %s    %s = %g %s    %s = %b\n")
	    call pargstr (KY_K1)
	    call pargr (apstatr (ap, K1))
	    call pargstr (UN_K1)
	    call pargstr (KY_BINSIZE)
	    call pargr (apstatr (ap, BINSIZE))
	    call pargstr (UN_BINSIZE)
	    call pargstr (KY_SMOOTH)
	    call pargi (itob (apstati (ap, SMOOTH)))

	call printf ("    %s = %g %s    %s = %d\n")
	    call pargstr (KY_K2)
	    call pargr (apstatr (ap, K2))
	    call pargstr (UN_K2)
	    call pargstr (KY_SMAXITER)
	    call pargi (apstati (ap, SMAXITER))

	call printf ("    %s = %d    %s = %g %s\n")
	    call pargstr (KY_SNREJECT)
	    call pargi (apstati (ap, SNREJECT))
	    call pargstr (KY_RGROW)
	    call pargr (apstatr (ap, RGROW))
	    call pargstr (UN_RGROW)

	call printf ("    %s = %b\n")
	    call pargstr (KY_MKSKY)
	    call pargb (itob (apstati (ap, MKSKY)))

	call sfree (sp)
end
