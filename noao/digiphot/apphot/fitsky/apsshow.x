include "../lib/display.h"
include "../lib/fitsky.h"


# AP_SSHOW -- Procedure to print sky fitting parameters on the terminal.

procedure ap_sshow (ap)

pointer	ap	# pointer to the apphot strucuture

bool	itob()
int	apstati()

begin
	call ap_nshow (ap)
	call printf ("\n")
	call ap_spshow (ap)
	call printf ("    %s = %b\n")
	    call pargstr (KY_RADPLOTS)
	    call pargb (itob (apstati (ap, RADPLOTS)))
end


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
	    call pargstr (UN_SALGORITHM)
	    call pargstr (KY_SKY_BACKGROUND)
	    call pargr (apstatr (ap, SKY_BACKGROUND))
	    call pargstr (UN_SCOUNTS)

	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_ANNULUS)
	    call pargr (apstatr (ap, ANNULUS))
	    call pargstr (UN_SSCALEUNIT)
	    call pargstr (KY_DANNULUS)
	    call pargr (apstatr (ap, DANNULUS))
	    call pargstr (UN_SSCALEUNIT)

	call printf ("    %s = %g %s    %s = %g %s    %s = %b\n")
	    call pargstr (KY_K1)
	    call pargr (apstatr (ap, K1))
	    call pargstr (UN_SSIGMA)
	    call pargstr (KY_BINSIZE)
	    call pargr (apstatr (ap, BINSIZE))
	    call pargstr (UN_SSIGMA)
	    call pargstr (KY_SMOOTH)
	    call pargb (itob (apstati (ap, SMOOTH)))

	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_SLOCLIP)
	    call pargr (apstatr (ap, SLOCLIP))
	    call pargstr (UN_SPERCENT)
	    call pargstr (KY_SHICLIP)
	    call pargr (apstatr (ap, SHICLIP))
	    call pargstr (UN_SPERCENT)

	call printf ("    %s = %g %s    %s = %g %s    %s = %d\n")
	    call pargstr (KY_SLOREJECT)
	    call pargr (apstatr (ap, SLOREJECT))
	    call pargstr (UN_SSIGMA)
	    call pargstr (KY_SHIREJECT)
	    call pargr (apstatr (ap, SHIREJECT))
	    call pargstr (UN_SSIGMA)
	    call pargstr (KY_SMAXITER)
	    call pargi (apstati (ap, SMAXITER))

	call printf ("    %s = %d    %s = %g %s\n")
	    call pargstr (KY_SNREJECT)
	    call pargi (apstati (ap, SNREJECT))
	    call pargstr (KY_RGROW)
	    call pargr (apstatr (ap, RGROW))
	    call pargstr (UN_SSCALEUNIT)

	call printf ("    %s = %b\n")
	    call pargstr (KY_MKSKY)
	    call pargb (itob (apstati (ap, MKSKY)))

	call sfree (sp)
end
