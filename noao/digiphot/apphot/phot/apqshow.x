include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_QSHOW -- Procedure to display the current data parameters.

procedure ap_qshow (ap)

pointer	ap	# pointer to the apphot strucuture

pointer	sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set the object charactersitics.
	call printf ("Quick Phot Parameters\n")
	call printf ("    %s: %s (%.2f,%.2f)    %s:  %s\n")
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	    call pargstr (KY_IMNAME)
	    call pargstr (Memc[str])
	    call pargr (apstatr (ap, CWX))
	    call pargr (apstatr (ap, CWY))
	call apstats (ap, OUTNAME, Memc[str], SZ_FNAME)
	    call pargstr (KY_OUTNAME)
	    call pargstr (Memc[str])

	call printf ("    %s: %s\n")
	call apstats (ap, CLNAME, Memc[str], SZ_FNAME)
	    call pargstr (KY_CLNAME)
	    call pargstr (Memc[str])

	call printf ("    %s = %g %s\n")
	    call pargstr (KY_CAPERT)
	    call pargr (2.0* apstatr (ap, CAPERT))
	    call pargstr ("pixels")

	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_ANNULUS)
	    call pargr (apstatr (ap, ANNULUS))
	    call pargstr ("pixels")
	    call pargstr (KY_DANNULUS)
	    call pargr (apstatr (ap, DANNULUS))
	    call pargstr ("pixels")

	call apstats (ap, APERTS, Memc[str], SZ_FNAME)
	call printf ("    %s = %s %s    %s = %g %s\n")
	    call pargstr (KY_APERTS)
	    call pargstr (Memc[str])
	    call pargstr ("pixels")
	    call pargstr (KY_ZMAG)
	    call pargr (apstatr (ap, ZMAG))
	    call pargstr (UN_PZMAG)
		
	call printf ("    %s = %g %s\n")
	    call pargstr (KY_EPADU)
	    call pargr (apstatr (ap, EPADU))
	    call pargstr (UN_NEPADU)
	call printf ("    %s = %s    %s = %s\n")
	call apstats (ap, EXPOSURE, Memc[str], SZ_FNAME)
	    call pargstr (KY_EXPOSURE)
	    call pargstr (Memc[str])
	call apstats (ap, AIRMASS, Memc[str], SZ_FNAME)
	    call pargstr (KY_AIRMASS)
	    call pargstr (Memc[str])
	call printf ("    %s = %s    %s = %s\n")
	call apstats (ap, FILTER, Memc[str], SZ_FNAME)
	    call pargstr (KY_FILTER)
	    call pargstr (Memc[str])
	call apstats (ap, OBSTIME, Memc[str], SZ_FNAME)
	    call pargstr (KY_OBSTIME)
	    call pargstr (Memc[str])

	call printf ("    %s = %b\n")
	    call pargstr (KY_RADPLOTS)
	    call pargb (itob (apstati (ap, RADPLOTS)))

	call sfree (sp)
end
