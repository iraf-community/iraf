include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_GQPPARS -- Procedure to fetch the phot task parameters.

procedure ap_gqppars (ap)

pointer	ap		# pointer to apphot structure

int	naperts
pointer	mp, aperts, str, apstr
real	cbox, annulus, dannulus
bool	clgetb()
int	ap_getaperts(), btoi()
real	clgetr()

begin
	call smark (mp)
	call salloc (aperts, MAX_NAPERTS, TY_REAL)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (apstr, SZ_LINE, TY_CHAR)

	# Get the major parameters.
	cbox = clgetr ("cbox") / 2.0
	annulus = clgetr ("annulus")
	dannulus = clgetr ("dannulus")
	call clgstr ("apertures", Memc[apstr], SZ_LINE)
	naperts = ap_getaperts (Memc[apstr], Memr[aperts], MAX_NAPERTS)

	# Open the apphot structure.
	if (naperts <= 0.0)
	    call appinit (ap, AP_CENTROID1D, cbox, AP_MODE, annulus,
	        dannulus, 0.0, 1, AP_PWCONSTANT, 1.0, AP_NPOISSON) 
	else
	    call appinit (ap, AP_CENTROID1D, cbox, AP_MODE, annulus, dannulus,
	        Memr[aperts], naperts, AP_PWCONSTANT, 1.0, AP_NPOISSON) 

	# Set remaining parameters.
	call clgstr ("exposure", Memc[str], SZ_FNAME)
	call apsets (ap, EXPOSURE, Memc[str])
	call clgstr ("airmass", Memc[str], SZ_FNAME)
	call apsets (ap, AIRMASS, Memc[str])
	call clgstr ("filter", Memc[str], SZ_FNAME)
	call apsets (ap, FILTER, Memc[str])
	call apsetr (ap, EPADU, clgetr ("epadu"))
	call apsetr (ap, ZMAG, clgetr ("zmag"))
	if (naperts > 0)
	    call apsets (ap, APSTRING, Memc[apstr])

	# Print the display parameters.
	call apseti (ap, MKCENTER, btoi (false))
	call apseti (ap, MKSKY, btoi (false))
	call apseti (ap, MKAPERT, btoi (false))
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))

	# Close the pset files.
	call sfree (mp)
end
