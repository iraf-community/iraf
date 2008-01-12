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

	# Get the center, sky fitting and photometry apertures.
	cbox = clgetr ("cbox") / 2.0
	annulus = clgetr ("annulus")
	dannulus = clgetr ("dannulus")
	call clgstr ("apertures", Memc[apstr], SZ_LINE)
	naperts = ap_getaperts (Memc[apstr], Memr[aperts], MAX_NAPERTS)

	# Open the apphot structure.
	if (naperts <= 0.0)
	    call appinit (ap, AP_CENTROID1D, cbox, AP_CENTROID, annulus,
	        dannulus, 0.0, 1, AP_PWCONSTANT, 2.5, AP_NPOISSON) 
	else
	    call appinit (ap, AP_CENTROID1D, cbox, AP_CENTROID, annulus,
	        dannulus, Memr[aperts], naperts, AP_PWCONSTANT, 2.5,
		AP_NPOISSON) 

	# Set remaining parameters.
	call apseti (ap, SMOOTH, YES)

	if (naperts > 0)
	    call apsets (ap, APSTRING, Memc[apstr])
	call apsetr (ap, ZMAG, clgetr ("zmag"))

	call clgstr ("exposure", Memc[str], SZ_FNAME)
	call apsets (ap, EXPOSURE, Memc[str])

	call clgstr ("airmass", Memc[str], SZ_FNAME)
	call apsets (ap, AIRMASS, Memc[str])
	call apsetr (ap, XAIRMASS, INDEFR)

	call clgstr ("filter", Memc[str], SZ_FNAME)
	call apsets (ap, FILTER, Memc[str])
	call apsets (ap, FILTERID, "INDEF")

	call clgstr ("obstime", Memc[str], SZ_FNAME)
	call apsets (ap, OBSTIME, Memc[str])
	call apsets (ap, OTIME, "INDEF")

	call apsetr (ap, EPADU, clgetr ("epadu"))

	# Print the display parameters.
	call apseti (ap, MKCENTER, btoi (true))
	call apseti (ap, MKSKY, btoi (true))
	call apseti (ap, MKAPERT, btoi (true))
	call apseti (ap, RADPLOTS, btoi (clgetb ("radplots")))

	# Close the pset files.
	call sfree (mp)
end
