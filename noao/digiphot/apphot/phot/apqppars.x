include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/noise.h"
include "../lib/display.h"

# AP_QPPARS -- Procedure to write out qthe phot task parameters.

procedure ap_qppars (ap)

pointer	ap		# pointer to apphot structure

pointer	mp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	call smark (mp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call clputr ("cbox", 2.0 * apstatr (ap, CAPERT))
	call clputr ("annulus", apstatr (ap, ANNULUS))
	call clputr ("dannulus", apstatr (ap, DANNULUS))
	call apstats (ap, APERTS, Memc[str], SZ_LINE)
	call clpstr ("apertures", Memc[str])

	call apstats (ap, EXPOSURE, Memc[str], SZ_LINE)
	call clpstr ("exposure", Memc[str])
	call apstats (ap, AIRMASS, Memc[str], SZ_LINE)
	call clpstr ("airmass", Memc[str])
	call apstats (ap, FILTER, Memc[str], SZ_LINE)
	call clpstr ("filter", Memc[str])
	call apstats (ap, OBSTIME, Memc[str], SZ_LINE)
	call clpstr ("obstime", Memc[str])
	call clputr ("epadu", apstatr (ap, EPADU))
	call clputr ("zmag", apstatr (ap, ZMAG))
	call clputb ("radplots", itob (apstati (ap, RADPLOTS)))

	call sfree (mp)
end
