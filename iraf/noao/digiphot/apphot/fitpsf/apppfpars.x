include "../lib/fitpsf.h"
include "../lib/display.h"

# AP_PPFPARS -- Procedure to write the fitpsf parameters to a parameter file.

procedure ap_ppfpars (ap)

pointer	ap		# pointer to apphot structure

pointer	sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	# Initialize and open psets.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Store the psf fitting parameters.
	call clputr ("box", 2.0 * apstatr (ap, PSFAPERT))
	call apstats (ap, PSFSTRING, Memc[str], SZ_FNAME)
	call clpstr ("function", Memc[str])
	call clputi ("maxiter", apstati (ap, PMAXITER))
	call clputr ("kreject", apstatr (ap, PK2))
	call clputi ("nreject", apstati (ap, PNREJECT))
	call clputb ("mkbox", itob (apstati (ap, MKPSFBOX)))

	# Store the data dependent parameters.
	call ap_dapars (ap)

	# Closeup.
	call sfree (sp)
end
