include "../lib/display.h"
include "../lib/noise.h"
include "../lib/fitpsf.h"

# AP_GPFPARS -- Procedure to fetch the fitpsf parameters.

procedure ap_gpfpars (ap)

pointer	ap		# pointer to the apphot structure

int	function
pointer	sp, str
bool	clgetb()
int	clgeti(), btoi(), clgwrd()
real	clgetr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the apphot structure.
	call apsfinit (ap, AP_RADGAUSS, 3.5, 2.0, AP_NPOISSON)
	call apsetr (ap, PSFAPERT, clgetr ("box") / 2.0)

	# Get the data dependent parameters.
	call ap_gdapars (ap)

	# Get the rest of the FITPSF fitting parameters.
	function = clgwrd ("function", Memc[str], SZ_LINE, PSFFUNCS)
	call apsets (ap, PSFSTRING, Memc[str])
	call apseti (ap, PSFUNCTION, function)
	call apseti (ap, PMAXITER, clgeti ("maxiter"))
	call apseti (ap, PNREJECT, clgeti ("nreject"))
	call apsetr (ap, PK2, clgetr ("kreject"))

	# Get the plotting parameters.
	call apseti (ap, MKPSFBOX, btoi (clgetb ("mkbox")))

	call sfree (sp)
end
