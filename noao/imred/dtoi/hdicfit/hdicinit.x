include	<mach.h>
include	"hdicfit.h"

# HDIC_INIT -- Initialize hdfit/icgfit interface.

procedure hdic_init (density, nvalues, dmax)

double	density[nvalues]	# Reference density above fog values
int	nvalues			# Number of values in sample
double	dmax			# Maximum possible density

int	i
pointer	sp, base
double	xxmax, xxmin, delta_den
bool	fp_equald()
include	"hdic.com"
errchk	malloc

begin
	call smark (sp)

	if (den == NULL || big_den == NULL) {
	    call malloc (den, nvalues, TY_DOUBLE)
	    call malloc (big_den, NVALS_FIT, TY_DOUBLE)
	} else if (nvalues != nraw) 
	    call realloc (den, nvalues, TY_DOUBLE)

	nraw = nvalues
	call salloc (base, NVALS_FIT, TY_DOUBLE)

	# Copy density array to pointer location
	call amovd (density, Memd[den], nraw)

	# Calculate big vector of density values.  The points are spaced 
	# linear in log space, to yield adequate spacing at low density values.

	call alimd (density, nraw, xxmin, xxmax)

	# Put user value for maximum density in common block if it is valid.
	if (! fp_equald (dmax, 0.0D0))
	    maxden = dmax

	# Make big_den go all the way to maxden, not just xxmax.  Make sure
	# the value of xxmin won't cause the log function to blow up.

	if (xxmin > 0.0D0)
	    ;
	else
	    xxmin = 2.0 * EPSILOND

	delta_den = (log10 (maxden) - log10 (xxmin)) / double (NVALS_FIT - 1)

	do i = 1, NVALS_FIT
	    Memd[big_den+i-1] = log10 (xxmin) + double (i-1) * delta_den

	call amovkd (10.0D0, Memd[base], NVALS_FIT)
	call aexpd (Memd[base], Memd[big_den], Memd[big_den], NVALS_FIT)

	call sfree (sp)
end
