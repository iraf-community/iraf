include	"extract.h"

# EX_RECEN -- Recenter the profiles by shifting either from or to the
# specified profile center relative to the center of the profile limits.
# Interpolate only if necessary.

procedure ex_recen (ex, pstart, pcen, pend, naps, avg, model, direction)

pointer	ex				# Pointer to extraction parameters
int	pstart[naps]			# Starting profile index
real	pcen[naps]			# Profile center index
int	pend[naps]			# Ending profile index
int	naps				# Number of apertures
real	avg[ARB]			# Profiles
real	model[ARB]			# Profiles
int	direction			# -1 = from pcen, 1 = to pcen

int	i, j, c1, c2, nc
real	shift, x, asieval()

begin
	do j = 1, naps {
	    if (pstart[j] == 0)
		next

	    c1 = pstart[j]
	    c2 = pend[j]
	    nc = c2 - c1 + 1
	    shift = direction * ((c1 + c2) / 2. - pcen[j])

	    if (abs (shift) < .05) {
		call amovr (avg[c1], model[c1], nc)
	    } else {
		iferr (call asifit (EX_ASI(ex), avg[c1], nc)) {
		    call amovr (avg[c1], model[c1], nc)
		    return
		}

		x = 1 + shift
	    	do i = pstart[j], pend[j] {
		    if ((x >= 1) && (x <= nc))
		        model[i] = asieval (EX_ASI(ex), x)
		    x = x + 1.
		}
	    }
	}
end
