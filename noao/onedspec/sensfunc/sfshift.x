include	"sensfunc.h"


# SF_SHIFT -- Shift or unshift all standard stars to have zero mean residual.

procedure sf_shift (stds, nstds, cv, flag)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
int	flag			# Shift flag

pointer	x, y, f
int	i, j, n
real	shift, minshift

begin
	# If flag is YES then unshift the data.
	if (flag == YES) {
	    do i = 1, nstds {
	        if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		    next
	        n = STD_NWAVES(stds[i])
	        if (n == 0)
		    next
	        y = STD_SENS(stds[i])
	        call asubkr (Memr[y], STD_SHIFT(stds[i]), Memr[y], n)
	        STD_SHIFT(stds[i]) = 0.
	    }
	    flag = NO
	    call printf ("Data unshifted")
	    return
	}

	# Determine the shifts needed to make the mean residual zero.
	# Also determine the minimum shift.

	minshift = 0.
	do i = 1, nstds {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    if (n == 0)
		next
	    x = STD_WAVES(stds[i])
	    y = STD_SENS(stds[i])
	    f = STD_FIT(stds[i])
	    shift = 0.
	    do j = 1, n
		shift = shift + Memr[f+j-1] - Memr[y+j-1]
	    shift = STD_SHIFT(stds[i]) + shift / n
	    if (shift < minshift)
		minshift = shift
	    STD_SHIFT(stds[i]) = shift
	}

	# Adjust the shifts to be upwards.
	do i = 1, nstds {
	    if (STD_FLAG(stds[i]) == SF_EXCLUDE)
		next
	    n = STD_NWAVES(stds[i])
	    if (n == 0)
		next
	    y = STD_SENS(stds[i])
	    shift = STD_SHIFT(stds[i]) - minshift
	    call aaddkr (Memr[y], shift, Memr[y], n)
	    STD_SHIFT(stds[i]) = shift
	}
	flag = YES
	call printf ("Data shifted")
end
