# DP_ALMSKY -- Determine the mean sky value for the current group of stars.

real procedure dp_almsky (sky, nstar)

real	sky[ARB]		# array of sky values
int	nstar			# number of stars in the group

int	i, nsky
real 	sky_sum

begin
	sky_sum = 0.0
	nsky = 0

	do i = 1, nstar {
	    if (IS_INDEFR(sky[i]))
		next
	    sky_sum = sky_sum + sky[i]
	    nsky = nsky + 1
	}

	if (nsky <= 0)
	    return (INDEFR)
	else
	    return (sky_sum / nsky)
end
