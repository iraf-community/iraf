# APSMED -- Procedure to compute averaged median given a sorted array
# and an averaging half width.

real procedure apsmed (skypix, index, nskypix, medcut)

real	skypix[ARB]	# array of sky pixels
int	index[ARB]	# sorted index array
int	nskypix		# number of sky pixels
int	medcut		# averaging half width

int	med, j, nmed
real	sumed

begin
	sumed = 0.0
	med = (nskypix + 1) / 2

	nmed = 0
	do j = max (1, med - medcut), min (nskypix, med + medcut) {
	    sumed = sumed + skypix[index[j]]
	    nmed = nmed + 1
	}

	return (sumed / nmed)
end


# APIMED -- Procedure to compute index of new median array element. Weight
# is an arbitrary weight array which is assumed to be zero if the pixels has
# been rejected and is positive otherwise.

int procedure apimed (weight, index, lo, hi, nmed)

real	weight[ARB]		# array of weights
int	index[ARB]		# array of sorted indices
int	lo, hi			# ranges of weights
int	nmed			# number of good sky pixels

int	npts, med

begin
	npts = 0
	for (med = lo; med <= hi && npts < nmed; med = med + 1) {
	    if (weight[index[med]] > 0.0)
		npts = npts + 1
	}

	if (npts == 0)
	    return (0)
	else
	    return (med)
end


# APWSMED -- Procedure to compute new median allowing for quantization
# effects assuming there been pixel rejection.

real procedure apwsmed (skypix, index, weight, nskypix, med, medcut)

real	skypix[ARB]		# sky values
int	index[ARB]		# sorted indices
real	weight[ARB]		# weights
int	nskypix			# skypixels
int	med			# index of median value
int	medcut			# of median cut

int	j, nmed
real	sumed

begin
	sumed = skypix[index[med]]

	nmed = 1
	for (j = med - 1; j >= 1; j = j - 1) {
	    if (nmed >=  medcut + 1)
		break
	    if (weight[index[j]] > 0.0) {
		sumed = sumed + skypix[index[j]]
		nmed = nmed + 1
	    }
	}
	for (j = med + 1; j <= nskypix; j = j + 1) {
	    if (nmed >= 2 * medcut + 1)
		break
	    if (weight[index[j]] > 0.0) {
		sumed = sumed + skypix[index[j]]
		nmed = nmed + 1
	    }
	}

	return (sumed / nmed)
end
