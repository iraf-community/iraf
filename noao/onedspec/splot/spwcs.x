define	VLIGHT	2.997925e5	# Speed of light, Km/sec

# SPWCS -- Convert between different WCS.  This can be expanded in future.

procedure spwcs (x1, x2, wcs1, wcs2)

real	x1		# Input coordinate
real	x2		# Output coordinate
int	wcs1		# Input coordinate WCS
int	wcs2		# Output coordinate WCS

real	w0		# Reference velocity
common	/wcscom/ w0

begin
	if (wcs1 == wcs2)
	    x2 = x1
	else if (wcs1 == 1)
	    x2 = (x1 / w0 - 1) * VLIGHT
	else
	    x2 = (x1 / VLIGHT + 1) * w0
end


# SPVWCS -- Convert between different WCS.  This can be expanded in future.

procedure spvwcs (x1, x2, n, wcs1, wcs2)

real	x1[n]		# Input coordinates
real	x2[n]		# Output coordinates
int	n		# Number of points
int	wcs1		# Input coordinate WCS
int	wcs2		# Output coordinate WCS

int	i

real	w0		# Reference velocity
common	/wcscom/ w0

begin
	if (wcs1 == wcs2)
	    call amovr (x1, x2, n)
	else if (wcs1 == 1) {
	    do i = 1, n
		x2[i] = (x1[i] / w0 - 1) * VLIGHT
	} else {
	    do i = 1, n
		x2[i] = (x1[i] / VLIGHT + 1) * w0
	}
end
