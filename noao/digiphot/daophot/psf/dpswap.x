# DP_SWAP -- Exchange the position of two stars in the APPHOT
# photometry results

procedure dp_swap (flip, flop, id, x, y, mag, sky)

int	flip, flop		# the numbers of the two stars to exchange
int	id[ARB]			# the star IDs
real	x[ARB], y[ARB]		# the X, Y positions of the stars
real	sky[ARB]		# the ksy values of the stars
real	mag[ARB]		# the magnitudes of the stars

real	xhold, yhold, mhold, shold
int	ihold

begin
	ihold = id[flip]
	xhold = x[flip]
	yhold = y[flip]
	mhold = mag[flip]
	shold = sky[flip]

	id[flip] = id[flop]
	x[flip]  = x[flop]
	y[flip]  = y[flop]
	mag[flip] = mag[flop]
	sky[flip] = sky[flop]

	id[flop] = ihold
	x[flop]  = xhold
	y[flop]  = yhold
	mag[flop] = mhold
	sky[flop] = shold
end
