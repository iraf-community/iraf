# AP_HGMSUB -- Procedure to subtract a point from an existing  histogram.

procedure ap_hgmsub (hgm, nbins, z1, z2, skypix)

real	hgm[ARB]		# histogram
int	nbins			# number of bins
real	z1, z2			# range of histogram
real	skypix	               	# sky value

int	bin
real	dh

begin
	if (skypix < z1 || skypix > z2)
	    return
	dh = real (nbins - 1) / (z2 - z1)
	bin = int ((skypix - z1) * dh) + 1
	hgm[bin] = hgm[bin] - 1.0
end


# AP_HGMSUB2 -- Procedure to subract points from the accumulated sums
# and the existing histogram.

procedure ap_hgmsub2 (hgm, nbins, z1, z2, skypix, sky_zero, sumpx, sumsqpx,
	sumcbpx)

real	hgm[ARB]		# histogram
int	nbins			# number of bins
real	z1, z2			# range of histogram
real	skypix	               	# sky value
real	sky_zero		# sky zero point for moment analysis
double	sumpx			# sum of the sky pixel values
double	sumsqpx			# sum of the squares of the sky pixel values
double	sumcbpx			# sum of the cubes of the sky pixel values

double	dsky
int	bin
real	dh

begin
	if (skypix < z1 || skypix > z2)
	    return
	dsky = skypix - sky_zero
	sumpx = sumpx - dsky
	sumsqpx = sumsqpx - dsky ** 2
	sumcbpx = sumcbpx - dsky ** 3
	dh = real (nbins - 1) / (z2 - z1)
	bin = int ((skypix - z1) * dh) + 1
	hgm[bin] = hgm[bin] - 1.0
end
