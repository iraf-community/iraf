# AP_ABOXR -- Vector boxcar smooth. The input vector is convolved with a uniform
# kernel of length knpix. Boundary extension is assumed to have been performed
# on the input array.

procedure ap_aboxr (in, out, npix, knpix)

real	in[npix+knpix-1]	# input array with boundary exntension
real	out[npix]		# output array
int	npix			# number of pixels
int	knpix			# length of the kernel

int	i
real	sum

begin
	sum = 0.0
	do i = 1, knpix - 1
	    sum = sum + in[i]
	do i = 1, npix {
	    sum = sum + in[i+knpix-1] 
	    out[i] = sum / knpix
	    sum = sum - in[i]
	}

end
