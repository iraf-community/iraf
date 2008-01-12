# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CNV_RADCNVR -- Vector convolution of a radially symmetric function. 
# The output vector is equal to the sum of its initial value and the
# convolution of the input vector with the kernel. The kernel length may be an
# even or odd number.  This routine assumes boundary extension on the input
# vector has been provided.  For short kernels, we unroll the inner do loop
# into a single statement to reduce loop overhead. This is a modified vops
# procedure.

procedure cnv_radcnvr (in, out, npix, kernel, knpix)

real	in[npix+knpix-1]	# input vector, including boundary pixels
real	out[ARB]		# output vector
int	npix			# length of output vector
real	kernel[knpix]		# convolution kernel
int	knpix			# size of convolution kernel

int	i, j, midpoint, hknpix
real	sum, k1, k2, k3

begin
	switch (knpix) {
	case 1:
	    k1 = kernel[1]
	    do i = 1, npix
		out[i] = out[i] + k1 * in[i]
	case 2:
	    k1 = kernel[1]
	    do i = 1, npix
	        out[i] = out[i] + k1 * (in[i] + in[i+1])
	case 3:
	    k1 = kernel[1]
	    k2 = kernel[2]
	    do i = 1, npix
	        out[i] = out[i] + k1 * (in[i] + in[i+2]) + k2 * in[i+1]
	case 4:
	    k1 = kernel[1]
	    k2 = kernel[2]
	    do i = 1, npix
	        out[i] = out[i] + k1 * (in[i] + in[i+3]) + k2 * (in[i+1] +
		    in[i+2])
	case 5:
	    k1 = kernel[1]
	    k2 = kernel[2]
	    k3 = kernel[3]
	    do i = 1, npix
	        out[i] = out[i] + k1 * (in[i] + in[i+4]) + k2 * (in[i+1] +
		    in[i+3]) + k3 * in[i+2]
	case 6:
	    k1 = kernel[1]
	    k2 = kernel[2]
	    k3 = kernel[3]
	    do i = 1, npix
	        out[i] = out[i] + k1 * (in[i] + in[i+5]) + k2 * (in[i+1] +
		    in[i+4]) + k3 * (in[i+2] + in[i+3])
	default:
	    hknpix = knpix / 2
	    midpoint = hknpix + 1
	    if (mod (knpix, 2) == 1) {
	        do i = 1, npix {
	            sum = out[i]
	            do j = 1, hknpix
		        sum = sum + kernel[j] * (in[i+j-1] + in[i-j+knpix])
		    out[i] = sum + kernel[midpoint] * in[i+hknpix]
	        }
	    } else {
	        do i = 1, npix {
	            sum = out[i]
		    do j = 1, hknpix
		        sum = sum + kernel[j] * (in[i+j-1] + in[i-j+knpix])
		    out[i] = sum
	        }
	    }
	}
end


# CNV_AWSUM1 -- Add two real vectors together after multiplying one of them
# by a constant.

procedure cnv_awsum1 (a, b, c, npts, k)

real	a[ARB]		# the first input vector
real	b[ARB]		# the second input vector
real	c[ARB]		# the output vector
int	npts		# the number of points
real	k		# the real constant

int	i

begin
	do i = 1, npts
	    c[i] = a[i] + k * b[i]
end
