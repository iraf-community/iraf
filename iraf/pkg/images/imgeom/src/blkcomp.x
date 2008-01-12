# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# BLKCOMP -- compute starting and ending input vectors for blocks in each
# dimension.  Initialize input-line vectors to block vectors.  Return total
# number of input lines mapping to current output line.

int procedure blkcomp (im1, blkfac, vout, blkin_s, blkin_e,
		vin_s, vin_e)

pointer	im1			# pointer to input image descriptor
int	blkfac[IM_MAXDIM]	# blocking factors for each dimension
long	vout[IM_MAXDIM]		# output image vectors for each dimension
long	blkin_s[IM_MAXDIM]	# index of starting block for each dimension
long	blkin_e[IM_MAXDIM]	# index of ending block for each dimension
long	vin_s[IM_MAXDIM]	# initial starting input vector
long	vin_e[IM_MAXDIM]	# initial ending input vector

int	num_ilines, dim

begin
	num_ilines = 1

	# Compute starting and ending indices of input image pixels in each
	# dimension mapping to current output line.

	do dim = 2, IM_NDIM(im1) {
	    blkin_s[dim] = long(1 + (vout[dim] - 1) * blkfac[dim])
	    blkin_e[dim] = long(min (IM_LEN(im1,dim), vout[dim] * blkfac[dim]))
	    vin_s[dim]   = blkin_s[dim]
	    vin_e[dim]   = blkin_s[dim]
	    num_ilines = num_ilines * (blkin_e[dim] - blkin_s[dim] + 1)
	}
	
	return (num_ilines)
end

