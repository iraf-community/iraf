# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>

define	AVG		1	# operation = arithmetic average
define	SUM		2	# operation = arithmetic sum

# change to (lrdx) in future


# BLKAVG -- Block average or sum on n-dimensional images.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  Remainder pixels at the end
# of a line, col, etc. are averaged correctly.

procedure blkavl (im1, im2, blkfac, option)

pointer	im1			# input image
pointer im2			# output image
int	blkfac[IM_MAXDIM]	# blocking factors
int	option			# block operation (average, sum, ...)

int	num_oblks[IM_MAXDIM], i, count, ndim, dim, nlines_in_sum, nfull_blkx
int	junk, num_ilines, num_olines, oline
long	blkin_s[IM_MAXDIM], blkin_e[IM_MAXDIM]
long	vin_s[IM_MAXDIM], vin_e[IM_MAXDIM], vout[IM_MAXDIM]
real	sum
pointer	sp, accum_ptr, iline_ptr, oline_ptr

int	blkcomp(), imggsl(), impnll() 
errchk	imggsl(), impnll()

begin
	call smark (sp)
	call salloc (accum_ptr, IM_LEN(im1, 1), TY_LONG)

	# Initialize; input and output vectors, block counters.
	ndim = IM_NDIM(im1)
	nfull_blkx = IM_LEN(im1, 1) / blkfac[1]
	blkin_s[1] = long(1)
	blkin_e[1] = long(IM_LEN(im1, 1))
	vin_s[1]   = blkin_s[1]
	vin_e[1]   = blkin_e[1]

	do i = 1, ndim {
	    num_oblks[i] = (IM_LEN(im1,i) + blkfac[i] - 1) / blkfac[i]
	    IM_LEN(im2, i) = num_oblks[i]
	}
	num_olines = 1
	do i = 2, ndim
	    num_olines = num_olines * num_oblks[i]
	call amovkl (long(1), vout, IM_MAXDIM)

	# For each sequential output-image line, ...
	do oline = 1, num_olines {

	    call aclrl (Meml[accum_ptr], IM_LEN(im1, 1))
	    nlines_in_sum = 0

	    # Compute block vector; initialize dim>1 input vector.
	    num_ilines = blkcomp (im1, blkfac, vout, blkin_s, blkin_e,
		vin_s, vin_e)

	    # Output pointer; note impnl$t returns vout for NEXT output line.
	    junk = impnll (im2, oline_ptr, vout)

	    # For all input lines mapping to current output line, ...
	    do i = 1, num_ilines {
		# Get line from input image.
		iline_ptr = imggsl (im1, vin_s, vin_e, ndim)

		# Increment general section input vector between block bounds.
		do dim = 2, ndim {
		    if (vin_s[dim] < blkin_e[dim]) {
			vin_s[dim] = vin_s[dim] + 1
			vin_e[dim] = vin_s[dim]
			break
		    } else {
			vin_s[dim] = blkin_s[dim]
			vin_e[dim] = vin_s[dim]
		    }
		}

		# Accumulate line into block sum.  Keep track of no. of 
		# lines in sum so that we can compute block average later.

		call aaddl (Meml[iline_ptr], Meml[accum_ptr],
		    Meml[accum_ptr], IM_LEN(im1,1))
		nlines_in_sum = nlines_in_sum + 1
	    }

	    # We now have a templine of sums; average/sum into output buffer
	    # first the full blocks using a vop.
	    if (option == AVG)
		call abavl (Meml[accum_ptr], Meml[oline_ptr], nfull_blkx,
		    blkfac[1])
	    else
		call absul (Meml[accum_ptr], Meml[oline_ptr], nfull_blkx,
		    blkfac[1])

	    # Now average/sum the final partial block in x, if any.
	    if (nfull_blkx < num_oblks[1]) {
		sum = 0
		count = 0
		do i = nfull_blkx * blkfac[1] + 1, IM_LEN(im1,1) {
		    sum = sum + Meml[accum_ptr+i-1]
		    count = count + 1
		}
		if (option == AVG)
		    Meml[oline_ptr+num_oblks[1]-1] = sum / count
		else
		    Meml[oline_ptr+num_oblks[1]-1] = sum
	    }
		
	    # Block average into output line from the sum of all lines block
	    # averaged in X.
	    if (option == AVG)
		call adivkl (Meml[oline_ptr], long(nlines_in_sum),
		    Meml[oline_ptr], num_oblks[1])
	}
	
	call sfree (sp)
end



# BLKAVG -- Block average or sum on n-dimensional images.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  Remainder pixels at the end
# of a line, col, etc. are averaged correctly.

procedure blkavr (im1, im2, blkfac, option)

pointer	im1			# input image
pointer im2			# output image
int	blkfac[IM_MAXDIM]	# blocking factors
int	option			# block operation (average, sum, ...)

int	num_oblks[IM_MAXDIM], i, count, ndim, dim, nlines_in_sum, nfull_blkx
int	junk, num_ilines, num_olines, oline
long	blkin_s[IM_MAXDIM], blkin_e[IM_MAXDIM]
long	vin_s[IM_MAXDIM], vin_e[IM_MAXDIM], vout[IM_MAXDIM]
real	sum
pointer	sp, accum_ptr, iline_ptr, oline_ptr

int	blkcomp(), imggsr(), impnlr() 
errchk	imggsr(), impnlr()

begin
	call smark (sp)
	call salloc (accum_ptr, IM_LEN(im1, 1), TY_REAL)

	# Initialize; input and output vectors, block counters.
	ndim = IM_NDIM(im1)
	nfull_blkx = IM_LEN(im1, 1) / blkfac[1]
	blkin_s[1] = long(1)
	blkin_e[1] = long(IM_LEN(im1, 1))
	vin_s[1]   = blkin_s[1]
	vin_e[1]   = blkin_e[1]

	do i = 1, ndim {
	    num_oblks[i] = (IM_LEN(im1,i) + blkfac[i] - 1) / blkfac[i]
	    IM_LEN(im2, i) = num_oblks[i]
	}
	num_olines = 1
	do i = 2, ndim
	    num_olines = num_olines * num_oblks[i]
	call amovkl (long(1), vout, IM_MAXDIM)

	# For each sequential output-image line, ...
	do oline = 1, num_olines {

	    call aclrr (Memr[accum_ptr], IM_LEN(im1, 1))
	    nlines_in_sum = 0

	    # Compute block vector; initialize dim>1 input vector.
	    num_ilines = blkcomp (im1, blkfac, vout, blkin_s, blkin_e,
		vin_s, vin_e)

	    # Output pointer; note impnl$t returns vout for NEXT output line.
	    junk = impnlr (im2, oline_ptr, vout)

	    # For all input lines mapping to current output line, ...
	    do i = 1, num_ilines {
		# Get line from input image.
		iline_ptr = imggsr (im1, vin_s, vin_e, ndim)

		# Increment general section input vector between block bounds.
		do dim = 2, ndim {
		    if (vin_s[dim] < blkin_e[dim]) {
			vin_s[dim] = vin_s[dim] + 1
			vin_e[dim] = vin_s[dim]
			break
		    } else {
			vin_s[dim] = blkin_s[dim]
			vin_e[dim] = vin_s[dim]
		    }
		}

		# Accumulate line into block sum.  Keep track of no. of 
		# lines in sum so that we can compute block average later.

		call aaddr (Memr[iline_ptr], Memr[accum_ptr],
		    Memr[accum_ptr], IM_LEN(im1,1))
		nlines_in_sum = nlines_in_sum + 1
	    }

	    # We now have a templine of sums; average/sum into output buffer
	    # first the full blocks using a vop.
	    if (option == AVG)
		call abavr (Memr[accum_ptr], Memr[oline_ptr], nfull_blkx,
		    blkfac[1])
	    else
		call absur (Memr[accum_ptr], Memr[oline_ptr], nfull_blkx,
		    blkfac[1])

	    # Now average/sum the final partial block in x, if any.
	    if (nfull_blkx < num_oblks[1]) {
		sum = 0.0
		count = 0
		do i = nfull_blkx * blkfac[1] + 1, IM_LEN(im1,1) {
		    sum = sum + Memr[accum_ptr+i-1]
		    count = count + 1
		}
		if (option == AVG)
		    Memr[oline_ptr+num_oblks[1]-1] = sum / count
		else
		    Memr[oline_ptr+num_oblks[1]-1] = sum
	    }
		
	    # Block average into output line from the sum of all lines block
	    # averaged in X.
	    if (option == AVG)
		call adivkr (Memr[oline_ptr], real(nlines_in_sum),
		    Memr[oline_ptr], num_oblks[1])
	}
	
	call sfree (sp)
end



# BLKAVG -- Block average or sum on n-dimensional images.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  Remainder pixels at the end
# of a line, col, etc. are averaged correctly.

procedure blkavd (im1, im2, blkfac, option)

pointer	im1			# input image
pointer im2			# output image
int	blkfac[IM_MAXDIM]	# blocking factors
int	option			# block operation (average, sum, ...)

int	num_oblks[IM_MAXDIM], i, count, ndim, dim, nlines_in_sum, nfull_blkx
int	junk, num_ilines, num_olines, oline
long	blkin_s[IM_MAXDIM], blkin_e[IM_MAXDIM]
long	vin_s[IM_MAXDIM], vin_e[IM_MAXDIM], vout[IM_MAXDIM]
double	sum
pointer	sp, accum_ptr, iline_ptr, oline_ptr

int	blkcomp(), imggsd(), impnld() 
errchk	imggsd(), impnld()

begin
	call smark (sp)
	call salloc (accum_ptr, IM_LEN(im1, 1), TY_DOUBLE)

	# Initialize; input and output vectors, block counters.
	ndim = IM_NDIM(im1)
	nfull_blkx = IM_LEN(im1, 1) / blkfac[1]
	blkin_s[1] = long(1)
	blkin_e[1] = long(IM_LEN(im1, 1))
	vin_s[1]   = blkin_s[1]
	vin_e[1]   = blkin_e[1]

	do i = 1, ndim {
	    num_oblks[i] = (IM_LEN(im1,i) + blkfac[i] - 1) / blkfac[i]
	    IM_LEN(im2, i) = num_oblks[i]
	}
	num_olines = 1
	do i = 2, ndim
	    num_olines = num_olines * num_oblks[i]
	call amovkl (long(1), vout, IM_MAXDIM)

	# For each sequential output-image line, ...
	do oline = 1, num_olines {

	    call aclrd (Memd[accum_ptr], IM_LEN(im1, 1))
	    nlines_in_sum = 0

	    # Compute block vector; initialize dim>1 input vector.
	    num_ilines = blkcomp (im1, blkfac, vout, blkin_s, blkin_e,
		vin_s, vin_e)

	    # Output pointer; note impnl$t returns vout for NEXT output line.
	    junk = impnld (im2, oline_ptr, vout)

	    # For all input lines mapping to current output line, ...
	    do i = 1, num_ilines {
		# Get line from input image.
		iline_ptr = imggsd (im1, vin_s, vin_e, ndim)

		# Increment general section input vector between block bounds.
		do dim = 2, ndim {
		    if (vin_s[dim] < blkin_e[dim]) {
			vin_s[dim] = vin_s[dim] + 1
			vin_e[dim] = vin_s[dim]
			break
		    } else {
			vin_s[dim] = blkin_s[dim]
			vin_e[dim] = vin_s[dim]
		    }
		}

		# Accumulate line into block sum.  Keep track of no. of 
		# lines in sum so that we can compute block average later.

		call aaddd (Memd[iline_ptr], Memd[accum_ptr],
		    Memd[accum_ptr], IM_LEN(im1,1))
		nlines_in_sum = nlines_in_sum + 1
	    }

	    # We now have a templine of sums; average/sum into output buffer
	    # first the full blocks using a vop.
	    if (option == AVG)
		call abavd (Memd[accum_ptr], Memd[oline_ptr], nfull_blkx,
		    blkfac[1])
	    else
		call absud (Memd[accum_ptr], Memd[oline_ptr], nfull_blkx,
		    blkfac[1])

	    # Now average/sum the final partial block in x, if any.
	    if (nfull_blkx < num_oblks[1]) {
		sum = 0.0D0
		count = 0
		do i = nfull_blkx * blkfac[1] + 1, IM_LEN(im1,1) {
		    sum = sum + Memd[accum_ptr+i-1]
		    count = count + 1
		}
		if (option == AVG)
		    Memd[oline_ptr+num_oblks[1]-1] = sum / count
		else
		    Memd[oline_ptr+num_oblks[1]-1] = sum
	    }
		
	    # Block average into output line from the sum of all lines block
	    # averaged in X.
	    if (option == AVG)
		call adivkd (Memd[oline_ptr], double(nlines_in_sum),
		    Memd[oline_ptr], num_oblks[1])
	}
	
	call sfree (sp)
end


