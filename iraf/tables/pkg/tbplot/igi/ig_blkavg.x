# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>

define	AVG		1	# operation = arithmetic average
define	SUM		2	# operation = arithmetic sum
define 	IG_MAXDIM	2
define 	IG_BLKFAC	5

# change to (lrdx) in future

# BLKAVG -- Block average or sum on n-dimensional images.
# For example, block averaging by a factor of 2 means that pixels 1 and 2
# are averaged to produce the first output pixel, 3 and 4 are averaged to
# produce the second output pixel, and so on.  Remainder pixels at the end
# of a line, col, etc. are averaged correctly.
# 
# 	This version was modified to operate on VECTORS instead of
#		images, for use with IGI's ZSECTION command.
#		As a result, blkfac now has the following 'structure':
#			blkfac[1],blkfac[2] - x and y block size
#			blkfac[3]	    - number of dimensions in vector
#			blkfac[4],blkfac[5] - x and y size of vector
#		19 Nov 1997 - WJH
#	Fixed a bug dealing with odd number of lines left in input image 
#		to be block averaged.	WJH  9 Mar 98

procedure ig_blkavr (im1, im2, blkfac, option)

pointer	im1			# input image
pointer im2			# output image
int	blkfac[IG_MAXDIM+3]	# blocking factors
int	option			# block operation (average, sum, ...)

int	num_oblks[IG_MAXDIM], i, count, ndim, nlines_in_sum, nfull_blkx
int	num_ilines, num_olines, oline
long	blkin_s[IG_MAXDIM], blkin_e[IG_MAXDIM]
long	vin_s[IG_MAXDIM], vin_e[IG_MAXDIM], vout[IG_MAXDIM]
real	sum
int	inpix, inrows

pointer	sp, accum_ptr, iline_ptr, oline_ptr


int	ig_blkcomp()

begin

	# Initialize; input and output vectors, block counters.
	ndim 	   = blkfac[IG_MAXDIM+1]
	inpix 	   = blkfac[IG_MAXDIM+2]
	inrows 	   = blkfac[IG_MAXDIM+3]
	nfull_blkx = inpix / blkfac[1]
	blkin_s[1] = long(1)
	blkin_e[1] = long(inpix)
	vin_s[1]   = blkin_s[1]
	vin_e[1]   = blkin_e[1]

	call smark (sp)
	call salloc (accum_ptr, inpix, TY_REAL)
	call salloc (iline_ptr, inpix, TY_REAL)

	do i = 1, ndim {
	    num_oblks[i] = (blkfac[IG_MAXDIM+i+1] + blkfac[i] - 1) / blkfac[i]
		blkfac[IG_MAXDIM+1+i] = num_oblks[i]
	}

	num_olines = 1
	do i = 2, ndim
	    num_olines = num_olines * num_oblks[i]

	call amovkl (long(1), vout, IG_MAXDIM)

	# Initialize output pointer space...
	call salloc (oline_ptr, num_oblks[1], TY_REAL)

	# For each sequential output-image line, ...
	do oline = 1, num_olines {

	    call aclrr (Memr[accum_ptr], inpix)
	    nlines_in_sum = 0



	    # Compute block vector; initialize dim>1 input vector.
	    num_ilines = ig_blkcomp (blkfac, vout, blkin_s, blkin_e,
		vin_s, vin_e)

		# Make sure that number of input lines for last output lines
		# is set correctly.  In case the number of input
		# lines left for the last output line is less than 
		# the blocking factor, it only uses what is left of the input
		# lines.  For example, 1201x1201 image blocked by 2, will
		# have 600 lines blocked by 2 and the last line will only
		# have num_ilines = 1, not 2.
		# Apply this check for the last output line ONLY...
		#  WJH 9 Mar 98
		if ( (oline == num_olines) && ((oline*num_ilines) > inrows) ) {
			num_ilines = inrows - (oline-1)*blkfac[2]
		#	call eprintf("BLKAVG: oline = %d, num_ilines = %d\n")
		#		call pargi(oline)
		#		call pargi(num_ilines)
		}

	    # Output pointer; note impnl$t returns vout for NEXT output line.
	    # For all input lines mapping to current output line, ...
	    do i = 1, num_ilines {
		# Get line from input image.
		call amovr(Memr[im1+vin_s[1]-1], Memr[iline_ptr], inpix)

		# Increment general section input vector between block bounds.
		vin_s[1] = vin_s[1]+blkin_e[1]

		# Accumulate line into block sum.  Keep track of no. of 
		# lines in sum so that we can compute block average later.

		call aaddr (Memr[iline_ptr], Memr[accum_ptr],
		    Memr[accum_ptr], blkin_e[1])
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
		do i = nfull_blkx * blkfac[1] + 1, inpix {
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

	    # Copy output line to output array...
	    call amovr(Memr[oline_ptr], Memr[im2-1+vout[1]], num_oblks[1]) 		
	    # Increment array counters for output array to point 
	    # to next output line
	    vout[1]= vout[1] + num_oblks[1]

	}
	
	call sfree (sp)
end



# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

#include	<imhdr.h>

# BLKCOMP -- compute starting and ending input vectors for blocks in each
# dimension.  Initialize input-line vectors to block vectors.  Return total
# number of input lines mapping to current output line.
#
#	This code was revised to operate on VECTORS instead of IMAGES
#		and to use the expanded version of 'blkfac'.
#	19 Nov 1997 - WJH
#
int procedure ig_blkcomp (blkfac, vout, blkin_s, blkin_e,
		vin_s, vin_e)

#pointer	im1		# pointer to input image descriptor
int	blkfac[IG_BLKFAC]	# blocking factors for each dimension
long	vout[ARB]		# output image vectors for each dimension
long	blkin_s[ARB]	# index of starting block for each dimension
long	blkin_e[ARB]	# index of ending block for each dimension
long	vin_s[ARB]	# initial starting input vector
long	vin_e[ARB]	# initial ending input vector

int	num_ilines, dim

begin
	num_ilines = 1

	# Compute starting and ending indices of input image pixels in each
	# dimension mapping to current output line.

	do dim = 2, blkfac[3] {
	    blkin_s[dim] = long(1 + (vout[dim] - 1) * blkfac[dim])
	    blkin_e[dim] = long(min (blkfac[3+dim], vout[dim] * blkfac[dim]))
	    vin_s[dim]   = blkin_s[dim]
	    vin_e[dim]   = blkin_s[dim]
	    num_ilines = num_ilines * (blkin_e[dim] - blkin_s[dim] + 1)
	}
	
	return (num_ilines)
end

