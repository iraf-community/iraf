# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <evvexpr.h>
include "export.h"

define	DEBUG 	false


.help ex_zscale
.nf ___________________________________________________________________________
EX_ZSCALE -- Compute the optimal Z1, Z2 (range of greyscale values to be
displayed) of an expression.  For efficiency a statistical subsample of the
expression is used.  The pixel sample evenly subsamples the expression in x 
and y.  The entire expression is used if the number of pixels in the expression 
is smaller than the desired sample.

The sample is accumulated in a buffer and sorted by greyscale value.
The median value is the central value of the sorted array.  The slope of a
straight line fitted to the sorted sample is a measure of the standard
deviation of the sample about the median value.  Our algorithm is to sort
the sample and perform an iterative fit of a straight line to the sample,
using pixel rejection to omit gross deviants near the endpoints.  The fitted
straight line is the transfer function used to map image Z into display Z.
If more than half the pixels are rejected the full range is used.  The slope
of the fitted line is divided by the user-supplied contrast factor and the
final Z1 and Z2 are computed, taking the origin of the fitted line at the
median value.
.endhelp ______________________________________________________________________

define	MIN_NPIXELS	5		# smallest permissible sample
define	MAX_REJECT	0.5		# max frac. of pixels to be rejected
define	GOOD_PIXEL	0		# use pixel in fit
define	BAD_PIXEL	1		# ignore pixel in all computations
define	REJECT_PIXEL	2		# reject pixel after a bit
define	KREJ		2.5		# k-sigma pixel rejection factor
define	MAX_ITERATIONS	5		# maximum number of fitline iterations


# EX_PATCH_ZSCALE - Rather than compute the optimal zscale values for each
# line in the expression we'll go through the expression string and compute
# the values here.  The expression string is modified with the values so that
# when evaluated they are seen as arguments to the function.

procedure ex_patch_zscale (ex, expnum)

pointer	ex				#i task struct pointer
int	expnum				#i expression number to fix

pointer	sp, exp, func
int	ip, pp

bool	streq()

begin
	call smark (sp)
	call salloc (exp, SZ_EXPSTR, TY_CHAR)
	call salloc (func, SZ_FNAME, TY_CHAR)
	call aclrc(Memc[exp], SZ_EXPSTR)
	call aclrc(Memc[func], SZ_FNAME)

        # Copy the final expression string to the output buffer.
        call strcpy (O_EXPR(ex,expnum), Memc[exp], SZ_EXPSTR)

        # Now fix up any zscale functions calls embedded in the expression.
        ip = 0
        repeat {
            # Skip ahead to a possible zscale()/mzscale() call.
            while (Memc[exp+ip] != 'z' && Memc[exp+ip] != EOS)
                ip = ip + 1
            if (Memc[exp+ip] == EOS)
                break

            # Get the function name.
            pp = 0
            call aclrc (Memc[func], SZ_FNAME)
            while (Memc[exp+ip] != '(' && Memc[exp+ip] != EOS) {
                Memc[func+pp] = Memc[exp+ip]
                ip = ip + 1
                pp = pp + 1
            }
            Memc[func+pp+1] = EOS 
            if (Memc[exp+ip] == EOS)
                break

            if (DEBUG) { call eprintf("\tfunc=`%s'\n");call pargstr(Memc[func])}

            # Update pointer into string past '('.
            ip = ip + 1

            if (streq(Memc[func],"zscale") || streq(Memc[func],"zscalem")) {
                iferr (call ex_edit_zscale (ex, Memc[exp], ip+1))
                    call erract (EA_FATAL)
                ip = ip + 1
            }
        }

        # Copy the final expression string to the output buffer.
        call strcpy (Memc[exp], O_EXPR(ex,expnum), SZ_EXPSTR)

	call sfree (sp)
end


# EX_EDIT_ZSCALE - Process the ZSCALE special function.  This function requires
# preprocessing in the event the user didn't supply a z1/z2 value.  What 
# we'll do here is pre-compute those values and patch up the expression 
# string.  Otherwise we'll make sure the rest of the arguments are legal.

procedure ex_edit_zscale (ex, expstr, pp)

pointer ex                              #i task struct pointer
char    expstr[ARB]                     #i expression string
int     pp                              #i position pointer

pointer sp, arg, arg2, exp, buf
pointer exptr, exptr2, ep
char    ch
int     ip, op, tp, tp2, plev
real    z1, z2

pointer	ex_evaluate()

begin
        call smark (sp)
        call salloc (arg, SZ_EXPSTR, TY_CHAR); call aclrc (Memc[arg], SZ_EXPSTR)
        call salloc (arg2, SZ_EXPSTR,TY_CHAR); call aclrc (Memc[arg2],SZ_EXPSTR)
        call salloc (exp, SZ_EXPSTR, TY_CHAR); call aclrc (Memc[exp], SZ_EXPSTR)
        call salloc (buf, SZ_EXPSTR, TY_CHAR); call aclrc (Memc[buf], SZ_EXPSTR)

        if (DEBUG) { call eprintf("\t\texp=`%s'\n");call pargstr(expstr)}

        # Gather the expression argument.
        ip = pp
        op = 0
        plev = 0
        repeat {
            ch = expstr[ip]
            if (ch == '(')   plev = plev + 1
            if (ch == ')')   plev = plev - 1
            Memc[arg+op] = ch
            if ((ch == ',' && plev == 0) || (ch == ')' && plev < 0))
                break
            ip = ip + 1
            op = op + 1
        }
        Memc[arg+op] = EOS
        tp = ip - 1
        tp2 = tp
        if (DEBUG) {call eprintf("\t\targ = `%s'\n");call pargstr(Memc[arg])}

	# Gather the mask argument.
	if (expstr[pp-2] == 'm' && ch == ',') {
	    ip = ip + 1
	    op = 0
	    plev = 0
	    repeat {
		ch = expstr[ip]
		if (ch == '(')   plev = plev + 1
		if (ch == ')')   plev = plev - 1
		Memc[arg2+op] = ch
		if ((ch == ',' && plev == 0) || (ch == ')' && plev < 0))
		    break
		ip = ip + 1
		op = op + 1
	    }
	    Memc[arg2+op] = EOS
	    tp2 = ip - 1
	    if (DEBUG) {
	        call eprintf("\t\targ2 = `%s'\n")
		call pargstr(Memc[arg2])
	    }
	}

        if (ch == ',') {
            # We have more arguments, assume they're okay and return.
            call sfree (sp)
            return

        } else if (ch == ')') {
            # This is the end of the zscale function, so compute the optimal
            # z1/z2 values for the given expression. First, dummy up an out-
            # bands pointer.

            call ex_alloc_outbands (exptr)
            call strcpy (Memc[arg], Memc[OB_EXPSTR(exptr)], SZ_EXPSTR)

	    # Get the size of the expression.
	    call ex_getpix (ex, 1)
	    ep = ex_evaluate (ex, Memc[OB_EXPSTR(exptr)])
            OB_WIDTH(exptr) = O_LEN(ep)
	    call evvfree (ep)
	    if (OB_WIDTH(exptr) == 0)
                OB_HEIGHT(exptr) = 1
	    else
                OB_HEIGHT(exptr) = EX_NLINES(ex)

	    # Setup the mask expression if needed.
	    if (Memc[arg2] != EOS) {
		call ex_alloc_outbands (exptr2)
		call strcpy (Memc[arg2], Memc[OB_EXPSTR(exptr2)], SZ_EXPSTR)

		# Get the size of the expression.
		ep = ex_evaluate (ex, Memc[OB_EXPSTR(exptr2)])
		OB_WIDTH(exptr2) = O_LEN(ep)
		call evvfree (ep)
		if (OB_WIDTH(exptr2) == 0)
		    OB_HEIGHT(exptr2) = 1
		else
		    OB_HEIGHT(exptr2) = EX_NLINES(ex)
		if (OB_WIDTH(exptr2) != OB_WIDTH(exptr) ||
		    OB_WIDTH(exptr2) != OB_WIDTH(exptr))
		    call error (1, "Sizes of zscalem arguments not the same.")
	    } else
	        exptr2 = NULL

	    if (EX_VERBOSE(ex) == YES) {
	    	call printf ("Computing zscale values...")
	    	call flush (STDOUT)
	    }

            call ex_zscale (ex, exptr, exptr2, z1, z2,
	        CONTRAST, SAMPLE_SIZE, SAMP_LEN)
            call ex_free_outbands (exptr)
	    if (exptr2 != NULL)
		call ex_free_outbands (exptr2)

            if (DEBUG) {call eprintf("\t\t\tz1=%g z2=%g\n")
                call pargr(z1) ; call pargr (z2) }

            # Now patch up the expression string to insert the computed values.
	    if (expstr[pp-2] == 'm') {
		call strcpy (expstr, Memc[exp], pp-3)
		call strcat (expstr[pp-1], Memc[exp], tp-1)
	    } else
		call strcpy (expstr, Memc[exp], tp)
            call sprintf (Memc[buf], SZ_EXPSTR, ",%g,%g,256")
                call pargr (z1)
                call pargr (z2)
            call strcat (Memc[buf], Memc[exp], SZ_EXPSTR)
            call strcat (expstr[tp2+1], Memc[exp], SZ_EXPSTR)

	    # Print the computed values to the screen.
	    if (EX_VERBOSE(ex) == YES) {
                call printf ("z1=%g z2=%g\n")
                    call pargr (z1)
                    call pargr (z2)
	    }
        }

        # Copy fixed-up expression to input buffer.
        call aclrc (expstr, SZ_EXPSTR)
        call strcpy (Memc[exp], expstr, SZ_EXPSTR)

        if (DEBUG){call eprintf("\t\tnew expr=`%s'\n");call pargstr(expstr)}

        call sfree (sp)
end


# EX_ZSCALE -- Sample the expression and compute Z1 and Z2.

procedure ex_zscale (ex, exptr, exptr2, z1, z2, contrast, optimal_sample_size, 
    len_stdline)

pointer	ex				# task struct pointer
pointer	exptr				# expression struct pointer
pointer	exptr2				# expression struct pointer (mask)
real	z1, z2				# output min and max greyscale values
real	contrast			# adj. to slope of transfer function
int	optimal_sample_size		# desired number of pixels in sample
int	len_stdline			# optimal number of pixels per line

int	npix, minpix, ngoodpix, center_pixel, ngrow
real	zmin, zmax, median
real	zstart, zslope
pointer	sample, left
int	ex_sample_expr(), ex_fit_line()

begin
	# Subsample the expression.
	npix = ex_sample_expr (ex, exptr, exptr2, sample, optimal_sample_size, 
	    len_stdline)
	center_pixel = max (1, (npix + 1) / 2)

	# Sort the sample, compute the minimum, maximum, and median pixel
	# values.

	call asrtr (Memr[sample], Memr[sample], npix)
	zmin = Memr[sample]
	zmax = Memr[sample+npix-1]

	# The median value is the average of the two central values if there 
	# are an even number of pixels in the sample.

	left = sample + center_pixel - 1
	if (mod (npix, 2) == 1 || center_pixel >= npix)
	    median = Memr[left]
	else
	    median = (Memr[left] + Memr[left+1]) / 2

	# Fit a line to the sorted sample vector.  If more than half of the
	# pixels in the sample are rejected give up and return the full range.
	# If the user-supplied contrast factor is not 1.0 adjust the scale
	# accordingly and compute Z1 and Z2, the y intercepts at indices 1 and
	# npix.

	minpix = max (MIN_NPIXELS, int (npix * MAX_REJECT))
	ngrow = max (1, nint (npix * .01))
	ngoodpix = ex_fit_line (Memr[sample], npix, zstart, zslope,
	    KREJ, ngrow, MAX_ITERATIONS)

	if (ngoodpix < minpix) {
	    z1 = zmin
	    z2 = zmax
	} else {
	    if (contrast > 0)
		zslope = zslope / contrast
	    z1 = max (zmin, median - (center_pixel - 1) * zslope)
	    z2 = min (zmax, median + (npix - center_pixel) * zslope)
	}

	call mfree (sample, TY_REAL)
end


# EX_SAMPLE_EXPR -- Extract an evenly gridded subsample of the pixels from
# a possibly two-dimensional expression into a one-dimensional vector.

int procedure ex_sample_expr (ex, exptr, exptr2, sample, optimal_sample_size, 
    len_stdline)

pointer	ex				# task struct pointer
pointer	exptr				# expression struct pointer
pointer	exptr2				# expression struct pointer (mask)
pointer	sample				# output vector containing the sample
int	optimal_sample_size		# desired number of pixels in sample
int	len_stdline			# optimal number of pixels per line

pointer	op, ep, out, bpm
int	ncols, nlines, col_step, line_step, maxpix, line
int	opt_npix_per_line, npix_per_line, nsubsample
int	opt_nlines_in_sample, min_nlines_in_sample, max_nlines_in_sample

pointer	ex_evaluate()

begin
	ncols  = OB_WIDTH(exptr)
	nlines = OB_HEIGHT(exptr)

	# Compute the number of pixels each line will contribute to the sample,
	# and the subsampling step size for a line.  The sampling grid must
	# span the whole line on a uniform grid.

	opt_npix_per_line = max (1, min (ncols, len_stdline))
	col_step = max (1, (ncols + opt_npix_per_line-1) / opt_npix_per_line)
	npix_per_line = max (1, (ncols + col_step-1) / col_step)

	# Compute the number of lines to sample and the spacing between lines.
	# We must ensure that the image is adequately sampled despite its
	# size, hence there is a lower limit on the number of lines in the
	# sample.  We also want to minimize the number of lines accessed when
	# accessing a large image, because each disk seek and read is expensive.
	# The number of lines extracted will be roughly the sample size divided
	# by len_stdline, possibly more if the lines are very short.

	min_nlines_in_sample = max (1, optimal_sample_size / len_stdline)
	opt_nlines_in_sample = max(min_nlines_in_sample, min(nlines,
	    (optimal_sample_size + npix_per_line-1) / npix_per_line))
	line_step = max (1, nlines / (opt_nlines_in_sample))
	max_nlines_in_sample = (nlines + line_step-1) / line_step

	# Allocate space for the output vector.  Buffer must be freed by our
	# caller.

	maxpix = npix_per_line * max_nlines_in_sample
	call malloc (sample, maxpix, TY_REAL)

	# Extract the vector.
	op = sample
	call malloc (out, ncols, TY_REAL)
	if (exptr2 != NULL)
	    call malloc (bpm, ncols, TY_INT)
	do line = (line_step + 1) / 2, nlines, line_step {

	    # Evaluate the expression at the current line.
	    call ex_getpix (ex, line)
	    ep = ex_evaluate (ex, Memc[OB_EXPSTR(exptr)])
	    switch (O_TYPE(ep)) {
	    case TY_CHAR:
	        call achtcr (Memc[O_VALP(ep)], Memr[out], O_LEN(ep))
	    case TY_SHORT:
	        call achtsr (Mems[O_VALP(ep)], Memr[out], O_LEN(ep))
	    case TY_INT:
	        call achtir (Memi[O_VALP(ep)], Memr[out], O_LEN(ep))
	    case TY_LONG:
	        call achtlr (Meml[O_VALP(ep)], Memr[out], O_LEN(ep))
	    case TY_REAL:
	        call amovr (Memr[O_VALP(ep)], Memr[out], O_LEN(ep))
	    case TY_DOUBLE:
	        call achtdr (Memd[O_VALP(ep)], Memr[out], O_LEN(ep))
	    default:
		call error (0, "Unknown expression type in zscale/zscalem().")
	    }
	    call evvfree (ep)
	    if (exptr2 != NULL) {
		ep = ex_evaluate (ex, Memc[OB_EXPSTR(exptr2)])
		switch (O_TYPE(ep)) {
		case TY_BOOL:
		    call amovi (Memi[O_VALP(ep)], Memi[bpm], O_LEN(ep))
		default:
		    call error (0,
		        "Selection expression must be boolean in zscalem().")
		}
		call ex_subsample1 (Memr[out], Memi[bpm], Memr[op], O_LEN(ep),
		    npix_per_line, col_step, nsubsample)
		call evvfree (ep)
	    } else
		call ex_subsample (Memr[out], Memr[op], O_LEN(ep),
		    npix_per_line, col_step, nsubsample)

	    op = op + nsubsample
	    if (op - sample + npix_per_line > maxpix)
		break
	}
	call mfree (out, TY_REAL)

	return (op - sample)
end


# EX_SUBSAMPLE -- Subsample an image line.  Extract the first pixel and
# every "step"th pixel thereafter for a total of npix pixels.

procedure ex_subsample (a, b, n, npix, step, nsubsample)

real	a[n]
real	b[npix]
int	n
int	npix, step, nsubsample
int	ip, i

begin
	nsubsample = npix
	if (step <= 1)
	    call amovr (a, b, npix)
	else {
	    ip = 1
	    do i = 1, npix {
		b[i] = a[ip]
		ip = ip + step
	    }
	}
end


# EX_SUBSAMPLE1 -- Subsample an image line.  Extract the first pixel and
# every "step"th pixel thereafter for a total of npix pixels.
#
# Check for mask values and exclude them from the sample.  In case a
# subsampled line has fewer than 75% good pixels then increment the starting
# pixel and step through again.  Return the number of good pixels.

procedure ex_subsample1 (a, c, b, n, npix, step, nsubsample)

real	a[ARB]
int	c[ARB]
real	b[npix]
int	n
int	npix, step, nsubsample
int	i, j

begin
	nsubsample = 0
	if (step <= 1) {
	    do i = 1, n {
	        if (c[i] == 0)
		    next
		nsubsample = nsubsample + 1
		b[nsubsample] = a[i]
		if (nsubsample == npix)
		    break
	    }
	} else {
	    do j = 1, step-1 {
		do i = j, n, step {
		    if (c[i] == 0)
		        next
		    nsubsample = nsubsample + 1
		    b[nsubsample] = a[i]
		    if (nsubsample == npix)
		        break
		}
		if (nsubsample >= 0.75 * npix)
		    break
	    }
	}
end


# EX_FIT_LINE -- Fit a straight line to a data array of type real.  This is
# an iterative fitting algorithm, wherein points further than ksigma from the
# current fit are excluded from the next fit.  Convergence occurs when the
# next iteration does not decrease the number of pixels in the fit, or when
# there are no pixels left.  The number of pixels left after pixel rejection
# is returned as the function value.

int procedure ex_fit_line (data, npix, zstart, zslope, krej, ngrow, maxiter)

real	data[npix]			#i data to be fitted
int	npix				#i number of pixels before rejection
real	zstart				#o Z-value of pixel data[1]
real	zslope				#o dz/pixel
real	krej				#i k-sigma pixel rejection factor
int	ngrow				#i number of pixels of growing
int	maxiter				#i max iterations

int	i, ngoodpix, last_ngoodpix, minpix, niter
real	xscale, z0, dz, x, z, mean, sigma, threshold
double	sumxsqr, sumxz, sumz, sumx, rowrat
pointer	sp, flat, badpix, normx
int	ex_reject_pixels(), ex_compute_sigma()

begin
	if (npix <= 0)
	    return (0)
	else if (npix == 1) {
	    zstart = data[1]
	    zslope = 0.0
	    return (1)
	} else
	    xscale = 2.0 / (npix - 1)

	# Allocate a buffer for data minus fitted curve, another for the
	# normalized X values, and another to flag rejected pixels.

	call smark (sp)
	call salloc (flat, npix, TY_REAL)
	call salloc (normx, npix, TY_REAL)
	call salloc (badpix, npix, TY_SHORT)
	call aclrs (Mems[badpix], npix)

	# Compute normalized X vector.  The data X values [1:npix] are
	# normalized to the range [-1:1].  This diagonalizes the lsq matrix
	# and reduces its condition number.

	do i = 0, npix - 1
	    Memr[normx+i] = i * xscale - 1.0

	# Fit a line with no pixel rejection.  Accumulate the elements of the
	# matrix and data vector.  The matrix M is diagonal with
	# M[1,1] = sum x**2 and M[2,2] = ngoodpix.  The data vector is
	# DV[1] = sum (data[i] * x[i]) and DV[2] = sum (data[i]).

	sumxsqr = 0
	sumxz = 0
	sumx = 0
	sumz = 0

	do i = 1, npix {
	    x = Memr[normx+i-1]
	    z = data[i]
	    sumxsqr = sumxsqr + (x ** 2)
	    sumxz   = sumxz + z * x
	    sumz    = sumz + z
	}

	# Solve for the coefficients of the fitted line.
	z0 = sumz / npix
	dz = sumxz / sumxsqr

	# Iterate, fitting a new line in each iteration.  Compute the flattened
	# data vector and the sigma of the flat vector.  Compute the lower and
	# upper k-sigma pixel rejection thresholds.  Run down the flat array
	# and detect pixels to be rejected from the fit.  Reject pixels from
	# the fit by subtracting their contributions from the matrix sums and
	# marking the pixel as rejected.

	ngoodpix = npix
	minpix = max (MIN_NPIXELS, int (npix * MAX_REJECT))

	for (niter=1;  niter <= maxiter;  niter=niter+1) {
	    last_ngoodpix = ngoodpix

	    # Subtract the fitted line from the data array.
	    call ex_flatten_data (data, Memr[flat], Memr[normx], npix, z0, dz)

	    # Compute the k-sigma rejection threshold.  In principle this
	    # could be more efficiently computed using the matrix sums
	    # accumulated when the line was fitted, but there are problems with
	    # numerical stability with that approach.

	    ngoodpix = ex_compute_sigma (Memr[flat], Mems[badpix], npix,
		mean, sigma)
	    threshold = sigma * krej

	    # Detect and reject pixels further than ksigma from the fitted
	    # line.
	    ngoodpix = ex_reject_pixels (data, Memr[flat], Memr[normx],
		Mems[badpix], npix, sumxsqr, sumxz, sumx, sumz, threshold,
		ngrow)

	    # Solve for the coefficients of the fitted line.  Note that after
	    # pixel rejection the sum of the X values need no longer be zero.

	    if (ngoodpix > 0) {
		rowrat = sumx / sumxsqr
		z0 = (sumz - rowrat * sumxz) / (ngoodpix - rowrat * sumx)
		dz = (sumxz - z0 * sumx) / sumxsqr
	    }

	    if (ngoodpix >= last_ngoodpix || ngoodpix < minpix)
		break
	}

	# Transform the line coefficients back to the X range [1:npix].
	zstart = z0 - dz
	zslope = dz * xscale

	call sfree (sp)
	return (ngoodpix)
end


# EX_FLATTEN_DATA -- Compute and subtract the fitted line from the data array,
# returned the flattened data in FLAT.

procedure ex_flatten_data (data, flat, x, npix, z0, dz)

real	data[npix]			# raw data array
real	flat[npix]			# flattened data  (output)
real	x[npix]				# x value of each pixel
int	npix				# number of pixels
real	z0, dz				# z-intercept, dz/dx of fitted line
int	i

begin
	do i = 1, npix
	    flat[i] = data[i] - (x[i] * dz + z0)
end


# EX_COMPUTE_SIGMA -- Compute the root mean square deviation from the
# mean of a flattened array.  Ignore rejected pixels.

int procedure ex_compute_sigma (a, badpix, npix, mean, sigma)

real	a[npix]				# flattened data array
short	badpix[npix]			# bad pixel flags (!= 0 if bad pixel)
int	npix
real	mean, sigma			# (output)

real	pixval
int	i, ngoodpix
double	sum, sumsq, temp

begin
	sum = 0
	sumsq = 0
	ngoodpix = 0

	# Accumulate sum and sum of squares.
	do i = 1, npix
	    if (badpix[i] == GOOD_PIXEL) {
		pixval = a[i]
		ngoodpix = ngoodpix + 1
		sum = sum + pixval
		sumsq = sumsq + pixval ** 2
	    }

	# Compute mean and sigma.
	switch (ngoodpix) {
	case 0:
	    mean = INDEF
	    sigma = INDEF
	case 1:
	    mean = sum
	    sigma = INDEF
	default:
	    mean = sum / ngoodpix
	    temp = sumsq / (ngoodpix - 1) - sum**2 / (ngoodpix * (ngoodpix - 1))
	    if (temp < 0)		# possible with roundoff error
		sigma = 0.0
	    else
		sigma = sqrt (temp)
	}

	return (ngoodpix)
end


# EX_REJECT_PIXELS -- Detect and reject pixels more than "threshold" greyscale
# units from the fitted line.  The residuals about the fitted line are given
# by the "flat" array, while the raw data is in "data".  Each time a pixel
# is rejected subtract its contributions from the matrix sums and flag the
# pixel as rejected.  When a pixel is rejected reject its neighbors out to
# a specified radius as well.  This speeds up convergence considerably and
# produces a more stringent rejection criteria which takes advantage of the
# fact that bad pixels tend to be clumped.  The number of pixels left in the
# fit is returned as the function value.

int procedure ex_reject_pixels (data, flat, normx, badpix, npix,
				 sumxsqr, sumxz, sumx, sumz, threshold, ngrow)

real	data[npix]			# raw data array
real	flat[npix]			# flattened data array
real	normx[npix]			# normalized x values of pixels
short	badpix[npix]			# bad pixel flags (!= 0 if bad pixel)
int	npix
double	sumxsqr,sumxz,sumx,sumz		# matrix sums
real	threshold			# threshold for pixel rejection
int	ngrow				# number of pixels of growing

int	ngoodpix, i, j
real	residual, lcut, hcut
double	x, z

begin
	ngoodpix = npix
	lcut = -threshold
	hcut = threshold

	do i = 1, npix
	    if (badpix[i] == BAD_PIXEL)
		ngoodpix = ngoodpix - 1
	    else {
		residual = flat[i]
		if (residual < lcut || residual > hcut) {
		    # Reject the pixel and its neighbors out to the growing
		    # radius.  We must be careful how we do this to avoid
		    # directional effects.  Do not turn off thresholding on
		    # pixels in the forward direction; mark them for rejection
		    # but do not reject until they have been thresholded.
		    # If this is not done growing will not be symmetric.

		    do j = max(1,i-ngrow), min(npix,i+ngrow) {
			if (badpix[j] != BAD_PIXEL) {
			    if (j <= i) {
				x = normx[j]
				z = data[j]
				sumxsqr = sumxsqr - (x ** 2)
				sumxz = sumxz - z * x
				sumx = sumx - x
				sumz = sumz - z
				badpix[j] = BAD_PIXEL
				ngoodpix = ngoodpix - 1
			    } else
				badpix[j] = REJECT_PIXEL
			}
		    }
		}
	    }

	return (ngoodpix)
end
