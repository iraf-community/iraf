include <mach.h>
include <imhdr.h>
include <imset.h>
include "vt.h"

define	WINDEXC		800.	# constant for weight index calculation
define	WINDEX6TH	75.	# constant for weight index calculation
define	LIMBR		.97	# Limb closeness rejection coefficient.
define	SOWTHRESH	20.	# Sum of weights threshold.
define	SZ_WT10830	1024	# size of weight table for destreak
define	FCORRECT	.9375	# fractional term for lattitude correction

# Structure for least square fitting parameters.

define	VT_LENSQSTRUCT	8		# Length of VT sq structure

# Pointers
define	VT_SQ1P		Memi[$1]	# pointers to arrays for least
define	VT_SQ1Q1P	Memi[$1+1]	# squares fit
define	VT_SQ1Q2P	Memi[$1+2]	#
define	VT_SQ1Q3P	Memi[$1+3]	#
define	VT_SQ2Q2P	Memi[$1+4]	#
define	VT_SQ2Q3P	Memi[$1+5]	#
define	VT_SQ3Q3P	Memi[$1+6]	#
define	VT_NUMDATAP	Memi[$1+7]	#

# Macro definitions
define	VT_SQ1		Memr[VT_SQ1P($1)+$2-1]
define	VT_SQ1Q1	Memr[VT_SQ1Q1P($1)+$2-1]
define	VT_SQ1Q2	Memr[VT_SQ1Q2P($1)+$2-1]
define	VT_SQ1Q3	Memr[VT_SQ1Q3P($1)+$2-1]
define	VT_SQ2Q2	Memr[VT_SQ2Q2P($1)+$2-1]
define	VT_SQ2Q3	Memr[VT_SQ2Q3P($1)+$2-1]
define	VT_SQ3Q3	Memr[VT_SQ3Q3P($1)+$2-1]
define	VT_NUMDATA	Memi[VT_NUMDATAP($1)+$2-1]


# DESTREAK -- Destreak 10830 grams.  On a 10830 full disk image.  For
# each diode, based on the data from that diode calculate coefficients for
# a best fit function and subtract this function from the data.  Apply a
# spatial filter to the resulting image.

procedure t_destreak()

char	heimage[SZ_FNAME]		# input image
char	heout[SZ_FNAME]	    		# output image
char	tempim[SZ_FNAME]	    	# temporary image
bool	verbose				# verbose flag
real	el[LEN_ELSTRUCT]		# ellipse parameters data structure
int	threshold			# squibby brightness threshold

int	diode, npix, i, line
int	kxdim, kydim
real	kernel[3,9]
pointer	weights
pointer	lgp1, lpp
pointer	heim, heoutp
pointer	a, c
pointer	sqs, sp

bool	clgetb()
int	clgeti()
real	imgetr()
pointer	imgl2s(), impl2s(), immap()
errchk	immap, imgl2s, impl2s, imfilt

begin
	call smark (sp)
	call salloc (sqs, VT_LENSQSTRUCT, TY_STRUCT)
	call salloc (VT_SQ1P(sqs), DIM_VTFD, TY_REAL)
	call salloc (VT_SQ1Q1P(sqs), DIM_VTFD, TY_REAL)
	call salloc (VT_SQ1Q2P(sqs), DIM_VTFD, TY_REAL)
	call salloc (VT_SQ1Q3P(sqs), DIM_VTFD, TY_REAL)
	call salloc (VT_SQ2Q2P(sqs), DIM_VTFD, TY_REAL)
	call salloc (VT_SQ2Q3P(sqs), DIM_VTFD, TY_REAL)
	call salloc (VT_SQ3Q3P(sqs), DIM_VTFD, TY_REAL)
	call salloc (VT_NUMDATAP(sqs), DIM_VTFD, TY_INT)
	call salloc (a, DIM_VTFD, TY_REAL)
	call salloc (c, DIM_VTFD, TY_REAL)
	call salloc (weights, SZ_WT10830, TY_REAL)
	
	# Get parameters from the cl.

	call clgstr ("heimage", heimage, SZ_FNAME)
	call clgstr ("heout", heout, SZ_FNAME)
	call clgstr ("tempim", tempim, SZ_FNAME)
	verbose = clgetb ("verbose")
	threshold = clgeti("threshold")

	# Open the images
	heim = immap (heimage, READ_WRITE, 0)
	heoutp = immap (tempim, NEW_COPY, heim)

	# Ellipse parameters.
	E_XCENTER[el] = imgetr (heim, "E_XCEN")
	E_YCENTER[el] = imgetr (heim, "E_YCEN")
	E_XSEMIDIAMETER[el] = imgetr (heim, "E_XSMD")
	E_YSEMIDIAMETER[el] = imgetr (heim, "E_XSMD")

	# Generate the weight array.
	do i = 1, SZ_WT10830
	    Memr[weights+i-1] = exp((real(i) - WINDEXC)/WINDEX6TH)

	# Set the sq arrays and the a and c arrays to zero.
	call aclrr (VT_SQ1(sqs,1), DIM_VTFD)
	call aclrr (VT_SQ1Q1(sqs,1), DIM_VTFD)
	call aclrr (VT_SQ1Q2(sqs,1), DIM_VTFD)
	call aclrr (VT_SQ1Q3(sqs,1), DIM_VTFD)
	call aclrr (VT_SQ2Q2(sqs,1), DIM_VTFD)
	call aclrr (VT_SQ2Q3(sqs,1), DIM_VTFD)
	call aclrr (VT_SQ3Q3(sqs,1), DIM_VTFD)
	call aclri (VT_NUMDATA(sqs,1), DIM_VTFD)
	call aclrr (Memr[a], DIM_VTFD)
	call aclrr (Memr[c], DIM_VTFD)

	# for all lines in the image {
	#    calculate which diode this line corresponds to
	#    get the line from the image
	#    sum the q's for this line
	# }

	npix = IM_LEN(heim,1)
	do line = 1, DIM_VTFD {
	    diode = mod((line - 1), SWTH_HIGH) + 1
	    lgp1 = imgl2s (heim, line)
	    call qsumq (Mems[lgp1], npix, el, threshold, weights, LIMBR,
		line, sqs)
	}

	# Fit the function to the data for each line. 
	do line = 1, DIM_VTFD {
	    call qfitdiode(sqs, line, npix, Memr[a+line-1], Memr[c+line-1],
		threshold, verbose)
	    if (verbose) {
		call printf ("line = %d\n")
		    call pargi (line)
		call flush (STDOUT)
	    }
	}

	# For each image line subtract the function from the data.
	do line = 1, DIM_VTFD {
	    diode = mod((line - 1), SWTH_HIGH) + 1
	    lgp1 = imgl2s (heim, line)
	    lpp  = impl2s (heoutp, line)
	    call qrfunct(Mems[lgp1], Mems[lpp], npix, el, threshold,
		Memr[a+line-1], Memr[c+line-1], LIMBR, line)
	}

	# Switch images
	call imunmap (heim)
	call imunmap (heoutp)
	heim = immap (tempim, READ_WRITE, 0)
	heoutp = immap (heout, NEW_COPY, heim)

	# Call the spacial filter program.

	# First we have to load up the filter kernel
	kxdim = 3
	kydim = 9
	kernel[1,1] = .017857
	kernel[1,2] = .017857
	kernel[1,3] = .035714
	kernel[1,4] = .035714
	kernel[1,5] = .035714
	kernel[1,6] = .035714
	kernel[1,7] = .035714
	kernel[1,8] = .017857
	kernel[1,9] = .017857
	kernel[2,1] = .017857
	kernel[2,2] = .053571
	kernel[2,3] = .071428
	kernel[2,4] = .071428
	kernel[2,5] = .071428
	kernel[2,6] = .071428
	kernel[2,7] = .071428
	kernel[2,8] = .053571
	kernel[2,9] = .017857
	kernel[3,1] = .017857
	kernel[3,2] = .017857
	kernel[3,3] = .035714
	kernel[3,4] = .035714
	kernel[3,5] = .035714
	kernel[3,6] = .035714
	kernel[3,7] = .035714
	kernel[3,8] = .017857
	kernel[3,9] = .017857

	if (verbose) {
	    call printf ("filtering\n")
	    call flush(STDOUT)
	}
	call imfilt(heim, heoutp, kernel, kxdim, kydim, el)

	# Unmap the images.
	call imunmap(heim)
	call imunmap(heoutp)

	call sfree (sp)

end


# QFITDIODE -- Calculate the coefficients of the best fit functions.

procedure qfitdiode (sqs, line, npix, a, c, threshold, verbose)

pointer	sqs						# q's structure
int	line						# line in image
int	npix						# number of pixels
real	a, c						# returned coeffs
int	threshold					# sqib threshold
bool	verbose						# verbose flag

int	i, j
real	zz[4,4], limbr

begin
	# If the number of points is insufficient, skip.
	if (VT_NUMDATA(sqs,line) < 50) {
	    a = 0.0
	    c = 0.0
	    return
	}

	# First set the out arrays equal to the in arrays, initialize limbr.
	limbr = LIMBR


	# Clear the z array.
	do i = 1,4 
	    do j = 1,4
		zz[i,j] = 0.0

	# Fill the z array.
	zz[1,2] = VT_SQ1Q1(sqs,line)
	zz[1,3] = VT_SQ1Q2(sqs,line)
	zz[1,4] = VT_SQ1Q3(sqs,line)
	zz[2,3] = VT_SQ2Q2(sqs,line)
	zz[2,4] = VT_SQ2Q3(sqs,line)
	zz[3,4] = VT_SQ3Q3(sqs,line)

	# Do the fit if the sum of weights is sufficient.
	if (VT_SQ1(sqs,line) > SOWTHRESH)
	    call lstsq(zz,4,VT_SQ1(sqs,line))
	else {
	    zz[3,1] = 0.0
	    zz[3,2] = 0.0
	}

	# Coefficients are:
	if (verbose) {
	    call printf ("a = %g, c = %g ")
	    call pargr(zz[3,1])
	    call pargr(zz[3,2])
	    call flush(STDOUT)
	}
	c = zz[3,1]
	a = zz[3,2]
end


# SUMQ -- Sum up the values of the Qs for the least squares fit.

procedure qsumq (in, npix, el, threshold, weights, limbr, y, sqs)

short	in[npix]			# array to sum from
pointer	weights				# weights
real	el[LEN_ELSTRUCT]		# limb fit ellipse struct
real	limbr				# limb closeness rejection coefficient
int	npix				# numpix in im line
int	threshold			# sqib threshold
int	y				# line in image
pointer	sqs				# pointer to q's structure

real	q1, q2, q3
int	i, windex, itemp
real	rsq, r4th, r6th, r8th
real	x, xfr, yfr, data
short	k

int	and()
short	shifts()

begin
	k = -4

	# First, calculate the y fractional radius squared.
	yfr = (abs(real(y) - E_YCENTER[el]))**2 / (E_YSEMIDIAMETER[el]**2)

	# Do this for all the pixels in this row.
	do i = 1, npix {
	    # Calculate the x fractional radius squared.
	    x = real(i)
	    xfr = (abs(x - E_XCENTER[el]))**2 / E_XSEMIDIAMETER[el]**2

	    # If off the disk, skip.
	    if (xfr > 1.0) {
		next
	    }

	    # Check to see if the brightness of this data point is above the
	    # threshold, if not, skip.

	    itemp = in[i]
	    if (and(itemp,17B) < threshold)
		next

	    # Strip off the squibby brightness, if data too big skip.
	    data = real(shifts(in[i], k))
	    if (data > 100.)
		next
	    
	    # Calculate the radius squared. (fractional)
	    rsq = xfr + yfr

	    # Check to see if the data point is on the disk.
	    if (rsq > limbr)
		next

	    r4th = rsq * rsq
	    r6th = rsq * r4th
	    r8th = r4th * r4th

	    # Calculate the weight index.
	    windex = WINDEXC + data + WINDEX6TH * r6th
	    if (windex < 1)
		windex = 1
	    if (windex > SZ_WT10830)
		windex = SZ_WT10830

	    # Calculate the Qs.
	    q1 = Memr[weights+windex-1]
	    q2 = q1 * r6th
	    q3 = q1 * data
	    VT_SQ1(sqs,y) = VT_SQ1(sqs,y) + q1
	    VT_SQ1Q1(sqs,y) = VT_SQ1Q1(sqs,y) + q1 * q1
	    VT_SQ1Q2(sqs,y) = VT_SQ1Q2(sqs,y) + q1 * q2
	    VT_SQ1Q3(sqs,y) = VT_SQ1Q3(sqs,y) + q1 * q3
	    VT_SQ2Q2(sqs,y) = VT_SQ2Q2(sqs,y) + q2 * q2
	    VT_SQ2Q3(sqs,y) = VT_SQ2Q3(sqs,y) + q2 * q3
	    VT_SQ3Q3(sqs,y) = VT_SQ3Q3(sqs,y) + q3 * q3
	    VT_NUMDATA(sqs,y) = VT_NUMDATA(sqs,y) + 1
	}
end
	
	
# QRFUNCT -- Remove FUNCTion. Remove the calculated function from the data
# from a particular diode.  Each data point is checked to see if it is on
# disk.  If it is not then the input pixel is copied to the output array.
# if it is on the disk, the function defined by a and c is subtracted from
# the data point before it is copied to the output array.

procedure qrfunct (in, out, npix, el, threshold, a, c, limbr, y)

short	in[npix]		# inline without fit removed
short	out[npix]		# inline with fit removed
real	el[LEN_ELSTRUCT]	# ellipse parameter struct
real	a, c			# fit coefficients
real	limbr			# limb closeness coefficient
int	y			# line of image
int	npix			# number of pixels in this line
int	threshold		# sqib threshold

int	i
short	fvalue
short	data
real	x, xfr, yfr, rsq, y4th, y6th
short	correction
short	k, kk

short 	shifts()

begin
	k = -4
	kk = 4

	# If a and c have zeros, skip.
	if (abs(a) < EPSILONR && abs(c) < EPSILONR) {
	    do i = 1, npix {
		out[i] = in[i]   # leave original data.
	    }
	    return
	}

	# First, calculate the y fractional radius.
	yfr = (abs(real(y) - E_YCENTER[el]))**2 / (E_YSEMIDIAMETER[el]**2)

	# Calculate the correction.
	y4th = yfr*yfr
	y6th = y4th*yfr
	correction = short(FCORRECT*(6.0*yfr + 8.0*y4th + 16.0*y6th))

	# Do this for all the pixels in the row.
	do i = 1, npix {
	    # Calculate the x fractional radius.
	    x = real(npix/2 - i + 1)
	    xfr = (abs(real(i) - E_XCENTER[el]))**2 / E_XSEMIDIAMETER[el]**2

	    # If off the disk, skip.
	    if (xfr > 1.0) {
		out[i] = in[i]		# leave original data
		next
	    }

	    # Check to see if the brightness of this data point is above the
	    # threshold, if not, skip.

	    if (and(int(in[i]),17B) < threshold) {
		out[i] = in[i]		# leave original data
		next
	    }

	    # Strip off the squibby brightness
	    data = shifts(in[i], k)
	    
	    # Calculate the radius squared. (fractional)
	    rsq = xfr + yfr

	    # Check to see if the data point is on the disk.
	    if (rsq > 1.0) {
		out[i] = in[i]		# leave original data
		next
	    }

	    # Calculate the function value.  Subtract it from the data value.
	    fvalue = short(a * rsq**3 + c)   # a * r**6 + c
	    data = data - fvalue + correction
	    # data + squib bright
	    out[i] = shifts(data, kk) + short(and(int(in[i]),17B))
	}
end
