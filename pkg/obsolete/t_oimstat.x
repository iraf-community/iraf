# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include "oimstat.h"


# T_OIMSTATISTICS -- Compute and print the statistics of images.

procedure t_oimstatistics ()

pointer	fieldstr			# Pointer to fields string
real	lower				# Lower limit of data value window
real	upper				# Upper limit of data value window
real	binwidth			# Width of histogram bin in sigma
int	format				# Format the output

int	nfields, nbins
int	minmax, npix, mean, median, mode, stddev, skew, kurtosis
pointer	sp, fields, image, v
pointer	im, list, ist, buf, hgm
real	hwidth, hmin, hmax

bool	clgetb()
int	ist_fields(), ist_isfield, imtgetim(), ist_ihist(), btoi()
pointer	imtopenp(), imgnlr()
real	clgetr()
pointer	immap()

begin
	call smark (sp)
	call salloc (fieldstr, SZ_LINE, TY_CHAR)
	call salloc (fields, NFIELDS, TY_INT)
	call salloc (ist, LEN_IMSTAT, TY_STRUCT)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Open the list of input images, the fields and the data value limits.
	list = imtopenp ("images")
	call clgstr ("fields", Memc[fieldstr], SZ_LINE)
	lower = clgetr ("lower")
	upper = clgetr ("upper")
	binwidth = clgetr ("binwidth")
	format = btoi (clgetb ("format"))

	# Get the selected fields.
	nfields = ist_fields (Memc[fieldstr], Memi[fields], NFIELDS)
	if (nfields <= 0) {
	    call imtclose (list)
	    call sfree (sp)
	    return
	}

	# Set the computation switches.
	npix = ist_isfield (IS_FNPIX, Memi[fields], nfields)
	mean = ist_isfield (IS_FMEAN, Memi[fields], nfields)
	median = ist_isfield (IS_FMEDIAN, Memi[fields], nfields)
	mode = ist_isfield (IS_FMODE, Memi[fields], nfields)
	stddev = ist_isfield (IS_FSTDDEV, Memi[fields], nfields)
	skew = ist_isfield (IS_FSKEW, Memi[fields], nfields)
	kurtosis = ist_isfield (IS_FKURTOSIS, Memi[fields], nfields)
	if (ist_isfield (IS_FMIN, Memi[fields], nfields) == YES)
	    minmax = YES
	else if (ist_isfield (IS_FMAX, Memi[fields], nfields) == YES)
	    minmax = YES
	else if (median == YES || mode == YES)
	    minmax = YES
	else
	    minmax = NO

	# Print a header banner for the selected fields.
	if (format == YES)
	    call ist_pheader (Memi[fields], nfields)

	# Loop through the input images.
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {

	    im = immap (Memc[image], READ_ONLY, 0)
	    call ist_initialize (ist, lower, upper)

	    # Accumulate the central moment statistics.
	    call amovkl (long(1), Meml[v], IM_MAXDIM)
	    if (kurtosis == YES) {
	    	while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ist_accumulate4 (ist, Memr[buf], int (IM_LEN(im, 1)),
		        lower, upper, minmax)
	    } else if (skew == YES) {
	    	while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ist_accumulate3 (ist, Memr[buf], int (IM_LEN (im, 1)),
		        lower, upper, minmax)
	    } else if (stddev == YES || median == YES || mode == YES) {
	    	while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ist_accumulate2 (ist, Memr[buf], int (IM_LEN(im,1)),
		        lower, upper, minmax)
	    } else if (mean == YES) {
	    	while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ist_accumulate1 (ist, Memr[buf], int (IM_LEN(im,1)),
		        lower, upper, minmax)
	    } else if (npix == YES) {
	    	while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ist_accumulate0 (ist, Memr[buf], int (IM_LEN(im,1)),
		        lower, upper, minmax)
	    } else if (minmax == YES) {
	    	while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ist_accumulate0 (ist, Memr[buf], int (IM_LEN(im,1)),
		        lower, upper, YES)
	    }

	    # Compute the central moment statistics.
	    call ist_stats (ist, skew, kurtosis)

	    # Accumulate the histogram.
	    hgm = NULL
	    if ((median == YES || mode == YES) && ist_ihist (ist, binwidth,
	        hgm, nbins, hwidth, hmin, hmax) == YES) {
		call aclri (Memi[hgm], nbins)
		call amovkl (long(1), Meml[v], IM_MAXDIM)
		while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ahgmr (Memr[buf], int(IM_LEN(im,1)), Memi[hgm], nbins,
		        hmin, hmax)
		if (median == YES)
		    call ist_hmedian (ist, Memi[hgm], nbins, hwidth, hmin,
			hmax)
		if (mode == YES)
		    call ist_hmode (ist, Memi[hgm], nbins, hwidth, hmin, hmax)
	    }

	    # Print the statistics.
	    if (format == YES)
	        call ist_print (Memc[image], ist, Memi[fields], nfields)
	    else
	        call ist_fprint (Memc[image], ist, Memi[fields], nfields)
		
	    if (hgm != NULL)
		call mfree (hgm, TY_INT)
	    call imunmap (im)
	}

	call imtclose (list)
	call sfree (sp)
end


# IST_FIELDS -- Procedure to decode the fields string into a list of the
# fields to be computed and printed.

int procedure ist_fields (fieldstr, fields, max_nfields)

char	fieldstr[ARB]		# string containing the list of fields
int	fields[ARB]		# fields array
int	max_nfields		# maximum number of fields

int	nfields, flist, field
pointer	sp, fname
int	fntopnb(), fntgfnb(), strdic()

begin
	nfields = 0

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	flist = fntopnb (fieldstr, NO)
	while (fntgfnb (flist, Memc[fname], SZ_FNAME) != EOF && 
	    (nfields < max_nfields)) {
	    field = strdic (Memc[fname], Memc[fname], SZ_FNAME, IS_FIELDS)
	    if (field == 0)
		next
	    nfields = nfields + 1
	    fields[nfields] = field
	}
	call fntclsb (flist)

	call sfree (sp)

	return (nfields)
end


# IST_ISFIELD -- Procedure to determine whether a specified field is one
# of the selected fields or not.

int procedure ist_isfield (field, fields, nfields)

int	field		# field to be tested
int	fields[ARB]	# array of selected fields
int	nfields		# number of fields

int	i, isfield

begin
	isfield = NO
	do i = 1, nfields {
	    if (field != fields[i])
		next
	    isfield = YES
	    break
	}

	return (isfield)
end


# IST_INITIALIZE -- Initialize the sum array to zero.

procedure ist_initialize (ist, lower, upper)

pointer	ist		# pointer to the statistics structure
real	lower		# lower datalimit
real	upper		# upperlimit

begin
	if (IS_INDEFR(lower))
	    IS_LO(ist) = -MAX_REAL
	else
	    IS_LO(ist) = lower
	if (IS_INDEFR(upper))
	    IS_HI(ist) = MAX_REAL
	else
	    IS_HI(ist) = upper

	IS_NPIX(ist) = 0
	IS_SUMX(ist) = 0.0d0
	IS_SUMX2(ist) = 0.0d0
	IS_SUMX3(ist) = 0.0d0
	IS_SUMX4(ist) = 0.0d0

	IS_MIN(ist) = MAX_REAL
	IS_MAX(ist) = -MAX_REAL
	IS_MEAN(ist) = INDEFR
	IS_MEDIAN(ist) = INDEFR
	IS_MODE(ist) = INDEFR
	IS_STDDEV(ist) = INDEFR
	IS_SKEW(ist) = INDEFR
	IS_KURTOSIS(ist) = INDEFR
end


# IST_ACCUMULATE4 -- Accumulate sums up to the fourth power of the data for
# data values between lower and upper.

procedure ist_accumulate4 (is, x, npts, lower, upper, minmax)

pointer	is		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
int	minmax		# compute the minimum and maximum

int	i, npix
real	lo, hi, xmin, xmax
double	xx, xx2, sumx, sumx2, sumx3, sumx4

begin
	lo = IS_LO(is)
	hi = IS_HI(is)
	npix = IS_NPIX(is)
	sumx = 0.0
	sumx2 = 0.0
	sumx3 = 0.0
	sumx4 = 0.0
	xmin = IS_MIN(is)
	xmax = IS_MAX(is)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (minmax == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    }
	} else {
	    if (minmax == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    npix = npix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		    sumx4 = sumx4 + xx2 * xx2
		}
	    }
	}

	IS_NPIX(is) = npix
	IS_SUMX(is) = IS_SUMX(is) + sumx
	IS_SUMX2(is) = IS_SUMX2(is) + sumx2
	IS_SUMX3(is) = IS_SUMX3(is) + sumx3
	IS_SUMX4(is) = IS_SUMX4(is) + sumx4
	IS_MIN(is) = xmin
	IS_MAX(is) = xmax
end


# IST_ACCUMULATE3 -- Accumulate sums up to the third power of the data for
# data values between lower and upper.

procedure ist_accumulate3 (is, x, npts, lower, upper, minmax)

pointer	is		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
int	minmax		# compute the minimum and maximum

int	i, npix
real	lo, hi, xmin, xmax
double 	xx, xx2, sumx, sumx2, sumx3

begin
	lo = IS_LO(is)
	hi = IS_HI(is)
	npix = IS_NPIX(is)
	sumx = 0.0
	sumx2 = 0.0
	sumx3 = 0.0
	xmin = IS_MIN(is)
	xmax = IS_MAX(is)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (minmax == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    }
	} else {
	    if (minmax == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    npix = npix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
		    xx2 = xx * xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx2
		    sumx3 = sumx3 + xx2 * xx
		}
	    }
	}

	IS_NPIX(is) = npix
	IS_SUMX(is) = IS_SUMX(is) + sumx
	IS_SUMX2(is) = IS_SUMX2(is) + sumx2
	IS_SUMX3(is) = IS_SUMX3(is) + sumx3
	IS_MIN(is) = xmin
	IS_MAX(is) = xmax
end


# IST_ACCUMULATE2 -- Accumulate sums up to the second power of the data for
# data values between lower and upper.

procedure ist_accumulate2 (is, x, npts, lower, upper, minmax)

pointer	is		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
int	minmax		# compute the minimum and maximum

int	i, npix
real	lo, hi, xmin, xmax
double	xx, sumx, sumx2

begin
	lo = IS_LO(is)
	hi = IS_HI(is)
	npix = IS_NPIX(is)
	sumx = 0.0
	sumx2 = 0.0
	xmin = IS_MIN(is)
	xmax = IS_MAX(is)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (minmax == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx * xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx * xx
		}
	    }
	} else {
	    if (minmax == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    npix = npix + 1
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx * xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
		    sumx = sumx + xx
		    sumx2 = sumx2 + xx * xx
		}
	    }
	}

	IS_NPIX(is) = npix
	IS_SUMX(is) = IS_SUMX(is) + sumx
	IS_SUMX2(is) = IS_SUMX2(is) + sumx2
	IS_MIN(is) = xmin
	IS_MAX(is) = xmax
end


# IST_ACCUMULATE1 -- Accumulate sums up to the first power of the data for
# data values between lower and upper.

procedure ist_accumulate1 (is, x, npts, lower, upper, minmax)

pointer	is		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
int	minmax		# compute the minimum and maximum

int	i, npix
real	lo, hi, xx, xmin, xmax
double	sumx

begin
	lo = IS_LO(is)
	hi = IS_HI(is)
	npix = IS_NPIX(is)
	sumx = 0.0
	xmin = IS_MIN(is)
	xmax = IS_MAX(is)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (minmax == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    sumx = sumx + xx
		}
	    } else {
		do i = 1, npts
		    sumx = sumx + x[i]
	    }
	} else {
	    if (minmax == YES) {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
		    if (xx < xmin)
			xmin = xx
		    if (xx > xmax)
			xmax = xx
		    sumx = sumx + xx
		}
	    } else {
		do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
			next
		    npix = npix + 1
		    sumx = sumx + xx
		}
	    }
	}

	IS_NPIX(is) = npix
	IS_SUMX(is) = IS_SUMX(is) + sumx
	IS_MIN(is) = xmin
	IS_MAX(is) = xmax
end


# IST_ACCUMULATE0 -- Accumulate sums up to the 0th power of the data for
# data values between lower and upper.

procedure ist_accumulate0 (is, x, npts, lower, upper, minmax)

pointer	is		# pointer to the statistics structure
real	x[ARB]		# the data array
int	npts		# the number of data points
real	lower		# lower data boundary
real	upper		# upper data boundary
int	minmax		# compute the minimum and maximum

int	i, npix
real	lo, hi, xx, xmin, xmax

begin
	lo = IS_LO(is)
	hi = IS_HI(is)
	npix = IS_NPIX(is)
	xmin = IS_MIN(is)
	xmax = IS_MAX(is)

	if (IS_INDEFR(lower) && IS_INDEFR(upper)) {
	    npix = npix + npts
	    if (minmax == YES) {
	        do i = 1, npts {
		    xx = x[i]
		    if (xx < xmin)
		        xmin = xx
		    if (xx > xmax)
		        xmax = xx
	        }
	    }
	} else {
	    if (minmax == YES) {
	        do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
		        next
		    npix = npix + 1
		    if (xx < xmin)
		        xmin = xx
		    if (xx > xmax)
		        xmax = xx
	        }
	    } else {
	        do i = 1, npts {
		    xx = x[i]
		    if (xx < lo || xx > hi)
		        next
		    npix = npix + 1
		}
	    }
	}

	IS_NPIX(is) = npix
	IS_MIN(is) = xmin
	IS_MAX(is) = xmax
end


# IST_STATS -- Procedure to compute the first four central moments of the
# distribution.

procedure ist_stats (ist, bskew, bkurtosis)

pointer	ist			# statistics structure
int	bskew			# skew switch
int	bkurtosis		# kurtosis switch

double	mean, var, stdev
bool	fp_equalr()

begin
	if (fp_equalr (IS_MIN(ist), MAX_REAL))
	    IS_MIN(ist) = INDEFR
	if (fp_equalr (IS_MAX(ist), -MAX_REAL))
	    IS_MAX(ist) = INDEFR

	if (IS_NPIX(ist) <= 0)
	    return
	mean = IS_SUMX(ist) / IS_NPIX(ist)
	IS_MEAN(ist) = mean

	if (IS_NPIX(ist) < 2)
	    return
	var = (IS_SUMX2(ist) - IS_SUMX(ist) * mean) /
	    (IS_NPIX(ist) - 1)
	if (var <= 0.0) {
	    IS_STDDEV(ist) = 0.0
	    return
	} else {
	    stdev = sqrt (var)
	    IS_STDDEV(ist) = stdev
	}

	if (bskew == YES)
	    IS_SKEW(ist) = (IS_SUMX3(ist) - 3.0d0 * IS_MEAN(ist) *
	        IS_SUMX2(ist) + 3.0d0 * mean * mean *
		IS_SUMX(ist) - IS_NPIX(ist) * mean ** 3) /
		IS_NPIX(ist) / stdev / stdev / stdev
	    
	if (bkurtosis == YES)
	    IS_KURTOSIS(ist) = (IS_SUMX4(ist) - 4.0d0 * mean *
	        IS_SUMX3(ist) + 6.0d0 * mean * mean *
	        IS_SUMX2(ist) - 4.0 * mean ** 3 * IS_SUMX(ist) +
	        IS_NPIX(ist) * mean ** 4) / IS_NPIX(ist) /
	        stdev / stdev / stdev / stdev - 3.0d0
end


# IST_IHIST -- Procedure to initilaize the histogram of the image pixels.

int procedure ist_ihist (ist, binwidth, hgm, nbins, hwidth, hmin, hmax)

pointer	ist		# pointer to the statistics structure
real	binwidth	# histogram bin width in sigma
pointer	hgm		# pointer to the histogram
int	nbins		# number of bins
real	hwidth		# histogram resolution
real	hmin		# minimum histogram value
real	hmax		# maximum histogram value

begin
	nbins = 0
	if (binwidth <= 0.0)
	    return (NO)
	hwidth = binwidth * IS_STDDEV(ist)
	if (hwidth <= 0.0)
	    return (NO)
	nbins = (IS_MAX(ist) - IS_MIN(ist)) / hwidth + 1
	if (nbins < 3)
	    return (NO)

	hmin = IS_MIN(ist)
	hmax = IS_MAX(ist)
	call malloc (hgm, nbins, TY_INT)
	return (YES)
end


# IST_HMEDIAN -- Procedure to compute the median of the values.

procedure ist_hmedian (ist, hgm, nbins, hwidth, hmin, hmax)

pointer	ist		# pointer to the statistics strucuture
int	hgm[ARB]	# histogram of the pixels
int	nbins		# number of bins in the histogram
real	hwidth		# resolution of the histogram
real	hmin		# minimum histogram value
real	hmax		# maximum histogram value

int	i, lo, hi
pointer	sp, ihgm
real	h1, hdiff, hnorm
bool	fp_equalr()

begin
	call smark (sp)
	call salloc (ihgm, nbins, TY_REAL)

	# Integrate the histogram and normalize.
	Memr[ihgm] = hgm[1]
	do i = 2, nbins
	    Memr[ihgm+i-1] = hgm[i] + Memr[ihgm+i-2]
	hnorm = Memr[ihgm+nbins-1]
	call adivkr (Memr[ihgm], hnorm, Memr[ihgm], nbins)

	# Initialize the low and high bin numbers.
	lo = 0
	hi = 1

	# Search for the point which divides the integral in half.
	do i = 1, nbins {
	    if (Memr[ihgm+i-1] > 0.5)
		break
	    lo = i
	}
	hi = lo + 1
	#call eprintf (
	    #"hmin=%g hmax=%g hw=%g nbins=%d lo=%d ih(lo)=%g hi=%d ih(hi)=%g\n")
	    #call pargr (hmin)
	    #call pargr (hmax)
	    #call pargr (hwidth)
	    #call pargi (nbins)
	    #call pargi (lo)
	    #call pargr (Memr[ihgm+lo-1])
	    #call pargi (hi)
	    #call pargr (Memr[ihgm+hi-1])

	# Approximate the histogram.
	h1 = hmin + lo * hwidth
	if (lo == 0)
	    hdiff = Memr[ihgm+hi-1]
	else
	    hdiff = Memr[ihgm+hi-1] - Memr[ihgm+lo-1]
	if (fp_equalr (hdiff, 0.0))
	    IS_MEDIAN(ist) = h1
	else if (lo == 0)
	    IS_MEDIAN(ist) = h1 + 0.5 / hdiff * hwidth
	else
	    IS_MEDIAN(ist) = h1 + (0.5 - Memr[ihgm+lo-1]) / hdiff * hwidth
	#call eprintf ("hlo=%g hhi=%g h1=%g hdiff=%g median=%g\n")
	    #call pargr (hmin)
	    #call pargr (hmin + (nbins - 1) * hwidth)
	    #call pargr (h1)
	    #call pargr (hdiff)
	    #call pargr (IS_MEDIAN(ist))

	call sfree (sp)
end


# IST_HMODE -- Procedure to compute the mode.

procedure ist_hmode (ist, hgm, nbins, hwidth, hmin, hmax)

pointer	ist		# pointer to the statistics strucuture
int	hgm[ARB]	# histogram of the pixels
int	nbins		# number of bins in the histogram
real	hwidth		# resolution of the histogram
real	hmin		# minimum histogram value
real	hmax		# maximum histogram value

int	i, bpeak
real	hpeak, dh1, dh2, denom
bool	fp_equalr()

begin
	# If there is a single bin return the midpoint of that bin.
	if (nbins == 1) {
	    IS_MODE(ist) = hmin + 0.5 * hwidth
	    return
	}

	# If there are two bins return the midpoint of the greater bin.
	if (nbins == 2) {
	    if (hgm[1] > hgm[2])
	        IS_MODE(ist) = hmin + 0.5 * hwidth
	    else if (hgm[2] > hgm[1])
	        IS_MODE(ist) = hmin + 1.5 * hwidth
	    else
	        IS_MODE(ist) = hmin + hwidth
	    return
	}

	# Find the bin containing the histogram maximum.
	hpeak = hgm[1]
	bpeak = 1
	do i = 2, nbins {
	    if (hgm[i] > hpeak) {
		hpeak = hgm[i]
		bpeak = i
	    }
	}

	# If the maximum is in the first bin return the midpoint of the bin.
	if (bpeak == 1) {
	    IS_MODE(ist) = hmin + 0.5 * hwidth
	    return
	}

	# If the maximum is in the last bin return the midpoint of the bin.
	if (bpeak == nbins) {
	    IS_MODE(ist) = hmin + (nbins - 0.5) * hwidth
	    return
	}

	# Compute the lower limit of bpeak.
	bpeak = bpeak - 1

	# Do a parabolic interpolation to find the peak.
	dh1 = hgm[bpeak+1] - hgm[bpeak]
	dh2 = hgm[bpeak+1] - hgm[bpeak+2]
	denom = dh1 + dh2
	if (fp_equalr (denom, 0.0)) {
	    IS_MODE(ist) = hmin + (bpeak + 0.5) * hwidth
	} else {
	    IS_MODE(ist) = bpeak + 1 + 0.5 * (dh1 - dh2) / denom
	    IS_MODE(ist) = hmin + (IS_MODE(ist) - 0.5) * hwidth
	}


	dh1 = hgm[bpeak] * (hmin + (bpeak - 0.5) * hwidth) +
	    hgm[bpeak+1] * (hmin + (bpeak + 0.5) * hwidth) +
	    hgm[bpeak+2] * (hmin + (bpeak + 1.5) * hwidth)
	dh2 = hgm[bpeak] + hgm[bpeak+1] + hgm[bpeak+2]
end


# IST_PHEADER -- Print the banner fields.

procedure ist_pheader (fields, nfields)

int	fields[ARB]		# fields to be printed
int	nfields			# number of fields

int	i

begin
	call printf ("#")
	do i = 1, nfields {
	    switch (fields[i]) {
	    case IS_FIMAGE:
	        call printf (IS_FSTRING)
		    call pargstr (IS_KIMAGE)
	    case IS_FNPIX:
	        call printf (IS_FCOLUMN)
		    call pargstr (IS_KNPIX)
	    case IS_FMIN:
		call printf (IS_FCOLUMN)
		    call pargstr (IS_KMIN)
	    case IS_FMAX:
		call printf (IS_FCOLUMN)
		    call pargstr (IS_KMAX)
	    case IS_FMEAN:
		call printf (IS_FCOLUMN)
		    call pargstr (IS_KMEAN)
	    case IS_FMEDIAN:
		call printf (IS_FCOLUMN)
		    call pargstr (IS_KMEDIAN)
	    case IS_FMODE:
		call printf (IS_FCOLUMN)
		    call pargstr (IS_KMODE)
	    case IS_FSTDDEV:
		call printf (IS_FCOLUMN)
		    call pargstr (IS_KSTDDEV)
	    case IS_FSKEW:
		call printf (IS_FCOLUMN)
		    call pargstr (IS_KSKEW)
	    case IS_FKURTOSIS:
		call printf (IS_FCOLUMN)
		    call pargstr (IS_KKURTOSIS)
	    }
	}

	call printf ("\n")
	call flush (STDOUT)
end


# IST_PRINT -- Print the fields

procedure ist_print (image, ist, fields, nfields)

char	image[ARB]		# image name
pointer	ist			# pointer to the statistics structure
int	fields[ARB]		# fields to be printed
int	nfields			# number of fields

int	i

begin
	call printf (" ")
	do i = 1, nfields {
	    switch (fields[i]) {
	    case IS_FIMAGE:
	        call printf (IS_FSTRING)
		    call pargstr (image)
	    case IS_FNPIX:
	        call printf (IS_FINTEGER)
		    call pargi (IS_NPIX(ist))
	    case IS_FMIN:
		call printf (IS_FREAL)
		    call pargr (IS_MIN(ist))
	    case IS_FMAX:
		call printf (IS_FREAL)
		    call pargr (IS_MAX(ist))
	    case IS_FMEAN:
		call printf (IS_FREAL)
		    call pargr (IS_MEAN(ist))
	    case IS_FMEDIAN:
		call printf (IS_FREAL)
		    call pargr (IS_MEDIAN(ist))
	    case IS_FMODE:
		call printf (IS_FREAL)
		    call pargr (IS_MODE(ist))
	    case IS_FSTDDEV:
		call printf (IS_FREAL)
		    call pargr (IS_STDDEV(ist))
	    case IS_FSKEW:
		call printf (IS_FREAL)
		    call pargr (IS_SKEW(ist))
	    case IS_FKURTOSIS:
		call printf (IS_FREAL)
		    call pargr (IS_KURTOSIS(ist))
	    }
	}

	call printf ("\n")
	call flush (STDOUT)
end


# IST_FPRINT -- Print the fields using a free format.

procedure ist_fprint (image, ist, fields, nfields)

char	image[ARB]		# image name
pointer	ist			# pointer to the statistics structure
int	fields[ARB]		# fields to be printed
int	nfields			# number of fields

int	i

begin
	do i = 1, nfields {
	    switch (fields[i]) {
	    case IS_FIMAGE:
	        call printf ("%s")
		    call pargstr (image)
	    case IS_FNPIX:
	        call printf ("%d")
		    call pargi (IS_NPIX(ist))
	    case IS_FMIN:
		call printf ("%g")
		    call pargr (IS_MIN(ist))
	    case IS_FMAX:
		call printf ("%g")
		    call pargr (IS_MAX(ist))
	    case IS_FMEAN:
		call printf ("%g")
		    call pargr (IS_MEAN(ist))
	    case IS_FMEDIAN:
		call printf ("%g")
		    call pargr (IS_MEDIAN(ist))
	    case IS_FMODE:
		call printf ("%g")
		    call pargr (IS_MODE(ist))
	    case IS_FSTDDEV:
		call printf ("%g")
		    call pargr (IS_STDDEV(ist))
	    case IS_FSKEW:
		call printf ("%g")
		    call pargr (IS_SKEW(ist))
	    case IS_FKURTOSIS:
		call printf ("%g")
		    call pargr (IS_KURTOSIS(ist))
	    }
	    if (i < nfields)
		call printf ("  ")
	}

	call printf ("\n")
	call flush (STDOUT)
end
