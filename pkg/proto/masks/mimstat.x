include <mach.h>
include "mimstat.h"


# MST_ALLOCATE -- Allocate space for the statistics structure.

procedure mst_allocate (mst)

pointer	mst		#O the statistics descriptor

begin
    	call calloc (mst, LEN_MIMSTAT, TY_STRUCT)
	call malloc (MIS_SW(mst), LEN_NSWITCHES, TY_INT)
end


# MST_FREE -- Free the statistics structure.

procedure mst_free (mst)

pointer	mst		#O the statistics descriptor

begin
	call mfree (MIS_SW(mst), TY_INT)
	call mfree (mst, TY_STRUCT)
end


# MST_FIELDS -- Procedure to decode the fields string into a list of the
# fields to be computed and printed.

int procedure mst_fields (fieldstr, fields, max_nfields)

char    fieldstr[ARB]           #I string containing the list of fields
int     fields[ARB]             #O fields array
int     max_nfields             #I maximum number of fields

int     nfields, flist, field
pointer sp, fname
int     fntopnb(), fntgfnb(), strdic()

begin
        nfields = 0

        call smark (sp)
        call salloc (fname, SZ_FNAME, TY_CHAR)

        flist = fntopnb (fieldstr, NO)
        while (fntgfnb (flist, Memc[fname], SZ_FNAME) != EOF &&
            (nfields < max_nfields)) {
            field = strdic (Memc[fname], Memc[fname], SZ_FNAME, MIS_FIELDS)
            if (field == 0)
                next
            nfields = nfields + 1
            fields[nfields] = field
        }
        call fntclsb (flist)

        call sfree (sp)

        return (nfields)
end


# MST_SWITCHES -- Set the processing switches.

procedure mst_switches (mst, fields, nfields, nclip)

pointer	mst			#I the statistics pointer
int     fields[ARB]             #I fields array
int     nfields                 #I maximum number of fields
int	nclip			#I the number of clipping iterations

pointer	sw
int	mst_isfield()

begin
	# Initialize.
	sw = MIS_SW(mst)
	call amovki (NO, Memi[sw], LEN_NSWITCHES)

        # Set the computation switches.
        MIS_SNPIX(sw) = mst_isfield (MIS_FNPIX, fields, nfields)
        MIS_SMEAN(sw) = mst_isfield (MIS_FMEAN, fields, nfields)
        MIS_SMEDIAN(sw) = mst_isfield (MIS_FMEDIAN, fields, nfields)
        MIS_SMODE(sw) = mst_isfield (MIS_FMODE, fields, nfields)
        if (nclip > 0)
            MIS_SSTDDEV(sw) = YES
	else
            MIS_SSTDDEV(sw) = mst_isfield (MIS_FSTDDEV, fields, nfields)
        MIS_SSKEW(sw) = mst_isfield (MIS_FSKEW, fields, nfields)
        MIS_SKURTOSIS(sw) = mst_isfield (MIS_FKURTOSIS, fields, nfields)

	# Adjust the computation switches.
        if (mst_isfield (MIS_FMIN, fields, nfields) == YES)
            MIS_SMINMAX(sw) = YES
        else if (mst_isfield (MIS_FMAX, fields, nfields) == YES)
            MIS_SMINMAX(sw) = YES
        else if (MIS_SMEDIAN(sw) == YES || MIS_SMODE(sw) == YES)
            MIS_SMINMAX(sw) = YES
        else
            MIS_SMINMAX(sw) = NO
end


# MST_PHEADER -- Print the banner fields.

procedure mst_pheader (fields, nfields)

int     fields[ARB]             # fields to be printed
int     nfields                 # number of fields

int     i

begin
        call printf ("#")
        do i = 1, nfields {
            switch (fields[i]) {
            case MIS_FIMAGE:
                call printf (MIS_FSTRING)
                    call pargstr (MIS_KIMAGE)
            case MIS_FMASK:
                call printf (MIS_FSTRING)
                    call pargstr (MIS_KMASK)
            case MIS_FNPIX:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KNPIX)
            case MIS_FMIN:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KMIN)
            case MIS_FMAX:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KMAX)
            case MIS_FMEAN:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KMEAN)
            case MIS_FMEDIAN:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KMEDIAN)
            case MIS_FMODE:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KMODE)
            case MIS_FSTDDEV:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KSTDDEV)
            case MIS_FSKEW:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KSKEW)
            case MIS_FKURTOSIS:
                call printf (MIS_FCOLUMN)
                    call pargstr (MIS_KKURTOSIS)
            }
        }

        call printf ("\n")
        call flush (STDOUT)
end


# MST_ISFIELD -- Procedure to determine whether a specified field is one
# of the selected fields or not.

int procedure mst_isfield (field, fields, nfields)

int     field           #I field to be tested
int     fields[ARB]     #I array of selected fields
int     nfields         #I number of fields

int     i, isfield

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


# MST_INITIALIZE -- Initialize the statistics computation.

procedure mst_initialize (mst, lower, upper)

pointer mst             #I pointer to the statistics structure
real    lower           #I lower good data limit
real    upper           #I upper good data limit

begin
        if (IS_INDEFR(lower))
            MIS_LO(mst) = -MAX_REAL
        else
            MIS_LO(mst) = lower
        if (IS_INDEFR(upper))
            MIS_HI(mst) = MAX_REAL
        else
            MIS_HI(mst) = upper

        MIS_NPIX(mst) = 0
        MIS_SUMX(mst) = 0.0d0
        MIS_SUMX2(mst) = 0.0d0
        MIS_SUMX3(mst) = 0.0d0
        MIS_SUMX4(mst) = 0.0d0

        MIS_MIN(mst) = MAX_REAL
        MIS_MAX(mst) = -MAX_REAL
        MIS_MEAN(mst) = INDEFR
        MIS_MEDIAN(mst) = INDEFR
        MIS_MODE(mst) = INDEFR
        MIS_STDDEV(mst) = INDEFR
        MIS_SKEW(mst) = INDEFR
        MIS_KURTOSIS(mst) = INDEFR
end


# MST_ACCUMULATE4 -- Accumulate sums up to the fourth power of the data for
# data values between lower and upper.

procedure mst_accumulate4 (mst, x, npts, lower, upper, minmax)

pointer mst             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

double  xx, xx2, sumx, sumx2, sumx3, sumx4
real    lo, hi, xmin, xmax
int     i, npix

begin
        lo = MIS_LO(mst)
        hi = MIS_HI(mst)
        npix = MIS_NPIX(mst)
        sumx = 0.0
        sumx2 = 0.0
        sumx3 = 0.0
        sumx4 = 0.0
        xmin = MIS_MIN(mst)
        xmax = MIS_MAX(mst)

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

        MIS_NPIX(mst) = npix
        MIS_SUMX(mst) = MIS_SUMX(mst) + sumx
        MIS_SUMX2(mst) = MIS_SUMX2(mst) + sumx2
        MIS_SUMX3(mst) = MIS_SUMX3(mst) + sumx3
        MIS_SUMX4(mst) = MIS_SUMX4(mst) + sumx4
        MIS_MIN(mst) = xmin
        MIS_MAX(mst) = xmax
end


# MST_ACCUMULATE3 -- Accumulate sums up to the third power of the data for
# data values between lower and upper.

procedure mst_accumulate3 (mst, x, npts, lower, upper, minmax)

pointer mst             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

double  xx, xx2, sumx, sumx2, sumx3
real    lo, hi, xmin, xmax
int     i, npix

begin
        lo = MIS_LO(mst)
        hi = MIS_HI(mst)
        npix = MIS_NPIX(mst)
        sumx = 0.0
        sumx2 = 0.0
        sumx3 = 0.0
        xmin = MIS_MIN(mst)
        xmax = MIS_MAX(mst)

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

        MIS_NPIX(mst) = npix
        MIS_SUMX(mst) = MIS_SUMX(mst) + sumx
        MIS_SUMX2(mst) = MIS_SUMX2(mst) + sumx2
        MIS_SUMX3(mst) = MIS_SUMX3(mst) + sumx3
        MIS_MIN(mst) = xmin
        MIS_MAX(mst) = xmax
end


# MST_ACCUMULATE2 -- Accumulate sums up to the second power of the data for
# data values between lower and upper.

procedure mst_accumulate2 (mst, x, npts, lower, upper, minmax)

pointer mst             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

double  xx, sumx, sumx2
real    lo, hi, xmin, xmax
int     i, npix

begin
        lo = MIS_LO(mst)
        hi = MIS_HI(mst)
        npix = MIS_NPIX(mst)
        sumx = 0.0
        sumx2 = 0.0
        xmin = MIS_MIN(mst)
        xmax = MIS_MAX(mst)

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

        MIS_NPIX(mst) = npix
        MIS_SUMX(mst) = MIS_SUMX(mst) + sumx
        MIS_SUMX2(mst) = MIS_SUMX2(mst) + sumx2
        MIS_MIN(mst) = xmin
        MIS_MAX(mst) = xmax
end


# MST_ACCUMULATE1 -- Accumulate sums up to the first power of the data for
# data values between lower and upper.

procedure mst_accumulate1 (mst, x, npts, lower, upper, minmax)

pointer mst             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

double  sumx
real    lo, hi, xx, xmin, xmax
int     i, npix

begin
        lo = MIS_LO(mst)
        hi = MIS_HI(mst)
        npix = MIS_NPIX(mst)
        sumx = 0.0
        xmin = MIS_MIN(mst)
        xmax = MIS_MAX(mst)

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

        MIS_NPIX(mst) = npix
        MIS_SUMX(mst) = MIS_SUMX(mst) + sumx
        MIS_MIN(mst) = xmin
        MIS_MAX(mst) = xmax
end


# MST_ACCUMULATE0 -- Accumulate sums up to the 0th power of the data for
# data values between lower and upper.

procedure mst_accumulate0 (mst, x, npts, lower, upper, minmax)

pointer mst             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

int     i, npix
real    lo, hi, xx, xmin, xmax

begin
        lo = MIS_LO(mst)
        hi = MIS_HI(mst)
        npix = MIS_NPIX(mst)
        xmin = MIS_MIN(mst)
        xmax = MIS_MAX(mst)

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

        MIS_NPIX(mst) = npix
        MIS_MIN(mst) = xmin
        MIS_MAX(mst) = xmax
end


# MST_STATS -- Procedure to compute the first four central moments of the
# distribution.

procedure mst_stats (mst)

pointer mst                     #I statistics structure

double  mean, var, stdev
pointer	sw
bool    fp_equalr()

begin
	sw = MIS_SW(mst)

	# Compute the basic statistics regardless of the switches.
        if (fp_equalr (MIS_MIN(mst), MAX_REAL))
            MIS_MIN(mst) = INDEFR
        if (fp_equalr (MIS_MAX(mst), -MAX_REAL))
            MIS_MAX(mst) = INDEFR
        if (MIS_NPIX(mst) <= 0)
            return

        mean = MIS_SUMX(mst) / MIS_NPIX(mst)
        MIS_MEAN(mst) = mean
        if (MIS_NPIX(mst) < 2)
            return

        var = (MIS_SUMX2(mst) - MIS_SUMX(mst) * mean) /
            (MIS_NPIX(mst) - 1)
        if (var <= 0.0) {
            MIS_STDDEV(mst) = 0.0
            return
        } else {
            stdev = sqrt (var)
            MIS_STDDEV(mst) = stdev
        }

	# Compute higher order moments if the switches are set.
        if (MIS_SSKEW(sw)== YES)
            MIS_SKEW(mst) = (MIS_SUMX3(mst) - 3.0d0 * MIS_MEAN(mst) *
                MIS_SUMX2(mst) + 3.0d0 * mean * mean *
                MIS_SUMX(mst) - MIS_NPIX(mst) * mean ** 3) /
                MIS_NPIX(mst) / stdev / stdev / stdev

        if (MIS_SKURTOSIS(sw) == YES)
            MIS_KURTOSIS(mst) = (MIS_SUMX4(mst) - 4.0d0 * mean *
                MIS_SUMX3(mst) + 6.0d0 * mean * mean *
                MIS_SUMX2(mst) - 4.0 * mean ** 3 * MIS_SUMX(mst) +
                MIS_NPIX(mst) * mean ** 4) / MIS_NPIX(mst) /
                stdev / stdev / stdev / stdev - 3.0d0
end



# MST_IHIST -- Initilaize the histogram of the image pixels.

int procedure mst_ihist (mst, binwidth, hgm, nbins, hwidth, hmin, hmax)

pointer mst             #I pointer to the statistics structure
real    binwidth        #I histogram bin width in sigma
pointer hgm             #O pointer to the histogram
int     nbins           #O number of bins
real    hwidth          #O histogram resolution
real    hmin            #O minimum histogram value
real    hmax            #O maximum histogram value

begin
        nbins = 0
        if (binwidth <= 0.0)
            return (NO)

        hwidth = binwidth * MIS_STDDEV(mst)
        if (hwidth <= 0.0)
            return (NO)

        nbins = (MIS_MAX(mst) - MIS_MIN(mst)) / hwidth + 1
        if (nbins < 3)
            return (NO)

        hmin = MIS_MIN(mst)
        hmax = MIS_MAX(mst)

        call malloc (hgm, nbins, TY_INT)

        return (YES)
end


# MST_HMEDIAN -- Estimate the median from the histogram.

procedure mst_hmedian (mst, hgm, nbins, hwidth, hmin, hmax)

pointer mst             #I pointer to the statistics structure
int     hgm[ARB]        #I histogram of the pixels
int     nbins           #I number of bins in the histogram
real    hwidth          #I resolution of the histogram
real    hmin            #I minimum histogram value
real    hmax            #I maximum histogram value

real    h1, hdiff, hnorm
pointer sp, ihgm
int     i, lo, hi

bool    fp_equalr()

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

        # Approximate the median.
        h1 = hmin + lo * hwidth
        if (lo == 0)
            hdiff = Memr[ihgm+hi-1]
        else
            hdiff = Memr[ihgm+hi-1] - Memr[ihgm+lo-1]
        if (fp_equalr (hdiff, 0.0))
            MIS_MEDIAN(mst) = h1
        else if (lo == 0)
            MIS_MEDIAN(mst) = h1 + 0.5 / hdiff * hwidth
        else
            MIS_MEDIAN(mst) = h1 + (0.5 - Memr[ihgm+lo-1]) / hdiff * hwidth

        call sfree (sp)
end


# MST_HMODE -- Procedure to compute the mode.

procedure mst_hmode (mst, hgm, nbins, hwidth, hmin, hmax)

pointer mst             #I pointer to the statistics strucuture
int     hgm[ARB]        #I histogram of the pixels
int     nbins           #I number of bins in the histogram
real    hwidth          #I resolution of the histogram
real    hmin            #I minimum histogram value
real    hmax            #I maximum histogram value

int     i, bpeak
real    hpeak, dh1, dh2, denom
bool    fp_equalr()

begin
        # If there is a single bin return the midpoint of that bin.
        if (nbins == 1) {
            MIS_MODE(mst) = hmin + 0.5 * hwidth
            return
        }

        # If there are two bins return the midpoint of the greater bin.
        if (nbins == 2) {
            if (hgm[1] > hgm[2])
                MIS_MODE(mst) = hmin + 0.5 * hwidth
            else if (hgm[2] > hgm[1])
                MIS_MODE(mst) = hmin + 1.5 * hwidth
            else
                MIS_MODE(mst) = hmin + hwidth
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
            MIS_MODE(mst) = hmin + 0.5 * hwidth
            return
        }

        # If the maximum is in the last bin return the midpoint of the bin.
        if (bpeak == nbins) {
            MIS_MODE(mst) = hmin + (nbins - 0.5) * hwidth
            return
        }

        # Compute the lower limit of bpeak.
        bpeak = bpeak - 1

        # Do a parabolic interpolation to find the peak.
        dh1 = hgm[bpeak+1] - hgm[bpeak]
        dh2 = hgm[bpeak+1] - hgm[bpeak+2]
        denom = dh1 + dh2
        if (fp_equalr (denom, 0.0)) {
            MIS_MODE(mst) = hmin + (bpeak + 0.5) * hwidth
        } else {
            MIS_MODE(mst) = bpeak + 1 + 0.5 * (dh1 - dh2) / denom
            MIS_MODE(mst) = hmin + (MIS_MODE(mst) - 0.5) * hwidth
        }

        #dh1 = hgm[bpeak] * (hmin + (bpeak - 0.5) * hwidth) +
            #hgm[bpeak+1] * (hmin + (bpeak + 0.5) * hwidth) +
            #hgm[bpeak+2] * (hmin + (bpeak + 1.5) * hwidth)
        #dh2 = hgm[bpeak] + hgm[bpeak+1] + hgm[bpeak+2]
end


# MST_PRINT -- Print the fields using builtin format strings.

procedure mst_print (image, mask, mst, fields, nfields)

char    image[ARB]              #I image name
char    mask[ARB]               #I mask name
pointer mst                     #I pointer to the statistics structure
int     fields[ARB]             #I fields to be printed
int     nfields                 #I number of fields

int     i

begin
        call printf (" ")
        do i = 1, nfields {
            switch (fields[i]) {
            case MIS_FIMAGE:
                call printf (MIS_FSTRING)
                    call pargstr (image)
            case MIS_FMASK:
                call printf (MIS_FSTRING)
                    call pargstr (mask)
            case MIS_FNPIX:
                call printf (MIS_FINTEGER)
                    call pargi (MIS_NPIX(mst))
            case MIS_FMIN:
                call printf (MIS_FREAL)
                    call pargr (MIS_MIN(mst))
            case MIS_FMAX:
                call printf (MIS_FREAL)
                    call pargr (MIS_MAX(mst))
            case MIS_FMEAN:
                call printf (MIS_FREAL)
                    call pargr (MIS_MEAN(mst))
            case MIS_FMEDIAN:
                call printf (MIS_FREAL)
                    call pargr (MIS_MEDIAN(mst))
            case MIS_FMODE:
                call printf (MIS_FREAL)
                    call pargr (MIS_MODE(mst))
            case MIS_FSTDDEV:
                call printf (MIS_FREAL)
                    call pargr (MIS_STDDEV(mst))
            case MIS_FSKEW:
                call printf (MIS_FREAL)
                    call pargr (MIS_SKEW(mst))
            case MIS_FKURTOSIS:
                call printf (MIS_FREAL)
                    call pargr (MIS_KURTOSIS(mst))
            }
        }

        call printf ("\n")
        call flush (STDOUT)
end


# MST_FPRINT -- Print the fields using a free format.

procedure mst_fprint (image, mask, mst, fields, nfields)

char    image[ARB]              #I image name
char    mask[ARB]               #I mask name
pointer mst                     #I pointer to the statistics structure
int     fields[ARB]             #I fields to be printed
int     nfields                 #I number of fields

int     i

begin
        do i = 1, nfields {
            switch (fields[i]) {
            case MIS_FIMAGE:
                call printf ("%s")
                    call pargstr (image)
            case MIS_FMASK:
                call printf ("%s")
                    call pargstr (mask)
            case MIS_FNPIX:
                call printf ("%d")
                    call pargi (MIS_NPIX(mst))
            case MIS_FMIN:
                call printf ("%g")
                    call pargr (MIS_MIN(mst))
            case MIS_FMAX:
                call printf ("%g")
                    call pargr (MIS_MAX(mst))
            case MIS_FMEAN:
                call printf ("%g")
                    call pargr (MIS_MEAN(mst))
            case MIS_FMEDIAN:
                call printf ("%g")
                    call pargr (MIS_MEDIAN(mst))
            case MIS_FMODE:
                call printf ("%g")
                    call pargr (MIS_MODE(mst))
            case MIS_FSTDDEV:
                call printf ("%g")
                    call pargr (MIS_STDDEV(mst))
            case MIS_FSKEW:
                call printf ("%g")
                    call pargr (MIS_SKEW(mst))
            case MIS_FKURTOSIS:
                call printf ("%g")
                    call pargr (MIS_KURTOSIS(mst))
            }
            if (i < nfields)
                call printf ("  ")
        }

        call printf ("\n")
        call flush (STDOUT)
end
