# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include <imset.h>
include "imstat.h"


# T_IMSTATISTICS -- Compute and print the statistics of images.

procedure t_imstatistics ()

real	lower, upper, binwidth, lsigma, usigma, low, up, hwidth, hmin, hmax
pointer	sp, fieldstr, fields, image, ist, v
pointer	im, buf, hgm
int	i, list, nclip, format, nfields, nbins, npix, cache, old_size

real	clgetr()
pointer	immap()
int	imtopenp(), btoi(), ist_fields(), imtgetim(), imgnlr(), ist_ihist()
int	clgeti()
bool	clgetb()
errchk	immap()

begin
	call smark (sp)
	call salloc (fieldstr, SZ_LINE, TY_CHAR)
	call salloc (fields, IST_NFIELDS, TY_INT)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Open the list of input images, the fields and the data value limits.
	list = imtopenp ("images")
	call clgstr ("fields", Memc[fieldstr], SZ_LINE)
	lower = clgetr ("lower")
	upper = clgetr ("upper")
	nclip = clgeti ("nclip")
	lsigma = clgetr ("lsigma")
	usigma = clgetr ("usigma")
	binwidth = clgetr ("binwidth")
	format = btoi (clgetb ("format"))
	cache = btoi (clgetb ("cache"))

	# Allocate space for statistics structure
	call ist_allocate (ist)

	# Get the selected fields.
	nfields = ist_fields (Memc[fieldstr], Memi[fields], IST_NFIELDS)
	if (nfields <= 0) {
	    call imtclose (list)
	    call sfree (sp)
	    return
	}

        # Set the processing switches
        call ist_switches (ist, Memi[fields], nfields, nclip)

        # Print header banner.
	if (format == YES)
            call ist_pheader (Memi[fields], nfields)

	# Loop through the input images.
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {

	    # Open the image.
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call printf ("Error reading image %s ...\n")
		    call pargstr (Memc[image])
		next
	    }

	    if (cache == YES)
		call ist_cache1 (cache, im, old_size)
		
	    # Accumulate the central moment statistics.
	    low = lower
	    up = upper
	    do i = 0, nclip {

	        call ist_initialize (ist, low, up)
	        call amovkl (long(1), Meml[v], IM_MAXDIM)

	        if (IST_SKURTOSIS(IST_SW(ist)) == YES) {
	    	    while (imgnlr (im, buf, Meml[v]) != EOF)
		        call ist_accumulate4 (ist, Memr[buf],
			    int (IM_LEN(im, 1)), low, up,
			    IST_SMINMAX(IST_SW(ist)))
	    	} else if (IST_SSKEW(IST_SW(ist)) == YES) {
	    	    while (imgnlr (im, buf, Meml[v]) != EOF)
		        call ist_accumulate3 (ist, Memr[buf],
			    int (IM_LEN (im, 1)), low, up,
			    IST_SMINMAX(IST_SW(ist)))
	        } else if (IST_SSTDDEV(IST_SW(ist)) == YES ||
		    IST_SMEDIAN(IST_SW(ist)) == YES ||
		    IST_SMODE(IST_SW(ist)) == YES) {
	    	    while (imgnlr (im, buf, Meml[v]) != EOF)
		        call ist_accumulate2 (ist, Memr[buf],
			    int (IM_LEN(im,1)), low, up,
			    IST_SMINMAX(IST_SW(ist)))
	        } else if (IST_SMEAN(IST_SW(ist)) == YES) {
	    	    while (imgnlr (im, buf, Meml[v]) != EOF)
		        call ist_accumulate1 (ist, Memr[buf],
			    int (IM_LEN(im,1)), low, up,
			    IST_SMINMAX(IST_SW(ist)))
	        } else if (IST_SNPIX(IST_SW(ist)) == YES) {
	    	    while (imgnlr (im, buf, Meml[v]) != EOF)
		        call ist_accumulate0 (ist, Memr[buf],
			    int (IM_LEN(im,1)), low, up,
			    IST_SMINMAX(IST_SW(ist)))
	        } else if (IST_SMINMAX(IST_SW(ist)) == YES) {
	    	    while (imgnlr (im, buf, Meml[v]) != EOF)
		        call ist_accumulate0 (ist, Memr[buf],
			    int (IM_LEN(im,1)), low, up, YES)
	        }


	        # Compute the central moment statistics.
	        call ist_stats (ist)

                # Compute new limits and iterate.
                if (i < nclip) {
                    if (IS_INDEFR(lsigma) || IS_INDEFR(IST_MEAN(ist)) ||
		        IS_INDEFR(IST_STDDEV(ist)))
                        low = -MAX_REAL
                    else if (lsigma > 0.0)
                        low = IST_MEAN(ist) - lsigma * IST_STDDEV(ist)
                    else
                        low = -MAX_REAL
                    if (IS_INDEFR(usigma) || IS_INDEFR(IST_MEAN(ist)) ||
		        IS_INDEFR(IST_STDDEV(ist)))
                        up = MAX_REAL
                    else if (usigma > 0.0)
                        up = IST_MEAN(ist) + usigma * IST_STDDEV(ist)
                    else
                        up = MAX_REAL
		    if (!IS_INDEFR(lower))
		        low = max (low, lower)
		    if (!IS_INDEFR(upper))
		        up = min (up, upper)
                    if (i > 0) {
                        if (IST_NPIX(ist) == npix)
                            break
                    }
                    npix = IST_NPIX(ist)
                }

	    }

	    # Accumulate the histogram.
	    hgm = NULL
	    if ((IST_SMEDIAN(IST_SW(ist)) == YES || IST_SMODE(IST_SW(ist)) ==
	        YES) && ist_ihist (ist, binwidth, hgm, nbins, hwidth, hmin,
		hmax) == YES) {
		call aclri (Memi[hgm], nbins)
		call amovkl (long(1), Meml[v], IM_MAXDIM)
		while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ahgmr (Memr[buf], int(IM_LEN(im,1)), Memi[hgm], nbins,
		        hmin, hmax)
		if (IST_SMEDIAN(IST_SW(ist)) == YES)
		    call ist_hmedian (ist, Memi[hgm], nbins, hwidth, hmin,
			hmax)
		if (IST_SMODE(IST_SW(ist)) == YES)
		    call ist_hmode (ist, Memi[hgm], nbins, hwidth, hmin, hmax)
	    }
	    if (hgm != NULL)
		call mfree (hgm, TY_INT)

	    # Print the statistics.
	    if (format == YES)
	        call ist_print (Memc[image], "", ist, Memi[fields], nfields)
	    else
	        call ist_fprint (Memc[image], "", ist, Memi[fields], nfields)
		
	    call imunmap (im)
	    if (cache == YES)
		call fixmem (old_size)
	}

	call ist_free (ist)
	call imtclose (list)
	call sfree (sp)
end


# IST_ALLOCATE -- Allocate space for the statistics structure.

procedure ist_allocate (ist)

pointer	ist		#O the statistics descriptor

begin
    	call calloc (ist, LEN_IMSTAT, TY_STRUCT)
	call malloc (IST_SW(ist), LEN_NSWITCHES, TY_INT)
end


# IST_FREE -- Free the statistics structure.

procedure ist_free (ist)

pointer	ist		#O the statistics descriptor

begin
	call mfree (IST_SW(ist), TY_INT)
	call mfree (ist, TY_STRUCT)
end


# IST_FIELDS -- Procedure to decode the fields string into a list of the
# fields to be computed and printed.

int procedure ist_fields (fieldstr, fields, max_nfields)

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
            field = strdic (Memc[fname], Memc[fname], SZ_FNAME, IST_FIELDS)
            if (field == 0)
                next
            nfields = nfields + 1
            fields[nfields] = field
        }
        call fntclsb (flist)

        call sfree (sp)

        return (nfields)
end


# IST_SWITCHES -- Set the processing switches.

procedure ist_switches (ist, fields, nfields, nclip)

pointer	ist			#I the statistics pointer
int     fields[ARB]             #I fields array
int     nfields                 #I maximum number of fields
int	nclip			#I the number of clipping iterations

pointer	sw
int	ist_isfield()

begin
	# Initialize.
	sw = IST_SW(ist)
	call amovki (NO, Memi[sw], LEN_NSWITCHES)

        # Set the computation switches.
        IST_SNPIX(sw) = ist_isfield (IST_FNPIX, fields, nfields)
        IST_SMEAN(sw) = ist_isfield (IST_FMEAN, fields, nfields)
        IST_SMEDIAN(sw) = ist_isfield (IST_FMEDIAN, fields, nfields)
        IST_SMODE(sw) = ist_isfield (IST_FMODE, fields, nfields)
        if (nclip > 0)
            IST_SSTDDEV(sw) = YES
	else
            IST_SSTDDEV(sw) = ist_isfield (IST_FSTDDEV, fields, nfields)
        IST_SSKEW(sw) = ist_isfield (IST_FSKEW, fields, nfields)
        IST_SKURTOSIS(sw) = ist_isfield (IST_FKURTOSIS, fields, nfields)

	# Adjust the computation switches.
        if (ist_isfield (IST_FMIN, fields, nfields) == YES)
            IST_SMINMAX(sw) = YES
        else if (ist_isfield (IST_FMAX, fields, nfields) == YES)
            IST_SMINMAX(sw) = YES
        else if (IST_SMEDIAN(sw) == YES || IST_SMODE(sw) == YES)
            IST_SMINMAX(sw) = YES
        else
            IST_SMINMAX(sw) = NO
end


# IST_PHEADER -- Print the banner fields.

procedure ist_pheader (fields, nfields)

int     fields[ARB]             # fields to be printed
int     nfields                 # number of fields

int     i

begin
        call printf ("#")
        do i = 1, nfields {
            switch (fields[i]) {
            case IST_FIMAGE:
                call printf (IST_FSTRING)
                    call pargstr (IST_KIMAGE)
            case IST_FNPIX:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KNPIX)
            case IST_FMIN:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KMIN)
            case IST_FMAX:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KMAX)
            case IST_FMEAN:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KMEAN)
            case IST_FMEDIAN:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KMEDIAN)
            case IST_FMODE:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KMODE)
            case IST_FSTDDEV:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KSTDDEV)
            case IST_FSKEW:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KSKEW)
            case IST_FKURTOSIS:
                call printf (IST_FCOLUMN)
                    call pargstr (IST_KKURTOSIS)
            }
        }

        call printf ("\n")
        call flush (STDOUT)
end


# IST_ISFIELD -- Procedure to determine whether a specified field is one
# of the selected fields or not.

int procedure ist_isfield (field, fields, nfields)

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


# IST_INITIALIZE -- Initialize the statistics computation.

procedure ist_initialize (ist, lower, upper)

pointer ist             #I pointer to the statistics structure
real    lower           #I lower good data limit
real    upper           #I upper good data limit

begin
        if (IS_INDEFR(lower))
            IST_LO(ist) = -MAX_REAL
        else
            IST_LO(ist) = lower
        if (IS_INDEFR(upper))
            IST_HI(ist) = MAX_REAL
        else
            IST_HI(ist) = upper

        IST_NPIX(ist) = 0
        IST_SUMX(ist) = 0.0d0
        IST_SUMX2(ist) = 0.0d0
        IST_SUMX3(ist) = 0.0d0
        IST_SUMX4(ist) = 0.0d0

        IST_MIN(ist) = MAX_REAL
        IST_MAX(ist) = -MAX_REAL
        IST_MEAN(ist) = INDEFR
        IST_MEDIAN(ist) = INDEFR
        IST_MODE(ist) = INDEFR
        IST_STDDEV(ist) = INDEFR
        IST_SKEW(ist) = INDEFR
        IST_KURTOSIS(ist) = INDEFR
end


# IST_ACCUMULATE4 -- Accumulate sums up to the fourth power of the data for
# data values between lower and upper.

procedure ist_accumulate4 (ist, x, npts, lower, upper, minmax)

pointer ist             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

double  xx, xx2, sumx, sumx2, sumx3, sumx4
real    lo, hi, xmin, xmax
int     i, npix

begin
        lo = IST_LO(ist)
        hi = IST_HI(ist)
        npix = IST_NPIX(ist)
        sumx = 0.0
        sumx2 = 0.0
        sumx3 = 0.0
        sumx4 = 0.0
        xmin = IST_MIN(ist)
        xmax = IST_MAX(ist)

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

        IST_NPIX(ist) = npix
        IST_SUMX(ist) = IST_SUMX(ist) + sumx
        IST_SUMX2(ist) = IST_SUMX2(ist) + sumx2
        IST_SUMX3(ist) = IST_SUMX3(ist) + sumx3
        IST_SUMX4(ist) = IST_SUMX4(ist) + sumx4
        IST_MIN(ist) = xmin
        IST_MAX(ist) = xmax
end


# IST_ACCUMULATE3 -- Accumulate sums up to the third power of the data for
# data values between lower and upper.

procedure ist_accumulate3 (ist, x, npts, lower, upper, minmax)

pointer ist             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

double  xx, xx2, sumx, sumx2, sumx3
real    lo, hi, xmin, xmax
int     i, npix

begin
        lo = IST_LO(ist)
        hi = IST_HI(ist)
        npix = IST_NPIX(ist)
        sumx = 0.0
        sumx2 = 0.0
        sumx3 = 0.0
        xmin = IST_MIN(ist)
        xmax = IST_MAX(ist)

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

        IST_NPIX(ist) = npix
        IST_SUMX(ist) = IST_SUMX(ist) + sumx
        IST_SUMX2(ist) = IST_SUMX2(ist) + sumx2
        IST_SUMX3(ist) = IST_SUMX3(ist) + sumx3
        IST_MIN(ist) = xmin
        IST_MAX(ist) = xmax
end


# IST_ACCUMULATE2 -- Accumulate sums up to the second power of the data for
# data values between lower and upper.

procedure ist_accumulate2 (ist, x, npts, lower, upper, minmax)

pointer ist             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

double  xx, sumx, sumx2
real    lo, hi, xmin, xmax
int     i, npix

begin
        lo = IST_LO(ist)
        hi = IST_HI(ist)
        npix = IST_NPIX(ist)
        sumx = 0.0
        sumx2 = 0.0
        xmin = IST_MIN(ist)
        xmax = IST_MAX(ist)

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

        IST_NPIX(ist) = npix
        IST_SUMX(ist) = IST_SUMX(ist) + sumx
        IST_SUMX2(ist) = IST_SUMX2(ist) + sumx2
        IST_MIN(ist) = xmin
        IST_MAX(ist) = xmax
end


# IST_ACCUMULATE1 -- Accumulate sums up to the first power of the data for
# data values between lower and upper.

procedure ist_accumulate1 (ist, x, npts, lower, upper, minmax)

pointer ist             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

double  sumx
real    lo, hi, xx, xmin, xmax
int     i, npix

begin
        lo = IST_LO(ist)
        hi = IST_HI(ist)
        npix = IST_NPIX(ist)
        sumx = 0.0
        xmin = IST_MIN(ist)
        xmax = IST_MAX(ist)

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

        IST_NPIX(ist) = npix
        IST_SUMX(ist) = IST_SUMX(ist) + sumx
        IST_MIN(ist) = xmin
        IST_MAX(ist) = xmax
end


# IST_ACCUMULATE0 -- Accumulate sums up to the 0th power of the data for
# data values between lower and upper.

procedure ist_accumulate0 (ist, x, npts, lower, upper, minmax)

pointer ist             #I pointer to the statistics structure
real    x[ARB]          #I the data array
int     npts            #I the number of data points
real    lower           #I lower data boundary
real    upper           #I upper data boundary
int     minmax          #I compute the minimum and maximum ?

int     i, npix
real    lo, hi, xx, xmin, xmax

begin
        lo = IST_LO(ist)
        hi = IST_HI(ist)
        npix = IST_NPIX(ist)
        xmin = IST_MIN(ist)
        xmax = IST_MAX(ist)

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

        IST_NPIX(ist) = npix
        IST_MIN(ist) = xmin
        IST_MAX(ist) = xmax
end


# IST_STATS -- Procedure to compute the first four central moments of the
# distribution.

procedure ist_stats (ist)

pointer ist                     #I statistics structure

double  mean, var, stdev
pointer	sw
bool    fp_equalr()

begin
	sw = IST_SW(ist)

	# Compute the basic statistics regardless of the switches.
        if (fp_equalr (IST_MIN(ist), MAX_REAL))
            IST_MIN(ist) = INDEFR
        if (fp_equalr (IST_MAX(ist), -MAX_REAL))
            IST_MAX(ist) = INDEFR
        if (IST_NPIX(ist) <= 0)
            return

        mean = IST_SUMX(ist) / IST_NPIX(ist)
        IST_MEAN(ist) = mean
        if (IST_NPIX(ist) < 2)
            return

        var = (IST_SUMX2(ist) - IST_SUMX(ist) * mean) /
            (IST_NPIX(ist) - 1)
        if (var <= 0.0) {
            IST_STDDEV(ist) = 0.0
            return
        } else {
            stdev = sqrt (var)
            IST_STDDEV(ist) = stdev
        }

	# Compute higher order moments if the switches are set.
        if (IST_SSKEW(sw)== YES)
            IST_SKEW(ist) = (IST_SUMX3(ist) - 3.0d0 * IST_MEAN(ist) *
                IST_SUMX2(ist) + 3.0d0 * mean * mean *
                IST_SUMX(ist) - IST_NPIX(ist) * mean ** 3) /
                IST_NPIX(ist) / stdev / stdev / stdev

        if (IST_SKURTOSIS(sw) == YES)
            IST_KURTOSIS(ist) = (IST_SUMX4(ist) - 4.0d0 * mean *
                IST_SUMX3(ist) + 6.0d0 * mean * mean *
                IST_SUMX2(ist) - 4.0 * mean ** 3 * IST_SUMX(ist) +
                IST_NPIX(ist) * mean ** 4) / IST_NPIX(ist) /
                stdev / stdev / stdev / stdev - 3.0d0
end



# IST_IHIST -- Initilaize the histogram of the image pixels.

int procedure ist_ihist (ist, binwidth, hgm, nbins, hwidth, hmin, hmax)

pointer ist             #I pointer to the statistics structure
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

        hwidth = binwidth * IST_STDDEV(ist)
        if (hwidth <= 0.0)
            return (NO)

        nbins = (IST_MAX(ist) - IST_MIN(ist)) / hwidth + 1
        if (nbins < 3)
            return (NO)

        hmin = IST_MIN(ist)
        hmax = IST_MAX(ist)

        call malloc (hgm, nbins, TY_INT)

        return (YES)
end


# IST_HMEDIAN -- Estimate the median from the histogram.

procedure ist_hmedian (ist, hgm, nbins, hwidth, hmin, hmax)

pointer ist             #I pointer to the statistics structure
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
            IST_MEDIAN(ist) = h1
        else if (lo == 0)
            IST_MEDIAN(ist) = h1 + 0.5 / hdiff * hwidth
        else
            IST_MEDIAN(ist) = h1 + (0.5 - Memr[ihgm+lo-1]) / hdiff * hwidth

        call sfree (sp)
end


# IST_HMODE -- Procedure to compute the mode.

procedure ist_hmode (ist, hgm, nbins, hwidth, hmin, hmax)

pointer ist             #I pointer to the statistics strucuture
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
            IST_MODE(ist) = hmin + 0.5 * hwidth
            return
        }

        # If there are two bins return the midpoint of the greater bin.
        if (nbins == 2) {
            if (hgm[1] > hgm[2])
                IST_MODE(ist) = hmin + 0.5 * hwidth
            else if (hgm[2] > hgm[1])
                IST_MODE(ist) = hmin + 1.5 * hwidth
            else
                IST_MODE(ist) = hmin + hwidth
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
            IST_MODE(ist) = hmin + 0.5 * hwidth
            return
        }

        # If the maximum is in the last bin return the midpoint of the bin.
        if (bpeak == nbins) {
            IST_MODE(ist) = hmin + (nbins - 0.5) * hwidth
            return
        }

        # Compute the lower limit of bpeak.
        bpeak = bpeak - 1

        # Do a parabolic interpolation to find the peak.
        dh1 = hgm[bpeak+1] - hgm[bpeak]
        dh2 = hgm[bpeak+1] - hgm[bpeak+2]
        denom = dh1 + dh2
        if (fp_equalr (denom, 0.0)) {
            IST_MODE(ist) = hmin + (bpeak + 0.5) * hwidth
        } else {
            IST_MODE(ist) = bpeak + 1 + 0.5 * (dh1 - dh2) / denom
            IST_MODE(ist) = hmin + (IST_MODE(ist) - 0.5) * hwidth
        }

        #dh1 = hgm[bpeak] * (hmin + (bpeak - 0.5) * hwidth) +
            #hgm[bpeak+1] * (hmin + (bpeak + 0.5) * hwidth) +
            #hgm[bpeak+2] * (hmin + (bpeak + 1.5) * hwidth)
        #dh2 = hgm[bpeak] + hgm[bpeak+1] + hgm[bpeak+2]
end


# IST_PRINT -- Print the fields using builtin format strings.

procedure ist_print (image, mask, ist, fields, nfields)

char    image[ARB]              #I image name
char    mask[ARB]               #I mask name
pointer ist                     #I pointer to the statistics structure
int     fields[ARB]             #I fields to be printed
int     nfields                 #I number of fields

int     i

begin
        call printf (" ")
        do i = 1, nfields {
            switch (fields[i]) {
            case IST_FIMAGE:
                call printf (IST_FSTRING)
                    call pargstr (image)
            case IST_FNPIX:
                call printf (IST_FINTEGER)
                    call pargi (IST_NPIX(ist))
            case IST_FMIN:
                call printf (IST_FREAL)
                    call pargr (IST_MIN(ist))
            case IST_FMAX:
                call printf (IST_FREAL)
                    call pargr (IST_MAX(ist))
            case IST_FMEAN:
                call printf (IST_FREAL)
                    call pargr (IST_MEAN(ist))
            case IST_FMEDIAN:
                call printf (IST_FREAL)
                    call pargr (IST_MEDIAN(ist))
            case IST_FMODE:
                call printf (IST_FREAL)
                    call pargr (IST_MODE(ist))
            case IST_FSTDDEV:
                call printf (IST_FREAL)
                    call pargr (IST_STDDEV(ist))
            case IST_FSKEW:
                call printf (IST_FREAL)
                    call pargr (IST_SKEW(ist))
            case IST_FKURTOSIS:
                call printf (IST_FREAL)
                    call pargr (IST_KURTOSIS(ist))
            }
        }

        call printf ("\n")
        call flush (STDOUT)
end


# IST_FPRINT -- Print the fields using a free format.

procedure ist_fprint (image, mask, ist, fields, nfields)

char    image[ARB]              #I image name
char    mask[ARB]               #I mask name
pointer ist                     #I pointer to the statistics structure
int     fields[ARB]             #I fields to be printed
int     nfields                 #I number of fields

int     i

begin
        do i = 1, nfields {
            switch (fields[i]) {
            case IST_FIMAGE:
                call printf ("%s")
                    call pargstr (image)
            case IST_FNPIX:
                call printf ("%d")
                    call pargi (IST_NPIX(ist))
            case IST_FMIN:
                call printf ("%g")
                    call pargr (IST_MIN(ist))
            case IST_FMAX:
                call printf ("%g")
                    call pargr (IST_MAX(ist))
            case IST_FMEAN:
                call printf ("%g")
                    call pargr (IST_MEAN(ist))
            case IST_FMEDIAN:
                call printf ("%g")
                    call pargr (IST_MEDIAN(ist))
            case IST_FMODE:
                call printf ("%g")
                    call pargr (IST_MODE(ist))
            case IST_FSTDDEV:
                call printf ("%g")
                    call pargr (IST_STDDEV(ist))
            case IST_FSKEW:
                call printf ("%g")
                    call pargr (IST_SKEW(ist))
            case IST_FKURTOSIS:
                call printf ("%g")
                    call pargr (IST_KURTOSIS(ist))
            }
            if (i < nfields)
                call printf ("  ")
        }

        call printf ("\n")
        call flush (STDOUT)
end


define	MEMFUDGE	1.05

# IST_CACHE1 -- Cache 1 image in memory using the image i/o buffer sizes.

procedure ist_cache1 (cache, im, old_size)

int	cache			#I cache the image pixels in the imio buffer
pointer	im			#I the image descriptor
int	old_size		#O the old working set size

int	i, req_size, buf_size
int	sizeof(), ist_memstat()

begin
	req_size = MEMFUDGE * IM_LEN(im,1) * sizeof (IM_PIXTYPE(im))
	do i = 2, IM_NDIM(im)
	    req_size = req_size * IM_LEN(im,i)
	if (ist_memstat (cache, req_size, old_size) == YES) 
	    call ist_pcache (im, INDEFI, buf_size)
end


# IST_MEMSTAT -- Figure out if there is  enough memory to cache the image
# pixels. If it is necessary to request more memory and the memory is
# avalilable return YES otherwise return NO.

int procedure ist_memstat (cache, req_size, old_size)

int	cache			#I cache memory ?
int	req_size		#I the requested working set size in chars 
int	old_size		#O the original working set size in chars 

int	cur_size, max_size
int	begmem()

begin
        # Find the default working set size.
        cur_size = begmem (0, old_size, max_size)

	# If cacheing is disabled return NO regardless of the working set size.
	if (cache == NO)
	    return (NO)

	# If the requested working set size is less than the current working
	# set size return YES.
	if (req_size <= cur_size)
	    return (YES)

	# Reset the current working set size.
	cur_size = begmem (req_size, old_size, max_size)
	if (req_size <= cur_size) {
	    return (YES)
	} else {
	    return (NO)
	}
end


# IST_PCACHE -- Cache the image pixels im memory by resetting the  default image
# buffer size. If req_size is INDEF the size of the image is used to determine
# the size of the image i/o buffers.

procedure ist_pcache (im, req_size, buf_size)

pointer im                      #I the input image point
int     req_size                #I the requested working set size in chars
int	buf_size		#O the new image buffer size

int     i, def_size, new_imbufsize
int     sizeof(), imstati()

begin
	# Find the default buffer size.
	def_size = imstati (im, IM_BUFSIZE)

        # Compute the new required image i/o buffer size in chars.
        if (IS_INDEFI(req_size)) {
            new_imbufsize = IM_LEN(im,1) * sizeof (IM_PIXTYPE(im))
	    do i = 2, IM_NDIM(im)
		new_imbufsize = new_imbufsize * IM_LEN(im,i)
        } else {
            new_imbufsize = req_size
        }

	# If the default image i/o buffer size is already bigger than
	# the requested size do nothing.
	if (def_size >= new_imbufsize) {
	    buf_size = def_size
	    return
	}

        # Reset the image i/o buffer.
        call imseti (im, IM_BUFSIZE, new_imbufsize)
        call imseti (im, IM_BUFFRAC, 0)
	buf_size = new_imbufsize
end

