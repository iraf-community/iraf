include <mach.h>
include <imhdr.h>
include <imset.h>
include <pmset.h>
include "mimstat.h"

# T_MIMSTATISTICS -- Compute image statistics through masks.

procedure t_mimstatistics()

real	lower, upper, lsigma, usigma, binwidth, low, up, hwidth, hmin, hmax
pointer	sp, inmasks, fieldstr, fields, image, imask, omask, masktemp, str, str2
pointer	mst, vs, ve, im, pmim, pmout, opm, mp, buf, hgm, smsk
int	i, imlist, inlist, outlist, nclip, nfields, format, mval, npts, npix
int	nbins, in_invert, nbad, cache, old_size

real	clgetr()
pointer	yt_mappm(), mp_miopen()
int	imtopenp(), imtopen(), imtlen(), imtgetim(), immap(), clgeti()
int	mst_fields(), btoi(), mio_glsegr(), mst_ihist(), imstati()
int	mst_umask(), strmatch()
bool	clgetb()
errchk	immap(), yt_mappm(), yt_pminvert()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (inmasks, SZ_FNAME, TY_CHAR)
	call salloc (fieldstr, SZ_LINE, TY_CHAR)
	call salloc (fields, MIS_NFIELDS, TY_INT)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imask, SZ_FNAME, TY_CHAR)
	call salloc (omask, SZ_FNAME, TY_CHAR)
	call salloc (masktemp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (str2, SZ_FNAME, TY_CHAR)

	call salloc (vs, IM_MAXDIM, TY_LONG)
	call salloc (ve, IM_MAXDIM, TY_LONG)

	# Open the input image list.
	imlist = imtopenp ("images")
	if (imtlen (imlist) <= 0) {
	    call eprintf ("The input image list is empty\n")
	    call imtclose (imlist)
	    call sfree (sp)
	    return
	}

	# Get the input mask specification
	call clgstr ("imasks", Memc[inmasks+1], SZ_FNAME)
	if (Memc[inmasks+1] == '^') {
	    in_invert = YES
	    inlist = imtopen (Memc[inmasks+2])
	} else {
	    in_invert = NO
	    inlist = imtopen (Memc[inmasks+1])
	}
	if (imtlen (inlist) > 1 && imtlen (inlist) != imtlen (imlist)) {
	    call eprintf ("The input mask and image lists don't match\n")
	    call imtclose (inlist)
	    call imtclose (imlist)
	    call sfree (sp)
	    return
	}

	# Open the output mask list. The number of output masks must be
	# zero equal to the number of input images.
	outlist = imtopenp ("omasks")
	if (imtlen (outlist) > 0 && imtlen(outlist) != imtlen(imlist)) {
	    call eprintf ("The output mask and image lists don't match\n")
	    call imtclose (outlist)
	    call imtclose (inlist)
	    call imtclose (imlist)
	    call sfree (sp)
	    return
	}

	# Get algorithm parameters.
	call clgstr ("fields", Memc[fieldstr], SZ_LINE)
	lower = clgetr ("lower")
	upper = clgetr ("upper")
	nclip = clgeti ("nclip")
	lsigma = clgetr ("lsigma")
	usigma = clgetr ("usigma")
	binwidth = clgetr ("binwidth")
	if (nclip > 0 && IS_INDEFR(lsigma) && IS_INDEFR(usigma))
	    nclip = 0

	# Get the other parameters.
	format = btoi(clgetb ("format"))
	cache = btoi(clgetb ("cache"))

	# Allocate space for statistics structure.
	call mst_allocate (mst)

        # Get the selected fields.
        nfields = mst_fields (Memc[fieldstr], Memi[fields], MIS_NFIELDS)
        if (nfields <= 0) {
            call imtclose (outlist)
            call imtclose (inlist)
            call imtclose (imlist)
            call sfree (sp)
            return
        }

	# Set the processing switches
	call mst_switches (mst, Memi[fields], nfields, nclip)

	if (format == YES)
	    call mst_pheader (Memi[fields], nfields)

	# Loop over the input images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image.
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		call printf ("Error reading image %s ...\n")
		    call pargstr (Memc[image])
		next
	    }

	    # Open the input mask.
	    if (imtgetim (inlist, Memc[str+1], SZ_FNAME) != EOF) {
		Memc[str] = '^'
		if (in_invert == YES)
		    pmim = yt_mappm (Memc[str+1], im, "logical",
			Memc[imask], SZ_FNAME)
		else
		    pmim = yt_mappm (Memc[str], im, "logical",
		        Memc[imask], SZ_FNAME)
	    } else if (imtlen (inlist) == 1) {
		Memc[inmasks] = '^'
		if (in_invert == YES)
		    pmim = yt_mappm (Memc[inmasks+1], im, "logical",
			Memc[imask], SZ_FNAME)
		else
		    pmim = yt_mappm (Memc[inmasks], im, "logical",
			Memc[imask], SZ_FNAME)
	    } else
		pmim = yt_mappm ("^EMPTY", im, "logical", Memc[imask], SZ_FNAME)

	    # Check the mask status and open an empty mask if there
	    # was an error.
	    if (pmim == NULL) {
		call printf ("Error reading mask for image %s ...\n")
		    call pargstr (Memc[image])
		call imunmap (im)
		next
	    }

	    # Get the output mask name if any and open a VIRTUAL output
	    # mask.
	    if (imtlen (outlist) > 0) {
		if (imtgetim (outlist, Memc[omask], SZ_FNAME) == EOF) {
		    call imunmap (pmim)
		    call imunmap (im)
		    next
		} else {
		    if (strmatch (Memc[omask], ".pl$") == 0)
			call strcat (".pl", Memc[omask], SZ_FNAME)
		    if (Memc[imask] == '^')
            	        call xt_mkimtemp (Memc[imask+1], Memc[omask],
			    Memc[masktemp], SZ_FNAME)
		    else
            	        call xt_mkimtemp (Memc[imask], Memc[omask],
			    Memc[masktemp], SZ_FNAME)
		    pmout = immap (Memc[omask], NEW_COPY, im)
		    call mp_mpcopy (im, pmim, pmout)
		}
	    } else {
		pmout = NULL
	    }

	    if (cache == YES) 
		call mst_cache1 (cache, im, old_size)

	    # Set up the input masking parameters.
	    mp = mp_miopen (im, pmim)

	    # Compute the image statistics.
	    low = lower
	    up = upper
	    do i = 0 , nclip {

		# Set up the mask i/o boundaries.
                call amovkl (long(1), Meml[vs], IM_NDIM(im))
                call amovl (IM_LEN(im,1), Meml[ve], IM_NDIM(im))
                call mio_setrange (mp, Meml[vs], Meml[ve], IM_NDIM(im))

		# Initialize the statistics computation.
		call mst_initialize (mst, low, up)

		# Accumulate the sums.
                if (MIS_SKURTOSIS(MIS_SW(mst)) == YES) {
                    while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                        call mst_accumulate4 (mst, Memr[buf], npts, low, up,
                            MIS_SMINMAX(MIS_SW(mst)))
                } else if (MIS_SSKEW(MIS_SW(mst)) == YES) {
                    while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                        call mst_accumulate3 (mst, Memr[buf], npts,
                            low, up, MIS_SMINMAX(MIS_SW(mst)))
                } else if (MIS_SSTDDEV(MIS_SW(mst)) == YES ||
		    MIS_SMEDIAN(MIS_SW(mst)) == YES ||
		    MIS_SMODE(MIS_SW(mst)) == YES) {
                    while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                        call mst_accumulate2 (mst, Memr[buf], npts,
                            low, up, MIS_SMINMAX(MIS_SW(mst)))
                } else if (MIS_SMEAN(MIS_SW(mst)) == YES) {
                    while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                        call mst_accumulate1 (mst, Memr[buf], npts,
                            low, up, MIS_SMINMAX(MIS_SW(mst)))
                } else if (MIS_SNPIX(MIS_SW(mst)) == YES) {
                    while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                        call mst_accumulate0 (mst, Memr[buf], npts,
                            low, up, MIS_SMINMAX(MIS_SW(mst)))
                } else if (MIS_SMINMAX(MIS_SW(mst)) == YES) {
                    while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                        call mst_accumulate0 (mst, Memr[buf], npts,
                            low, up, YES)
                }

                # Compute the central moment statistics.
                call mst_stats (mst)

		# Compute new limits and iterate.
		if (i < nclip) {
		    if (IS_INDEFR(lsigma) || IS_INDEFR(MIS_MEAN(mst)) ||
		        IS_INDEFR(MIS_STDDEV(mst)))
			low = -MAX_REAL
		    else if (lsigma > 0.0)
			low = MIS_MEAN(mst) - lsigma * MIS_STDDEV(mst)
		    else
			low = -MAX_REAL
		    if (IS_INDEFR(usigma) || IS_INDEFR(MIS_MEAN(mst)) ||
		        IS_INDEFR(MIS_STDDEV(mst)))
			up = MAX_REAL
		    else if (usigma > 0.0)
			up = MIS_MEAN(mst) + usigma * MIS_STDDEV(mst)
		    else
			up = MAX_REAL
		    if (!IS_INDEFR(lower))
		        low = max (low, lower)
		    if (!IS_INDEFR(upper))
		        up = min (up, upper)
		    if (i > 0) {
			if (MIS_NPIX(mst) == npix)
			    break
		    }
		    npix = MIS_NPIX(mst)
		}

	    }

	    # Estimate the median and the mode by accumulating the histogram.
            hgm = NULL
            if ((MIS_SMEDIAN(MIS_SW(mst)) == YES ||
	        MIS_SMODE(MIS_SW(mst)) == YES) && mst_ihist (mst, binwidth,
		hgm, nbins, hwidth, hmin, hmax) == YES) {
                call aclri (Memi[hgm], nbins)
                call amovkl (long(1), Meml[vs], IM_NDIM(im))
                call amovl (IM_LEN(im,1), Meml[ve], IM_NDIM(im))
                call mio_setrange (mp, Meml[vs], Meml[ve], IM_NDIM(im))
                while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                    call ahgmr (Memr[buf], npts, Memi[hgm], nbins, hmin, hmax)
                if (MIS_SMEDIAN(MIS_SW(mst)) == YES)
                    call mst_hmedian (mst, Memi[hgm], nbins, hwidth, hmin,
                        hmax)
                if (MIS_SMODE(MIS_SW(mst)) == YES)
                    call mst_hmode (mst, Memi[hgm], nbins, hwidth, hmin, hmax)
            }
            if (hgm != NULL)
                call mfree (hgm, TY_INT)

	    # Print the statistics.
            if (format == YES)
                call mst_print (Memc[image], Memc[imask], mst, Memi[fields],
		    nfields)
            else
                call mst_fprint (Memc[image], Memc[imask], mst, Memi[fields],
		    nfields)

	    # Save the new mask to an output image.
	    if (pmout != NULL) {
	        call malloc (smsk, IM_LEN(im,1), TY_SHORT)
                call amovkl (long(1), Meml[vs], IM_NDIM(im))
                call amovl (IM_LEN(im,1), Meml[ve], IM_NDIM(im))
                call mio_setrange (mp, Meml[vs], Meml[ve], IM_NDIM(im))
                call amovkl (long(1), Meml[vs], IM_NDIM(im))
		opm = imstati (pmout, IM_PMDES)
                while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF) { 
		    nbad = mst_umask (Memr[buf], Mems[smsk], npts, low, up)
		    if (nbad > 0)
	    		call pm_plps (opm, Meml[vs], Mems[smsk], 1, npts,
			    PIX_SRC)
		}
		call yt_pminvert (opm)
		call imseti (pmout, IM_PMDES, opm) 
	        call mfree (smsk, TY_SHORT)
	    }

	    # Close the images and descriptors.
	    call mio_close (mp)
	    if (pmout != NULL) {
		#call pm_savef (opm, Memc[omask], "", 0)
		call imunmap (pmout)
	        call imunmap (pmim)
		call xt_delimtemp (Memc[omask], Memc[masktemp])
	    } else
	        call imunmap (pmim)
	    call imunmap (im)
	    if (cache == YES)
		call fixmem (old_size)
	}

	call mst_free (mst)
	call imtclose (outlist)
	call imtclose (inlist)
	call imtclose (imlist)

	call sfree (sp)
end


# MST_UMASK -- Update the mask.

int procedure mst_umask (pix, msk, npts, lower, upper)

real	pix[ARB]		#I array of image pixels
short	msk[ARB]		#O array of mask pixels, set to 1 and 0
int	npts			#I the number of pixels
real	lower			#I the lower good data limit
real	upper			#I the upper good data limit

real	lo, up
int	i, nbad

begin
	if (IS_INDEFR(lower) && IS_INDEFR(upper))
	    return (0)

	if (IS_INDEFR(lower))
	    lo = -MAX_REAL
	else
	    lo = lower
	if (IS_INDEFR(upper))
	    up = MAX_REAL
	else
	    up = upper

	nbad = 0
	do i = 1, npts {
	    if (pix[i] < lo || pix[i] > up) {
		msk[i] = 0
		nbad = nbad + 1
	    } else
		msk[i] = 1
	}

	return (nbad)
end


