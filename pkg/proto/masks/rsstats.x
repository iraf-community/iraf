include <mach.h>
include <imhdr.h>
include <imset.h>
include <pmset.h>
include "mimstat.h"
include "rskysub.h" 


# RS_STATS -- Compute the input image scaling factors.

procedure rs_stats (inlist, imsklist, omsklist, sclist, rs, msk_invert,
	cache, verbose)

int	inlist			#I the input image list
int	imsklist		#I the input mask list
int	omsklist		#I the output mask list
int	sclist			#I the input scale factors list
pointer	rs			#I the sky subtraction descriptor
bool	msk_invert		#I invert the pixel masks ?
bool	cache			#I cache the image i/o buffers ?
bool	verbose			#I print image statistics ?

real	fscale
pointer	sp, image, imaskname, omaskname, masktemp, str
pointer	im, ims, pmim, pmout
int	ip, old_size
real	imgetr()
pointer	immap(), im_pmmap(), mp_open()
int	imtgetim(), imtlen(), imtrgetim(), ctor(), ctowrd(), btoi()
int	fntgfnb(), imstati(), imaccess()
bool	strne(), streq()
errchk	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imaskname, SZ_FNAME, TY_CHAR)
	call salloc (omaskname, SZ_FNAME, TY_CHAR)
	call salloc (masktemp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Loop over the input images and compute the scale factors.
	# At some point we might combine this with the later running
	# mean / median loop for more efficient operation especially in an
	# observing environment but that can easily be rearranged later.

	while (imtgetim (inlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image. This image is opened READ_WRITE
	    # so some header information can be added ...
	    iferr (im = immap (Memc[image], READ_WRITE, 0)) {
		call printf ("Error opening image %s ...\n")
		    call pargstr (Memc[image])
		next
	    }

	    # Check for a statistics section. If the image image already 
	    # includes a section strip it off, append the statistics
	    # section to the input image name, and open the statistics
	    # section image.

	    if (streq (RS_ISCALES(rs), "median") && RS_STATSEC(rs) != EOS) {
	        call imgimage (Memc[image], Memc[str], SZ_FNAME)
		call strcat (RS_STATSEC(rs), Memc[str], SZ_FNAME)
	        iferr (ims = immap (Memc[str], READ_ONLY, 0)) {
		    call imunmap (im)
		    call printf ("Error opening image %s ...\n")
		        call pargstr (Memc[image])
		    next
	        }
	    } else
		ims = NULL

	    # Open input the mask if any. The input and output mask
	    # lists are ignored if the scaling factor is not median
	    # or if the list lengths are both zero.
	    if (strne (RS_ISCALES(rs), "median")) {
		pmim = NULL
		pmout = NULL
	    } else if (imtlen (omsklist) == 0 && imtlen (imsklist) == 0) {
		pmim = NULL
		pmout = NULL
	    } else {

		# Get the input mask which defaults to the empty mask if
		# there is none.
	        if (imtgetim (imsklist, Memc[str+1], SZ_FNAME) != EOF) {
		    if (msk_invert) {
		        Memc[str] = '^'
		        pmim = mp_open (Memc[str], im, Memc[imaskname],
			    SZ_FNAME)
		    } else
		        pmim = mp_open (Memc[str+1], im, Memc[imaskname],
			    SZ_FNAME)
	        } else if (imtrgetim (imsklist, 1, Memc[str],
		    SZ_FNAME) != EOF) {
		    pmim = mp_open (Memc[str], im, Memc[imaskname], SZ_FNAME)
	        } else {
		    pmim = mp_open ("", im, Memc[imaskname], SZ_FNAME)
	        }
		if (pmim == NULL) {
		    call printf ("Error reading mask for image %s ...\n")
		        call pargstr (Memc[image])
		    call imunmap (im)
		    next
		}

                # Get the output mask name if any.
                if (imtlen (omsklist) > 0) {
                    if (imtgetim (omsklist, Memc[omaskname], SZ_FNAME) == EOF) {
                        call imunmap (pmim)
                        call imunmap (im)
                        next
                    } else {
		        if (Memc[imaskname] == '^')
                            call xt_mkimtemp (Memc[imaskname+1],
			        Memc[omaskname], Memc[masktemp], SZ_FNAME)
		        else
                            call xt_mkimtemp (Memc[imaskname], Memc[omaskname],
			        Memc[masktemp], SZ_FNAME)
                        pmout = im_pmmap (Memc[omaskname], NEW_IMAGE, 0)
			call mp_mpcopy (im, pmim, pmout)
                    }
                } else {
                    pmout = NULL
		}
	    }


	    # Print title.
	    if (verbose) {
		if (pmim == NULL) {
		    call printf ("Computing scale factor for image %s\n")
			call pargstr (Memc[image])
		} else {
		    call printf (
		        "Computing scale factor for image %s using mask %s\n")
			call pargstr (Memc[image])
			call pargstr (Memc[imaskname])
		}
		call flush (STDOUT)
	    }

	    # Check for existence of scaling keyword. If the keyword is
	    # present and the rescaling flag is turned off then proceed
	    # to the next image, otherwise compute the new scale factor.

	    if (RS_RESCALE(rs) == NO) {
	        ifnoerr (fscale = imgetr (im, RS_KYFSCALE(rs))) {
		    if (verbose) {
			call printf ("    Using precomputed value %g\n")
			    call pargr (fscale)
		    }
		    call imunmap (pmim)
		    if (ims != NULL)
			call imunmap (ims)
		    call imunmap (im)
		    next
	        }
	    }

	    # Compute the scaling factor. The scaling factor defaults
	    # to, 1 if the scaling method is "none", the value of the image
	    # header keyowrd if the scaling factor is !KEYWORD, 1 / median
	    # if the the scaling methid is "median", or the value in the
	    # scaling factors file if the scaling factor is "@file". If an
	    # error occurs the scaling factor is set to 1.0.

	    if (streq (RS_ISCALES(rs), "none")) {
		fscale = 1.0
	    } else if (RS_ISCALES(rs) == '!') {
		ip = 2
		if (ctowrd (RS_ISCALES(rs), ip, Memc[str], SZ_FNAME) <= 0)
		    Memc[str] = EOS
	        iferr (fscale = imgetr (im, Memc[str]))
		    fscale = 1.0
	    } else if (streq (RS_ISCALES(rs), "median")) {
	        if (ims != NULL)
		     call rs_cache1 (btoi(cache), ims, old_size)
	        else 
		     call rs_cache1 (btoi(cache), im, old_size)
		if (pmim == NULL) {
		    if (ims != NULL)
		        call rs_med (ims, rs, fscale)
		    else
		        call rs_med (im, rs, fscale)
		} else {
		    if (ims != NULL)
		        call rs_mmed (im, ims, pmim, pmout, rs, fscale)
		    else
		        call rs_mmed (im, im, pmim, pmout, rs, fscale)
		}
		if (IS_INDEFR(fscale))
		    fscale = 1.0
		else
		    fscale = 1.0 / fscale 
		call fixmem (old_size)
	    } else if (fntgfnb (sclist, Memc[str], SZ_FNAME) != EOF) {
		ip = 1
		if (ctor (Memc[str], ip, fscale) <= 0)
		    fscale = 1.0
	    } else {
		fscale = 1.0
	    }

	    # Print the computed scaling factor.
	    if (verbose) {
		call printf ("    New scale factor is  1 / %g\n")
		    call pargr (1.0 / fscale)
		if (pmout != NULL) {
		    call printf ("    Writing new image mask %s\n")
			call pargstr (Memc[masktemp])
		}
		call flush (STDOUT)
	    }

	    # Store the new scaling factor in the input image header.
	    call imaddr (im, RS_KYFSCALE(rs), fscale)

	    # Close the input image and mask.
	    if (pmout != NULL) {
		if (imaccess (Memc[omaskname], YES) == YES)
		    call imdelete (Memc[omaskname])
		call pm_savef (imstati (pmout, IM_PMDES), Memc[omaskname],
		    "", 0)
		call imunmap (pmout)
		if (pmim != NULL)
	            call imunmap (pmim)
		call xt_delimtemp (Memc[omaskname], Memc[masktemp])
	    } else {
		if (pmim != NULL)
		    call imunmap (pmim)
	    }
	    if (ims != NULL)
		call imunmap (ims)
	    call imunmap (im)
	}

	call sfree (sp)
end


# RS_MMED -- Estimate the image median using iterative rejection and
# a pixel mask. The input image and input statistics image descriptors may
# be the same.

procedure rs_mmed (im, ims, pmim, pmout, rs, fscale)

pointer	im			#I the input image descriptor
pointer	ims			#I the input image statistics descriptor
pointer	pmim			#I the input mask image descriptor
pointer	pmout			#I the output mask image descriptor
pointer	rs			#I the sky subtraction pointer
real	fscale			#O the scaling factor

real	low, up, hmin, hmax, hwidth
pointer	sp, vs, ve, mst, pm, mp, buf, hgm, smsk
int	i, mval, npts, npix, nbins, nbad

pointer	mp_miopen()
int	imstati(), mio_glsegr(), mst_ihist(), rs_umask()

begin
	call smark (sp)
	call salloc (vs, IM_MAXDIM, TY_LONG)
	call salloc (ve, IM_MAXDIM, TY_LONG)

	# Allocate space for statistics structure.
	call mst_allocate (mst)

        # Get the selected fields.
        #nfields = mst_fields ("midpt,stddev" Memi[fields], MIS_NFIELDS)

	# Set the processing switches
	#call mst_switches (mst, Memi[fields], nfields, RS_MAXITER(rs))

	# Set up the region masking parameters.
	mp = mp_miopen (ims, pmim)

	# Compute the image statistics.
	low = RS_LOWER(rs)
	up = RS_UPPER(rs)
	do i = 0 , RS_MAXITER(rs) {

	    # Set up the mask i/o boundaries.
            call amovkl (long(1), Meml[vs], IM_NDIM(ims))
            call amovl (IM_LEN(ims,1), Meml[ve], IM_NDIM(ims))
            call mio_setrange (mp, Meml[vs], Meml[ve], IM_NDIM(ims))

	    # Initialize the statistics computation.
	    call mst_initialize (mst, low, up)

	    # Accumulate the statistics.
            while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                call mst_accumulate2 (mst, Memr[buf], npts, low, up, YES)

            # Compute the 2nd order central moment statistics.
            call mst_stats (mst)

	    # Compute new limits and iterate.
	    if (i < RS_MAXITER(rs)) {
		if (IS_INDEFR(RS_LNSIGREJ(rs)))
		    low = -MAX_REAL
		else if (RS_LNSIGREJ(rs) > 0.0 || IS_INDEFR(MIS_MEAN(mst)) ||
		    IS_INDEFR(MIS_STDDEV(mst)))
		    low = MIS_MEAN(mst) - RS_LNSIGREJ(rs) * MIS_STDDEV(mst)
		else
		    low = -MAX_REAL
		if (IS_INDEFR(RS_UNSIGREJ(rs)))
		    up = MAX_REAL
		else if (RS_UNSIGREJ(rs) > 0.0 || IS_INDEFR(MIS_MEAN(mst)) ||
		    IS_INDEFR(MIS_STDDEV(mst)))
		    up = MIS_MEAN(mst) + RS_UNSIGREJ(rs) * MIS_STDDEV(mst)
		else
		    up = MAX_REAL
		if (i > 0) {
		    if (MIS_NPIX(mst) == npix)
		        break
		}
		npix = MIS_NPIX(mst)
	    }

	}

	# Estimate the median and the mode by accumulating the histogram.
        hgm = NULL
        if (mst_ihist (mst, RS_BINWIDTH(rs), hgm, nbins, hwidth, hmin,
	    hmax) == YES) {
            call aclri (Memi[hgm], nbins)
            call amovkl (long(1), Meml[vs], IM_NDIM(ims))
            call amovl (IM_LEN(ims,1), Meml[ve], IM_NDIM(ims))
            call mio_setrange (mp, Meml[vs], Meml[ve], IM_NDIM(ims))
            while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF)
                call ahgmr (Memr[buf], npts, Memi[hgm], nbins, hmin, hmax)
            call mst_hmedian (mst, Memi[hgm], nbins, hwidth, hmin, hmax)
        }
        if (hgm != NULL)
            call mfree (hgm, TY_INT)

	# Set the statistic
	fscale = MIS_MEDIAN(mst)

	if (pmout != NULL) {
            call malloc (smsk, IM_LEN(im,1), TY_SHORT)
            call amovkl (long(1), Meml[vs], IM_NDIM(im))
            call amovl (IM_LEN(im,1), Meml[ve], IM_NDIM(im))
            call mio_setrange (mp, Meml[vs], Meml[ve], IM_NDIM(im))
            pm = imstati (pmout, IM_PMDES)
            while (mio_glsegr (mp, buf, mval, Meml[vs], npts) != EOF) {
                nbad = rs_umask (Memr[buf], Mems[smsk], npts, low, up)
                if (nbad > 0)
                    call pm_plps (pm, Meml[vs], Mems[smsk], 1, npts, PIX_SRC)
            }
            call mp_invert (pm)
            call imseti (pmout, IM_PMDES, pm)
            call mfree (smsk, TY_SHORT)
	}

	# Close the maskio descriptor.
	call mio_close (mp)

	call mst_free (mst)
	call sfree (sp)
end


# RS_MED -- Estimate the image median using iterative rejection and
# no pixel mask.

procedure rs_med (im, rs, fscale)

pointer	im			#I the input image descriptor
pointer rs			#I the sky subtraction descriptor
real	fscale			#I the computed scaling factor


real	low, up, hmin, hmax, hwidth
pointer	sp, v, mst, buf, hgm
int	i, npts, npix, nbins
int	imgnlr(), mst_ihist()

begin
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Allocate space for statistics structure.
	call mst_allocate (mst)

        # Get the selected fields.
        #nfields = mst_fields ("midpt,stddev" Memi[fields], MIS_NFIELDS)

	# Set the processing switches
	#call mst_switches (mst, Memi[fields], nfields, RS_MAXITER(rs))

	# Compute the image statistics.
	low = RS_LOWER(rs)
	up = RS_UPPER(rs)
	do i = 0 , RS_MAXITER(rs) {

	    # Initialize the statistics computation.
	    call mst_initialize (mst, low, up)

	    # Accumulate the statistics.
	    npts = IM_LEN(im,1)
            call amovkl (long(1), Meml[v], IM_NDIM(im))
            while (imgnlr (im, buf, Meml[v]) != EOF)
                call mst_accumulate2 (mst, Memr[buf], npts, low, up, YES)

            # Compute the 2nd order central moment statistics.
            call mst_stats (mst)

	    # Compute new limits and iterate.
	    if (i < RS_MAXITER(rs)) {
		if (IS_INDEFR(RS_LNSIGREJ(rs)))
		    low = -MAX_REAL
		else if (RS_LNSIGREJ(rs) > 0.0)
		    low = MIS_MEAN(mst) - RS_LNSIGREJ(rs) * MIS_STDDEV(mst)
		else
		    low = -MAX_REAL
		if (IS_INDEFR(RS_UNSIGREJ(rs)))
		    up = MAX_REAL
		else if (RS_UNSIGREJ(rs) > 0.0)
		    up = MIS_MEAN(mst) + RS_UNSIGREJ(rs) * MIS_STDDEV(mst)
		else
		    up = MAX_REAL
		if (i > 0) {
		    if (MIS_NPIX(mst) == npix)
		        break
		}
		npix = MIS_NPIX(mst)
	    }

	}

	# Estimate the median and the mode by accumulating the histogram.
        hgm = NULL
        if (mst_ihist (mst, RS_BINWIDTH(rs), hgm, nbins, hwidth, hmin,
	    hmax) == YES) {
            call aclri (Memi[hgm], nbins)
            call amovkl (long(1), Meml[v], IM_NDIM(im))
            while (imgnlr (im, buf, Meml[v]) != EOF)
                call ahgmr (Memr[buf], npts, Memi[hgm], nbins, hmin, hmax)
            call mst_hmedian (mst, Memi[hgm], nbins, hwidth, hmin, hmax)
        }
        if (hgm != NULL)
            call mfree (hgm, TY_INT)

	# Set the statistic
	fscale = MIS_MEDIAN(mst)

	call mst_free (mst)
	call sfree (sp)
end


# RS_UMASK -- Update the mask.

int procedure rs_umask (pix, msk, npts, lower, upper)

real    pix[ARB]                #I array of image pixels
short   msk[ARB]                #O array of mask pixels, set to 1 and 0
int     npts                    #I the number of pixels
real    lower                   #I the lower good data limit
real    upper                   #I the upper good data limit

real    lo, up
int     i, nbad

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
