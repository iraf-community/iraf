include <imhdr.h>
include "rskysub.h"

# T_RSKYSUB  -- Sky subtract a set of input images using image scaling and
# a running statistics compution

procedure t_rskysub()

pointer	sp, imasks, str
pointer	rs
int	inlist, imsklist, outlist, omsklist, hmsklist, sclist, tmplist
bool	msk_invert, useimasks, cache, verbose

real	clgetr()
int	imtopenp(), imtopen(), imtlen(), fntopnb(), fntlenb()
int	clgeti(), btoi(), clgwrd(), rs_imlist(), rs_olist(), rs_omlist()
bool	clgetb(), strne()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (imasks, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Open the input image list. Make this a test versus nmin ?
	inlist = imtopenp ("input")
	if (imtlen (inlist) <= 0) {
	    call eprintf ("The input image list is empty\n")
	    call imtclose (inlist)
	    call sfree (sp)
	    return
	}

	# Open the output image list. The number of output images must be
	# zero equal to the number of input images.
	call clgstr ("output", Memc[str], SZ_FNAME)
	outlist = rs_olist (inlist, Memc[str], "default", "sub")
	if (imtlen (outlist) > 0 && imtlen(outlist) != imtlen(inlist)) {
	    call eprintf ("The output mask and image lists don't match\n")
	    call imtclose (outlist)
	    call imtclose (inlist)
	    call sfree (sp)
	    return
	}

	# Open the input mask list.
	call clgstr ("imasks", Memc[imasks], SZ_FNAME)
	if (Memc[imasks] == '^') {
	    #imsklist = imtopen (Memc[imasks+1])
	    imsklist = rs_imlist (inlist, Memc[imasks+1], "default", "obm")
	    msk_invert = true
	} else {
	    #imsklist = imtopen (Memc[imasks])
	    imsklist = rs_imlist (inlist, Memc[imasks], "default", "obm")
	    msk_invert = false
	}
	if (imtlen (imsklist) > 1 && imtlen (imsklist) != imtlen (inlist)) {
	    call eprintf ("The input mask and image lists don't match\n")
	    call imtclose (imsklist)
	    call imtclose (outlist)
	    call imtclose (inlist)
	    call sfree (sp)
	    return
	}

        # Open the output mask list. The number of output masks must be
        # zero equal to the number of input images.
	call clgstr ("omasks", Memc[str], SZ_FNAME)
        omsklist = rs_omlist (inlist, Memc[str], "default", "skm")
        if (imtlen (omsklist) > 0 && imtlen(omsklist) != imtlen(inlist)) {
            call eprintf ("The output mask and image lists don't match\n")
            call imtclose (omsklist)
            call imtclose (imsklist)
            call imtclose (outlist)
            call imtclose (inlist)
            call sfree (sp)
            return
        }

        # Open the output holes mask list. The number of output holes masks
	# must be zero equal to the number of input images.
	call clgstr ("hmasks", Memc[str], SZ_FNAME)
        hmsklist = rs_omlist (inlist, Memc[str], "default", "hom")
        if (imtlen (hmsklist) > 0 && imtlen(hmsklist) != imtlen(inlist)) {
            call eprintf ("The holes mask and image lists don't match\n")
            call imtclose (hmsklist)
            call imtclose (omsklist)
            call imtclose (imsklist)
            call imtclose (outlist)
            call imtclose (inlist)
            call sfree (sp)
            return
        }

	# Allocate the sky subtraction structure
	call malloc (rs, LEN_RSKYSUB, TY_STRUCT)

	# Get the scaling factor computation method.
	RS_RESCALE(rs) = btoi(clgetb ("rescale"))
	call clgstr ("scale", RS_ISCALES(rs), SZ_FNAME)
	sclist = fntopnb (RS_ISCALES(rs), NO)
	if (fntlenb (sclist) > 1 && fntlenb (sclist) != imtlen (inlist)) {
	    call eprintf ("The scaling factor and image lists don't match\n")
	    call fntclsb (sclist)
	    call imtclose (hmsklist)
	    call imtclose (omsklist)
	    call imtclose (imsklist)
	    call imtclose (outlist)
	    call imtclose (inlist)
	    call sfree (sp)
	    return
	}

	# If the scaling algorith is not "median" then new output masks 
	# cannot be created.
	if (strne (RS_ISCALES(rs), "median")) {
	    call imtclose (omsklist)
	    omsklist = imtopen ("")
	}
	call clgstr ("skyscale", RS_KYFSCALE(rs), SZ_FNAME)

	# Get statisitics computation parameters.
	useimasks = clgetb ("useimasks")
	call clgstr ("statsec", RS_STATSEC(rs), SZ_FNAME)
	RS_LOWER(rs) = clgetr ("lower")
	RS_UPPER(rs) = clgetr ("upper")
	RS_MAXITER(rs) = clgeti ("maxiter")
	RS_LNSIGREJ(rs) = clgetr ("lnsigrej")
	RS_UNSIGREJ(rs) = clgetr ("unsigrej")
	RS_BINWIDTH(rs) = clgetr ("binwidth")
	if (RS_MAXITER(rs) > 0 && IS_INDEFR(RS_LNSIGREJ(rs)) &&
	    IS_INDEFR(RS_UNSIGREJ(rs)))
	    RS_MAXITER(rs) = 0

	# Get the sky subtraction parameters
	RS_RESUBTRACT(rs) = btoi(clgetb ("resubtract"))
	RS_COMBINE(rs) = clgwrd ("combine", Memc[str], SZ_FNAME, RS_COMBINESTR)
	RS_NCOMBINE(rs) = clgeti ("ncombine")
	RS_NMIN(rs) = clgeti ("nmin")
	if (RS_NMIN(rs) <= 0 || RS_NMIN(rs) > RS_NCOMBINE(rs)) {
	    RS_NMIN(rs) = RS_NCOMBINE(rs)
	    call eprintf ("Warning: resetting nmin to %d\n")
		call pargi (RS_NMIN(rs))
	}

	# Get starting values for the rejection parameters. These may have
	# to be adjusted if image masking is enabled and for cases where
	# the number of combined images is greater then equal to nmin but
	# less than ncombine.
	RS_NLOREJ(rs) = clgeti ("nlorej")
	RS_NHIREJ(rs) = clgeti ("nhirej")
	switch (RS_COMBINE(rs)) {
	case RS_MEAN:
	    if ((RS_NMIN(rs) - RS_NLOREJ(rs) - RS_NHIREJ(rs)) < 1) {
		call eprintf ("Too many rejected pixels\n")
	        call fntclsb (sclist)
	        call imtclose (hmsklist)
	        call imtclose (omsklist)
	        call imtclose (imsklist)
	        call imtclose (outlist)
	        call imtclose (inlist)
	        call sfree (sp)
	        return
	    }
	case RS_MEDIAN:
	    if (mod (RS_NCOMBINE(rs), 2) == 0) {
		RS_NLOREJ(rs) = RS_NCOMBINE(rs) / 2 - 1
		RS_NHIREJ(rs) = RS_NCOMBINE(rs) / 2 - 1
	    } else {
		RS_NLOREJ(rs) = RS_NCOMBINE(rs) / 2
		RS_NHIREJ(rs) = RS_NCOMBINE(rs) / 2
	    }
	default:
	}
	RS_BLANK(rs) = clgetr ("blank")
	call clgstr ("skysub", RS_KYSKYSUB(rs), SZ_FNAME)
	call clgstr ("holes", RS_KYHMASK(rs), SZ_FNAME)

	cache = clgetb ("cache")
	verbose = clgetb ("verbose")

	# Compute the sky statistics and optionally the output sky masks.

	if (useimasks) {
	    call rs_stats (inlist, imsklist, omsklist, sclist, rs, msk_invert,
	        cache, verbose)
	} else {
	    tmplist = imtopen ("")
	    call rs_stats (inlist, tmplist, omsklist, sclist, rs, msk_invert,
	        cache, verbose)
	    call imtclose (tmplist)
	}

	# Do the sky subtraction with or without image masking and with or
	# without bad pixel rejection. Unmasked image medians can be handled
	# by setting the  high and low pixel rejection parameters appropriately.
	# Masked image means and medians may require dynaimc altering of the
	# high and low rejection parameters.

	switch (RS_COMBINE(rs)) {
	case RS_MEAN, RS_MEDIAN:
	    if (imtlen (omsklist) > 0) {
		if (RS_NLOREJ(rs) > 0 || RS_NHIREJ(rs) > 0)
		    # Choose which of the two routines to use later based
		    # on timing tests.
		    #call rs_prmsub (inlist, omsklist, outlist, hmsklist, rs,
		        #msk_invert, cache, verbose)
		    call rs_prrmsub (inlist, omsklist, outlist, hmsklist, rs,
		        msk_invert, cache, verbose)
		else
	            call rs_pmsub (inlist, omsklist, outlist, hmsklist, rs,
		        msk_invert, cache, verbose)
	    } else if (imtlen (imsklist) > 0) {
		if (RS_NLOREJ(rs) > 0 || RS_NHIREJ(rs) > 0)
		    # Choose which of the two routines to use later based on
		    # timing tests.
		    #call rs_prmsub (inlist, imsklist, outlist, hmsklist, rs,
		        #msk_invert, cache, verbose)
		    call rs_prrmsub (inlist, imsklist, outlist, hmsklist, rs,
		        msk_invert, cache, verbose)
		else
	            call rs_pmsub (inlist, imsklist, outlist, hmsklist, rs,
		        msk_invert, cache, verbose)
	    } else {
		if (RS_NLOREJ(rs) > 0 || RS_NHIREJ(rs) > 0)
		    # Choose which of the two routines to use later based on
		    # timing tests.
	            #call rs_rmsub (inlist, outlist, rs, cache, verbose)
	            call rs_rrmsub (inlist, outlist, rs, cache, verbose)
		else
	            call rs_msub (inlist, outlist, rs, cache, verbose)
	    }
	default:
	    ;
	}

	# Close image and file lists.
	call fntclsb (sclist)
	call imtclose (hmsklist)
	call imtclose (imsklist)
	call imtclose (omsklist)
	call imtclose (outlist)
	call imtclose (inlist)

	call mfree (rs, TY_STRUCT)
	call sfree (sp)
end

