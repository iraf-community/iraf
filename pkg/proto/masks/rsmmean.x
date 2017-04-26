include <imhdr.h>
include <imset.h>
include <pmset.h>
include "rskysub.h"

# RS_PRRMSUB -- Perform a running mean sky subtraction on a list of images
# with masking and minmax rejection using a cylindrical buffer of image
# pointers.

procedure rs_prrmsub (inlist, msklist, outlist, hmsklist, rs, msk_invert,
	cache, verbose)

int	inlist			#I the input image list
int	msklist			#I the input mask list
int	outlist			#I the output image list
int	hmsklist		#I the output holes mask list
pointer	rs			#I the sky subtraction descriptor
bool	msk_invert		#I invert the input masks ?
bool	cache			#I cache temp image buffer in memory ?
bool	verbose			#I print task statistics

real	flow, fhigh, fscale
pointer	sp, image, imask, outimage, tmpimage, tmpmask, imptrs, mskptrs, str
pointer	hmask, imids, tmpim, tmpmsk, im, pmim, outim, hmim
int	i, imno, nlo, nhi, ostart, ofinish, start, finish, nimages, old_size
int	new_size, first, last, hstat
pointer	immap(), im_pmmap()
int	imtlen(), imtrgetim(), btoi(), imaccess(), rs_prmout(), imstati()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imask, SZ_FNAME, TY_CHAR)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (hmask, SZ_FNAME, TY_CHAR)
	call salloc (tmpimage, SZ_FNAME, TY_CHAR)
	call salloc (tmpmask, SZ_FNAME, TY_CHAR)
	call salloc (imptrs, RS_NCOMBINE(rs) + 1, TY_POINTER)
	call salloc (mskptrs, RS_NCOMBINE(rs) + 1, TY_POINTER)
	call salloc (imids, RS_NCOMBINE(rs) + 1, TY_INT)
	call salloc (str, SZ_FNAME, TY_CHAR)

        # Check image status. If resubtract is yes then delete the output
        # images if they already exist. Otherwise determine whether the
        # images already exist and if so whether or not they need to be
        # sky subtracted again.

        nimages = imtlen (inlist)
        if (RS_RESUBTRACT(rs) == NO) {
            first = 0
            last = 0
            do i = 1, nimages {
                if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
                    break
                if (imaccess (Memc[outimage], 0) == NO) {
                    if (first == 0) {
                        first = i
                        last = i
                    } else
                        last = i
                } else {
                    outim = immap (Memc[outimage], READ_ONLY, 0)
                    iferr (call imgstr (outim, RS_KYSKYSUB(rs), Memc[str],
                        SZ_FNAME)) {
                        if (first == 0) {
                            first = i
                            last = i
                        } else
                            last = i
                    }
                    call imunmap (outim)
                }
            }
        } else {
            first = 1
            last = nimages
            do i = 1, nimages {
                if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
                    break
                if (imaccess (Memc[outimage], 0) == YES)
                    call imdelete (Memc[outimage])
                if (imtrgetim (hmsklist, i, Memc[hmask], SZ_FNAME) == EOF)
                    next
                if (imaccess (Memc[hmask], 0) == YES)
                    call imdelete (Memc[hmask])
            }
        }

        # Check the sky subtraction status.
        if (first <= 0 && last <= 0) {
            if (verbose) {
                call printf (
                    "The output images have already been sky subtracted \n")
            }
            call sfree (sp)
            return
        }

	# Create the temporary image.
	call mktemp ("_rsum", Memc[tmpimage], SZ_FNAME)
	tmpim = immap (Memc[tmpimage], NEW_IMAGE, 0)

	# Make temporary mask image. Use pmmap instead of immap ? Set mask
	# to 8 bits deep to save space. This assumes no more than 255
	# images are averaged. This mask will get converted to a 1 bit
	# holes masks if holes mask are saved.
	call mktemp ("_rnpts", Memc[tmpmask], SZ_FNAME)
	tmpmsk = immap (Memc[tmpmask], NEW_IMAGE, 0)

	# Compute the sliding mean parameters.
	nlo = RS_NCOMBINE(rs) / 2
	nhi = RS_NCOMBINE(rs) - nlo

	# Loop over the images.
	ostart = 0
	ofinish = 0
	do imno = 1, nimages {

            # Skip over beginning and ending images that have already been
            # sky subtracted.

            if (imno < first || imno > last) {
                if (verbose) {
                    if (imtrgetim (outlist, imno, Memc[outimage],
                        SZ_FNAME) == EOF) {
                        call printf (
                            "The sky subtracted image %s already exists\n")
                            call pargstr (Memc[outimage])
                    }
                }
                next
            }

	    # Determine which images will contribute to the sky image.
	    # Start and finish set the endpoints of the sequence. Imno
	    # is the current image which is to be sky subtracted.

	    if ((imno - nlo) < 1) {
	        start = 1
	        finish = min (nimages, max (2 * imno - 1, RS_NMIN(rs) + 1))
	    } else if ((imno + nhi) > nimages) {
	        start = max (1, min (2 * imno - nimages, nimages - RS_NMIN(rs)))
	        finish = nimages
	    } else {
	        start = imno - nlo
	        finish = imno + nhi
	    }

	    # Check that the minimum number of images exists.
	    if ((finish - start) < RS_NMIN(rs)) {
	        call eprintf ("There are too few images for sky subtraction\n")
		break
	    }

	    # Get the input image name.
	    if (imtrgetim (inlist, imno, Memc[image], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading input image list\n")
		break
	    }

	    # Get the input mask name.
	    if (imtrgetim (msklist, imno, Memc[imask], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading input mask list\n")
		break
	    }

	    # Get the output image name.
	    if (imtrgetim (outlist, imno, Memc[outimage], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading output image list\n")
		break
	    }

	    # Get the holes mask name.
	    if (imtrgetim (hmsklist, imno, Memc[hmask], SZ_FNAME) == EOF)
		Memc[hmask] = EOS

	    if (verbose) {
		call printf (
		"Sky subtracting image %s using mask %s and writing to %s\n")
		    call pargstr (Memc[image])
		    call pargstr (Memc[imask])
		    call pargstr (Memc[outimage])
		call flush (STDOUT)
	    }

	    # Accumulate the running mean. The first time through the loop
	    # the number of dimensions, size, and pixel type of the temporary
	    # storage image and mask are set and the first set of images are
	    # accumulated into the temporary image. Attempt to cache the
	    # input image. It is probably not necessary to cache the mask
	    # since it is already in memory ...
	    if (imno == start) {
		call rs_piptrs (inlist, msklist, Memi[imptrs], Memi[mskptrs],
		    Memi[imids], start, finish, msk_invert, cache, old_size)
		IM_NDIM(tmpim) = IM_NDIM(Memi[imptrs])
		call amovl (IM_LEN(Memi[imptrs],1), IM_LEN(tmpim,1), IM_MAXDIM)
		IM_PIXTYPE(tmpim) = TY_REAL
		call rs_cachen (btoi(cache), (finish - start + 2), tmpim,
		    new_size)
		IM_NDIM(tmpmsk) = IM_NDIM(Memi[imptrs])
		call amovl (IM_LEN(Memi[imptrs],1), IM_LEN(tmpmsk,1), IM_MAXDIM)
		IM_PIXTYPE(tmpmsk) = TY_INT
		call rs_cachen (btoi(cache), (finish - start + 3), tmpmsk,
		    new_size)
	    } else {
		call rs_pasptrs (inlist, msklist, Memi[imptrs], Memi[mskptrs],
		    Memi[imids], start, finish, ostart, ofinish, msk_invert,
		    cache)
	    }

	    # Determine the input image and mask pointers.
	    im = NULL
	    pmim = NULL
	    do i = 1, finish - start + 1 {
		if (Memi[imids+i-1] != imno)
		    next
		im = Memi[imptrs+i-1]
		pmim = Memi[mskptrs+i-1]
		break
	    }

	    iferr {
	        outim = immap (Memc[outimage], NEW_COPY, im)
	    } then {
	        call eprintf ("Error opening output image %s\n")
		    call pargstr (Memc[outimage])

	    } else {

		# Cache the output image.
		call rs_cachen (btoi(cache), (finish - start + 3), outim,
		    new_size)

	        if (Memc[hmask] == EOS)
		    hmim = NULL
	        else {
		    hmim = im_pmmap (Memc[hmask], NEW_IMAGE, 0)
		    call pm_ssize (imstati(hmim, IM_PLDES), IM_NDIM(outim),
		        IM_LEN(outim,1), 1)
	        }

	        # Accumulate the sky image.
	        if (RS_COMBINE(rs) == RS_MEAN) {
		    flow = RS_NLOREJ(rs)
		    if (RS_NLOREJ(rs) >= 1)
		        flow = flow / (finish - start)
		    else
		        flow = 0.0
		    fhigh = RS_NHIREJ(rs)
		    if (RS_NHIREJ(rs) >= 1)
		        fhigh = fhigh / (finish - start)
		    else
		        fhigh = 0.0
	            call rs_apsumr (Memi[imptrs], Memi[mskptrs], Memi[imids],
		        tmpim, tmpmsk, start, finish, imno, flow, fhigh,
		        RS_KYFSCALE(rs))
	        } else {
	            call rs_apsumr (Memi[imptrs], Memi[mskptrs], Memi[imids],
		        tmpim, tmpmsk, start, finish, imno, INDEFR, INDEFR,
		        RS_KYFSCALE(rs))
	        }

		# Compute the new normalization factor.
		call rs_prmnorm (rs, im, pmim, tmpim, tmpmsk, outim, fscale)

	        # Write the output image.
	        hstat = rs_prmout (im, tmpim, tmpmsk, outim, hmim,
		    RS_BLANK(rs), fscale, RS_KYSKYSUB(rs))

	        # Close up images.
	        if (hmim != NULL) {
		    if (hstat == YES) {
		        call pm_savef (imstati (hmim, IM_PLDES), Memc[hmask],
			    "", 0)
			call imastr (outim, RS_KYHMASK(rs), Memc[hmask])
		    }
	            call imunmap (hmim)
	        }
	        call imunmap (outim)
	    }

	    # Close up remaining buffered images
	    if (imno == last) {
		do i = 1, finish - start + 1
		    call imunmap (Memi[mskptrs+i-1])
		    call imunmap (Memi[imptrs+i-1])
	    }

	    ostart = start
	    ofinish = finish

	}

	# Close and delete temporary image.
	call imunmap (tmpmsk)
	call imunmap (tmpim)
	call imdelete (Memc[tmpimage])
	call imdelete (Memc[tmpmask])

	call fixmem (old_size)

	call sfree (sp)
end


# RS_PRMSUB -- Perform a running mean sky subtraction on a list of images
# with masking and minmax rejection using input and output image lists.

procedure rs_prmsub (inlist, msklist, outlist, hmsklist, rs, msk_invert,
	cache, verbose)

int	inlist			#I the input image list
int	msklist			#I the input mask list
int	outlist			#I the output image list
int	hmsklist		#I the output mask list
pointer	rs			#I the sky subtraction descriptor
bool	msk_invert		#I invert the input masks ?
bool	cache			#I cache temp image buffer in memory ?
bool	verbose			#I print task statistics

real	flow, fhigh, fscale
pointer	sp, image, imask, outimage, tmpimage, tmpmask, hmask, str
pointer	tmpim, tmpmsk, im, pmim, outim, hmim
int	i, imno, nlo, nhi, ostart, ofinish, start, finish, nimages, old_size
int	new_size, first, last, hstat
pointer	immap(), im_pmmap(), mp_open()
int	imtlen(), imtrgetim(), btoi(), imaccess(), rs_prmout(), imstati()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imask, SZ_FNAME, TY_CHAR)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (hmask, SZ_FNAME, TY_CHAR)
	call salloc (tmpimage, SZ_FNAME, TY_CHAR)
	call salloc (tmpmask, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

        # Check image status. If resubtract is yes then delete the output
        # images if they already exist. Otherwise determine whether the
        # images already exist and if so whether or not they need to be
        # sky again.

        nimages = imtlen (inlist)
        if (RS_RESUBTRACT(rs) == NO) {
            first = 0
            last = 0
            do i = 1, nimages {
                if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
                    break
                if (imaccess (Memc[outimage], 0) == NO) {
                    if (first == 0) {
                        first = i
                        last = i
                    } else
                        last = i
                } else {
                    outim = immap (Memc[outimage], READ_ONLY, 0)
                    iferr (call imgstr (outim, RS_KYSKYSUB(rs), Memc[str],
                        SZ_FNAME)) {
                        if (first == 0) {
                            first = i
                            last = i
                        } else
                            last = i
                    }
                    call imunmap (outim)
                }
            }
        } else {
            first = 1
            last = nimages
            do i = 1, nimages {
                if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
                    break
                if (imaccess (Memc[outimage], 0) == YES)
                    call imdelete (Memc[outimage])
                if (imtrgetim (hmsklist, i, Memc[hmask], SZ_FNAME) == EOF)
                    next
                if (imaccess (Memc[hmask], 0) == YES)
                    call imdelete (Memc[hmask])
            }
        }

        # Check the sky subtraction status.
        if (first <= 0 && last <= 0) {
            if (verbose) {
                call printf (
                    "The output images have already been sky subtracted \n")
            }
            call sfree (sp)
            return
        }

	# Create the temporary image.
	call mktemp ("_rsum", Memc[tmpimage], SZ_FNAME)
	tmpim = immap (Memc[tmpimage], NEW_IMAGE, 0)

	# Make temporary mask image. Use pmmap instead of immap? Set mask
	# to 8 bits deep to save space. This assumes no more than 255
	# images are averaged. This mask will get converted to a 1 bit
	# holes masks if holes mask are saved.
	call mktemp ("_rnpts", Memc[tmpmask], SZ_FNAME)
	tmpmsk = immap (Memc[tmpmask], NEW_IMAGE, 0)

	# Compute the sliding mean parameters.
	nlo = RS_NCOMBINE(rs) / 2
	nhi = RS_NCOMBINE(rs) - nlo

	# Loop over the images.
	ostart = 0
	ofinish = 0
	do imno = 1, nimages {

            # Skip over beginning and ending images that have already been
            # sky subtracted.

            if (imno < first || imno > last) {
                if (verbose) {
                    if (imtrgetim (outlist, imno, Memc[outimage],
                        SZ_FNAME) == EOF) {
                        call printf (
                            "The sky subtracted image %s already exists\n")
                            call pargstr (Memc[outimage])
                    }
                }
                next
            }

	    # Determine which images will contribute to the sky image.
	    # Start and finish set the endpoints of the sequence. Imno
	    # is the current image which is to be sky subtracted.

	    if ((imno - nlo) < 1) {
	        start = 1
	        finish = min (nimages, max (2 * imno - 1, RS_NMIN(rs) + 1))
	    } else if ((imno + nhi) > nimages) {
	        start = max (1, min (2 * imno - nimages, nimages - RS_NMIN(rs)))
	        finish = nimages
	    } else {
	        start = imno - nlo
	        finish = imno + nhi
	    }

	    # Check that the minimum number of images exists.
	    if ((finish - start) < RS_NMIN(rs)) {
	        call eprintf ("There are too few images for sky subtraction\n")
		break
	    }

	    # Open the current input image.
	    if (imtrgetim (inlist, imno, Memc[image], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading input image list\n")
		break
	    }
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
	        call eprintf ("Error opening input image %s\n")
		    call pargstr (Memc[image])
		break
	    }

	    # Open the current input mask.
            if (imtrgetim (msklist, imno, Memc[str+1], SZ_FNAME) != EOF) {
                if (msk_invert) {
                    Memc[str] = '^'
                    pmim = mp_open (Memc[str], im, Memc[imask], SZ_FNAME)
                } else
                    pmim = mp_open (Memc[str+1], im, Memc[imask], SZ_FNAME)
            } else if (imtrgetim (msklist, 1, Memc[str], SZ_FNAME) != EOF) {
                pmim = mp_open (Memc[str], im, Memc[imask], SZ_FNAME)
            } else {
                call printf ("Error reading mask for image %s ...\n")
                    call pargstr (Memc[image])
                call imunmap (im)
                break
            }

	    # Open the output image. At present this is the combined sky image.
	    # Eventually it will be the sky subtracted input image. Assume
	    # that the input and output lists are now the same size.

	    if (imtrgetim (outlist, imno, Memc[outimage], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading output image list\n")
		call imunmap (im)
		break
	    }
	    iferr (outim = immap (Memc[outimage], NEW_COPY, im)) {
	        call eprintf ("Error opening output image %s\n")
		    call pargstr (Memc[outimage])
		call imunmap (pmim)
		call imunmap (im)
		break
	    }
	    call rs_cachen (btoi(cache), 1, outim, old_size)

	    # Open the holes mask as a virtual mask.
	    if (imtrgetim (hmsklist, imno, Memc[hmask], SZ_FNAME) != EOF) {
		hmim = im_pmmap (Memc[hmask], NEW_IMAGE, 0)
		call pm_ssize (imstati(hmim, IM_PLDES), IM_NDIM(outim),
		    IM_LEN(outim,1), 1)
	    } else {
	        hmim = NULL
	    }

	    if (verbose) {
		call printf (
		"Sky subtracting image %s using mask %s and writing to %s\n")
		    call pargstr (Memc[image])
		    call pargstr (Memc[imask])
		    call pargstr (Memc[outimage])
		call flush (STDOUT)
	    }

	    # Accumulate the running mean. The first time through the loop
	    # the number of dimensions, size, and pixel type of the temporary
	    # storage image and mask are set and the first set of images are
	    # accumulated into the temporary image. Attempt to cache the
	    # input image. It is probably not necessary to cache the mask
	    # since it is already in memory ...
	    if (imno == first) {
		IM_NDIM(tmpim) = IM_NDIM(im)
		call amovl (IM_LEN(im,1), IM_LEN(tmpim,1), IM_MAXDIM)
		IM_PIXTYPE(tmpim) = TY_REAL
		call rs_cachen (btoi(cache), 2, tmpim, new_size)
		IM_NDIM(tmpmsk) = IM_NDIM(im)
		call amovl (IM_LEN(im,1), IM_LEN(tmpmsk,1), IM_MAXDIM)
		IM_PIXTYPE(tmpmsk) = TY_INT
		call rs_cachen (btoi(cache), 3, tmpmsk, new_size)
	    }

	    # Accumulate the sky image.
	    if (RS_COMBINE(rs) == RS_MEAN) {
		flow = RS_NLOREJ(rs)
		if (RS_NLOREJ(rs) >= 1)
		    flow = flow / (finish - start)
		else
		    flow = 0.0
		fhigh = RS_NHIREJ(rs)
		if (RS_NHIREJ(rs) >= 1)
		    fhigh = fhigh / (finish - start)
		else
		    fhigh = 0.0
	        call rs_psumr (inlist, msklist, tmpim, tmpmsk, start, finish,
	            imno, flow, fhigh, msk_invert, RS_KYFSCALE(rs))
	    } else
	        call rs_psumr (inlist, msklist, tmpim, tmpmsk, start, finish,
	            imno, INDEFR, INDEFR, msk_invert, RS_KYFSCALE(rs))


	    # Compute the new normalization factor.
	    call rs_prmnorm (rs, im, pmim, tmpim, tmpmsk, outim, fscale)

	    # Write the output image.
	    hstat = rs_prmout (im, tmpim, tmpmsk, outim, hmim, RS_BLANK(rs),
	        fscale, RS_KYSKYSUB(rs))

	    # Close up images.
	    if (hmim != NULL) {
		if (hstat == YES) {
		    call pm_savef (imstati (hmim, IM_PLDES), Memc[hmask], "", 0)
		    call imastr (outim, RS_KYHMASK(rs), Memc[hmask])
		}
	        call imunmap (hmim)
	    }
	    call imunmap (outim)
	    call imunmap (pmim)
	    call imunmap (im)

	    ostart = start
	    ofinish = finish

	}

	# Close and delete temporary image.
	call imunmap (tmpmsk)
	call imunmap (tmpim)
	call imdelete (Memc[tmpimage])
	call imdelete (Memc[tmpmask])

	call fixmem (old_size)

	call sfree (sp)
end


# RS_PMSUB -- Perform a running mean sky subtraction on a list of images
# with masking but no rejection.

procedure rs_pmsub (inlist, msklist, outlist, hmsklist, rs, msk_invert,
	cache, verbose)

int	inlist			#I the input image list
int	msklist			#I the input mask list
int	outlist			#I the output image list
int	hmsklist		#I the output holes mask list
pointer	rs			#I the sky subtraction descriptor
bool	msk_invert		#I invert the input masks ?
bool	cache			#I cache temp image buffer in memory ?
bool	verbose			#I print task statistics

real	fscale
pointer	sp, image,  imask, outimage, hmask, tmpimage, tmpmask, str
pointer	tmpim, tmpmsk, im, pmim, outim, hmim
int	i, imno, nlo, nhi, ostart, ofinish, start, finish, nimages, old_size
int	new_size, first, last, hstat
pointer	immap(), mp_open(), im_pmmap()
int	imtlen(), imtrgetim(), btoi(), imaccess(), imstati(), rs_pmout()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imask, SZ_FNAME, TY_CHAR)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (hmask, SZ_FNAME, TY_CHAR)
	call salloc (tmpimage, SZ_FNAME, TY_CHAR)
	call salloc (tmpmask, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

        # Check image status. If resubtract is yes then delete the output
        # images if they already exist. Otherwise determine whether the
        # images already exist and if so whether or not they need to be
        # sky again.

        nimages = imtlen (inlist)
        if (RS_RESUBTRACT(rs) == NO) {
            first = 0
            last = 0
            do i = 1, nimages {
                if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
                    break
                if (imaccess (Memc[outimage], 0) == NO) {
                    if (first == 0) {
                        first = i
                        last = i
                    } else
                        last = i
                } else {
                    outim = immap (Memc[outimage], READ_ONLY, 0)
                    iferr (call imgstr (outim, RS_KYSKYSUB(rs), Memc[str],
                        SZ_FNAME)) {
                        if (first == 0) {
                            first = i
                            last = i
                        } else
                            last = i
                    }
                    call imunmap (outim)
                }
            }
        } else {
            first = 1
            last = nimages
            do i = 1, nimages {
                if (imtrgetim (outlist, i, Memc[outimage], SZ_FNAME) == EOF)
                    break
                if (imaccess (Memc[outimage], 0) == YES)
                    call imdelete (Memc[outimage])
                if (imtrgetim (hmsklist, i, Memc[hmask], SZ_FNAME) == EOF)
                    next
                if (imaccess (Memc[hmask], 0) == YES)
                    call imdelete (Memc[hmask])
            }
        }

        # Check the sky subtraction status.
        if (first <= 0 && last <= 0) {
            if (verbose) {
                call printf (
                    "The output images have already been sky subtracted \n")
            }
            call sfree (sp)
            return
        }


	# Create the temporary image.
	call mktemp ("_rsum", Memc[tmpimage], SZ_FNAME)
	tmpim = immap (Memc[tmpimage], NEW_IMAGE, 0)

	# Make temporary mask image. Use pmmap instead of immap? Set mask
	# to 8 bits deep to save space. This assumes no more than 255
	# images are averaged. This mask will get converted to a 1 bit
	# holes masks if holes mask are saved.
	call mktemp ("_rnpts", Memc[tmpmask], SZ_FNAME)
	tmpmsk = immap (Memc[tmpmask], NEW_IMAGE, 0)

	# Compute the sliding mean parameters.
	nlo = RS_NCOMBINE(rs) / 2
	nhi = RS_NCOMBINE(rs) - nlo

	# Loop over the images.
	ostart = 0
	ofinish = 0
	do imno = 1, nimages {

            # Skip over beginning and ending images that have already been
            # sky subtracted.

            if (imno < first || imno > last) {
                if (verbose) {
                    if (imtrgetim (outlist, imno, Memc[outimage],
                        SZ_FNAME) == EOF) {
                        call printf (
                            "The sky subtracted image %s already exists\n")
                            call pargstr (Memc[outimage])
                    }
                }
                next
            }

	    # Determine which images will contribute to the sky image.
	    # Start and finish set the endpoints of the sequence. Imno
	    # is the current image which is to be sky subtracted.

	    if ((imno - nlo) < 1) {
	        start = 1
	        finish = min (nimages, max (2 * imno - 1, RS_NMIN(rs) + 1))
	    } else if ((imno + nhi) > nimages) {
	        start = max (1, min (2 * imno - nimages, nimages - RS_NMIN(rs)))
	        finish = nimages
	    } else {
	        start = imno - nlo
	        finish = imno + nhi
	    }

	    # Check that the minimum number of images exists.
	    if ((finish - start) < RS_NMIN(rs)) {
	        call eprintf ("There are too few images for sky subtraction\n")
		break
	    }

	    # Open the current input image.
	    if (imtrgetim (inlist, imno, Memc[image], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading input image list\n")
		break
	    }
	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
	        call eprintf ("Error opening input image %s\n")
		    call pargstr (Memc[image])
		break
	    }

            # Get the input mask.
            if (imtrgetim (msklist, imno, Memc[str+1], SZ_FNAME) != EOF) {
                if (msk_invert) {
                    Memc[str] = '^'
                    pmim = mp_open (Memc[str], im, Memc[imask], SZ_FNAME)
                } else
                    pmim = mp_open (Memc[str+1], im, Memc[imask], SZ_FNAME)
            } else if (imtrgetim (msklist, 1, Memc[str], SZ_FNAME) != EOF) {
                pmim = mp_open (Memc[str], im, Memc[imask], SZ_FNAME)
            } else {
                call printf ("Error reading mask for image %s ...\n")
                    call pargstr (Memc[image])
                call imunmap (im)
                break
	    }

	    # Open the output image.
	    if (imtrgetim (outlist, imno, Memc[outimage], SZ_FNAME) == EOF) {
	        call eprintf ("Error reading output image list\n")
		call imunmap (pmim)
		call imunmap (im)
		break
	    }
	    iferr (outim = immap (Memc[outimage], NEW_COPY, im)) {
	        call eprintf ("Error opening output image %s\n")
		    call pargstr (Memc[outimage])
		call imunmap (pmim)
		call imunmap (im)
		break
	    }

	    # Open the holes mask as a virtual mask.
	    if (imtrgetim (hmsklist, imno, Memc[hmask], SZ_FNAME) != EOF) {
		hmim = im_pmmap (Memc[hmask], NEW_IMAGE, 0)
		call pm_ssize (imstati(hmim, IM_PLDES), IM_NDIM(outim),
		    IM_LEN(outim,1), 1)
	    } else {
	        hmim = NULL
	    }

	    if (verbose) {
		call printf (
		"Sky subtracting image %s using mask %s and writing to %s\n")
		    call pargstr (Memc[image])
		    call pargstr (Memc[imask])
		    call pargstr (Memc[outimage])
		call flush (STDOUT)
	    }

	    # Accumulate the running mean. The first time through the loop
	    # the number of dimensions, size, and pixel type of the temporary
	    # storage image and mask are set and the first set of images are
	    # accumulated into the temporary image. Attempt to cache the
	    # input image. It is probably not necessary to cache the mask
	    # since it is already in memory ...
	    if (imno == first) {
		IM_NDIM(tmpim) = IM_NDIM(im)
		call amovl (IM_LEN(im,1), IM_LEN(tmpim,1), IM_MAXDIM)
		IM_PIXTYPE(tmpim) = TY_REAL
		call rs_cache1 (btoi(cache), tmpim, old_size)
		IM_NDIM(tmpmsk) = IM_NDIM(im)
		call amovl (IM_LEN(im,1), IM_LEN(tmpmsk,1), IM_MAXDIM)
		IM_PIXTYPE(tmpmsk) = TY_INT
		call rs_cachen (btoi(cache), 2, tmpmsk, new_size)
		call rs_pminit (inlist, msklist, msk_invert, tmpim, tmpmsk,
		    start, finish, RS_KYFSCALE(rs))
	    } else if ((ostart > 0 && start > ostart) || (ofinish > 0 &&
	        finish > ofinish)) {
		call rs_pmaddsub (inlist, msklist, msk_invert, tmpim, tmpmsk,
		    start, finish, ostart, ofinish, RS_KYFSCALE(rs))
	    }

	    # Cache the input and output images.
	    call rs_cachen (btoi(cache), 3, im, new_size)
	    call rs_cachen (btoi(cache), 4, outim, new_size)

	    # Compute the normalization factor.
	    call rs_pmnorm (rs, im, pmim, tmpim, tmpmsk, outim, fscale)

	    # Write the output image.
	    hstat = rs_pmout (im, pmim, tmpim, tmpmsk, outim, hmim,
	        RS_BLANK(rs), fscale, RS_KYFSCALE(rs), RS_KYSKYSUB(rs))

	    # Close up images.
	    if (hmim != NULL) {
		if (hstat == YES)
		    call pm_savef (imstati(hmim, IM_PLDES), Memc[hmask], "", 0)
	        call imunmap (hmim)
	    }
	    call imunmap (outim)
	    call imunmap (pmim)
	    call imunmap (im)

	    ostart = start
	    ofinish = finish

	}

	# Close and delete temporary image.
	call imunmap (tmpmsk)
	call imunmap (tmpim)
	call imdelete (Memc[tmpimage])
	call imdelete (Memc[tmpmask])

	call fixmem (old_size)

	call sfree (sp)
end


# RS_PMINIT -- Initialize the accumulation buffer for the running median
# using masks.

procedure rs_pminit (inlist, msklist, msk_invert, tmpim, tmpmsk, start,
	finish, skyscale)

int	inlist			#I the input image list
int	msklist			#I the input mask list
bool	msk_invert		#I invert the input masks
pointer	tmpim			#I the output storage image
pointer	tmpmsk			#I the output mask counts image
int	start			#I the starting image in the list
int	finish			#I the ending image in the list
#real	normsum			#U the normalization accumulator 
char	skyscale[ARB]		#I the scaling factor keyword 

pointer	sp, image, imask, imptrs, mkptrs, mpptrs, imnorm
pointer	vout, mvout, vs, ve, vin
pointer	str, obuf, ibuf, ombuf
int	i, j, nin, npix, mval, npts
real	imgetr()
pointer	immap(), mp_open(), mio_openo()
int	imtrgetim(), impnlr(), impnli(), mio_glsegr(), imstati()
errchk	imgetr()

begin
	nin = finish - start + 1
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR) 
	call salloc (imask, SZ_FNAME, TY_CHAR) 
	call salloc (imptrs, nin, TY_INT) 
	call salloc (imnorm, nin, TY_REAL) 
	call salloc (mkptrs, nin, TY_INT) 
	call salloc (mpptrs, nin, TY_INT) 
	call salloc (vout, IM_MAXDIM, TY_LONG) 
	call salloc (mvout, IM_MAXDIM, TY_LONG) 
	call salloc (vs, IM_MAXDIM, TY_LONG) 
	call salloc (ve, IM_MAXDIM, TY_LONG) 
	call salloc (vin, IM_MAXDIM, TY_LONG) 
	call salloc (str, SZ_FNAME, TY_CHAR) 

	# Open the initial input images and masks.
	j = 1
	#normsum = 0.0
	do i = start, finish {
	    if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) == EOF)
		;
	    Memi[imptrs+j-1] = immap (Memc[image], READ_ONLY, 0)
	    if (imtrgetim (msklist, i, Memc[str+1], SZ_FNAME) != EOF) {
                if (msk_invert) {
                    Memc[str] = '^'
                    Memi[mkptrs+j-1] = mp_open (Memc[str], Memi[imptrs+j-1],
		        Memc[imask], SZ_FNAME)
                } else {
                    Memi[mkptrs+j-1] = mp_open (Memc[str+1], Memi[imptrs+j-1],
		        Memc[imask], SZ_FNAME)
	        }
	    } else if (imtrgetim (msklist, 1, Memc[str], SZ_FNAME) != EOF) {
                Memi[mkptrs+j-1] = mp_open (Memc[str], Memi[imptrs+j-1],
		    Memc[imask], SZ_FNAME)
	    } else {
                Memi[mkptrs+j-1] = mp_open ("", Memi[imptrs+j-1], Memc[imask],
		    SZ_FNAME)
	    }
	    Memi[mpptrs+j-1] = mio_openo (imstati(Memi[mkptrs+j-1], IM_PLDES),
	        Memi[imptrs+j-1])
	    iferr (Memr[imnorm+j-1] = imgetr (Memi[imptrs+j-1], skyscale))
		Memr[imnorm+j-1] = 1.0
		#normsum = normsum + 1.0
	    #else
		#normsum = normsum + Memr[imnorm+j-1]
	    j = j + 1
	}

	# Initialize image and mask i/o.
	npix = IM_LEN(tmpim,1)
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	call amovkl (long(1), Meml[mvout], IM_MAXDIM)
	call amovkl (long(1), Meml[vs], IM_MAXDIM)
	call amovkl (long(1), Meml[ve], IM_MAXDIM)
	Meml[ve] = npix

	# Do the initial accumulation of counts and numbers of pixels.
	while (impnlr (tmpim, obuf, Meml[vout]) != EOF &&
	    impnli (tmpmsk, ombuf, Meml[mvout]) != EOF) {
	    call amovkr (0.0, Memr[obuf], npix)
	    call amovki (0, Memi[ombuf], npix)
	    do j = 1, nin {
	        call mio_setrange (Memi[mpptrs+j-1], Meml[vs], Meml[ve],
		    IM_NDIM(Memi[imptrs+j-1])) 
		call amovl (Meml[vs], Meml[vin], IM_MAXDIM)
		while (mio_glsegr (Memi[mpptrs+j-1], ibuf, mval,
		    Meml[vin], npts) != EOF) {
		    call amulkr (Memr[ibuf], Memr[imnorm+j-1], Memr[ibuf],
		        npts) 
		    call aaddr (Memr[ibuf], Memr[obuf+Meml[vin]-1],
		        Memr[obuf+Meml[vin]-1], npts)
		    call aaddki (Memi[ombuf+Meml[vin]-1], 1,
		        Memi[ombuf+Meml[vin]-1], npts)
		}
	    }
	    call amovl (Meml[vout], Meml[vs], IM_MAXDIM)
	    call amovl (Meml[vout], Meml[ve], IM_MAXDIM)
	    Meml[vs] = 1
	    Meml[ve] = npix
	}

	# Close the input images.
	do j = 1, nin {
	    call mio_close (Memi[mpptrs+j-1])
	    call imunmap (Memi[mkptrs+j-1])
	    call imunmap (Memi[imptrs+j-1])
	}

	call sfree (sp)
end


# RS_PMNORM -- Compute the normalized image and the new normalization factor.

procedure rs_pmnorm (rs, im, pmim, tmpim, tmpmsk, outim, fscale)

pointer	rs			#I the sky subtraction descriptor
pointer	im			#I the input image descriptor
pointer	pmim			#I pointer to the input mask
pointer	tmpim			#I the storage image descriptor
pointer	tmpmsk			#I the counter image descriptor
pointer	outim			#I the output image descriptor
real	fscale			#O the new normalization factor

real	norm1, pout, rmin, rmax
pointer	sp, vin, vmin, vout, vtmp, vmtmp
pointer	obuf, ibuf, imbuf, tbuf, tmbuf
int	i, npix, npout
real	imgetr()
int	impnlr(), imgnlr(), imgnli()
errchk	imgetr()

begin
	call smark (sp)
	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vmin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)
	call salloc (vtmp, IM_MAXDIM, TY_LONG)
	call salloc (vmtmp, IM_MAXDIM, TY_LONG)

	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	call amovkl (long(1), Meml[vin], IM_MAXDIM)
	call amovkl (long(1), Meml[vmin], IM_MAXDIM)
	call amovkl (long(1), Meml[vtmp], IM_MAXDIM)
	call amovkl (long(1), Meml[vmtmp], IM_MAXDIM)

	# Accumulate the normalized input image.
	iferr (norm1 = imgetr (im, RS_KYFSCALE(rs)))
	    norm1 = 1.0
	npix = IM_LEN(im,1)
	while (impnlr (outim, obuf, Meml[vout]) != EOF &&
	   imgnlr (im, ibuf, Meml[vin]) != EOF &&
	   imgnli (pmim, imbuf, Meml[vmin]) != EOF &&
	   imgnlr (tmpim, tbuf, Meml[vtmp]) != EOF &&
	   imgnli (tmpmsk, tmbuf, Meml[vmtmp]) != EOF) {

	    do i = 1, npix {
		if (Memi[imbuf+i-1] > 0) {
		    pout = Memr[tbuf+i-1] - norm1 * Memr[ibuf+i-1]
		    npout = Memi[tmbuf+i-1] - 1 
		} else {
		    pout = Memr[tbuf+i-1]
		    npout = Memi[tmbuf+i-1] 
		}
		if (npout <= 0 || pout == 0.0)
		    Memr[obuf+i-1] = Memr[ibuf+i-1]
		else
		    Memr[obuf+i-1] = Memr[ibuf+i-1] / (pout / npout) 
	     }
	  
	}

	# Compute the new normalization factor.
	rmin = RS_LOWER(rs)
	rmax = RS_UPPER(rs)
	RS_LOWER(rs) = INDEFR
	RS_UPPER(rs) = INDEFR
	call rs_mmed (outim, outim, pmim, NULL, rs, fscale)
	RS_LOWER(rs) = rmin
	RS_UPPER(rs) = rmax

	call sfree (sp)
end


# RS_PMOUT -- Write the output image. Subtract the normalized current input
# image and mask from the accumulation buffers before computing the final
# average. 

int procedure rs_pmout (im, pmim, tmpim, tmpmsk, outim, hmim, blank,
	fscale, skyscale, skysub)

pointer	im			#I the input image descriptor
pointer	pmim			#I pointer to the input mask
pointer	tmpim			#I the storage image descriptor
pointer	tmpmsk			#I the counter image descriptor
pointer	outim			#I the output image descriptor
pointer	hmim			#I the output holes mask descriptor
real	blank			#I the undefined pixel value
real	fscale			#I the new normalization factor
char	skyscale[ARB]		#I the sky scaling keyword
char	skysub[ARB]		#I the sky subtraction keyword

real	norm1, pout
pointer	sp, vin, vmin, vout, vtmp, vmtmp, vs, str
pointer	obuf, ibuf, imbuf, tbuf, tmbuf, hbuf
int	i, npix, npout, stat
real	imgetr()
int	impnlr(), imgnlr(), imgnli(), imstati()
errchk	imgetr()

begin
	call smark (sp)
	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vmin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)
	call salloc (vtmp, IM_MAXDIM, TY_LONG)
	call salloc (vmtmp, IM_MAXDIM, TY_LONG)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	call amovkl (long(1), Meml[vin], IM_MAXDIM)
	call amovkl (long(1), Meml[vmin], IM_MAXDIM)
	call amovkl (long(1), Meml[vtmp], IM_MAXDIM)
	call amovkl (long(1), Meml[vmtmp], IM_MAXDIM)

	call sprintf (Memc[str], SZ_FNAME,
	    "Sky subtracted with scale factor = %g")
	    call pargr (fscale)
	call imastr (outim, skysub, Memc[str])

	iferr (norm1 = imgetr (im, skyscale))
	    norm1 = 1.0
	stat = NO
	npix = IM_LEN(im,1)
	if (hmim == NULL) {
	    while (impnlr (outim, obuf, Meml[vout]) != EOF &&
	       imgnlr (im, ibuf, Meml[vin]) != EOF &&
	       imgnli (pmim, imbuf, Meml[vmin]) != EOF &&
	       imgnlr (tmpim, tbuf, Meml[vtmp]) != EOF &&
	       imgnli (tmpmsk, tmbuf, Meml[vmtmp]) != EOF) {

	        do i = 1, npix {
		    if (Memi[imbuf+i-1] > 0) {
		        pout = Memr[tbuf+i-1] - norm1 * Memr[ibuf+i-1]
		        npout = Memi[tmbuf+i-1] - 1 
		    } else {
		        pout = Memr[tbuf+i-1]
		        npout = Memi[tmbuf+i-1] 
		    }
		    if (npout <= 0) {
		        stat = YES
		        Memr[obuf+i-1] = blank
		    } else {
		        Memr[obuf+i-1] = fscale * (pout / npout) 
		    }
	        }
	        call asubr (Memr[ibuf], Memr[obuf], Memr[obuf], npix)
	    }
	} else {
	    call salloc (vs, IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[vs], IM_MAXDIM)
	    call salloc (hbuf, npix, TY_SHORT)
	    while (impnlr (outim, obuf, Meml[vout]) != EOF &&
	       imgnlr (im, ibuf, Meml[vin]) != EOF &&
	       imgnli (pmim, imbuf, Meml[vmin]) != EOF &&
	       imgnlr (tmpim, tbuf, Meml[vtmp]) != EOF &&
	       imgnli (tmpmsk, tmbuf, Meml[vmtmp]) != EOF) {

	        do i = 1, npix {
		    if (Memi[imbuf+i-1] > 0) {
		        pout = Memr[tbuf+i-1] - norm1 * Memr[ibuf+i-1]
		        npout = Memi[tmbuf+i-1] - 1 
		    } else {
		        pout = Memr[tbuf+i-1]
		        npout = Memi[tmbuf+i-1] 
		    }
		    if (npout <= 0) {
		        stat = YES
		        Mems[hbuf+i-1] = 0
		        Memr[obuf+i-1] = blank
		    } else {
		        Mems[hbuf+i-1] = 1
		        Memr[obuf+i-1] = fscale * (pout / npout) 
		    }
	        }
	        call asubr (Memr[ibuf], Memr[obuf], Memr[obuf], npix)

	        call pm_plps (imstati(hmim, IM_PLDES), Meml[vs], Mems[hbuf],
		    1, npix, PIX_SRC)
	        call amovl (Meml[vin], Meml[vs], IM_MAXDIM)
	    }
	}

	call sfree (sp)

	return (stat)
end


# RS_PMADDSUB -- Add images to and subtract images from the accumulation 
# buffer using masks.

procedure rs_pmaddsub (inlist, msklist, msk_invert, tmpim, tmpmsk, start,
	finish, ostart, ofinish, skyscale)

int	inlist			#I the input image list
int	msklist			#I the input mask list
bool	msk_invert		#I invert the input masks
pointer	tmpim			#I the storage image descriptor
pointer	tmpmsk			#I the storage counter image
int	start			#I the current starting image
int	finish			#I the current ending image
int	ostart			#I the previous starting image
int	ofinish			#I the previous ending image
#real	normsum			#I the norm factor accumulator
char	skyscale[ARB]		#I the sky scaling keyword

pointer	sp, image, vin, vout, v, imsub, imadd, norma, norms
pointer	imask, str, mksub, mkadd, vs, ve, mpsub, mpadd, mvin, mvout
pointer	ibuf, obuf, mibuf, mobuf, sbuf, abuf
int	i, j, nsub, nadd, npix, doadd, dosub, npts, mval
real	imgetr()
pointer	immap(), mp_open(), mio_openo()
int	imtrgetim(), impnlr(), imgnlr(), impnli(), imgnli(), imstati()
int	mio_glsegr()
errchk	imgetr()

begin
	if (start == ostart && finish == ofinish)
	    return
	nsub = start - ostart
	nadd = finish - ofinish

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imask, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)
	call salloc (mvin, IM_MAXDIM, TY_LONG)
	call salloc (mvout, IM_MAXDIM, TY_LONG)
	call salloc (vs, IM_MAXDIM, TY_LONG)
	call salloc (ve, IM_MAXDIM, TY_LONG)
	call salloc (v, IM_MAXDIM, TY_LONG)

	call salloc (imsub, nsub, TY_INT)
	call salloc (mksub, nsub, TY_INT)
	call salloc (mpsub, nsub, TY_INT)
	call salloc (norms, nsub, TY_REAL)
	call salloc (imadd, nadd, TY_INT)
	call salloc (mkadd, nadd, TY_INT)
	call salloc (mpadd, nadd, TY_INT)
	call salloc (norma, nadd, TY_REAL)

	# Open the images to be subtracted. In most cases there will be
	# one such image.
	if (ostart < start) {
	    dosub = YES
	    j = 1
	    do i = ostart, start - 1 {
	        if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF) {
	    	    Memi[imsub+j-1] = immap (Memc[image], READ_ONLY, 0)
                    if (imtrgetim (msklist, i, Memc[str+1], SZ_FNAME) != EOF) {
                        if (msk_invert) {
                            Memc[str] = '^'
                            Memi[mksub+j-1] = mp_open (Memc[str],
				Memi[imsub+j-1], Memc[imask], SZ_FNAME)
                        } else
                            Memi[mksub+j-1] = mp_open (Memc[str+1],
			        Memi[imsub+j-1], Memc[imask], SZ_FNAME)
                    } else if (imtrgetim (msklist, 1, Memc[str],
			SZ_FNAME) != EOF) {
                        Memi[mksub+j-1] = mp_open (Memc[str], Memi[imsub+j-1],
                            Memc[imask], SZ_FNAME)
                    } else {
                        Memi[mksub+j-1] = mp_open ("", Memi[imsub+j-1],
			    Memc[imask], SZ_FNAME)
                    }
                    Memi[mpsub+j-1] = mio_openo (imstati(Memi[mksub+j-1],
			IM_PLDES), Memi[imsub+j-1])
	    	    iferr (Memr[norms+j-1] = imgetr (Memi[imsub+j-1], skyscale))
			Memr[norms+j-1] = 1.0
	    	    #normsum = normsum - Memr[norms+j-1]
	        }
		j = j + 1
	    }
	} else
	    dosub = NO

	# Open the images to be added. In most cases there will be one
	# such image.
	if (finish > ofinish) {
	    doadd = YES
	    j = 1
	    do i = ofinish + 1, finish {
	        if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF) {
	            Memi[imadd+j-1] = immap (Memc[image], READ_ONLY, 0)
                    if (imtrgetim (msklist, i, Memc[str+1], SZ_FNAME) != EOF) {
                        if (msk_invert) {
                            Memc[str] = '^'
                            Memi[mkadd+j-1] = mp_open (Memc[str],
				Memi[imadd+j-1], Memc[imask], SZ_FNAME)
                        } else
                            Memi[mkadd+j-1] = mp_open (Memc[str+1],
			        Memi[imadd+j-1], Memc[imask], SZ_FNAME)
                    } else if (imtrgetim (msklist, 1, Memc[str],
			SZ_FNAME) != EOF) {
                        Memi[mkadd+j-1] = mp_open (Memc[str], Memi[imadd+j-1],
                            Memc[imask], SZ_FNAME)
                    } else {
                        Memi[mkadd+j-1] = mp_open ("", Memi[imadd+j-1],
			    Memc[imask], SZ_FNAME)
                    }
                    Memi[mpadd+j-1] = mio_openo (imstati(Memi[mkadd+j-1],
			IM_PLDES), Memi[imadd+j-1])
	            iferr (Memr[norma+j-1] = imgetr (Memi[imadd+j-1], skyscale))
			Memr[norma+j-1] = 1.0
	            #normsum = normsum + Memr[norma+j-1]
		}
		j = j + 1
	    }
	} else
	    doadd = NO

	# Make the vector operators in-line code later if necessary.
	npix = IM_LEN(tmpim,1)
	call amovkl (long(1), Meml[vin], IM_MAXDIM)
	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	call amovkl (long(1), Meml[mvin], IM_MAXDIM)
	call amovkl (long(1), Meml[mvout], IM_MAXDIM)
	call amovkl (long(1), Meml[vs], IM_MAXDIM)
	call amovkl (long(1), Meml[ve], IM_MAXDIM)
	Meml[ve] = npix

	while (impnlr (tmpim, obuf, Meml[vout]) != EOF &&
	    impnli (tmpmsk, mobuf, Meml[mvout]) != EOF &&
	    imgnlr (tmpim, ibuf, Meml[vin]) != EOF &&
	    imgnli (tmpmsk, mibuf, Meml[mvin]) != EOF) {
	    call amovr (Memr[ibuf], Memr[obuf], npix)
	    call amovi (Memi[mibuf], Memi[mobuf], npix)
	    if (dosub == YES && doadd == YES) {
		do i = 1, nsub {
		    call mio_setrange (Memi[mpsub+i-1], Meml[vs], Meml[ve],
		        IM_NDIM(Memi[imsub+i-1]))
		    call amovl (Meml[vs], Meml[v], IM_MAXDIM)
		    while (mio_glsegr (Memi[mpsub+i-1], sbuf, mval, Meml[v],
		        npts) != EOF) {
		        call amulkr (Memr[sbuf], Memr[norms+i-1], Memr[sbuf],
			    npts)
		        call asubr (Memr[obuf+Meml[v]-1], Memr[sbuf],
			    Memr[obuf+Meml[v]-1], npts)
		        call asubki (Memi[mobuf+Meml[v]-1], 1,
			    Memi[mobuf+Meml[v]-1], npts)
		    }
		}
		do i = 1, nadd {
		    call mio_setrange (Memi[mpadd+i-1], Meml[vs], Meml[ve],
		        IM_NDIM(Memi[imadd+i-1]))
		    call amovl (Meml[vs], Meml[v], IM_MAXDIM)
		    while (mio_glsegr (Memi[mpadd+i-1], abuf, mval, Meml[v],
		        npts) != EOF) {
		        call amulkr (Memr[abuf], Memr[norma+i-1], Memr[abuf],
			    npts)
		        call aaddr (Memr[obuf+Meml[v]-1], Memr[abuf],
			    Memr[obuf+Meml[v]-1], npts)
		        call aaddki (Memi[mobuf+Meml[v]-1], 1,
			    Memi[mobuf+Meml[v]-1], npts)
		    }
		}
	    } else if (dosub == YES) {
		do i = 1, nsub {
		    call mio_setrange (Memi[mpsub+i-1], Meml[vs], Meml[ve],
		        IM_NDIM(Memi[imsub+i-1]))
		    call amovl (Meml[vs], Meml[v], IM_MAXDIM)
		    while (mio_glsegr (Memi[mpsub+i-1], sbuf, mval, Meml[v],
		        npts) != EOF) {
		        call amulkr (Memr[sbuf], Memr[norms+i-1], Memr[sbuf],
			    npts)
		        call asubr (Memr[obuf+Meml[v]-1], Memr[sbuf],
			    Memr[obuf+Meml[v]-1], npts)
		        call asubki (Memi[mobuf+Meml[v]-1], 1,
			    Memi[mobuf+Meml[v]-1], npts)
		    }
		}
	    } else if (doadd == YES) {
		do i = 1, nadd {
		    call mio_setrange (Memi[mpadd+i-1], Meml[vs], Meml[ve],
		        IM_NDIM(Memi[imadd+i-1]))
		    call amovl (Meml[vs], Meml[v], IM_MAXDIM)
		    while (mio_glsegr (Memi[mpadd+i-1], abuf, mval, Meml[v],
		        npts) != EOF) {
		        call amulkr (Memr[abuf], Memr[norma+i-1], Memr[abuf],
			    npts)
		        call aaddr (Memr[ibuf+Meml[v]-1], Memr[abuf],
			    Memr[obuf+Meml[v]-1], npts)
		        call aaddki (Memi[mibuf+Meml[v]-1], 1,
			    Memi[mobuf+Meml[v]-1], npts)
		    }
		}
	    }
	    call amovl (Meml[vout], Meml[vs], IM_MAXDIM)
	    call amovl (Meml[vout], Meml[ve], IM_MAXDIM)
	    Meml[vs] = 1
	    Meml[ve] = npix
	}

	# Close the images to be added or subtracted.
	do i = 1, nsub {
	    call mio_close (Memi[mpsub+i-1])
	    call imunmap (Memi[mksub+i-1])
	    call imunmap (Memi[imsub+i-1])
	}
	do i = 1, nadd {
	    call mio_close (Memi[mpadd+i-1])
	    call imunmap (Memi[mkadd+i-1])
	    call imunmap (Memi[imadd+i-1])
	}

	call sfree (sp)
end


# RS_PRMNORM -- Compute the new normalization factor.

procedure rs_prmnorm (rs, im, pmim, tmpim, tmpmsk, outim, fscale)

pointer	rs			#I the sky subtraction descriptor
pointer	im			#I the input image descriptor
pointer	pmim			#I the input image mask descriptor
pointer	tmpim			#I the storage image descriptor
pointer	tmpmsk			#I the counter image descriptor
pointer	outim			#I the output image descriptor
real	fscale			#O the new scale factor

real	rmin, rmax
pointer	sp, vin, vout, vtmp, vmtmp
pointer	obuf, ibuf, tbuf, tmbuf
int	i, npix
int	impnlr(), imgnlr(), imgnli()

begin
	call smark (sp)
	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)
	call salloc (vtmp, IM_MAXDIM, TY_LONG)
	call salloc (vmtmp, IM_MAXDIM, TY_LONG)

	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	call amovkl (long(1), Meml[vin], IM_MAXDIM)
	call amovkl (long(1), Meml[vtmp], IM_MAXDIM)
	call amovkl (long(1), Meml[vmtmp], IM_MAXDIM)

	# Accumulate the normalized image.
	npix = IM_LEN(im,1)
	while (impnlr (outim, obuf, Meml[vout]) != EOF &&
	    imgnlr (im, ibuf, Meml[vin]) != EOF &&
	    imgnlr (tmpim, tbuf, Meml[vtmp]) != EOF &&
	    imgnli (tmpmsk, tmbuf, Meml[vmtmp]) != EOF) {

	    do i = 1, npix {
	        if (Memi[tmbuf+i-1] > 0)
		    Memr[obuf+i-1] = Memr[ibuf+i-1] / Memr[tbuf+i-1]
		else
		    Memr[obuf+i-1] = Memr[ibuf+i-1]
	    }

	}

	# Compute the new normalization factor.
	rmin = RS_LOWER(rs)
	rmax = RS_UPPER(rs)
	RS_LOWER(rs) = INDEFR
	RS_UPPER(rs) = INDEFR
	call rs_mmed (outim, outim, pmim, NULL, rs, fscale)
	RS_LOWER(rs) = rmin
	RS_UPPER(rs) = rmax

	call sfree (sp)
end


# RS_PRMOUT -- Write the output image. Currently this is the sky image itself
# not the sky subtracted input image. Note that normsum is not actually
# required (I think I have now  got the normalization correct) so we may be
# able to eliminate it from the code eventually. For now keep it in case there
# is a mistake.

int procedure rs_prmout (im, tmpim, tmpmsk, outim, hmim, blank, fscale, skysub)

pointer	im			#I the input image descriptor
pointer	tmpim			#I the storage image descriptor
pointer	tmpmsk			#I the counter image descriptor
pointer	outim			#I the output image descriptor
pointer	hmim			#I the output mask descriptor
real	blank			#I the undefined pixel value
real	fscale			#I the normalization factor
char	skysub[ARB]		#I the sky subtraction keyword

pointer	sp, vin, vout, vtmp, vmtmp, vs, str, obuf, ibuf, tbuf, tmbuf, hbuf
int	i, npix, stat
int	impnlr(), imgnlr(), imgnli(), imstati()

begin
	call smark (sp)
	call salloc (vin, IM_MAXDIM, TY_LONG)
	call salloc (vout, IM_MAXDIM, TY_LONG)
	call salloc (vtmp, IM_MAXDIM, TY_LONG)
	call salloc (vmtmp, IM_MAXDIM, TY_LONG)
	call salloc (str, SZ_FNAME, TY_CHAR)

	call amovkl (long(1), Meml[vout], IM_MAXDIM)
	call amovkl (long(1), Meml[vin], IM_MAXDIM)
	call amovkl (long(1), Meml[vtmp], IM_MAXDIM)
	call amovkl (long(1), Meml[vmtmp], IM_MAXDIM)

	call sprintf (Memc[str], SZ_FNAME,
	    "Sky subtracted with scale factor = %g")
	    call pargr (fscale)
	call imastr (outim, skysub, Memc[str])

	stat = NO
	npix = IM_LEN(im,1)
	if (hmim != NULL) {
	    call salloc (hbuf, npix, TY_SHORT)
	    call salloc (vs, IM_MAXDIM, TY_LONG)
	    call amovkl (long(1), Meml[vs], IM_MAXDIM)
	    while (impnlr (outim, obuf, Meml[vout]) != EOF &&
	        imgnlr (im, ibuf, Meml[vin]) != EOF &&
	        imgnlr (tmpim, tbuf, Meml[vtmp]) != EOF &&
	        imgnli (tmpmsk, tmbuf, Meml[vmtmp]) != EOF) {

	        do i = 1, npix {
		    if (Memi[tmbuf+i-1] > 0) {
			Mems[hbuf+i-1] = 1
		        Memr[obuf+i-1] = fscale * Memr[tbuf+i-1]
		    } else {
			stat = YES
			Mems[hbuf+i-1] = 0
		        Memr[obuf+i-1] = blank
		    }
	        }
	        call asubr (Memr[ibuf], Memr[obuf], Memr[obuf], npix)

	        call pm_plps (imstati(hmim, IM_PLDES), Meml[vs], Mems[hbuf],
		    1, npix, PIX_SRC)
	        call amovl (Meml[vin], Meml[vs], IM_MAXDIM)
	    }
	} else {
	    while (impnlr (outim, obuf, Meml[vout]) != EOF &&
	        imgnlr (im, ibuf, Meml[vin]) != EOF &&
	        imgnlr (tmpim, tbuf, Meml[vtmp]) != EOF &&
	        imgnli (tmpmsk, tmbuf, Meml[vmtmp]) != EOF) {

	        do i = 1, npix {
		    if (Memi[tmbuf+i-1] > 0) {
		        Memr[obuf+i-1] = fscale * Memr[tbuf+i-1]
		    } else {
			stat = YES
		        Memr[obuf+i-1] = blank
		    }
	        }
	        call asubr (Memr[ibuf], Memr[obuf], Memr[obuf], npix)
	    }
	}

	call sfree (sp)

	return (stat)
end


# RS_PIPTRS -- Get the initial set of image and mask pointers.

procedure rs_piptrs (inlist, msklist, imptrs, mskptrs, imids, start, finish,
	msk_invert, cache, old_size)

int     inlist                          #I the input image list
int	msklist				#I the input mask list
pointer imptrs[ARB]                     #O the input image pointers
pointer	mskptrs[ARB]			#O the output mask pointers
int     imids[ARB]                      #O the input image ids
int     start                           #I the starting image in the series
int     finish                          #I the ending image in the serious
bool	msk_invert			#I invert the input masks
bool    cache                           #I cache the image in memory ?
int     old_size                        #O the original working set size

pointer sp, image, str
int     n, i, bufsize
pointer immap(), mp_open()
int     imtrgetim(), btoi()

begin
        call smark (sp)
        call salloc (image, SZ_FNAME, TY_CHAR)
        call salloc (str, SZ_FNAME, TY_CHAR)

        n = 1
        do i = start, finish {
            if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF) {
                imids[n] = i
                imptrs[n] = immap (Memc[image], READ_ONLY, 0)
                if (imtrgetim (msklist, i, Memc[str+1], SZ_FNAME) != EOF) {
                    if (msk_invert) {
                        Memc[str] = '^'
                        mskptrs[n] = mp_open (Memc[str], imptrs[n],
			    Memc[image], SZ_FNAME)
                    } else
                        mskptrs[n] = mp_open (Memc[str+1], imptrs[n],
                            Memc[image], SZ_FNAME)
                } else if (imtrgetim (msklist, 1, Memc[str], SZ_FNAME) != EOF) {
                    mskptrs[n] = mp_open (Memc[str], imptrs[n], Memc[image],
			SZ_FNAME)
                } else {
                    mskptrs[n] = mp_open ("", imptrs[n], Memc[image], SZ_FNAME)
                }
                call rs_cachen (btoi(cache), n, imptrs[n], bufsize)
                if (n == 1)
                    old_size = bufsize
                n = n + 1
            }
        }

        call sfree (sp)
end


# RS_PASPTRS -- Advance the image pointer and id buffers for the next
# current image.

procedure rs_pasptrs (inlist, msklist, imptrs, mskptrs, imids, start, finish,
	ostart, ofinish, msk_invert, cache)

int     inlist                          #I the input image list
int	msklist				#I the input mask list
pointer imptrs[ARB]                     #U the input image pointers
pointer mskptrs[ARB]                    #U the input mask pointers
int     imids[ARB]                      #U the input image ids
int     start                           #I the starting image in the series
int     finish                          #I the ending image in the serious
int     ostart                          #I the old starting image in the series
int     ofinish                         #I the old ending image in the serious
bool	msk_invert			#I invert the input masks
bool    cache                           #I cache image buffers ?

pointer sp, image, str
int     i, n, nold, nsub, nadd, bufsize
pointer immap(), mp_open()
int     imtrgetim(), btoi()

begin
        # No new images are added or deleted.
        if (start == ostart && finish == ofinish)
            return

        call smark (sp)
        call salloc (image, SZ_FNAME, TY_CHAR)
        call salloc (str, SZ_FNAME, TY_CHAR)

        nold = ofinish - start + 1

        # Delete some images and masks from the combine list.
        nsub = start - ostart
        if (nsub > 0) {
            # Unmap the images to be deleted.
            do i = 1, nsub {
                call imunmap (mskptrs[i])
                call imunmap (imptrs[i])
            }
            # Rotate the image pointer buffer.
            do i = 1, nold {
                imptrs[i] = imptrs[i+nsub]
                mskptrs[i] = mskptrs[i+nsub]
                imids[i] = imids[i+nsub]
            }
        }

        # Add new images to the combine list. Note that the cacheing
        # mechanism must include the temporarys image hence a request for
        # n + 2 cached image buffers is issued instead of a request for n.
        nadd = finish - ofinish
        if (nadd > 0) {
            n = nold + 1
            do i = ofinish + 1, finish {
                if (imtrgetim (inlist, i, Memc[image], SZ_FNAME) != EOF) {
                    imids[n] = i
                    imptrs[n] = immap (Memc[image], READ_ONLY, 0)
                    if (imtrgetim (msklist, i, Memc[str+1], SZ_FNAME) != EOF) {
                        if (msk_invert) {
                            Memc[str] = '^'
                            mskptrs[n] = mp_open (Memc[str], imptrs[n],
			        Memc[image], SZ_FNAME)
                        } else
                            mskptrs[n] = mp_open (Memc[str+1], imptrs[n],
                                Memc[image], SZ_FNAME)
                    } else if (imtrgetim (msklist, 1, Memc[str],
		        SZ_FNAME) != EOF) {
                        mskptrs[n] = mp_open (Memc[str], imptrs[n], Memc[image],
			    SZ_FNAME)
                    } else {
                        mskptrs[n] = mp_open ("", imptrs[n], Memc[image],
			    SZ_FNAME)
                    }
                    if ((finish - start) > (ofinish - ostart))
                        call rs_cachen (btoi(cache), n+2, imptrs[n], bufsize)
                    n = n + 1
                }
            }
        }

        call sfree (sp)
end

