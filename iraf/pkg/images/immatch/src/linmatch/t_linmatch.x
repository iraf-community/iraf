include <fset.h>
include <imhdr.h>
include <imset.h>
include <error.h>
include "linmatch.h"

# T_LINMATCH -- Compute the parameters required to match the intensity scale
# of an image to that of a reference image using an expression of the form
# I(ref) = a + b * I(image)

procedure t_linmatch()

pointer	freglist		#I pointer to reference regions list
pointer	database		#I pointer to database file
int	dformat			#I write the output file in database format
int	interactive		#I interactive mode ?
int	verbose			#I verbose mode

int	list1, listr, list2, reglist, reclist, stat, nregions, shiftslist
int	rpfd, ipfd, sfd
pointer	sp, reference, imager, image1, imtemp, image2, str, str1, shifts
pointer	ls, db, gd, id, imr, im1, im2
bool	clgetb()
int	imtopen(), fntopnb(), imtlen(), fntlenb(), access(), btoi(), open()
int	rg_lstati(), imtgetim(), fntgfnb(), rg_lregions(), rg_lscale()
int	rg_lrphot(), rg_liscale()
pointer	dtmap(), gopen(), immap()
real	rg_lstatr()
errchk	gopen()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate temporary space.
	call smark (sp)

	call salloc (reference, SZ_FNAME, TY_CHAR)
	call salloc (freglist, SZ_LINE, TY_CHAR)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (shifts, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)

	# Open the input and output image lists.
	call clgstr ("input", Memc[str], SZ_LINE)
	list1 = imtopen (Memc[str])
	call clgstr ("reference", Memc[reference], SZ_LINE)
	call clgstr ("regions", Memc[freglist], SZ_LINE)
	call clgstr ("lintransform", Memc[database], SZ_LINE)
	call clgstr ("output", Memc[str], SZ_LINE)
	list2 = imtopen (Memc[str])
	call clgstr ("records", Memc[str], SZ_LINE)
	if (Memc[str] == EOS)
	    reclist = NULL
	else
	    reclist = fntopnb (Memc[str], NO)
	call clgstr ("shifts", Memc[shifts], SZ_LINE)


	# Open the cross correlation fitting structure.
	call rg_glpars (ls)

	# Test the reference image list length
	if ((rg_lstati (ls, BZALGORITHM) == LS_FILE || rg_lstati(ls,
	    BSALGORITHM) == LS_FILE) || (rg_lstati(ls, BZALGORITHM) ==
	    LS_NUMBER && rg_lstati(ls, BSALGORITHM) == LS_NUMBER)) {
            listr = NULL
            reglist = NULL
	    shiftslist = NULL
            call rg_lsets (ls, REGIONS, "")
	} else if (rg_lstati(ls, BZALGORITHM) == LS_PHOTOMETRY || rg_lstati (ls,
		BSALGORITHM) == LS_PHOTOMETRY) {
	    listr = fntopnb (Memc[reference], NO)
	    if (fntlenb (listr) <= 0)
                call error (0, "The reference photometry list is empty.")
            reglist = fntopnb (Memc[freglist], NO)
            if (fntlenb (listr) > 1 && fntlenb (listr) != imtlen (list1)) {
                call eprintf ("The number of reference photometry files")
                call eprintf (" and input images is not the same.\n")
		call erract (EA_FATAL)
	    }
	    if (fntlenb(reglist) != imtlen(list1)) {
		call eprintf ("The number of input photometry files and")
		call eprintf ("images are not the same.\n")
		call erract (EA_FATAL)
	    }
	    shiftslist = NULL
            call rg_lsets (ls, REGIONS, Memc[freglist])
	} else {
	    listr = imtopen (Memc[reference])
	    if (imtlen (listr) <= 0)
                call error (0, "The reference image list is empty.")
            if (imtlen (listr) > 1 && imtlen (listr) != imtlen (list1))
                call error (0,
                "The number of reference and input images is not the same.")
            iferr {
                reglist = fntopnb (Memc[freglist], NO)
            } then
                reglist = NULL
	    if (Memc[shifts] == EOS)
	        shiftslist = NULL
	    else {
	        shiftslist = fntopnb (Memc[shifts], NO)
	        if (imtlen(listr) != fntlenb (shiftslist))
	            call error (0,
		    "The number of shifts files and images is not the same.")
	    }
            call rg_lsets (ls, REGIONS, Memc[freglist])
	}


	# Close the output image list if it is empty.
	if (imtlen (list2) <= 0) {
	    call imtclose (list2)
	    list2 = NULL
	}

	# Check that the  output image list is the same as the input image
	# list.
	if (list2 != NULL) {
	    if (imtlen (list1) != imtlen (list2))
	       call error (0,
	           "The number of input and output images are not the same.")
	}

	# Check that the record list is the same length as the input image
	# list length.
	if (reclist != NULL) {
	    if (fntlenb (reclist) != imtlen (list1))
	        call error (0,
	            "Input image and record lists are not the same length")
	}

	# Open the database file.
	dformat = btoi (clgetb ("databasefmt"))
	if (rg_lstati(ls, BZALGORITHM) == LS_FILE && rg_lstati(ls,
	    BSALGORITHM) == LS_FILE) {
	    if (dformat == YES)
	        db = dtmap (Memc[database], READ_ONLY)
	    else
	        db = open (Memc[database], READ_ONLY, TEXT_FILE)
	} else if (clgetb ("append")) {
	    if (dformat == YES)
	        db = dtmap (Memc[database], APPEND)
	    else
	        db = open (Memc[database], NEW_FILE, TEXT_FILE)
	} else if (access(Memc[database], 0, 0) == YES) {
	    call error (0, "The shifts database file already exists")
	} else {
	    if (dformat == YES)
	        db = dtmap (Memc[database], NEW_FILE)
	    else
	        db = open (Memc[database], NEW_FILE, TEXT_FILE)
	}
	call rg_lsets (ls, DATABASE, Memc[database])

	if ((rg_lstati(ls, BZALGORITHM) == LS_FILE || rg_lstati(ls,
	    BSALGORITHM) == LS_FILE) || (rg_lstati(ls, BZALGORITHM) ==
	    LS_NUMBER && rg_lstati(ls, BSALGORITHM) == LS_NUMBER))
	    interactive = NO
	else
	    interactive = btoi (clgetb ("interactive"))
	if (interactive == YES) {
	    call clgstr ("graphics", Memc[str], SZ_FNAME)
	    iferr (gd = gopen (Memc[str], NEW_FILE, STDGRAPH))
		gd = NULL
	    call clgstr ("display", Memc[str], SZ_FNAME)
	    iferr (id = gopen (Memc[str], APPEND, STDIMAGE))
		id = NULL
	    verbose = YES
	} else {
	    gd = NULL
	    id = NULL
	    verbose = btoi (clgetb ("verbose"))
	}

	# Initialize the reference image pointer.
	imr = NULL
	sfd = NULL
	rpfd = NULL
	ipfd = NULL

	# Do each set of input and output images.
	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF)) {

	    # Open the reference image and associated regions files
	    # if the correlation function is not file.
	    if (rg_lstati(ls, BZALGORITHM) == LS_PHOTOMETRY || rg_lstati(ls,
	        BSALGORITHM) == LS_PHOTOMETRY) {
		if (fntgfnb(listr, Memc[str], SZ_FNAME) != EOF) {
		    if (rpfd != NULL)
		        call close (rpfd)
		    rpfd = open (Memc[str], READ_ONLY, TEXT_FILE)
		    call rg_lsets (ls, REFIMAGE, Memc[str])
		    call rg_lsetr (ls, RGAIN, rg_lstatr (ls,GAIN))
		    call rg_lsetr (ls, RREADNOISE, rg_lstatr (ls,READNOISE))
		    nregions = rg_lrphot (rpfd, ls, 1, rg_lstati(ls,
		        MAXNREGIONS), YES) 
		    if (nregions <= 0 && interactive == NO)
		        call error (0,
			    "The reference photometry file is empty.")
		}
	    } else if ((rg_lstati(ls, BZALGORITHM) == LS_FILE || rg_lstati(ls,
	        BSALGORITHM) == LS_FILE) || (rg_lstati(ls,BZALGORITHM) ==
		LS_NUMBER && rg_lstati(ls,BSALGORITHM) == LS_NUMBER)) {
		call rg_lsets (ls, REFIMAGE, "reference")
	    } else {
		if (imtgetim(listr, Memc[str], SZ_FNAME) != EOF) {
		    if (imr != NULL)
		        call imunmap (imr)
		    imr = immap (Memc[str], READ_ONLY, 0)
		    if (IM_NDIM(imr) > 2)
			call error (0, "Referenc image must be 1D or 2D")
		    call rg_lgain (imr, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,GAIN)))
		        call rg_lsetr (ls, RGAIN, rg_lstatr (ls,GAIN))
		    call rg_lrdnoise (imr, ls)
		    if (!IS_INDEFR(rg_lstatr(ls,READNOISE)))
		        call rg_lsetr (ls, RREADNOISE, rg_lstatr (ls,READNOISE))
		    call rg_lsets (ls, REFIMAGE, Memc[str])
		    nregions = rg_lregions (reglist, imr, ls, 1, NO)
		    if (nregions <= 0 && interactive == NO)
			call error (0, "The regions list is empty.")
		    if (shiftslist != NULL) {
			if (sfd != NULL)
			    call close (sfd)
			if (fntgfnb (shiftslist, Memc[str], SZ_FNAME) == EOF) {
			    call rg_lsets (ls, SHIFTSFILE, "")
			    sfd = NULL
			} else {
			    call rg_lsets (ls, SHIFTSFILE, Memc[str])
			    sfd = open (Memc[str], READ_ONLY, TEXT_FILE)
			}
		    }
		}
	    }

	    # Open the input image.
	    if (list2 == NULL && imr == NULL) 
		im1 = NULL
	    else {
	        im1 = immap (Memc[image1], READ_ONLY, 0)
	        if (IM_NDIM(im1) > 2) {
                     call error (0, "Input images must be 1D or 2D")
                } else if (imr != NULL) {
                    if (IM_NDIM(im1) != IM_NDIM(imr)) {
                        call eprintf ("Input images must have same")
			call eprintf (" dimensionality as reference images.\n")
			call erract (EA_FATAL)
		    }
                }
	        call rg_lgain (im1, ls)
	        if (!IS_INDEFR(rg_lstatr(ls,GAIN)))
	            call rg_lsetr (ls, IGAIN, rg_lstatr (ls, GAIN))
	        call rg_lrdnoise (im1, ls)
	        if (!IS_INDEFR(rg_lstatr(ls,READNOISE)))
	            call rg_lsetr (ls, IREADNOISE, rg_lstatr (ls, READNOISE))
	    }
	    call rg_lsets (ls, IMAGE, Memc[image1])

	    # Open the input photometry file.
	    if (rpfd != NULL) {
		if (fntgfnb (reglist, Memc[str], SZ_FNAME) != EOF) {
		    ipfd = open (Memc[str], READ_ONLY, TEXT_FILE)
		    call rg_lsets (ls, PHOTFILE, Memc[str])
		}
	   	 nregions = rg_lrphot (ipfd, ls, 1, rg_lstati (ls,
		    NREGIONS), NO) 
		 if (nregions <= 0 && interactive == NO)
		    call error (0,
			"The input photometry file is empty.")
		 if (nregions < rg_lstati (ls, NREGIONS) && interactive == NO) {
		    call eprintf ("The input photometry file has fewer")
		    call eprintf (" objects than the reference photoemtry")
		    call eprintf (" file.\n")
		    call erract (EA_FATAL)
		}
	    }

	    # Open the output image if any.
	    if (list2 == NULL) {
		im2 = NULL
		Memc[image2] = EOS
	    } else if (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF) {
		call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
		    SZ_FNAME)
	        im2 = immap (Memc[image2], NEW_COPY, im1)
	    } else {
	        im2 = NULL
		Memc[image2] = EOS
	    }
	    call rg_lsets (ls, OUTIMAGE, Memc[image2])

	    # Get the record names.
	    if (reclist == NULL)
		call strcpy (Memc[image1], Memc[str], SZ_FNAME)
	    else if (fntgfnb (reclist, Memc[str], SZ_FNAME) == EOF)
		call strcpy (Memc[image1], Memc[str], SZ_FNAME)
	    call rg_lsets (ls, RECORD, Memc[str])

	    # Compute the initial shift.
	    if (sfd != NULL) {
		call rg_lgshift (sfd, ls)
	    } else {
		call rg_lsetr (ls, SXSHIFT, rg_lstatr (ls, XSHIFT))
		call rg_lsetr (ls, SYSHIFT, rg_lstatr (ls, YSHIFT))
	    }

	    # Compute the scaling factors.
	    if (interactive == YES) {
		stat = rg_liscale (imr, im1, im2, db, dformat, reglist,
		    rpfd, ipfd, sfd, ls, gd, id)
	    } else {
	        stat = rg_lscale (imr, im1, db, dformat, ls)
	        if (verbose == YES) {
		    if (rg_lstati(ls,BSALGORITHM) == LS_PHOTOMETRY ||
		        rg_lstati(ls,BZALGORITHM) == LS_PHOTOMETRY)
			call rg_lstats (ls, PHOTFILE, Memc[str1], SZ_FNAME)
		    else
			call strcpy (Memc[image1], Memc[str1], SZ_FNAME)
	            call rg_lstats (ls, REFIMAGE, Memc[str], SZ_LINE)
	            call printf (
		        "Average scale factors from %s to %s are %g %g\n")
	                call pargstr (Memc[str1])
	                call pargstr (Memc[str])
	                call pargr (rg_lstatr (ls, TBSCALE))
	                call pargr (rg_lstatr (ls, TBZERO))
	        }
	    }

	    # Scale the image.
	    if (im2 != NULL && stat == NO) {
		if (verbose == YES) {
		    call printf (
			"\tScaling image %s to image %s ...\n")
			call pargstr (Memc[image1])
			call pargstr (Memc[imtemp])
		}
		call imseti (im1, IM_CANCEL, YES)
		call rg_limscale (im1, im2, rg_lstatr (ls, TBSCALE),
		    rg_lstatr (ls, TBZERO))
	    }

	    # Close up the input and output images.
	    if (im1 != NULL)
	        call imunmap (im1)
	    if (im2 != NULL) {
		call imunmap (im2)
	        if (stat == YES)
		    call imdelete (Memc[image2])
		else
		    call xt_delimtemp (Memc[image2], Memc[imtemp])
	    }

	    if (stat == YES)
		break
	}

	# Close up the files and images.
	if (imr != NULL)
	    call imunmap (imr)

	# Close up the lists.
	if (list1 != NULL)
	    call imtclose (list1)
	if (listr != NULL) {
	    if (rg_lstati (ls, BZALGORITHM) == LS_PHOTOMETRY || rg_lstati(ls,
		BSALGORITHM) == LS_PHOTOMETRY)
	        call fntclsb (listr)
	    else
	        call imtclose (listr)
	}
	if (list2 != NULL)
	    call imtclose (list2)
	if (sfd != NULL)
	    call close (sfd)
	if (rpfd != NULL)
	    call close (rpfd)
	if (ipfd != NULL)
	    call close (ipfd)
	if (shiftslist != NULL)
	    call fntclsb (shiftslist)
	if (reglist != NULL)
	    call fntclsb (reglist)
	if (reclist != NULL)
	    call fntclsb (reclist)
	if (dformat == YES)
	    call dtunmap (db)
	else
	    call close (db)

	# Close up the graphics and image display devices.
	if (gd != NULL)
	    call gclose (gd)
	if (id != NULL)
	    call gclose (id)

	# Free the matching structure.
	call rg_lfree (ls)

	call sfree (sp)
end


# RG_LGAIN -- Fetch the gain parameter from the image header.

procedure rg_lgain (im, ls)

pointer	im		#I pointer to the input image
pointer	ls		#I pointer to the intensity matching structure

int	ip
pointer	sp, key
real	epadu
int	ctor()
real	imgetr()
errchk	imgetr()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	call rg_lstats (ls, CCDGAIN, Memc[key], SZ_FNAME)
	ip = 1
	if (ctor (Memc[key], ip, epadu) <= 0) {
	    iferr {
	        epadu = imgetr (im, Memc[key])
	    } then {
		epadu = INDEFR
		call eprintf ("Warning: Image %s  Keyword %s not found.\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	} else
	    epadu = INDEFR
	if (IS_INDEFR(epadu) || epadu <= 0.0)
	    call rg_lsetr (ls, GAIN, INDEFR)
	Else
	    call rg_lsetr (ls, GAIN, epadu)

	call sfree (sp)
end


# LG_LRDNOISE -- Fetch the readout noise from the image header.

procedure rg_lrdnoise (im, ls)

pointer	im		#I pointer to the input image
pointer	ls		#I pointer to the intensity matching structure

int	ip
pointer	sp, key
real	rdnoise
int	ctor()
real	imgetr()
errchk	imgetr()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	call rg_lstats (ls, CCDREAD, Memc[key], SZ_FNAME)
	ip = 1
	if (ctor (Memc[key], ip, rdnoise) <= 0) {
	    iferr {
	        rdnoise = imgetr (im, Memc[key])
	    } then {
		rdnoise = INDEFR
		call eprintf ("Warning: Image %s  Keyword %s not found.\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (Memc[key])
	    }
	} else
	    rdnoise = INDEFR
	if (IS_INDEFR(rdnoise) || rdnoise <= 0.0)
	    call rg_lsetr (ls, READNOISE, INDEFR)
	else
	    call rg_lsetr (ls, READNOISE, rdnoise)

	call sfree (sp)
end


# RG_LGSHIFT -- Read the x and y shifts from a file

procedure rg_lgshift (fd, ls)

int	fd		#I input shifts file descriptor
pointer	ls		#I pointer to the intensity matching structure

real	xshift, yshift
int	fscan(), nscan()

begin
	xshift = 0.0
	yshift = 0.0

	while (fscan(fd) != EOF) {
	    call gargr (xshift)
	    call gargr (yshift)
	    if (nscan() >= 2)
		break
	    xshift = 0.0
	    yshift = 0.0
	} 

	call rg_lsetr (ls, SXSHIFT, xshift)
	call rg_lsetr (ls, SYSHIFT, yshift)
end


# RG_LIMSCALE -- Linearly scale the input image.

procedure rg_limscale (im1, im2, bscale, bzero)

pointer	im1		#I pointer to the input image
pointer	im2		#I pointer to the output image
real	bscale		#I the bscale value
real	bzero		#I the bzero value

int	ncols
pointer	sp, v1, v2, buf1, buf2
int	imgnlr(), impnlr()

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)

	ncols = IM_LEN(im1,1)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	while (imgnlr (im1, buf1, Meml[v1]) != EOF) {
	    if (impnlr (im2, buf2, Meml[v2]) != EOF)
		call altmr (Memr[buf1], Memr[buf2], ncols, bscale, bzero)
	}

	call sfree (sp)
end
