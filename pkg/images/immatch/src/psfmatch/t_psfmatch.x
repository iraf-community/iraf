include <fset.h>
include <imhdr.h>
include "psfmatch.h"

# T_PSFMATCH -- Match the resolution of an image to that of a reference
# image.

procedure t_psfmatch ()

pointer	image1			# pointer to the input image name
pointer	imager			# pointer to the reference image name
pointer	fpsflist		# pointer to the regions list
pointer	image2			# pointer to the output image name
pointer	kernel			# pointer to the kernel image name
pointer	pspectra		# pointer to the fourier spectra image name
int	interactive		# interactive mode ?
int	verbose			# verbose mode ?
int	boundary		# boundary extension type
real	constant		# constant for boundary extension

int	list1, listr, psflist, listk, list2
int	nregions, newref, stat
pointer	sp, imtemp, str, pm, gd, id, imr, im1, impsf, imk, im2
bool	clgetb()
int	imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb(), clgwrd(), btoi()
int	rg_pstati(), rg_ptmpimage(), rg_pregions(), rg_psfm(), rg_pisfm()
pointer	gopen(), immap(), rg_pstatp()
real	clgetr()
errchk	fntopnb(), fntclsb()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate temporary space.
	call smark (sp)
	call salloc (image1, SZ_FNAME, TY_CHAR)
	call salloc (imager, SZ_FNAME, TY_CHAR)
	call salloc (fpsflist, SZ_LINE, TY_CHAR)
	call salloc (kernel, SZ_FNAME, TY_CHAR)
	call salloc (image2, SZ_FNAME, TY_CHAR)
	call salloc (pspectra, SZ_FNAME, TY_CHAR)
	call salloc (imtemp, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get task parameters.
	call clgstr ("input", Memc[str], SZ_LINE)
	list1 = imtopen (Memc[str])
	call clgstr ("reference", Memc[str], SZ_LINE)
	listr = imtopen (Memc[str])
	call clgstr ("psfdata", Memc[fpsflist], SZ_LINE)
	call clgstr ("kernel", Memc[str], SZ_LINE)
	listk = imtopen (Memc[str])
	call clgstr ("output", Memc[str], SZ_LINE)
	list2 = imtopen (Memc[str])

	# Open the psf matching fitting structure.
	call rg_pgpars (pm)

	# Will the task run in interactive mode?
	if (rg_pstati (pm, CONVOLUTION) == PM_CONKERNEL)
	    interactive = NO
	else
	    interactive = btoi (clgetb ("interactive"))

	if (rg_pstati (pm, CONVOLUTION) != PM_CONKERNEL) {
	    if (imtlen (listr) <= 0)
	        call error (0, "The reference image list is empty.")
	    if (imtlen (listr) > 1 && imtlen (listr) != imtlen (list1))
	        call error (0,
	        "The number of reference and input images is not the same.")
	    if (interactive == NO && Memc[fpsflist] == EOS) {
	        call error (0, "The objects list is empty.")
	    } else if (rg_pstati (pm, CONVOLUTION) == PM_CONIMAGE) {
	        psflist = fntopnb (Memc[fpsflist], NO)
	        if (fntlenb(psflist) > 0 && imtlen (listr) != fntlenb (psflist))
		    call error (0,
	     "The number of reference images and objects lists is not the same")
	    } else {
	        psflist = imtopen (Memc[fpsflist])
	        if (imtlen (list1) != imtlen (psflist))
		    call error (0,
		    "The number of input and psf images is not the same")
	    }
	    call rg_psets (pm, PSFDATA, Memc[fpsflist])
	} else {
	    call imtclose (listr)
	    listr = NULL
	    psflist = NULL
	    call rg_psets (pm, PSFDATA, "")
	}

	# Compare the lengths of the input and output lists.
	if (imtlen(listk) <= 0) {
	    call imtclose (listk)
	    listk = NULL
	} else if (imtlen (list1) != imtlen (listk))
	    call error (0,
		"The number of input and kernel images is not the same.")

	if (imtlen (list2) <= 0) {
	    call imtclose (list2)
	    list2 = NULL
	} else if (imtlen (list1) != imtlen (list2))
	       call error (0,
	           "The number of input and output images are not the same.")

	# Get the boundary extension parameters for the image convolution.
	boundary = clgwrd ("boundary", Memc[str], SZ_LINE,
	   "|constant|nearest|reflect|wrap|")
	constant = clgetr ("constant")

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

	imr = NULL
	impsf = NULL

	# Do each set of input and output images.
	while ((imtgetim (list1, Memc[image1], SZ_FNAME) != EOF)) {

	    # Open reference image and the associated objects file
	    if (rg_pstati (pm, CONVOLUTION) != PM_CONKERNEL) {
	        if (imtgetim (listr, Memc[imager], SZ_FNAME) != EOF) {
		    if (imr != NULL)
		        call imunmap (imr)
		    imr = immap (Memc[imager], READ_ONLY, 0)
		    if (IM_NDIM(imr) > 2)
			call error (0, "Reference psf/image must be 1D or 2D")
		    call rg_psets (pm, REFIMAGE, Memc[imager])
		    if (rg_pstati (pm, CONVOLUTION) == PM_CONIMAGE) {
		        nregions = rg_pregions (psflist, imr, pm, 1, NO)
			if (nregions <= 0 && interactive == NO)
		            call error (0, "The objects list is empty.")
		        call rg_psets (pm, PSFIMAGE, "")
		    }
		    newref = YES
	        }
		if (rg_pstati (pm, CONVOLUTION) == PM_CONPSF) {
		    if (imtgetim (psflist, Memc[str], SZ_FNAME) != EOF) {
			impsf = immap (Memc[str], READ_ONLY, 0)
			if (IM_NDIM(impsf) != IM_NDIM(imr))
			    call error (0,
			"Image and reference psf must have same dimensionality")
			if (IM_LEN(impsf,1) != IM_LEN(imr,1))
			    call error (0,
			    "Image and reference psf are not the same size")
			if (IM_NDIM(impsf) == 2 && (IM_LEN(impsf,2) !=
			    IM_LEN(imr,2)))
			    call error (0,
			    "Image and reference psf are not the same size")
		        call rg_psets (pm, PSFIMAGE, Memc[str])
			newref = YES
		    }
		}
	    } else {
		imr = NULL
		impsf = NULL
		call rg_psets (pm, REFIMAGE, "")
		call rg_psets (pm, PSFIMAGE, "")
		call rg_psets (pm, OBJLIST, "")
		newref = NO
	    }

	    # Open the input image.
	    im1 = immap (Memc[image1], READ_ONLY, 0)
	    if (IM_NDIM(im1) > 2) {
		call error (0, "Input image must be 1D or 2D")
	    } else if (imr != NULL) {
		if (IM_NDIM(im1) != IM_NDIM(imr))
		    call error (0,
		    "Input and reference images must have same dimensionality")
	    }
	    call rg_psets (pm, IMAGE, Memc[image1])

	    # Open the kernel image name.
	    if (listk != NULL) {
	        if (imtgetim (listk, Memc[kernel], SZ_FNAME) != EOF)
		    ;
	    } else {
		if (rg_ptmpimage (Memc[image1], "ker", "ker", Memc[kernel],
		    SZ_FNAME) == NO)
		    ;
	    }
	    if (rg_pstati (pm, CONVOLUTION) != PM_CONKERNEL)
	        imk = immap (Memc[kernel], NEW_IMAGE, 0)
	    else
	        imk = immap (Memc[kernel], READ_ONLY, 0)
	    call rg_psets (pm, KERNEL, Memc[kernel])


	    # Construct the output image name.
	    if (list2 == NULL) {
		im2 = NULL
		Memc[image2] = NULL
	    } else if (imtgetim (list2, Memc[image2], SZ_FNAME) != EOF) {
		call xt_mkimtemp (Memc[image1], Memc[image2], Memc[imtemp],
		    SZ_FNAME)
	        im2 = immap (Memc[image2], NEW_COPY, im1)
	    } else {
		im2 = NULL
		Memc[image2] = NULL
	    }
	    call rg_psets (pm, OUTIMAGE, Memc[image2])

	    # Compute the the psf matching kernel.
	    if (interactive == YES) {
		stat = rg_pisfm (pm, imr, psflist, impsf, im1, imk, NULL, im2,
		    gd, id)
	    } else {
	        if (rg_psfm (pm, imr, im1, impsf, imk, newref) == OK) {
		    if (verbose == YES) {
			call printf (
			"Completed computing/reading kernel %s for image %s\n")
			    call pargstr (Memc[kernel])
			    call pargstr (Memc[image1])
		        if (rg_pstati(pm, CONVOLUTION) != PM_CONKERNEL)
		            call rg_pwrite (pm, imk, NULL)
		    }
		} else {
		    if (verbose == YES) {
		        call printf (
			    "Error computing/reading kernel %s for image %s\n")
			    call pargstr (Memc[kernel])
			    call pargstr (Memc[image1])
		    }
		}
		stat = NO
	    }

	    # Convolve the image.
	    if (im2 != NULL && stat == NO) {
		if (verbose == YES) {
		    if (rg_pstatp(pm, CONV) != NULL)
			call printf (
			    "\tComputing matched image %s ...\n")
		    else
			call printf (
			    "\tComputing matched image %s ...\n")
		    call pargstr (Memc[imtemp])
		    call pargstr (Memc[kernel])
		}
		if (rg_pstatp(pm, CONV) != NULL)
		    call rg_pconvolve (im1, im2, Memr[rg_pstatp(pm,CONV)],
		        rg_pstati(pm,KNX), rg_pstati(pm,KNY), boundary,
			constant)
	    }

	    # Close up the images.
	    if (im2 != NULL) {
		call imunmap (im2)
		if (rg_pstatp(pm, CONV) == NULL)
		    call imdelete (Memc[image2])
		else
		    call xt_delimtemp (Memc[image2], Memc[imtemp])
	    }
	    if (impsf != NULL)
		call imunmap (impsf)
	    if (imk != NULL) {
	        call imunmap (imk)
		if (rg_pstati (pm, CONVOLUTION) != PM_CONKERNEL &&
		    rg_pstatp(pm, CONV) == NULL)
		    call imdelete (Memc[kernel])
	    }
	    call imunmap (im1)

	    if (stat == YES)
		break
	    newref = NO
	}

	# Close up the lists.
	if (imr != NULL)
	    call imunmap (imr)

	if (list2 != NULL)
	    call imtclose (list2)
	if (listk != NULL)
	    call imtclose (listk)
	if (psflist != NULL) {
	    if (rg_pstati (pm, CONVOLUTION) == PM_CONIMAGE)
	        call fntclsb (psflist)
	    else
		call imtclose (psflist)
	}
	if (listr != NULL)
	    call imtclose (listr)
	call imtclose (list1)

	call rg_pfree (pm)

	# Close up te graphics and the display.
	if (gd != NULL)
	    call gclose (gd)
	if (id != NULL)
	    call gclose (id)

	call sfree (sp)
end


# RG_PTMPIMAGE -- Generate either a permanent image name using a user specified
# prefix or temporary image name using a default prefix. Return NO if the
# image is temporary or YES if it is permanent.

int procedure rg_ptmpimage (image, prefix, tmp, name, maxch)

char    image[ARB]              #I image name
char    prefix[ARB]             #I user supplied prefix
char    tmp[ARB]                #I user supplied temporary root
char    name[ARB]               #O output name
int     maxch                   #I max number of chars

int     npref, ndir
int     fnldir(), rg_pimroot(), strlen()

begin
        npref = strlen (prefix)
        ndir = fnldir (prefix, name, maxch)
        if (npref == ndir) {
            call mktemp (tmp, name[ndir+1], maxch)
            return (NO)
        } else {
            call strcpy (prefix, name, npref)
            if (rg_pimroot (image, name[npref+1], maxch) <= 0)
		;
            return (YES)
        }
end


# RG_PIMROOT -- Fetch the root image name minus the directory specification
# and the section notation. The length of the root name is returned.

int procedure rg_pimroot (image, root, maxch)

char    image[ARB]              #I image specification
char    root[ARB]               #O rootname
int     maxch                   #I maximum number of characters

int     nchars
pointer sp, str
int     fnldir(), strlen()

begin
        call smark (sp)
        call salloc (str, SZ_FNAME, TY_CHAR)

        call imgimage (image, root, maxch)
        nchars = fnldir (root, Memc[str], maxch)
        call strcpy (root[nchars+1], root, maxch)

        call sfree (sp)
        return (strlen (root))
end
