include	<fset.h>
include	<imset.h>
include	<imhdr.h>
include	"../lib/daophot.h"
include "../lib/daophotdef.h"

# T_SUBSTAR  -- Procedure to subtract DAOPHOT photometry from an image.

procedure t_substar ()

pointer	image				# name of the image
pointer	psfimage			# name of the output PSF
pointer	photfile			# Input rough photometry
pointer	subimage			# Subtracted image

bool	ap_text
int	psffd, photfd, root, verify, update
int	imlist, limlist, alist, lalist, pimlist, lpimlist, simlist, lsimlist
pointer	sp, input, output, dao, outfname

bool	clgetb(), itob()
int	tbtopn(), fnldir(), strlen(), strncmp(), access(), fstati(), btoi()
int	imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb(), fntgfnb()
pointer	immap(), open

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (photfile, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (subimage, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("subimage", Memc[subimage], SZ_FNAME)
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[photfile], NO)
	lalist = fntlenb (alist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	simlist = imtopen (Memc[subimage])
	lsimlist = imtlen (simlist)

	# Test the lengths of the photometry file, psf image and subtracted
	# image lists are the same as the length of the input image.

	if ((limlist != lalist) && (strncmp (Memc[photfile], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and photometry file list lengths")
	}

	if ((limlist != lpimlist) && (strncmp (Memc[psfimage], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and psf file list lengths")
	}

	if ((limlist != lsimlist) && (strncmp (Memc[subimage], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and subtracted image list lengths")
	}

	# Initialize DAOPHOT structure, get pset parameters.
	call dp_gppars (dao, NULL)	
	call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))
	if (verify == YES) {
	    call dp_sconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}
	call dp_fitsetup (dao)
	call dp_apselsetup (dao)

	# Loop over the images
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open input and output images
	    input = immap (Memc[image], READ_ONLY, 0)		
	    call dp_sets (dao, IMNAME, Memc[image])

	    # If the output image name is "default", dir$default or a
	    # directory specification then the extension "sub" is added to
	    # the image name and a suitable version number is appended to the
	    # output name.

	    if (imtgetim (simlist, Memc[subimage], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[subimage], SZ_FNAME)
	    root = fnldir (Memc[subimage], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[subimage + root], 7) == 0 || root ==
	        strlen (Memc[subimage])) {
	        call dp_oimname (Memc[image], "", "sub", Memc[outfname],
		    SZ_FNAME)
	        output = immap (Memc[outfname], NEW_COPY, input)
	    } else {
	        call strcpy (Memc[subimage], Memc[outfname], SZ_FNAME)
	        output = immap (Memc[outfname], NEW_COPY, input)
	    }
	    call dp_sets (dao, SUBIMAGE, Memc[subimage])

	    # Open input photometry table and read in the photometry.
	    if (fntgfnb (alist, Memc[photfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[photfile], SZ_FNAME)
	    root = fnldir (Memc[photfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[photfile+root], 7) == 0 || root ==
	        strlen (Memc[photfile]))
	        call dp_inname (Memc[image], "", "nst", Memc[outfname],
		    SZ_FNAME)
	    else
	        call strcpy (Memc[photfile], Memc[outfname], SZ_FNAME)
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        photfd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else 
	        photfd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_getapert (dao, photfd, DP_MAXSTAR(dao), ap_text)
	    call dp_sets (dao, NSTARFILE, Memc[outfname])

	    # Read in the PSF
	    if (imtgetim (pimlist, Memc[psfimage], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[psfimage], SZ_FNAME)
	    root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[psfimage+root], 7) == 0 || root ==
	        strlen (Memc[psfimage]))
	        call dp_iimname (Memc[image], "", "psf", Memc[outfname],
		    SZ_FNAME)
	    else
	        call strcpy (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    psffd = immap (Memc[outfname], READ_ONLY, 0)
	    call dp_readpsf (dao, psffd)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])
	
	    # Now go and subtract those stars!
	    call dp_substar (dao, psffd, input, output)

	    # Close the input and output images.
	    call imunmap (input)
	    call imunmap (output)

	    # Close the photometry file.
	    if (ap_text)
		call close (photfd)
	    else
	        call tbtclo (photfd)

	    # Close the PSF image.
	    call imunmap (psffd)
	}

	# Close the lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call imtclose (pimlist)
	call imtclose (simlist)

	# Free the daophot strucutures.
	call dp_apclose (dao)
	call dp_fitclose (dao)
	call dp_free (dao)

	call sfree (sp)
end	


# DP_IMCOPY -- Make a copy of an image

procedure  dp_imcopy (in, out)

pointer	in		# the input image
pointer	out		# input and output descriptors

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	l1, l2
pointer	imgnlr(), impnlr()

begin
	# Initialize position vectors.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)
	npix = IM_LEN(in, 1)

	# Copy the image.
	while (imgnlr (in, l1, v1) != EOF && impnlr (out, l2, v2) != EOF)
	    call amovr (Memr[l1], Memr[l2], npix)
	call imflush (out)
end
