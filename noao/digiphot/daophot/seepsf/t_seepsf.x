include	<fset.h>

# T_SEEPSF  -- Produce the PSF in image scale coordinates.

procedure t_seepsf ()

pointer	psfimage			# name of the input PSF
pointer	image				# name of the output image
int	dimen				# size of the image
real	magnitude			# magnitude of star

int	psffd, pimlist, lpimlist, imlist, limlist
pointer	im, sp, dao
real	xpsf, ypsf

int	clgeti(), fstati(), imtopen(), imtlen(), imtgetim()
real	clgetr()
pointer	immap()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	     call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some memory.
	call smark (sp)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("image", Memc[image], SZ_FNAME)
	dimen = clgeti ("dimension")
	if (! IS_INDEFI(dimen)) {
	    if (mod (dimen, 2) == 0)
	        dimen = dimen + 1
	}
	xpsf = clgetr ("xpsf")
	ypsf = clgetr ("ypsf")
	magnitude = clgetr ("magnitude")

	# Get the lists.
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)

	# Check the list lengths for equality.
	if (lpimlist != limlist) {
	    call imtclose (pimlist)
	    call imtclose (imlist)
	    call sfree (sp)
	    call error (0,
	        "The psf and output image lengths are imcompatibale")
	}

	# Initialize the daophot structure and get the pset parameters.
	call dp_init (dao)
	call dp_fitsetup (dao)

	# Loop over the input images
	while ((imtgetim (pimlist, Memc[psfimage], SZ_FNAME) != EOF) &&
	    (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF)) {

	    # Open the psfimage.
	    psffd = immap (Memc[psfimage], READ_ONLY, 0)

	    # Open the output image.
	    im = immap (Memc[image], NEW_COPY, psffd)

	    # Read the PSF.
	    call dp_readpsf (dao, psffd)

	    # Make PSF image.
	    call dp_mkimage (dao, psffd, im, dimen, xpsf, ypsf, magnitude)

	    # Close the PSF image and the output image.
	    call imunmap (psffd)
	    call imunmap (im)
	}

	# Close the daophot structure.
	call dp_fitclose (dao)
	call dp_free (dao)

	# Close the lists
	call imtclose (pimlist)
	call imtclose (imlist)

	call sfree (sp)
end	
