include	<fset.h>
include <imhdr.h>
include "../lib/daophotdef.h"

define	NSEED	3

# T_ADDSTAR  -- Add artificial stars to a list of images.

procedure t_addstar ()

pointer	image				# the input image
pointer	psfimage			# the input PSF image
pointer	photfile			# the input photometry file
pointer	addimage 			# root name for output image and file
real	minmag, maxmag			# magnitude range of artificial stars 
int	nstar				# number of artificial stars
int	nimage				# number of new images
int	cache				# cache the output image pixels

pointer	sp, outfname, im, psffd, oim, dao, str
int	imlist, limlist, plist, lplist, pimlist, lpimlist, oimlist, loimlist
int	ifd, ofd, j, simple, idoffset, root, verbose, verify, update, wcs
int	req_size, old_size, buf_size, memstat
int	seed, iseed[NSEED]
bool	coo_text

real	clgetr()
pointer	immap(), tbtopn()
int	clgeti(), fstati(), btoi(), fnldir(), strlen(), strncmp()
int	access(), open(), imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb()
int	fntgfnb(), clgwrd(), sizeof(), dp_memstat()
bool	itob(), clgetb()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate some memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (photfile, SZ_FNAME, TY_CHAR)
	call salloc (addimage, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the file names.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("addimage", Memc[addimage], SZ_FNAME)

	# Open the file/image lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	plist = fntopnb (Memc[photfile], NO)
	lplist = fntlenb (plist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	oimlist = imtopen (Memc[addimage])
	loimlist = imtlen (oimlist)

	# Check that the image and input photometry list lengths match.
	if ((lplist > 1) && (lplist != limlist)) {
	    call imtclose (imlist)
	    call fntclsb (plist)
	    call imtclose (pimlist)
	    call imtclose (oimlist)
	    call sfree (sp)
	    call error (0, "Incompatible image and photometry list lengths")
	}

	# Check that the image and psf image list lengths match.
	if ((limlist != lpimlist) && (strncmp (Memc[psfimage], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (plist)
	    call imtclose (pimlist)
	    call imtclose (oimlist)
	    call sfree (sp)
	    call error (0, "Incompatible image and psf image list lengths")
	}

	# Check that image and output image list lengths match.
	if ((loimlist != limlist) && (strncmp (Memc[addimage], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (plist)
	    call imtclose (pimlist)
	    call imtclose (oimlist)
	    call sfree (sp)
	    call error (0, "Incompatible input and output image list lengths")
	}

	# Set the type of input text file.
	simple = btoi (clgetb ("simple_text"))

	# Get the articial star parameters.
	nimage = clgeti ("nimage")
	if (lplist <= 0) {
	    nstar = clgeti ("nstar")
	    minmag = clgetr ("minmag")
	    maxmag = clgetr ("maxmag")
	}
	seed = clgeti ("seed")
	idoffset = clgeti ("idoffset")

	verbose = btoi (clgetb ("verbose"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	cache = btoi (clgetb("cache"))

	# Initialize the daophot structure.
	call dp_gppars (dao)	
	call dp_seti (dao, VERBOSE, verbose)

	# Verify, confirm, update the parameters.
	if (verify == YES) {
	    call dp_adconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}

        # Get the wcs information.
        wcs = clgwrd ("wcsin", Memc[str], SZ_FNAME, WCSINSTR)
        if (wcs <= 0) {
            call eprintf (
                "Warning: Setting the input coordinate system to logical\n")
            wcs = WCS_LOGICAL
        }
        call dp_seti (dao, WCSIN, wcs)
        wcs = clgwrd ("wcsout", Memc[str], SZ_FNAME, WCSOUTSTR)
        if (wcs <= 0) {
            call eprintf (
                "Warning: Setting the output coordinate system to logical\n")
            wcs = WCS_LOGICAL
        }
        call dp_seti (dao, WCSOUT, wcs)
        wcs = clgwrd ("wcspsf", Memc[str], SZ_FNAME, WCSPSFSTR)
        if (wcs <= 0) {
            call eprintf (
                "Warning: Setting the psf coordinate system to logical\n")
            wcs = WCS_LOGICAL
        }
        call dp_seti (dao, WCSPSF, wcs)


	# Open the PSF structure
	call dp_fitsetup (dao)

	# Initialize the random number generator.
	call dp_seed3 (seed, iseed)

	# Now loop over the input image list
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image.
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_imkeys (dao, im)
	    call dp_sets (dao, INIMAGE, Memc[image])

            # Cache the output image pixels.
            req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
            memstat = dp_memstat (cache, req_size, old_size)
            #if (memstat == YES)
                #call dp_pcache (im, INDEFI, buf_size)

	    # Read the PSF image.
	    if (imtgetim (pimlist, Memc[psfimage], SZ_FNAME) == EOF)
	        call strcpy (DEF_DEFNAME, Memc[psfimage], SZ_FNAME)
	    root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[psfimage+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[psfimage]))
	        call dp_iimname (Memc[image], Memc[outfname], "psf",
		    Memc[outfname], SZ_FNAME)
	    else
	        call strcpy (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    psffd = immap (Memc[outfname], READ_ONLY, 0)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])
	    call dp_readpsf (dao, psffd)

	    # Open the input photometry file.
	    if (lplist <= 0 ) {
	    	ifd = NULL
	    	call strcpy ("", Memc[photfile], SZ_FNAME)
	    } else if (fntgfnb (plist, Memc[photfile], SZ_FNAME) != EOF) {
		coo_text = itob (access (Memc[photfile], 0, TEXT_FILE))
		if (coo_text)
	    	    ifd = open (Memc[photfile], READ_ONLY, TEXT_FILE)
		else
		    ifd = tbtopn (Memc[photfile], READ_ONLY, 0)
	    } else
	    	call seek (ifd, BOF)
	    call dp_sets (dao, INPHOTFILE, Memc[photfile])

	    # Get the output image and file root name.
	    if (imtgetim (oimlist, Memc[addimage], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[addimage], SZ_FNAME)

	    # Now loop over the number of output images per input image.
	    do j = 1, nimage {

		# Open the output image.
		root = fnldir (Memc[addimage], Memc[outfname], SZ_FNAME)
		if (strncmp (DEF_DEFNAME, Memc[addimage+root],
		    DEF_LENDEFNAME) == 0 || root == strlen (Memc[addimage])) {
		    call dp_oimname (Memc[image], Memc[outfname], "add",
		        Memc[outfname], SZ_FNAME)
		    oim = immap (Memc[outfname], NEW_COPY, im)
		} else {
		    call strcpy (Memc[addimage], Memc[outfname], SZ_FNAME)
		    if (nimage > 1) {
	    	        call sprintf (Memc[outfname+
			    strlen(Memc[outfname])], SZ_FNAME, "%03d")
	                call pargi (j)
		    }
		    oim = immap (Memc[outfname], NEW_COPY, im)
		}
		call dp_sets (dao, OUTIMAGE, Memc[outfname])
                if (memstat == YES)
                    call dp_pcache (oim, INDEFI, buf_size)

		# Copy the input image to the new output image.
	    	call dp_imcopy (im, oim)
		if (memstat == NO)
		    call imflush (oim)

	    	# Open the output photometry file.
		root = fnldir (Memc[addimage], Memc[outfname], SZ_FNAME)
		if (strncmp (DEF_DEFNAME, Memc[addimage+root],
		    DEF_LENDEFNAME) == 0 || root == strlen (Memc[addimage])) {
		    call dp_outname (Memc[image], Memc[outfname], "art",
		        Memc[outfname], SZ_FNAME)
		    if (DP_TEXT(dao) == YES)
		        ofd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    else
	    	        ofd = tbtopn (Memc[outfname], NEW_FILE, 0)
		} else {
		    call strcpy (Memc[addimage], Memc[outfname], SZ_FNAME)
		    if (nimage > 1) {
	    	        call sprintf (Memc[outfname+
			    strlen(Memc[outfname])], SZ_FNAME, "%03d")
	                call pargi (j)
		    }
		    call strcat (".art", Memc[outfname], SZ_FNAME)
		    if (DP_TEXT(dao) == YES)
		        ofd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    else
	    	        ofd = tbtopn (Memc[outfname], NEW_FILE, 0)
		}
		call dp_sets (dao, OUTPHOTFILE, Memc[outfname])

	    	# Now go and actually add the stars.
	    	call dp_artstar (dao, im, oim, ifd, ofd, nstar, minmag,
		    maxmag, iseed, coo_text, simple, idoffset, memstat)

	    	# Close the output image and output table. 
	    	call imunmap (oim)
		if (DP_TEXT(dao) == YES)
		    call close (ofd)
		else
		    call tbtclo (ofd)
	    }

	    # Close the input image
	    call imunmap (im)

	    # Close the PSF image.
	    call imunmap (psffd)

	    # Close the input photometry file if there is more than one.
	    if ((ifd != NULL) && (lplist > 1)) {
		if (coo_text)
		    call close (ifd)
		else
		    call tbtclo (ifd)
		ifd = NULL
	    }

	    # Uncache memory.
	    call fixmem (old_size)
	}

	# If there was only a single photometry file close it.
	if (ifd != NULL && lplist == 1) {
	    if (coo_text)
		call close (ifd)
	    else
		call tbtclo (ifd)
	}

	# Close the image/file lists.
	call imtclose (imlist)
	call fntclsb (plist)
	call imtclose (pimlist)
	call imtclose (oimlist)

	# Close the PSF structure.
	call dp_fitclose (dao)

	# Close the daophot structure.
	call dp_free (dao)

	call sfree (sp)
end


# DP_IMCOPY -- Make a copy of an image.

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
end
