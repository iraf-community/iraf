include	<fset.h>
include	"../lib/daophot.h"
include "../lib/daophotdef.h"

# T_ADDSTAR  -- Procedure to add artificial stars to some images.

procedure t_addstar ()

pointer	image				# name of the image
pointer	psfimage			# name of the input PSF image
pointer	coords				# input coord and mag list
pointer	addimage 			# root name for output images and file
real	minmag, maxmag			# range of magnitudes to add
int	nstar				# number of stars to add
int	nimage				# number of new images to create

bool	coo_text
int	imlist, limlist, clist, lclist, pimlist, lpimlist, oimlist, loimlist
int	cl, ofd, j, simple, idoffset, root, verify, update
pointer	sp, outfname, im, psffd, oim, dao

bool	itob(), clgetb()
int	tbtopn(), clgeti(), fstati(), btoi(), fnldir(), strlen(), strncmp()
int	access(), open(), imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb()
int	fntgfnb()
pointer	immap()
real	clgetr()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate some memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (addimage, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)

	# Get the file names.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("starlist", Memc[coords], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("addimage", Memc[addimage], SZ_FNAME)

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	clist = fntopnb (Memc[coords], NO)
	lclist = fntlenb (clist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	oimlist = imtopen (Memc[addimage])
	loimlist = imtlen (oimlist)

	# Check that the image and coordinate list lengths match.
	if ((lclist > 1) && (lclist != limlist)) {
	    call imtclose (imlist)
	    call fntclsb (clist)
	    call imtclose (pimlist)
	    call imtclose (oimlist)
	    call sfree (sp)
	    call error (0, "Incompatible image and coordinate list lengths")
	}

	# Check that the image and psf image list lengths match.
	if ((limlist != lpimlist) && (strncmp (Memc[psfimage], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (clist)
	    call imtclose (pimlist)
	    call imtclose (oimlist)
	    call sfree (sp)
	    call error (0, "Incompatible image and psf image list lengths")
	}

	# Check that image and output image list lengths match.
	if ((loimlist != limlist) && (strncmp (Memc[addimage], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (clist)
	    call imtclose (pimlist)
	    call imtclose (oimlist)
	    call sfree (sp)
	    call error (0, "Incompatible input and output images lists")
	}

	# Get the remaining parameters.
	nimage = clgeti ("nimage")
	if (lclist <= 0) {
	    nstar = clgeti ("nstar")
	    minmag = clgetr ("minmag")
	    maxmag = clgetr ("maxmag")
	}
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	idoffset = clgeti ("idoffset")
	simple = btoi (clgetb ("simple_text"))

	# Initialize the daophot structure.
	call dp_gppars (dao, NULL)	
	call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))
	if (verify == YES) {
	    call dp_adconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}
	call dp_fitsetup (dao)

	# Now loop over the input image list
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image.
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_padu (im, dao)
	    call dp_rdnoise (im, dao)
	    call dp_otime (im, dao)
	    call dp_filter (im, dao)
	    call dp_airmass (im, dao)
	    call dp_sets (dao, IMNAME, Memc[image])

	    # Read the PSF image.
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
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])
	    call dp_readpsf (dao, psffd)

	    # Open the coordinate file, where coords is assumed to be
	    # a simple text file in which the x and Y positions are in
	    # columns 1 and 2 and the magnitude is on column 3 or a 
	    # table.

	    if (lclist <= 0 ) {
	    	cl = NULL
	    	call strcpy ("", Memc[coords], SZ_FNAME)
	    } else if (fntgfnb (clist, Memc[coords], SZ_FNAME) != EOF) {
		coo_text = itob (access (Memc[coords], 0, TEXT_FILE))
		if (coo_text)
	    	    cl = open (Memc[coords], READ_ONLY, TEXT_FILE)
		else
		    cl = tbtopn (Memc[coords], READ_ONLY, 0)
	    } else
	    	call seek (cl, BOF)
	    call dp_sets (dao, APFILE, Memc[coords])

	    # Get the output image and table file root name.
	    if (imtgetim (oimlist, Memc[addimage], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[addimage], SZ_FNAME)

	    # Now loop over the number of output images per input image.
	    do j = 1, nimage {

		# Open the output image.
		root = fnldir (Memc[addimage], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[addimage+root], 7) == 0 || root ==
		    strlen (Memc[addimage])) {
		    call dp_oimname (Memc[image], "", "add", Memc[outfname],
			SZ_FNAME)
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
		call dp_sets (dao, ADDIMAGE, Memc[outfname])

		# Copy the input image to the new output image.
	    	call dp_imcopy (im, oim)

	    	# Open output table to contain list of stars added.
		root = fnldir (Memc[addimage], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[addimage+root], 7) == 0 || root ==
		    strlen (Memc[addimage])) {
		    call dp_outname (Memc[image], "", "art", Memc[outfname],
		        SZ_FNAME)
		    if (DP_TEXT(dao) == YES)
		        ofd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    else
	    	        ofd = tbtopn (Memc[outfname], NEW_FILE, 0)
		} else {
		    call strcpy (Memc[addimage], Memc[outfname], SZ_FNAME)
		    if (DP_TEXT(dao) == YES)
		        ofd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    else
	    	        ofd = tbtopn (Memc[outfname], NEW_FILE, 0)
		    if (nimage > 1) {
	    	        call sprintf (Memc[outfname+
			    strlen(Memc[outfname])], SZ_FNAME, "%03d")
	                call pargi (j)
		    }
		}
		call dp_sets (dao, ADDFILE, Memc[outfname])

	    	# Now go and actually add the stars.
	    	call dp_addstar (dao, oim, cl, ofd, nstar, minmag,
		    maxmag, coo_text, simple, idoffset)

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

	    # Close the coordinate list if there is more than one.
	    if ((cl != NULL) && (lclist > 1)) {
		if (coo_text)
		    call close (cl)
		else
		    call tbtclo (cl)
		cl = NULL
	    }
	}

	# If there was only a single coordinate file close it.
	if (cl != NULL && lclist == 1) {
	    if (coo_text)
		call close (cl)
	    else
		call tbtclo (cl)
	}

	# Close the lists.
	call imtclose (imlist)
	call fntclsb (clist)
	call imtclose (pimlist)
	call imtclose (oimlist)

	call dp_fitclose (dao)
	call dp_free (dao)

	call sfree (sp)
end
