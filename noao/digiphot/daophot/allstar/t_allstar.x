include <imhdr.h>
include	<fset.h>
include	"../lib/daophot.h"
include "../lib/daophotdef.h"

# T_ALLSTAR  -- Group a list of stars into physical associations, fit the PSF
# to the stars in each group simultaneously, and produce an output subtracted
# image.

procedure t_allstar ()

pointer	image			# the input image
pointer	psfimage		# the input psf image
pointer	photfile		# the input photometry file
pointer	allstarfile		# the output photometry file
pointer	subimage		# the output subtracted image
int	cache			# cache the data in memory

bool	ap_text
int	imlist, limlist, alist, lalist, pimlist, lpimlist, olist, lolist
int	simlist, lsimlist
int	photfd, psffd, allfd, root, savesub, verify, update
pointer	sp, outfname, im, subim, dao

bool	itob(), clgetb()
int	open(), tbtopn(), fnldir(), strncmp(), strlen(), btoi(), access()
int	fstati(), imtopen, imtlen(), imtgetim(), fntopnb(), fntlenb()
int	fntgfnb()
pointer	immap()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (photfile, SZ_FNAME, TY_CHAR)
	call salloc (allstarfile, SZ_FNAME, TY_CHAR)
	call salloc (subimage, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)

	# Get the task input and output file names.
	call clgstr ("image", Memc[image], SZ_FNAME)
 	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("allstarfile", Memc[allstarfile], SZ_FNAME)
	call clgstr ("subimage", Memc[subimage], SZ_FNAME)

	# Get the task mode parameters.
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	cache = btoi (clgetb ("cache"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[photfile], NO)
	lalist = fntlenb (alist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	olist = fntopnb (Memc[allstarfile], NO)
	lolist = fntlenb (olist)
	simlist = imtopen (Memc[subimage])
	lsimlist = imtlen (simlist)

	# Test the lengths of the photometry file, psf image and subtracted
	# image lists are the same as the length of the input image.

	if ((limlist != lalist) && (strncmp (Memc[photfile], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
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
	    call fntclsb (olist)
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
	    call fntclsb (olist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and subtracted image list lengths")
	}

	if ((limlist != lolist) && (strncmp (Memc[allstarfile], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and subtracted image list lengths")
	}

	# Open the input image, initialize the daophot structure, get the
	# pset parameters

	call dp_gppars (dao, NULL)	
	call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))

	# Verify and update the parameters as appropriate.
	if (verify == YES) {
	    call dp_aconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}

	# Initialize the daophot fitting structure.
	call dp_apsetup (dao)
	call dp_fitsetup (dao)
	call dp_allstarsetup (dao)

	# Loop over the images. 
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_padu (im, dao)
	    call dp_rdnoise (im, dao)
	    call dp_otime (im, dao)
	    call dp_filter (im, dao)
	    call dp_airmass (im, dao)
	    call dp_sets (dao, IMNAME, Memc[image])

	    # Open the input photometry file.
	    if (fntgfnb (alist, Memc[photfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[photfile], SZ_FNAME)
	    root = fnldir (Memc[photfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[photfile+root], 7) == 0 || root ==
	        strlen (Memc[photfile]))
	        call dp_inname (Memc[image], "", "mag", Memc[outfname],
		    SZ_FNAME)
	    else
	        call strcpy (Memc[photfile], Memc[outfname], SZ_FNAME)
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        photfd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        photfd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_sets (dao, APFILE, Memc[outfname])
	    call dp_getapert (dao, photfd, DP_MAXSTAR(dao) + 1, ap_text)

	    # Open the PSF image and read in the PSF.
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
	
	    # Open the output ALLSTAR file. If the output is "default",
	    # dir$default or a directory specification then the extension
	    # "als" is added to the image name and a suitable version
	    # number if appended to the output name.

	    if (fntgfnb (olist, Memc[allstarfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[allstarfile], SZ_FNAME)
	    root = fnldir (Memc[allstarfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[allstarfile+root], 7) == 0 || root ==
	        strlen (Memc[allstarfile])) {
	        call dp_outname (Memc[image], "", "als", Memc[outfname],
		    SZ_FNAME)
	        if (DP_TEXT(dao)  == YES)
		    allfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
		    allfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    } else {
	        call strcpy (Memc[allstarfile], Memc[outfname], SZ_FNAME)
	        if (DP_TEXT(dao)  == YES)
		    allfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
		    allfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    }
	    call dp_sets (dao, ALLSTARFILE, Memc[outfname])

	    # Open the subtracted image.
	    savesub = YES
	    if (imtgetim (simlist, Memc[subimage], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[subimage], SZ_FNAME)
	    root = fnldir (Memc[subimage], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[subimage+root], 7) == 0 || root ==
	        strlen (Memc[subimage])) {
	        call dp_oimname (Memc[image], "", "sub", Memc[outfname],
		    SZ_FNAME)
	        subim = immap (Memc[outfname], NEW_COPY, im)
	        IM_PIXTYPE(subim) = TY_REAL
	    } else {
	        call strcpy (Memc[subimage], Memc[outfname], SZ_FNAME)
	        subim = immap (Memc[outfname], NEW_COPY, im)
	        IM_PIXTYPE(subim) = TY_REAL
	    }
	    call dp_sets (dao, SUBIMAGE, Memc[outfname])

	    # Fit the stars.
	    call dp_astar (dao, im, subim, allfd, cache, savesub)

	    # Close the input image.
	    call imunmap (im)

	    # Close the input photometry file.
	    if (ap_text)
		call close (photfd)
	    else
	        call tbtclo (photfd)

	    # Close PSF image.
	    call imunmap (psffd)

	    # Close the output photometry file.
	    if (DP_TEXT(dao) == YES)
		call close (allfd)
	    else
	        call tbtclo (allfd)

	    # Close the output subtracted image.
	    call strcpy (IM_HDRFILE(subim), Memc[subimage], SZ_FNAME)
	    call imunmap (subim)
	    if (savesub == NO)
	        call imdelete (Memc[subimage])
	}

	# Close the lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call imtclose (pimlist)
	call fntclsb (olist)
	call imtclose (simlist)

	# Close up the daophot structure.
	call dp_alclose (dao)
	call dp_apclose (dao)
	call dp_fitclose (dao)
	call dp_free (dao)

	call sfree(sp)
end	
