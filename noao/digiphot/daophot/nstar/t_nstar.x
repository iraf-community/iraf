include	<fset.h>
include	"../lib/daophot.h"
include "../lib/daophotdef.h"

# T_NSTAR  -- Procedure to fit the PSF to multiple stars.

procedure t_nstar ()

pointer	image				# name of the image
pointer	psfimage			# name of the input PSF image
pointer	groupfile			# input group file
pointer	nstarfile			# output nstar table

bool	ap_text
int	root, verify, update
int	imlist, limlist, alist, lalist, pimlist, lpimlist, olist, lolist
pointer	sp, outfname, im, psfim, grp, nst, dao

bool	clgetb(), itob()
int	tbtopn(), strlen(), strncmp(), fnldir(), fstati(), open(), btoi()
int	access(), imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb()
int	fntgfnb()
pointer	immap()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (groupfile, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (nstarfile, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)

	# Get the input and output file names.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("groupfile", Memc[groupfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("nstarfile", Memc[nstarfile], SZ_FNAME)

	# Get the task mode parameters.
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[groupfile], NO)
	lalist = fntlenb (alist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	olist = fntopnb (Memc[nstarfile], NO)
	lolist = fntlenb (olist)


	# Test that the lengths of the photometry file, psf image, and
	# output file lists are the same as the length of the input image
	# list.

	if ((limlist != lalist) && (strncmp (Memc[groupfile],
	    "default", 7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and group file list lengths")
	}

	if ((limlist != lpimlist) && (strncmp (Memc[psfimage],
	    "default", 7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and psf file list lengths")
	}

	if ((limlist != lolist) && (strncmp (Memc[nstarfile],
	    "default", 7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and nstar file list lengths")
	}

	# Open the input image and initialize the daophot structure and
	# get some pset parameters.
	call dp_gppars (dao, NULL)	
	call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))
	# Verify and update the parameters as appropriate.
	if (verify == YES) {
	    call dp_nconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}

	# Initalize the daophot fitting structure.
	call dp_fitsetup (dao)
	call dp_apsetup (dao)
	call dp_nstarsetup (dao)

	# Loop over the images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image.
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_padu (im, dao)
	    call dp_rdnoise (im, dao)
	    call dp_otime (im, dao)
	    call dp_filter (im, dao)
	    call dp_airmass (im, dao)
	    call dp_sets (dao, IMNAME, Memc[image])

	    # Open the input group table.
	    if (fntgfnb (alist, Memc[groupfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[groupfile], SZ_FNAME)
	    root = fnldir (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[groupfile+root], 7) == 0 || (root ==
	        strlen (Memc[groupfile])))
	        call dp_inname (Memc[image], "", "grp", Memc[outfname],
		    SZ_FNAME)
	    else
	    	call strcpy (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        grp = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        grp = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_sets (dao, GRPFILE, Memc[outfname])

	    # Open and read the PSF image.
	    if (imtgetim (pimlist, Memc[psfimage], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[psfimage], SZ_FNAME)
	    root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[psfimage+root], 7) == 0 || (root ==
	        strlen (Memc[psfimage])))
	        call dp_iimname (Memc[image], "", "psf", Memc[outfname],
		    SZ_FNAME)
	    else
	        call strcpy (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    psfim = immap (Memc[outfname], READ_ONLY, 0)
	    call dp_readpsf (dao, psfim)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])
	
	    # Open the output NSTAR file. If the output is "default",
	    # dir$default or a directory specification then the extension
	    # "nst" is added to the image name and a suitable version number
	    # is appended to the output name.

	    if (fntgfnb (olist, Memc[nstarfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[nstarfile], SZ_FNAME)
	    root = fnldir (Memc[nstarfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[nstarfile+root], 7) == 0 || (root ==
	        strlen (Memc[nstarfile]))) {
	        call dp_outname (Memc[image], "", "nst", Memc[outfname],
		    SZ_FNAME)
	        if (DP_TEXT(dao) == YES)
	            nst = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            nst = tbtopn (Memc[outfname], NEW_FILE, 0)
	    } else {
	        call strcpy (Memc[nstarfile], Memc[outfname], SZ_FNAME)
	        if (DP_TEXT(dao) == YES)
	            nst = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            nst = tbtopn (Memc[outfname], NEW_FILE, 0)
	    }
	    call dp_sets (dao, NSTARFILE, Memc[outfname])

	    # Do the PSF fitting.
	    call dp_nphot (dao, im, grp, nst, ap_text)

	    # Close the input image.
	    call imunmap (im)

	    # Close the group file. 
	    if (ap_text)
		call close (grp)
	    else
	        call tbtclo (grp)

	    # Close the PSF image.
	    call imunmap (psfim)

	    # Close the output photometry file.
	    if (DP_TEXT(dao) == YES)
		call close (nst)
	    else
	        call tbtclo (nst)
	}

	# Close the lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call imtclose (pimlist)
	call fntclsb (olist)

	# Free the daophot structure.
	call dp_apclose (dao)
	call dp_nsclose (dao)
	call dp_fitclose (dao)
	call dp_free (dao)

	call sfree(sp)
end	
