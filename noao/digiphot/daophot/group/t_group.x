include	<fset.h>
include "../lib/daophotdef.h"
include "../lib/daophot.h"

# T_GROUP  -- Procedure to divide the stars in a photometry table into
# natural groups based on the magnitude level at which they overlap.

procedure t_group ()

pointer	image				# name of the image
pointer	apfile				# aperture photometry file
pointer	psfimage			# name of the output PSF
pointer	groupfile			# output group table

bool	ap_text
int	apd, root, verify, update
int	imlist, limlist, alist, lalist, pimlist, lpimlist, olist, lolist
pointer	sp, im, tp, grp, dao, outfname

bool	clgetb(), itob()
int	tbtopn(), access(), fnldir(), strlen(), strncmp(), fstati(), btoi()
int	imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb(), fntgfnb()
pointer	immap(), open()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (apfile, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (groupfile, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[apfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("groupfile", Memc[groupfile], SZ_FNAME)
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[apfile], NO)
	lalist = fntlenb (alist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	olist = fntopnb (Memc[groupfile], NO)
	lolist = fntlenb (olist)

	# Test the lengths of the photometry file, psf image, and output
	# file lists are the same as the input image list.

	if ((limlist != lalist) && (strncmp (Memc[apfile], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
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
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and psf file list lengths")
	}

	if ((limlist != lolist) && (strncmp (Memc[groupfile], "default",
	    7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and group file list lengths")
	}


	# Initialize the daophot structure and get the pset parameters.
	call dp_gppars (dao, NULL)	
	call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))
	if (verify == YES) {
	    call dp_gconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}
	call dp_fitsetup (dao)
	call dp_apselsetup (dao)

	# Loop over the image list.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open input image
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_padu (im, dao)
	    call dp_rdnoise (im, dao)
	    call dp_otime (im, dao)
	    call dp_filter (im, dao)
	    call dp_airmass (im, dao)
	    call dp_sets (dao, IMNAME, Memc[image])

	    # Open input photometry list and read in the photometry.
	    if (fntgfnb (alist, Memc[apfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[apfile], SZ_FNAME)
	    root = fnldir (Memc[apfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[apfile+root], 7) == 0 || root ==
	        strlen (Memc[apfile]))
	        call dp_inname (Memc[image], "", "mag", Memc[outfname],
		    SZ_FNAME)
		else
	    	    call strcpy (Memc[apfile], Memc[outfname], SZ_FNAME)
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        apd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        apd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_getapert (dao, apd, DP_MAXSTAR(dao), ap_text)
	    call dp_sets (dao, APFILE, Memc[outfname])

	    # Read the PSF.
	    if (imtgetim (pimlist, Memc[psfimage], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[psfimage], SZ_FNAME)
	    root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[psfimage+root], 7) == 0 || root ==
	        strlen (Memc[psfimage]))
	        call dp_iimname (Memc[image], "", "psf", Memc[outfname],
		    SZ_FNAME)
	    else
	    	call strcpy (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    tp = immap (Memc[outfname], READ_ONLY, 0)
	    call dp_readpsf (dao, tp)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])

	    # Open output GROUP file. If the output is "default", dir$default
	    # or a directory specification then the extension "grp" is added to
	    # the image name and a suitable version number is appended to the
	    # output name.

	    if (fntgfnb (olist, Memc[groupfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[groupfile], SZ_FNAME)
	    root = fnldir (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[groupfile + root], 7) == 0 || root ==
	        strlen (Memc[groupfile])) {
	        call dp_outname (Memc[image], "", "grp", Memc[outfname],
		    SZ_FNAME)
	        if (DP_TEXT(dao) == YES)
		    grp = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            grp = tbtopn (Memc[outfname], NEW_FILE, 0)
	    } else {
	        call strcpy (Memc[groupfile], Memc[outfname], SZ_FNAME)
	        if (DP_TEXT(dao) == YES)
		    grp = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            grp = tbtopn (Memc[outfname], NEW_FILE, 0)
	    }
	    call dp_sets (dao, GRPFILE, Memc[outfname])

	    # Now go and group the stars.
	    call dp_mkgroup (dao, im, grp)

	    # Close up the input image.
	    call imunmap (im)

	    # Close up the photometry file.
	    if (ap_text)
	        call close (apd)
	    else
		call tbtclo (apd)

	    # Close up the PSF image.
	    call imunmap (tp)

	    # Close up the group table.
	    if (DP_TEXT(dao) == YES)
		call close (grp)
	    else
	        call tbtclo (grp)
	}

	# Close the lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call imtclose (pimlist)
	call fntclsb (olist)

	# Free up daophot memory.
	call dp_apclose (dao)
	call dp_fitclose (dao)
	call dp_free (dao)

	call sfree(sp)
end	
