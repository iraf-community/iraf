include	<fset.h>
include <gset.h>
include	"../lib/daophot.h"
include "../lib/daophotdef.h"

# T_PEAK  -- Procedure to fit the PSF to single stars.

procedure t_peak ()

pointer	image				# name of the image
pointer	psfimage			# name of the output PSF
pointer	photfile			# Input rough photometry
pointer	peakfile			# Output PEAK table

bool	ap_text
int	apd, root, verify, update
int	imlist, limlist, alist, lalist, pimlist, lpimlist, olist, lolist
pointer	sp, im, psfim, pkfd, dao, outfname

bool	clgetb(), itob()
int	tbtopn(), open(), fnldir(), strlen(), strncmp(), fstati(), btoi()
int	access(), imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb()
int	fntgfnb()
pointer	immap()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (photfile, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (peakfile, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)

	# Get the various task parameters
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("peakfile", Memc[peakfile], SZ_FNAME)
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[photfile], NO)
	lalist = fntlenb (alist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	olist = fntopnb (Memc[peakfile], NO)
	lolist =  fntlenb (olist)

	# Test that the lengths of the photometry file, psf image, and
	# output file lists are the same as the length of the input image
	# list.

	if ((limlist != lalist) && (strncmp (Memc[photfile],
	    "default", 7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and photometry file list lengths")
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

	if ((limlist != lolist) && (strncmp (Memc[peakfile],
	    "default", 7) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and peak file list lengths")
	}

	# Initialize DAOPHOT structure, and get the pset parameters.
	call dp_gppars (dao, NULL)	
	call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))
	if (verify == YES) {
	    call dp_pkconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}
	call dp_fitsetup (dao)

	# Open input image
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the image.
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_padu (im, dao)
	    call dp_rdnoise (im, dao)
	    call dp_otime (im, dao)
	    call dp_filter (im, dao)
	    call dp_airmass (im, dao)
	    call dp_sets (dao, IMNAME, Memc[image])

	    # Open the input photometry table.
	    if (fntgfnb (alist, Memc[photfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[photfile], SZ_FNAME)
	    root = fnldir (Memc[photfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[photfile+root], 7) == 0 || root ==
	        strlen (Memc[photfile]))
	        call dp_inname (Memc[image], "", "mag", Memc[outfname],
		    SZ_FNAME)
	    else
	        call strcpy (Memc[photfile], Memc[outfname], SZ_FNAME)
	    ap_text =  itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        apd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        apd = tbtopn (Memc[outfname], READ_ONLY, 0)
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
	    psfim = immap (Memc[outfname], READ_ONLY, 0)
	    call dp_readpsf (dao, psfim)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])
	
	    # Open output PEAK file. If the output is "default", dir$default
	    # or a directory specification then the extension .pk is added to
	    # the image name and a suitable version number is appended to
	    # the output name.

	    if (fntgfnb (olist, Memc[peakfile], SZ_FNAME) == EOF)
		call strcpy ("default", Memc[peakfile], SZ_FNAME)
	    root = fnldir (Memc[peakfile], Memc[outfname], SZ_FNAME)
	    if (strncmp ("default", Memc[peakfile + root], 7) == 0 || root ==
	        strlen (Memc[peakfile])) {
	        call dp_outname (Memc[image], "", "pk", Memc[outfname],
		    SZ_FNAME)
	        if (DP_TEXT(dao) == YES)
	            pkfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            pkfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    } else {
	        call strcpy (Memc[peakfile], Memc[outfname], SZ_FNAME)
	        if (DP_TEXT(dao) == YES)
	            pkfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            pkfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    }
	    call dp_sets (dao, PKFILE, Memc[outfname])

	    # Now go and do the PSF fitting!
	    call dp_peakphot (dao, im, apd, pkfd, ap_text)

	    # Close the input image.
	    call imunmap (im)

	    # Close the photometry file. 
	    if (ap_text)
	        call close (apd)
	    else
	        call tbtclo (apd)

	    # Close PSF image.
	    call imunmap (psfim)

	    # Close the output table.
	    if (DP_TEXT(dao) == YES)
	        call close (pkfd)
	    else
	        call tbtclo (pkfd)
	}

	# Close the lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call imtclose (pimlist)
	call fntclsb (olist)

	# Free up memory.
	call dp_fitclose (dao)
	call dp_free (dao)

	call sfree(sp)
end	
