include	<fset.h>
include <imhdr.h>
include "../lib/daophotdef.h"

# T_GROUP  -- Procedure to divide the stars in a photometry table into
# natural groups based on the magnitude level at which they overlap.

procedure t_group ()

pointer	image				# name of the image
pointer	apfile				# aperture photometry file
pointer	psfimage			# name of the output PSF
pointer	groupfile			# output group table

pointer	sp, im, dao, outfname, str
int	apd, root, cache, verbose, verify, update, grp, tp, wcs
int	imlist, limlist, alist, lalist, pimlist, lpimlist, olist, lolist
int	req_size, old_size, buf_size, memstat
bool	ap_text

pointer	immap(), tbtopn()
int	access(), fnldir(), strlen(), strncmp(), fstati(), btoi()
int	imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb(), fntgfnb()
int	open(), clgwrd(), sizeof(), dp_memstat()
bool	clgetb(), itob()

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
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[apfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("groupfile", Memc[groupfile], SZ_FNAME)
	verbose = btoi (clgetb ("verbose"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	cache = btoi (clgetb ("cache"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[apfile], NO)
	lalist = fntlenb (alist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	olist = fntopnb (Memc[groupfile], NO)
	lolist = fntlenb (olist)

	# Test that the lengths of the photometry file, psf image, and output
	# file lists are the same as the length of the input image list.

	if ((limlist != lalist) && (strncmp (Memc[apfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and photometry file list lengths")
	}

	if ((limlist != lpimlist) && (strncmp (Memc[psfimage], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and psf file list lengths")
	}

	if ((limlist != lolist) && (strncmp (Memc[groupfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and group file list lengths")
	}

	# Initialize the daophot structure and get the pset parameters.
	call dp_gppars (dao)	
	call dp_seti (dao, VERBOSE, verbose)

	# Optionally verify and update the parameters.
	if (verify == YES) {
	    call dp_gconfirm (dao)
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

	# Open the PSF structure.
	call dp_fitsetup (dao)

	# Open the photometry list structure.
	call dp_apselsetup (dao)

	# Loop over the image list.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open input image and grab some header parameters.
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_imkeys (dao, im)
	    call dp_sets (dao, INIMAGE, Memc[image])

            # Cache the input image pixels.
            req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
            memstat = dp_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call dp_pcache (im, INDEFI, buf_size)

	    # Open input photometry list and read in the photometry.
	    if (fntgfnb (alist, Memc[apfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[apfile], SZ_FNAME)
	    root = fnldir (Memc[apfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[apfile+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[apfile]))
	        call dp_inname (Memc[image], Memc[outfname], "mag",
		    Memc[outfname], SZ_FNAME)
	    else
	    	call strcpy (Memc[apfile], Memc[outfname], SZ_FNAME)
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        apd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        apd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_wgetapert (dao, im, apd, DP_MAXNSTAR(dao), ap_text)
	    call dp_sets (dao, INPHOTFILE, Memc[outfname])

	    # Read the PSF.
	    if (imtgetim (pimlist, Memc[psfimage], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[psfimage], SZ_FNAME)
	    root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[psfimage+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[psfimage]))
	        call dp_iimname (Memc[image], Memc[outfname], "psf",
		    Memc[outfname], SZ_FNAME)
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
		call strcpy (DEF_DEFNAME, Memc[groupfile], SZ_FNAME)
	    root = fnldir (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[groupfile + root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[groupfile]))
	        call dp_outname (Memc[image], Memc[outfname], "grp",
		    Memc[outfname], SZ_FNAME)
	    else
	        call strcpy (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    if (DP_TEXT(dao) == YES)
		grp = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	    else
	        grp = tbtopn (Memc[outfname], NEW_FILE, 0)
	    call dp_sets (dao, OUTPHOTFILE, Memc[outfname])

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

            # Uncache memory.
            call fixmem (old_size)

	}

	# Close the image/file lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call imtclose (pimlist)
	call fntclsb (olist)

	# Free the photometry structure
	call dp_apclose (dao)

	# Free the PSF structure.
	call dp_fitclose (dao)

	# Free the daophot structure.
	call dp_free (dao)

	call sfree(sp)
end	
