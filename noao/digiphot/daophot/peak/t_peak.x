include	<fset.h>
include <imhdr.h>
include	"../lib/daophotdef.h"

# T_PEAK  -- Procedure to fit the PSF to single stars.

procedure t_peak ()

pointer	image				# name of the image
pointer	photfile			# input photometry file
pointer	psfimage			# the PSF image
pointer	peakfile			# output PEAK photometry file
pointer	rejfile				# output PEAK rejections file

pointer	sp, im, psfim, outfname, dao, str
int	apd, root, verbose, verify, cache, update, pkfd, rejfd
int	imlist, limlist, alist, lalist, pimlist, lpimlist, olist, lolist
int	rlist, lrlist, wcs, req_size, old_size, buf_size, memstat
bool	ap_text

pointer	immap(), tbtopn()
int	open(), fnldir(), strlen(), strncmp(), fstati(), btoi()
int	access(), imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb()
int	fntgfnb(), clgwrd(), sizeof(), dp_memstat()
bool	clgetb(), itob()

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
	call salloc (rejfile, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the various task parameters
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("peakfile", Memc[peakfile], SZ_FNAME)
	call clgstr ("rejfile", Memc[rejfile], SZ_FNAME)
	verbose = btoi (clgetb ("verbose"))
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
	olist = fntopnb (Memc[peakfile], NO)
	lolist =  fntlenb (olist)
	rlist = fntopnb (Memc[rejfile], NO)
	lrlist =  fntlenb (rlist)

	# Test that the lengths of the photometry file, psf image, and
	# output file lists are the same as the length of the input image
	# list.

	if ((limlist != lalist) && (strncmp (Memc[photfile],
	    DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and input photometry file list lengths")
	}

	if ((limlist != lpimlist) && (strncmp (Memc[psfimage],
	    DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and psf image list lengths")
	}

	if ((limlist != lolist) && (strncmp (Memc[peakfile],
	    DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and output photometry file list lengths")
	}

	if ((lrlist != 0) && (limlist != lolist) && (strncmp (Memc[rejfile],
	    DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and rejections file list lengths")
	}

	# Initialize the DAOPHOT structure, and get the pset parameters.
	call dp_gppars (dao)	
	call dp_seti (dao, VERBOSE, verbose)

	# Verify the standard algorithm parameters.
	if (verify == YES) {
	    call dp_pkconfirm (dao)
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

	# Initialize the PSF structure.
	call dp_fitsetup (dao)

	# Initialize the PEAK structure.
	call dp_pksetup (dao)

	# Loop over the list of images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the image.
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_imkeys (dao, im)
	    call dp_sets (dao, INIMAGE, Memc[image])

            # Cache the input image pixels.
            req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
            memstat = dp_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call dp_pcache (im, INDEFI, buf_size)

	    # Open the input photometry file.
	    if (fntgfnb (alist, Memc[photfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[photfile], SZ_FNAME)
	    root = fnldir (Memc[photfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[photfile+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[photfile]))
	        call dp_inname (Memc[image], Memc[outfname], "mag",
		    Memc[outfname], SZ_FNAME)
	    else
	        call strcpy (Memc[photfile], Memc[outfname], SZ_FNAME)
	    ap_text =  itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        apd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        apd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_sets (dao, INPHOTFILE, Memc[outfname])

	    # Read in the PSF function.
	    if (imtgetim (pimlist, Memc[psfimage], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[psfimage], SZ_FNAME)
	    root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[psfimage+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[psfimage]))
	        call dp_iimname (Memc[image], Memc[outfname], "psf",
		    Memc[outfname], SZ_FNAME)
	    else
	        call strcpy (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    psfim = immap (Memc[outfname], READ_ONLY, 0)
	    call dp_readpsf (dao, psfim)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])
	
	    # Open the output photometry file. If the output is "default",
	    # dir$default or a directory specification then the extension .pk
	    # is added to the image name and a suitable version number is
	    # appended to the output name.

	    if (fntgfnb (olist, Memc[peakfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[peakfile], SZ_FNAME)
	    root = fnldir (Memc[peakfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[peakfile + root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[peakfile]))
	        call dp_outname (Memc[image], Memc[outfname], "pk",
		    Memc[outfname], SZ_FNAME)
	    else
	        call strcpy (Memc[peakfile], Memc[outfname], SZ_FNAME)
	    if (DP_TEXT(dao) == YES)
	        pkfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	    else
	        pkfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    call dp_sets (dao, OUTPHOTFILE, Memc[outfname])

	    # Open the output rejections file if any are defined. If the
	    # output is "default", dir$default or a directory specification
	    # then the extension .prj is added to the image name and a
	    # suitable version number is appended to the output file name.

	    if (lrlist <= 0) {
		rejfd = NULL
		Memc[outfname] = EOS
	    } else {
	        if (fntgfnb (rlist, Memc[rejfile], SZ_FNAME) == EOF)
		    call strcpy (DEF_DEFNAME, Memc[rejfile], SZ_FNAME)
	        root = fnldir (Memc[rejfile], Memc[outfname], SZ_FNAME)
	        if (strncmp (DEF_DEFNAME, Memc[rejfile+root],
		    DEF_LENDEFNAME) == 0 || root == strlen (Memc[rejfile]))
	            call dp_outname (Memc[image], Memc[outfname], "prj",
		        Memc[outfname], SZ_FNAME)
	        else
	            call strcpy (Memc[rejfile], Memc[outfname], SZ_FNAME)
	        if (DP_TEXT(dao) == YES)
	            rejfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            rejfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    }
	    call dp_sets (dao, OUTREJFILE, Memc[outfname])

	    # Now go and do the PSF fitting.
	    call dp_peakphot (dao, im, apd, pkfd, rejfd, ap_text)

	    # Close the input image.
	    call imunmap (im)

	    # Close the input photometry file. 
	    if (ap_text)
	        call close (apd)
	    else
	        call tbtclo (apd)

	    # Close the PSF image.
	    call imunmap (psfim)

	    # Close the output photometry file.
	    if (DP_TEXT(dao) == YES)
	        call close (pkfd)
	    else
	        call tbtclo (pkfd)

	    # Close the output rejections file.
	    if (rejfd != NULL) {
	        if (DP_TEXT(dao) == YES)
	            call close (rejfd)
	        else
	            call tbtclo (rejfd)
	    }

            # Uncache memory.
            call fixmem (old_size)

	}

	# Close the image/file lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call imtclose (pimlist)
	call fntclsb (olist)
	call fntclsb (rlist)

	# Free the PEAK structure.
	call dp_pkclose (dao)

	# Free the PSF structure.
	call dp_fitclose (dao)
	
	# Free the daophot structure.
	call dp_free (dao)

	call sfree(sp)
end	
