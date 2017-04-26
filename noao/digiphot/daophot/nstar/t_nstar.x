include	<fset.h>
include <imhdr.h>
include "../lib/daophotdef.h"

# T_NSTAR  -- Procedure to fit the PSF to multiple stars.

procedure t_nstar ()

pointer	image				# input image descriptor
pointer	groupfile			# input group file descriptor
pointer	psfimage			# input PSF image descriptor
pointer	nstarfile			# output photometry file descriptor
pointer	rejfile				# output rejections file descriptor
int	verbose				# print messages 
int	verify				# verify the critical parameters
int	update				# update the parameter set
int	cache				# cache the input image pixels

pointer	sp, outfname, im, psfim, dao, str
int	imlist, limlist, alist, lalist, pimlist, lpimlist, olist, lolist
int	rlist, lrlist, root, grp, nst, rejfd, wcs, req_size, old_size
int	buf_size, memstat
bool	ap_text

pointer	immap(), tbtopn()
int	strlen(), strncmp(), fnldir(), fstati(), open(), btoi()
int	access(), imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb()
int	fntgfnb(), clgwrd(), sizeof(), dp_memstat()
bool	clgetb(), itob()

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
	call salloc (rejfile, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the input and output file names.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("groupfile", Memc[groupfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("nstarfile", Memc[nstarfile], SZ_FNAME)
	call clgstr ("rejfile", Memc[rejfile], SZ_FNAME)

	# Get the task mode parameters.
	verbose = btoi (clgetb ("verbose"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	cache = btoi (clgetb ("cache"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[groupfile], NO)
	lalist = fntlenb (alist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	olist = fntopnb (Memc[nstarfile], NO)
	lolist = fntlenb (olist)
	rlist = fntopnb (Memc[rejfile], NO)
	lrlist = fntlenb (rlist)

	# Test that the lengths of the photometry file, psf image, and
	# output file lists are the same as the length of the input image
	# list.

	if ((limlist != lalist) && (strncmp (Memc[groupfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and group file list lengths")
	}

	if ((limlist != lpimlist) && (strncmp (Memc[psfimage], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and psf file list lengths")
	}

	if ((limlist != lolist) && (strncmp (Memc[nstarfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and nstar file list lengths")
	}

	if ((lrlist > 0) && (limlist != lrlist) && (strncmp (Memc[rejfile],
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

	# Open the daophot structure and get some parameters.
	call dp_gppars (dao)	

	# Set some parameters.
	call dp_seti (dao, VERBOSE, verbose)

	# Verify and update the parameters if appropriate.
	if (verify == YES) {
	    call dp_nconfirm (dao)
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

	# Initialize the photometry structure.
	call dp_apsetup (dao)

	# Initialize the PSF structure.
	call dp_fitsetup (dao)

	# Initialize the nstar structure.
	call dp_nstarsetup (dao)

	# Loop over the images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image.
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_imkeys (dao, im)
	    call dp_sets (dao, INIMAGE, Memc[image])

            # Cache the input image pixels.
            req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
            memstat = dp_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call dp_pcache (im, INDEFI, buf_size)

	    # Open the input group table.
	    if (fntgfnb (alist, Memc[groupfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[groupfile], SZ_FNAME)
	    root = fnldir (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[groupfile+root],
	        DEF_LENDEFNAME) == 0 || (root == strlen (Memc[groupfile])))
	        call dp_inname (Memc[image], Memc[outfname], "grp",
		    Memc[outfname], SZ_FNAME)
	    else
	    	call strcpy (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        grp = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        grp = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_sets (dao, INPHOTFILE, Memc[outfname])

	    # Open and read the PSF image.
	    if (imtgetim (pimlist, Memc[psfimage], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[psfimage], SZ_FNAME)
	    root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[psfimage+root],
	        DEF_LENDEFNAME) == 0 || (root == strlen (Memc[psfimage])))
	        call dp_iimname (Memc[image], Memc[outfname], "psf",
		    Memc[outfname], SZ_FNAME)
	    else
	        call strcpy (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    psfim = immap (Memc[outfname], READ_ONLY, 0)
	    call dp_readpsf (dao, psfim)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])
	
	    # Open the output NSTAR file. If the output is DEF_DEFNAME,
	    # dir$default or a directory specification then the extension
	    # "nst" is added to the image name and a suitable version number
	    # is appended to the output name.

	    if (fntgfnb (olist, Memc[nstarfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[nstarfile], SZ_FNAME)
	    root = fnldir (Memc[nstarfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[nstarfile+root],
	        DEF_LENDEFNAME) == 0 || (root == strlen (Memc[nstarfile]))) {
	        call dp_outname (Memc[image], Memc[outfname], "nst",
		    Memc[outfname], SZ_FNAME)
	    } else
	        call strcpy (Memc[nstarfile], Memc[outfname], SZ_FNAME)
	    if (DP_TEXT(dao) == YES)
	        nst = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	    else
	        nst = tbtopn (Memc[outfname], NEW_FILE, 0)
	    call dp_sets (dao, OUTPHOTFILE, Memc[outfname])

	    if (lrlist <= 0) {
		rejfd = NULL
		Memc[outfname] = EOS
	    } else {
	        if (fntgfnb (rlist, Memc[rejfile], SZ_FNAME) == EOF)
		    call strcpy (DEF_DEFNAME, Memc[rejfile], SZ_FNAME)
	        root = fnldir (Memc[rejfile], Memc[outfname], SZ_FNAME)
	        if (strncmp (DEF_DEFNAME, Memc[rejfile+root],
		    DEF_LENDEFNAME) == 0 || (root == strlen (Memc[rejfile])))
	            call dp_outname (Memc[image], Memc[outfname], "nrj",
		        Memc[outfname], SZ_FNAME)
	        else
	            call strcpy (Memc[rejfile], Memc[outfname], SZ_FNAME)
	        if (DP_TEXT(dao) == YES)
	            rejfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            rejfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    }
	    call dp_sets (dao, OUTREJFILE, Memc[outfname])

	    # Do the PSF fitting.
	    call dp_nphot (dao, im, grp, nst, rejfd, ap_text)

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

	# Close the nstar structure.
	call dp_nsclose (dao)

	# Close the PSF structure.
	call dp_fitclose (dao)

	# Close the photometry structure.
	call dp_apclose (dao)

	# Free the daophot structure.
	call dp_free (dao)

	call sfree(sp)
end	
