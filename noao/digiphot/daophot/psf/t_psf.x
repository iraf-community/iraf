include	<fset.h>
include <imhdr.h>
include <gset.h>
include	"../lib/daophotdef.h"
include	"../lib/apseldef.h"
include	"../lib/psfdef.h"

# T_PSF  -- Generate a point spread function from one or more stars in the
# image frame.

procedure t_psf ()

pointer	image				# the input image
pointer	photfile			# input aperture photometry file
pointer	pstarfile			# input psf star file
pointer	psfimage			# output psf image
pointer	groupfile			# output psf group file
pointer	opstfile			# output psf star file
pointer	graphics			# pointer to graphics device name
pointer	plotfile			# pointer to plotfile name
int	cache				# cache the input image pixels
pointer	display				# pointer to display device name
bool	matchbyid			# match psf stars by id or position
bool	interactive			# the mode of task operation
bool	showplots			# display plots of the psf stars
pointer	plottype			# type of psf plot
bool	mkstars				# mark deleted and accepted psf stars

pointer	sp, im, apd, psfim, dao, mgd, gd, id
pointer	outfname, curfile, str
int	imlist, limlist, alist, lalist, clist, lclist, pimlist, lpimlist
int	olist, lolist, oclist, loclist, up, verify, update, wcs
int	root, min_lenuserarea, pltype, pfd, pst, psfgr, opst
int	req_size, old_size, buf_size, memstat
bool	ap_text, pst_text

pointer	immap(), tbtopn(), gopen()
int	fnldir(), strlen(), strncmp(), btoi(), envfind(), ctoi(), clgwrd()
int	strdic(), open(), access(), fstati(), dp_stati(), dp_pstati()
int	imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb(), fntgfnb()
int	sizeof(), dp_memstat()
bool	streq(), clgetb(), itob(), dp_updatepsf()
errchk 	gopen

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (photfile, SZ_FNAME, TY_CHAR)
	call salloc (pstarfile, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (groupfile, SZ_FNAME, TY_CHAR)
	call salloc (opstfile, SZ_FNAME, TY_CHAR)
	call salloc (plottype, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (plotfile, SZ_FNAME, TY_CHAR)
	call salloc (curfile, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("pstfile", Memc[pstarfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("opstfile", Memc[opstfile], SZ_FNAME)
	call clgstr ("groupfile", Memc[groupfile], SZ_FNAME)
	cache = btoi (clgetb ("cache"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[photfile], NO)
	lalist = fntlenb (alist)
	clist = fntopnb (Memc[pstarfile], NO)
	lclist = fntlenb (clist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	olist = fntopnb (Memc[groupfile], NO)
	lolist =  fntlenb (olist)
	oclist = fntopnb (Memc[opstfile], NO)
	loclist = fntlenb (oclist)

	# Test that the lengths of the photometry file, psf image, and
        # output file lists are the same as the length of the input image
        # list.

	# Compare the image and photometry file list lengths.
        if ((limlist != lalist) && (strncmp (Memc[photfile],
            DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
            call imtclose (imlist)
            call fntclsb (alist)
            call fntclsb (clist)
            call imtclose (pimlist)
            call fntclsb (olist)
            call fntclsb (oclist)
            call sfree (sp)
            call error (0,
                "Incompatable image and photometry file list lengths")
        }

	# Compare the image and psf star list lengths.
        if ((lclist != 0) && (limlist != lclist) && (strncmp (Memc[pstarfile],
            DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
            call imtclose (imlist)
            call fntclsb (alist)
            call fntclsb (clist)
            call imtclose (pimlist)
            call fntclsb (olist)
            call fntclsb (oclist)
            call sfree (sp)
            call error (0,
                "Incompatable image and psf star file list lengths")
        }

	# Compare the image and psf image list lengths.
        if ((limlist != lpimlist) && (strncmp (Memc[psfimage],
            DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
            call imtclose (imlist)
            call fntclsb (alist)
            call fntclsb (clist)
            call imtclose (pimlist)
            call fntclsb (olist)
            call fntclsb (oclist)
            call sfree (sp)
            call error (0,
                "Incompatable image and psf file list lengths")
        }

	# Compare the image and groupfile list lengths.
        if ((limlist != lolist) && (strncmp (Memc[groupfile],
            DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
            call imtclose (imlist)
            call fntclsb (alist)
            call fntclsb (clist)
            call imtclose (pimlist)
            call fntclsb (olist)
            call fntclsb (oclist)
            call sfree (sp)
            call error (0,
                "Incompatable image and group file list lengths")
        }

	# Compare the image and output pstfile list lengths.
        if ((limlist != loclist) && (strncmp (Memc[opstfile],
            DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
            call imtclose (imlist)
            call fntclsb (alist)
            call fntclsb (clist)
            call imtclose (pimlist)
            call fntclsb (olist)
            call fntclsb (oclist)
            call sfree (sp)
            call error (0,
                "Incompatable image and output psf file list lengths")
        }

	# Initialize DAOPHOT main structure, get pset parameters. 
	call dp_gppars (dao)	

	# Verify the critical parameters and update if appropriate.
	if (verify == YES) {
	    call dp_pconfirm (dao)
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

	# Initialize the photometry structure.
	call dp_apselsetup (dao)

	# Initialize the PSF structure.
	call dp_fitsetup (dao)

	# Intialize the PSF fitting structure.
	call dp_psfsetup (dao)

	# Matching algorithm for stars in the psf star list.
	matchbyid = clgetb ("matchbyid")

	# Is the task interactive or not?
	call clgstr ("icommands.p_filename", Memc[curfile], SZ_FNAME)
	if (Memc[curfile] == EOS)
	    interactive = clgetb ("interactive")
	else
	    interactive = false

	# Get the graphics, display and plot file devices.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)
	call clgstr ("plottype", Memc[plottype], SZ_FNAME)
	call clgstr ("plotfile", Memc[plotfile], SZ_FNAME)

	# Open graphics and display devices if appropriate.
	if (interactive) {
	    call dp_seti (dao, VERBOSE, YES)
	    showplots = clgetb ("showplots")
	    if (Memc[graphics] == EOS)
	        gd = NULL
	    else {
	        iferr {
		    gd = gopen (Memc[graphics], APPEND+AW_DEFER, STDGRAPH)
	        } then {
		    call eprintf (
		        "Warning: Error opening graphics device. \n")
		    gd = NULL
	        }
  	    }
	    if (Memc[display] == EOS)
	        id = NULL
	    else if (streq (Memc[graphics], Memc[display]))
	        id = gd
	    else {
	        iferr {
		    id = gopen (Memc[display], APPEND, STDIMAGE)
	        } then {
		    call eprintf (
		"Warning: Graphics overlay not available for display device.\n")
		    id = NULL
	        }
	    }
            if (id != NULL)
                mkstars = clgetb ("mkstars")
            else
                mkstars = false
	} else {
	    gd = NULL
	    id = NULL
	    call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))
	    showplots = false
	    mkstars = false
	}

	# Open the plot file.
	if (Memc[plotfile] == EOS)
	    pfd = NULL
	else
	    pfd = open (Memc[plotfile], APPEND, BINARY_FILE)
	if (pfd != NULL)
	    mgd = gopen (Memc[graphics], NEW_FILE, pfd)
	else
	    mgd = NULL

	# Set the default plot type.
	pltype = strdic (Memc[plottype], Memc[plottype], SZ_FNAME, PSF_PLOTS)
	call dp_pseti (dao, PLOTTYPE, pltype)

	# Loop over the list of images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open input image
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_imkeys (dao, im)
	    call dp_sets (dao, INIMAGE, Memc[image])

            # Set up the display coordinate system.
            if ((id != NULL) && (id != gd))
                call dp_gswv (id, Memc[image], im, 4)

            # Cache the input image pixels.
            req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
            memstat = dp_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call dp_pcache (im, INDEFI, buf_size)

	    # Open the input photometry list and store the descriptor.
	    # PSF can read either an APPHOT PHOT file or an ST TABLE 
	    # file.

	    if (fntgfnb (alist, Memc[photfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[photfile], SZ_FNAME)
	    root = fnldir (Memc[photfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[photfile+root], DEF_LENDEFNAME) ==
	        0 || root == strlen (Memc[photfile])) 
	        call dp_inname (Memc[image], Memc[outfname], "mag",
		    Memc[outfname], SZ_FNAME)
	    else
	        call strcpy (Memc[photfile], Memc[outfname], SZ_FNAME)
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        apd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        apd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_wgetapert (dao, im, apd, dp_stati (dao, MAXNSTAR), ap_text)
	    call dp_sets (dao, INPHOTFILE, Memc[outfname])

	    # Open the input photometry list and store the descriptor.
	    # PSF can read either an APPHOT PHOT file or an ST TABLE 
	    # file.

	    if (lclist == 0) {
		pst = NULL
		Memc[outfname] = EOS
	    } else {
	        if (fntgfnb (clist, Memc[pstarfile], SZ_FNAME) == EOF)
		    call strcpy (DEF_DEFNAME, Memc[pstarfile], SZ_FNAME)
	        root = fnldir (Memc[pstarfile], Memc[outfname], SZ_FNAME)
	        if (strncmp (DEF_DEFNAME, Memc[pstarfile+root],
		    DEF_LENDEFNAME) == 0 || root == strlen (Memc[pstarfile])) 
	            call dp_inname (Memc[image], Memc[outfname], "pst",
		        Memc[outfname], SZ_FNAME)
	        else
	            call strcpy (Memc[pstarfile], Memc[outfname], SZ_FNAME)
	        pst_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	        if (pst_text)
	            pst = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	        else
	            pst = tbtopn (Memc[outfname], READ_ONLY, 0)
	    }
	    call dp_sets (dao, COORDS, Memc[outfname])

	    # Open output image containing PSF, output file containing list
	    # of PSF stars actually used, and the file for PSF neighbors
	    # If the output is "default", dir$default or a directory
	    # specification then the extension "psf" is added to the PSF image
	    # and "psg" to the neighbors file. A suitable version number is
	    # added to the output name as well. Check that there is enough
	    # space in the image header user area to hold many PSF stars.

	    if (imtgetim (pimlist, Memc[psfimage], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[psfimage], SZ_FNAME)
	    root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[psfimage + root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[psfimage])) {
	        call dp_oimname (Memc[image], Memc[outfname], "psf",
		    Memc[outfname], SZ_FNAME)
	    } else
	        call strcpy (Memc[psfimage], Memc[outfname], SZ_FNAME)
	    if (envfind ("min_lenuserarea", Memc[str], SZ_FNAME) > 0) {
	        up = 1
	        if (ctoi (Memc[str], up, min_lenuserarea) <= 0)
		    min_lenuserarea = MIN_LENUSERAREA
	        else
		    min_lenuserarea = max (MIN_LENUSERAREA, min_lenuserarea)
            } else
	        min_lenuserarea = MIN_LENUSERAREA
	    call dp_pseti (dao, LENUSERAREA, min_lenuserarea)
	    psfim = immap (Memc[outfname], NEW_IMAGE, min_lenuserarea)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])

	    if (fntgfnb (olist, Memc[groupfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[groupfile], SZ_FNAME)
	    root = fnldir (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[groupfile + root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[groupfile])) {
	        call dp_outname (Memc[image], Memc[outfname], "psg",
		    Memc[outfname], SZ_FNAME)
	    } else
	        call strcpy (Memc[groupfile], Memc[outfname], SZ_FNAME)
	    if (dp_stati (dao, TEXT) == YES)
	        psfgr = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	    else
	        psfgr = tbtopn (Memc[outfname], NEW_FILE, 0)
	    call dp_sets (dao, OUTPHOTFILE, Memc[outfname])


	    if (fntgfnb (oclist, Memc[opstfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[opstfile], SZ_FNAME)
	    root = fnldir (Memc[opstfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[opstfile+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[opstfile])) {
	        call dp_outname (Memc[image], Memc[outfname], "pst",
		    Memc[outfname], SZ_FNAME)
	    } else
	        call strcpy (Memc[opstfile], Memc[outfname], SZ_FNAME)
	    if (dp_stati (dao, TEXT) == YES)
	        opst = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	    else
	        opst = tbtopn (Memc[outfname], NEW_FILE, 0)
	    call dp_sets (dao, OUTREJFILE, Memc[outfname])

	    # Print banner.
	    if (DP_VERBOSE(dao) == YES) {
		call printf ("\nComputing PSF for image: %s\n")
		    call pargstr (Memc[image])
		call dp_stats (dao, INPHOTFILE, Memc[str], SZ_FNAME)
		call printf ("%d stars read from %s\n\n")
		    call pargi (dp_stati (dao, APNUM))
		    call pargstr (Memc[str])
	    }

	    # Read in the PSF star list.
	    call dp_pseti (dao, PNUM, 0)
	    if (pst != NULL) {
	        call dp_rpstars (dao, im, pst, pst_text, gd, mgd, id, mkstars,
		    matchbyid, showplots)
	        if (DP_VERBOSE(dao) == YES) {
		    call dp_stats (dao, COORDS, Memc[str], SZ_FNAME)
		    call printf ("\n%d PSF stars read from %s\n\n")
		        call pargi (dp_pstati (dao, PNUM))
		        call pargstr (Memc[str])
	        }
	    }

	    # Make the PSF.
	    if (Memc[curfile] != EOS)
	        call dp_mkpsf (dao, im, psfim, opst, psfgr, gd, mgd, id, false,
	            false, false)
	    else if (interactive)
	        call dp_mkpsf (dao, im, psfim, opst, psfgr, gd, mgd, id,
		    mkstars, true, true)
	    else if (! dp_updatepsf (dao, im, psfim, opst, psfgr, false, false,
	        false))
		call dp_rmpsf (dao, psfim, opst, psfgr)

	    # Close the input image.
	    call imunmap (im)

	    # Close the input photometry file.
	    if (apd != NULL) {
	        if (ap_text)
	    	    call close (apd)
	        else
		    call tbtclo (apd)
	    }

	    # Close the input psf star file.
	    if (pst != NULL) {
	        if (pst_text)
	    	    call close (pst)
	        else
		    call tbtclo (pst)
	    }

	    # Close PSF image.
	    if (psfim != NULL)
	        call imunmap (psfim)

	    # Close the output PSF star file.
	    if (opst != NULL) {
	        if (dp_stati (dao, TEXT) == YES)
		    call close (opst)
	        else
	            call tbtclo (opst)
	    }

	    # Close the group file.
	    if (psfgr != NULL) {
	        if (dp_stati (dao, TEXT) == YES)
		    call close (psfgr)
	        else
	            call tbtclo (psfgr)
	    }

            # Uncache memory.
            call fixmem (old_size)
	}

	# Close up the graphics and display streams.
	if (id == gd && id != NULL)
	    call gclose (id)
	else {
	    if (gd != NULL)
		call gclose (gd)
	    if (id != NULL)
		call gclose (id)
	}

	# Close the metacode plot files.
	if (mgd != NULL)
	    call gclose (mgd)
	if (pfd != NULL)
	    call close (pfd)

	# Close the image / file lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call fntclsb (clist)
	call imtclose (pimlist)
	call fntclsb (olist)
	call fntclsb (oclist)

	# Free the photometry structure.
	call dp_apclose (dao)

	# Free the PSF fitting structure.
	call dp_psfclose (dao)

	# Free the PSF structure.
	call dp_fitclose (dao)

	# Close up the daophot structures.
	call dp_free (dao)

	call sfree (sp)
end	
