include <imhdr.h>
include	<fset.h>
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
pointer	rejfile			# the output rejections file
int	cache			# cache the data in memory
int	verbose			# verbose mode ?
int	verify			# verify critical task parameters ?
int	update			# update the task parameters ?
int	version			# version number

pointer	sp, outfname, im, subim, dao, str
int	imlist, limlist, alist, lalist, pimlist, lpimlist, olist, lolist
int	simlist, lsimlist, rlist, lrlist, photfd, psffd, allfd, root, savesub
int	rejfd, wcs
bool	ap_text

pointer	immap(), tbtopn()
int	clgwrd(), clgeti()
int	open(), fnldir(), strncmp(), strlen(), btoi(), access()
int	fstati(), imtopen, imtlen(), imtgetim(), fntopnb(), fntlenb()
int	fntgfnb()
bool	itob(), clgetb()

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
	call salloc (rejfile, SZ_FNAME, TY_CHAR)
	call salloc (subimage, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the task input and output file names.
	call clgstr ("image", Memc[image], SZ_FNAME)
 	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("allstarfile", Memc[allstarfile], SZ_FNAME)
	call clgstr ("subimage", Memc[subimage], SZ_FNAME)
	call clgstr ("rejfile", Memc[rejfile], SZ_FNAME)

	# Get the task mode parameters.
	verbose = btoi (clgetb ("verbose"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	cache = btoi (clgetb ("cache"))
	version = clgeti ("version")
	version = 2

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
	rlist = fntopnb (Memc[rejfile], NO)
	lrlist = fntlenb (rlist)

	# Test the lengths of the photometry file, psf image and subtracted
	# image lists are the same as the length of the input image.

	if ((limlist != lalist) && (strncmp (Memc[photfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call imtclose (simlist)
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
	    call fntclsb (rlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and psf file list lengths")
	}

	if ((limlist != lsimlist) && (strncmp (Memc[subimage], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and subtracted image list lengths")
	}

	if ((limlist != lolist) && (strncmp (Memc[allstarfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and subtracted image list lengths")
	}

	if ((lrlist > 0) && (limlist != lrlist) && (strncmp (Memc[rejfile],
	    DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call imtclose (pimlist)
	    call fntclsb (olist)
	    call fntclsb (rlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and subtracted image list lengths")
	}

	# Open the input image, initialize the daophot structure, get the
	# pset parameters

	call dp_gppars (dao)	

	# Set some parameters.
	call dp_seti (dao, VERBOSE, verbose)

	# Verify and update the parameters as appropriate.
	if (verify == YES) {
	    call dp_aconfirm (dao)
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

	# Initialize the daophot fitting structure.
	call dp_apsetup (dao)

	# Initialize the allstar structure.
	call dp_allstarsetup (dao)

	# Loop over the images. 
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the image and store some header parameters.
	    im = immap (Memc[image], READ_ONLY, 0)		
	    call dp_imkeys (dao, im)
	    call dp_sets (dao, INIMAGE, Memc[image])

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
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        photfd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        photfd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_sets (dao, INPHOTFILE, Memc[outfname])
	    call dp_wgetapert (dao, im, photfd, DP_MAXNSTAR(dao), ap_text)

	    # Open the PSF image and read in the PSF.
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
	
	    # Open the output ALLSTAR file. If the output is DEF_DEFNAME,
	    # dir$default or a directory specification then the extension
	    # "als" is added to the image name and a suitable version
	    # number if appended to the output name.

	    if (fntgfnb (olist, Memc[allstarfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[allstarfile], SZ_FNAME)
	    root = fnldir (Memc[allstarfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[allstarfile+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[allstarfile]))
	        call dp_outname (Memc[image], Memc[outfname], "als",
		    Memc[outfname], SZ_FNAME)
	    else 
	        call strcpy (Memc[allstarfile], Memc[outfname], SZ_FNAME)
	    if (DP_TEXT(dao)  == YES)
	        allfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	    else
	        allfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    call dp_sets (dao, OUTPHOTFILE, Memc[outfname])

	    if (lrlist <= 0) {
		rejfd = NULL
		Memc[outfname] = EOS
	    } else {
	        if (fntgfnb (rlist, Memc[rejfile], SZ_FNAME) == EOF)
		    call strcpy (DEF_DEFNAME, Memc[rejfile], SZ_FNAME)
	        root = fnldir (Memc[rejfile], Memc[outfname], SZ_FNAME)
	        if (strncmp (DEF_DEFNAME, Memc[rejfile+root],
		    DEF_LENDEFNAME) == 0 || root == strlen (Memc[rejfile]))
	            call dp_outname (Memc[image], Memc[outfname], "arj",
		        Memc[outfname], SZ_FNAME)
	        else 
	            call strcpy (Memc[rejfile], Memc[outfname], SZ_FNAME)
	        if (DP_TEXT(dao)  == YES)
	            rejfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	        else
	            rejfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    }
	    call dp_sets (dao, OUTREJFILE, Memc[outfname])

	    # Open the subtracted image.
	    savesub = YES
	    if (imtgetim (simlist, Memc[subimage], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[subimage], SZ_FNAME)
	    root = fnldir (Memc[subimage], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[subimage+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[subimage])) {
	        call dp_oimname (Memc[image], Memc[outfname], "sub",
		    Memc[outfname], SZ_FNAME)
	        subim = immap (Memc[outfname], NEW_COPY, im)
		IM_NDIM(subim) = 2
	        IM_PIXTYPE(subim) = TY_REAL
	    } else {
	        call strcpy (Memc[subimage], Memc[outfname], SZ_FNAME)
	        subim = immap (Memc[outfname], NEW_COPY, im)
		IM_NDIM(subim) = 2
	        IM_PIXTYPE(subim) = TY_REAL
	    }
	    call dp_sets (dao, OUTIMAGE, Memc[outfname])

	    # Fit the stars.
	    call dp_astar (dao, im, subim, allfd, rejfd, cache, savesub,
	        version)

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

	    # Close the output rejections files.
	    if (rejfd != NULL) {
	        if (DP_TEXT(dao) == YES)
		    call close (rejfd)
	        else
	            call tbtclo (rejfd)
	    }

	    # Close the output subtracted image.
	    call strcpy (IM_HDRFILE(subim), Memc[subimage], SZ_FNAME)
	    call imunmap (subim)
	    if (savesub == NO)
	        call imdelete (Memc[subimage])
	}

	# Close the file/image lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call imtclose (pimlist)
	call fntclsb (olist)
	call fntclsb (rlist)
	call imtclose (simlist)

	# Close the allstar structure.
	call dp_alclose (dao)

	# Close the photometry structure.
	call dp_apclose (dao)

	# Close the PSF structure.
	call dp_fitclose (dao)

	# Close up the daophot structure.
	call dp_free (dao)

	call sfree(sp)
end	
