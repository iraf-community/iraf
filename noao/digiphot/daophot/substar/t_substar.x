include	<fset.h>
include <imhdr.h>
include "../lib/daophotdef.h"

# T_SUBSTAR  -- Procedure to subtract DAOPHOT photometry from an image.

procedure t_substar ()

pointer	image				# name of the image
pointer	photfile			# input photometry file
pointer	exfile				# input exclude file
pointer	psfimage			# name of the output PSF
pointer	subimage			# subtracted image

pointer	sp, input, output, dao, outfname, str
int	psffd, photfd, root, verify, update, wcs
int	imlist, limlist, alist, lalist, pimlist, lpimlist, simlist, lsimlist
int	exfd, elist, lelist, cache, req_size, old_size, buf_size, memstat
bool	ap_text, ex_text

pointer	immap(), tbtopn()
int	open(), fnldir(), strlen(), strncmp(), access(), fstati(), btoi()
int	imtopen(), imtlen(), imtgetim(), fntopnb(), fntlenb(), fntgfnb()
int	clgwrd(), sizeof(), dp_memstat()
bool	clgetb(), itob()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (photfile, SZ_FNAME, TY_CHAR)
	call salloc (exfile, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (subimage, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("exfile", Memc[exfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("subimage", Memc[subimage], SZ_FNAME)
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	cache = btoi (clgetb ("cache"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[photfile], NO)
	lalist = fntlenb (alist)
	elist = fntopnb (Memc[exfile], NO)
	lelist = fntlenb (elist)
	pimlist = imtopen (Memc[psfimage])
	lpimlist = imtlen (pimlist)
	simlist = imtopen (Memc[subimage])
	lsimlist = imtlen (simlist)

	# Test that the lengths of the photometry file, psf image and
	# subtracted image lists are the same as the length of the input
	# image list.

	if ((limlist != lalist) && (strncmp (Memc[photfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call fntclsb (elist)
	    call imtclose (pimlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and photometry file list lengths")
	}

	if ((lelist != 0) && (limlist != lelist) && (strncmp (Memc[exfile],
	    DEF_DEFNAME, DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call fntclsb (elist)
	    call imtclose (pimlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and exclude file list lengths")
	}

	if ((limlist != lpimlist) && (strncmp (Memc[psfimage], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call fntclsb (elist)
	    call imtclose (pimlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and psf file list lengths")
	}

	if ((limlist != lsimlist) && (strncmp (Memc[subimage], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call fntclsb (elist)
	    call imtclose (pimlist)
	    call imtclose (simlist)
	    call sfree (sp)
	    call error (0,
	        "Incompatible image and subtracted image list lengths")
	}

	# Initialize the DAOPHOT structure and get the pset parameters.
	call dp_gppars (dao)	
	call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))

	# Verify the critical parameters.
	if (verify == YES) {
	    call dp_sconfirm (dao)
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

	# Initialize the star list.
	call dp_apselsetup (dao)

	# Loop over the images
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open input and output images
	    input = immap (Memc[image], READ_ONLY, 0)		
	    call dp_sets (dao, INIMAGE, Memc[image])

            # Cache the input image pixels.
            req_size = MEMFUDGE * (2 * IM_LEN(input,1) * IM_LEN(input,2) *
                sizeof (IM_PIXTYPE(input)))
            memstat = dp_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call dp_pcache (input, INDEFI, buf_size)

	    # If the output image name is DEF_DEFNAME, dir$default or a
	    # directory specification then the extension "sub" is added to
	    # the image name and a suitable version number is appended to the
	    # output name.

	    if (imtgetim (simlist, Memc[subimage], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[subimage], SZ_FNAME)
	    root = fnldir (Memc[subimage], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[subimage + root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[subimage])) {
	        call dp_oimname (Memc[image], Memc[outfname], "sub",
		    Memc[outfname], SZ_FNAME)
	        output = immap (Memc[outfname], NEW_COPY, input)
	    } else {
	        call strcpy (Memc[subimage], Memc[outfname], SZ_FNAME)
	        output = immap (Memc[outfname], NEW_COPY, input)
	    }
	    call dp_sets (dao, OUTIMAGE, Memc[outfname])
            if (memstat == YES)
                call dp_pcache (output, INDEFI, buf_size)

	    # Open input photometry table and read in the photometry.
	    if (fntgfnb (alist, Memc[photfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[photfile], SZ_FNAME)
	    root = fnldir (Memc[photfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[photfile+root],
	        DEF_LENDEFNAME) == 0 || root == strlen (Memc[photfile]))
	        call dp_inname (Memc[image], Memc[outfname], "nst",
		    Memc[outfname], SZ_FNAME)
	    else
	        call strcpy (Memc[photfile], Memc[outfname], SZ_FNAME)
	    ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        photfd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else 
	        photfd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_wgetapert (dao, input, photfd, DP_MAXNSTAR(dao), ap_text)
	    call dp_sets (dao, INPHOTFILE, Memc[outfname])

	    # Open the input exclude file.
	    if (lelist == 0) {
		exfd = NULL
		Memc[outfname] = EOS
	    } else {
	        if (fntgfnb (elist, Memc[exfile], SZ_FNAME) == EOF)
		    call strcpy (DEF_DEFNAME, Memc[exfile], SZ_FNAME)
	        root = fnldir (Memc[exfile], Memc[outfname], SZ_FNAME)
	        if (strncmp (DEF_DEFNAME, Memc[exfile+root],
	            DEF_LENDEFNAME) == 0 || root == strlen (Memc[exfile]))
	            call dp_inname (Memc[image], Memc[outfname], "pst",
		        Memc[outfname], SZ_FNAME)
	        else
	            call strcpy (Memc[exfile], Memc[outfname], SZ_FNAME)
	        ex_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	        if (ex_text)
	            exfd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	        else 
	            exfd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    }
	    call dp_sets (dao, COORDS, Memc[outfname])

	    # Read in the PSF
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
	    call dp_readpsf (dao, psffd)
	    call dp_sets (dao, PSFIMAGE, Memc[outfname])
	
	    # Now go and subtract those stars!
	    call dp_substar (dao, input, exfd, ex_text, output)

	    # Close the input and output images.
	    call imunmap (input)
	    call imunmap (output)

	    # Close the photometry file.
	    if (ap_text)
		call close (photfd)
	    else
	        call tbtclo (photfd)

	    # Close the exclude file.
	    if (ex_text)
		call close (exfd)
	    else
	        call tbtclo (exfd)

	    # Close the PSF image.
	    call imunmap (psffd)

            # Uncache memory
            call fixmem (old_size)

	}

	# Close the lists.
	call imtclose (imlist)
	call fntclsb (alist)
	call fntclsb (elist)
	call imtclose (pimlist)
	call imtclose (simlist)

	# Free the daophot structures.
	call dp_apclose (dao)
	call dp_fitclose (dao)
	call dp_free (dao)

	call sfree (sp)
end	
