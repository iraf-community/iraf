include	<fset.h>
include <gset.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/psfdef.h"

# T_PSTSELECT -- Select good candidate PSF stars from a DAOPHOT photometry
# file.

procedure t_pstselect ()

pointer	image			# input image
pointer	photfile		# input rough photometry
pointer	pstfile			# output PSTFILE table
int	maxnpsf			# maximimum number of psf stars
pointer	plotfile		# pointer to the plot metacode file
bool	interactive		# interactive mode
pointer	plottype		# default plot type
int	verify			# verify the critical parameters
int	update			# update the critical parameters
pointer	graphics		# the graphics device
pointer	display			# the display device

bool	ap_text
int	imlist, limlist, alist, lalist, olist, lolist, root, apd, pmgd, pltype
pointer	sp, pfd, dao, outfname, curfile, im, gd, id, mgd

bool	clgetb(), itob(), streq()
int	tbtopn(), open(), fnldir(), strlen(), strncmp(), fstati(), btoi()
int	access(), fntopnb(), fntlenb(), clgeti(), imtopen(), imtlen()
int	fntgfnb(), imtgetim(), strdic(), dp_stati()
pointer	immap(), gopen()
errchk	gopen()

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (photfile, SZ_FNAME, TY_CHAR)
	call salloc (pstfile, SZ_FNAME, TY_CHAR)
	call salloc (plotfile, SZ_FNAME, TY_CHAR)
	call salloc (plottype, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (curfile, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[photfile], SZ_FNAME)
	call clgstr ("pstfile", Memc[pstfile], SZ_FNAME)
	maxnpsf = clgeti ("maxnpsf")
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	# Get the lists.
	imlist = imtopen (Memc[image])
	limlist = imtlen (imlist)
	alist = fntopnb (Memc[photfile], NO)
	lalist = fntlenb (alist)
	olist = fntopnb (Memc[pstfile], NO)
	lolist =  fntlenb (olist)

	# Test that the lengths of the photometry file and psf star file
	# lists are the same as the input image list.

	if ((limlist != lalist) && (strncmp (Memc[photfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and photometry file list lengths\n")
	}

	if ((limlist != lolist) && (strncmp (Memc[pstfile], DEF_DEFNAME,
	    DEF_LENDEFNAME) != 0)) {
	    call imtclose (imlist)
	    call fntclsb (alist)
	    call fntclsb (olist)
	    call sfree (sp)
	    call error (0,
	        "Incompatable image and photometry file list lengths\n")
	}

	# Initialize the DAOPHOT structure, and get the pset parameters.
	call dp_gppars (dao, NULL)	
	if (verify == YES) {
	    call dp_ptconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}

	# Initialize the photometry structure.
	call dp_apsetup (dao)

	# Initialize the PSF fitting structure.
	call dp_psfsetup (dao)

	# Initialize the PSF structure.
	call dp_fitsetup (dao)

	# Is the task interactive or not?
	call clgstr ("icommands.p_filename", Memc[curfile], SZ_FNAME)
	if (Memc[curfile] == EOS)
	    interactive = clgetb ("interactive")
	else
	    interactive = false

	# Get the graphics display and plot file devices.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)
	call clgstr ("plottype", Memc[plottype], SZ_FNAME)
	call clgstr ("plotfile", Memc[plotfile], SZ_FNAME)

	# Open graphics and display devices if appropriate.
	if (interactive) {
	    call dp_seti (dao, VERBOSE, YES)
	    if (Memc[graphics] == EOS)
		gd = NULL
	    else {
		iferr {
		    gd = gopen (Memc[graphics], APPEND+AW_DEFER, STDGRAPH)
		} then {
		    call eprintf ("Warning: Error opening graphics device\n")
		    gd = NULL
		}
	    }
	    if (Memc[display] == EOS)
		id = NULL
	    else if (streq (Memc[graphics], Memc[display])) {
		id = gd
	    } else {
		iferr {
		    id = gopen (Memc[display], APPEND, STDIMAGE)
		} then {
		    call eprintf (
		"Warning: Graphics overlay not available for display device\n")
		    id = NULL
		}
	    }
	} else {
	    gd = NULL
	    id = NULL
	    call dp_seti (dao, VERBOSE, btoi (clgetb ("verbose")))
	}

	# Open the plot file.
	if (Memc[plotfile] == EOS)
	    pmgd = NULL
	else
	    pmgd = open (Memc[plotfile], APPEND, BINARY_FILE)
	if (pmgd != NULL)
	    mgd = gopen (Memc[graphics], NEW_FILE, pmgd)
	else
	    mgd = NULL

	# Set the default plot type.
	pltype = strdic (Memc[plottype], Memc[plottype], SZ_FNAME, PSF_PLOTS)
	call dp_pseti (dao, PLOTTYPE, pltype)

	# Loop over the list of input files
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image.
	    im = immap (Memc[image], READ_ONLY, 0)
	    call dp_sets (dao, INIMAGE, Memc[image])

	    # Open the input photometry table and read in the photometry.
	    if (fntgfnb (alist, Memc[photfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[photfile], SZ_FNAME)
	    root = fnldir (Memc[photfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[photfile+root], DEF_LENDEFNAME) ==
	        0 || root == strlen (Memc[photfile]))
	        call dp_inname (Memc[image], "", "mag", Memc[outfname],
		    SZ_FNAME)
	    else
	        call strcpy (Memc[photfile], Memc[outfname], SZ_FNAME)
	    ap_text =  itob (access (Memc[outfname], 0, TEXT_FILE))
	    if (ap_text)
	        apd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    else
	        apd = tbtopn (Memc[outfname], READ_ONLY, 0)
	    call dp_getapert (dao, apd, DP_MAXNSTAR(dao), ap_text)
	    call dp_sets (dao, INPHOTFILE, Memc[outfname])

	    # Open the output PSTSELECT file. If the output is "default",
	    # dir$default or a directory specification then the extension .pst
	    # is added to the image name and a suitable version number is
	    # appended to the output name.

	    if (fntgfnb (olist, Memc[pstfile], SZ_FNAME) == EOF)
		call strcpy (DEF_DEFNAME, Memc[pstfile], SZ_FNAME)
	    root = fnldir (Memc[pstfile], Memc[outfname], SZ_FNAME)
	    if (strncmp (DEF_DEFNAME, Memc[pstfile + root], DEF_LENDEFNAME) ==
	        0 || root == strlen (Memc[pstfile]))
	        call dp_outname (Memc[image], "", "pst", Memc[outfname],
		    SZ_FNAME)
	    else
	        call strcpy (Memc[pstfile], Memc[outfname], SZ_FNAME)
	    if (ap_text)
	        pfd = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	    else
	        pfd = tbtopn (Memc[outfname], NEW_FILE, 0)
	    call dp_sets (dao, OUTPHOTFILE, Memc[outfname])

	    if (DP_VERBOSE(dao) == YES) {
		call printf ("\nSelecting PSF stars for image %s\n")
		    call pargstr (Memc[image])
	        call dp_stats (dao, INPHOTFILE, Memc[outfname], SZ_FNAME)
		call printf ("\t%d stars read from file %s\n\n")
		    call pargi (dp_stati (dao, APNUM))
		    call pargstr (Memc[outfname])
	    }

	    # Now select the PSF stars.
	    if (Memc[curfile] != EOS)
	        call dp_gpstars (dao, im, apd, pfd, ap_text, maxnpsf, NULL,
		    mgd, NULL, false, true)
	    else if (interactive)
	        call dp_gpstars (dao, im, apd, pfd, ap_text, maxnpsf, gd, mgd,
		    id, true, false)
	    else
	        call dp_gpstars (dao, im, apd, pfd, ap_text, maxnpsf, NULL,
		    mgd, NULL, false, false)

	    # Close the input image.
	    call imunmap (im)

	    # Close the photometry file. 
	    if (ap_text)
	        call close (apd)
	    else
	        call tbtclo (apd)

	    # Close the output table.
	    if (ap_text)
	        call close (pfd)
	    else
	        call tbtclo (pfd)
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
	if (pmgd != NULL)
	    call close (pmgd)

	# Close the image/file lists.
	call fntclsb (alist)
	call fntclsb (olist)

	# Close the PSF structure.
	call dp_fitclose (dao)

	# Close the PSF fitting structure.
	call dp_psfclose (dao)

	# Free the photometry structure.
	call dp_apclose (dao)
	
	# Free the daophot structure.
	call dp_free (dao)

	call sfree(sp)
end	
