include	<fset.h>
include <gset.h>
include "../lib/daophotdef.h"
include	"../lib/daophot.h"
include	"../lib/psfdef.h"
include "../lib/psf.h"

# T_PSF  -- Generate a point spread function from one or more stars in the
# image frame.

procedure t_psf ()

pointer	image				# name of the image
pointer	psfimage			# name of the output PSF
pointer	apfile				# aperture photometry file
pointer	groupfile			# file for PSF neighbors
pointer	graphics			# pointer to graphics device name
pointer	plotfile			# pointer to plotfile name
pointer	display				# pointer to display device name
pointer	plottype			# type of psf plot

bool	ap_text, interactive, showplots
int	up, verify, update, index, root, min_lenuserarea
pointer	sp, im, apd, psfim, psfgr, dao, pfd, mgd, gd, id
pointer	outfname, curfile, str, psf, psfpl

bool	streq(), clgetb(), itob()
int	fnldir(), strlen(), strncmp(), btoi(), envfind(), ctoi()
int	strdic(), tbtopn(), access(), fstati()
pointer	immap(), open(), gopen()
errchk 	gopen

begin
	# Set the standard output to flush on newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (apfile, SZ_FNAME, TY_CHAR)
	call salloc (psfimage, SZ_FNAME, TY_CHAR)
	call salloc (groupfile, SZ_FNAME, TY_CHAR)
	call salloc (plottype, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (plotfile, SZ_FNAME, TY_CHAR)
	call salloc (curfile, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the various task parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("photfile", Memc[apfile], SZ_FNAME)
	call clgstr ("psfimage", Memc[psfimage], SZ_FNAME)
	call clgstr ("groupfile", Memc[groupfile], SZ_FNAME)
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)
	call clgstr ("plottype", Memc[plottype], SZ_FNAME)
	call clgstr ("plotfile", Memc[plotfile], SZ_FNAME)

	# Open input image
	im = immap (Memc[image], READ_ONLY, 0)		

	# Initialize DAOPHOT main structure, get pset parameters. 
	# Verify and update parameters if appropriate.

	call dp_gppars (dao, im)	
	if (verify == YES) {
	    call dp_pconfirm (dao)
	    if (update == YES)
		call dp_pppars (dao)
	}

	# Initialize the psf and apselect structures.
	call dp_fitsetup (dao)
	call dp_psfsetup (dao)
	call dp_apselsetup (dao)
	call dp_sets (dao, IMNAME, Memc[image])

	# Dereference some pointers.
	psf = DP_PSF(dao)
	psfpl = DP_PSFPLOT(psf)

	# Get remaining parameters.
	call clgstr ("commands.p_filename", Memc[curfile], SZ_FNAME)
	if (Memc[curfile] == EOS) {
	    interactive = true
	    showplots = clgetb ("showplots")
	} else {
	    interactive = false
	    showplots = false
	}

	# Set up the psf plot types.
	index = strdic (Memc[plottype], Memc[plottype], SZ_FNAME, PSF_PLOTS)
	switch (index) {
	case DP_MESHPLOT:
	    DP_PLOT_TYPE (psfpl) = DP_MESHPLOT
	case DP_CONTOURPLOT:
	    DP_PLOT_TYPE (psfpl) = DP_CONTOURPLOT
	}

	# Open the input photometry list and store the descriptor.
	# PSF can read either an APPHOT PHOT file or an ST TABLE 
	# file.

	root = fnldir (Memc[apfile], Memc[outfname], SZ_FNAME)
	if (strncmp ("default", Memc[apfile+root], 7) == 0 || root ==
	    strlen (Memc[apfile])) 
	    call dp_inname (Memc[image], "", "mag", Memc[outfname], SZ_FNAME)
	else
	    call strcpy (Memc[apfile], Memc[outfname], SZ_FNAME)
	ap_text = itob (access (Memc[outfname], 0, TEXT_FILE))
	if (ap_text)
	    apd = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	else
	    apd = tbtopn (Memc[outfname], READ_ONLY, 0)
	call dp_getapert (dao, apd, DP_MAXSTAR(dao), ap_text)
	call dp_sets (dao, APFILE, Memc[outfname])

	# Open output image containing PSF and the file for PSF neighbors
	# If the output is "default", dir$default or a directory
	# specification then the extension "psf" is added to the PSF image
	# and "psg" to the neighbors file. A suitable version number is added
	# to the output name as well.

	root = fnldir (Memc[psfimage], Memc[outfname], SZ_FNAME)
	if (strncmp ("default", Memc[psfimage + root], 7) == 0 || root ==
	    strlen (Memc[psfimage])) {
	    call dp_oimname (Memc[image], "", "psf", Memc[outfname], SZ_FNAME)
	} else
	    call strcpy (Memc[psfimage], Memc[outfname], SZ_FNAME)
	if (envfind ("min_lenuserarea", Memc[str], SZ_FNAME) > 0) {
	    up = 1
	    if (ctoi (Memc[str], up, min_lenuserarea) <= 0)
		min_lenuserarea = LEN_USERAREA
	    else
		min_lenuserarea = max (LEN_USERAREA, min_lenuserarea)
        } else
	    min_lenuserarea = LEN_USERAREA
	psfim = immap (Memc[outfname], NEW_IMAGE, min_lenuserarea)
	call dp_sets (dao, PSFIMAGE, Memc[outfname])

	root = fnldir (Memc[groupfile], Memc[outfname], SZ_FNAME)
	if (strncmp ("default", Memc[groupfile + root], 7) == 0 || root ==
	    strlen (Memc[groupfile])) {
	    call dp_outname (Memc[image], "", "psg", Memc[outfname], SZ_FNAME)
	} else
	    call strcpy (Memc[groupfile], Memc[outfname], SZ_FNAME)
	if (DP_TEXT(dao) == YES)
	    psfgr = open (Memc[outfname], NEW_FILE, TEXT_FILE)
	else
	    psfgr = tbtopn (Memc[outfname], NEW_FILE, 0)
	call dp_sets (dao, GRPSFFILE, Memc[outfname])

	# Open graphics and display devices.
	if (interactive) {
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
		"Warning: Gaphics overlay not available for display device.\n")
		    id = NULL
	        }
	    }
	} else {
	    gd = NULL
	    id = NULL
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

	# Move the graphics descriptors into the structure.
	DP_PSFGD(psfpl) = gd
	DP_PSFID(psfpl) = id
	DP_PSFMGD(psfpl) = mgd

	# Make the PSF.
	call dp_mkpsf (dao, im, psfim, psfgr, gd, mgd, id, interactive,
	    showplots)

	# Close the input image.
	call imunmap (im)

	# Close the input photometry file.
	if (apd != NULL) {
	    if (ap_text)
	    	call close (apd)
	    else
		call tbtclo (apd)
	}

	# Close PSF image and group table.
	if (psfim != NULL)
	    call imunmap (psfim)
	if (psfgr != NULL) {
	    if (DP_TEXT(dao) == YES)
		call close (psfgr)
	    else
	        call tbtclo (psfgr)
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

	# Close the metacode plot files
	if (mgd != NULL)
	    call gclose (mgd)
	if (pfd != NULL)
	    call close (pfd)

	# Close up the daophot structures.
	call dp_apclose (dao)
	call dp_psfclose (dao)
	call dp_fitclose (dao)
	call dp_free (dao)

	call sfree(sp)
end	
