include <gset.h>
include <fset.h>
include "../lib/apphot.h"

# T_FITPSF -- Procedure to fit an analytic function to the PSF for a list
# of objects in a list of images.

procedure t_fitpsf ()

pointer image		# pointer to the name of the image
pointer output		# pointer to the output file name
pointer coords		# pointer to the coordinate file
pointer graphics	# pointer to the graphics display device
pointer display		# pointer to the display device
int	interactive	# mode of use
int	verify		# verify critical parameters
int	update 		# update the critical parameter
int	verbose		# verbose mode

int	cl, out, limlist, lclist, lolist, lid, sid, root, stat
int	imlist, clist, olist
pointer	sp, outfname, ap, im, gd, id, cname

bool	clgetb(), streq()
int	imtlen(), imtgetim(), clplen(), btoi(), clgfil(), fnldir()
int	open(), strncmp(), strlen(), apfitpsf(), imtopenp(), clpopnu()
pointer	gopen(), immap()
errchk	gopen

begin
	# Allocate workin space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (cname, SZ_FNAME, TY_CHAR)

	# Set standard output to flush on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get input image list.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)

	# Get input coordinate lists.
	clist = clpopnu ("coords")
	lclist = clplen (clist)

	# Get output file list and check for zero length list.
	olist = clpopnu ("output")
	lolist = clplen (olist)

	# Check that image and coordinate list lengths match.
	if (limlist < 1 || (lclist > 1 && lclist != limlist)) {
	    call imtclose (imlist)
	    call clpcls (clist)
	    call clpcls (olist)
	    call error (0, "Imcompatable image and coordinate list lengths")
	}

	# Check that image and output list lengths match.
	if (lolist > 1 && lolist != limlist) {
	    call imtclose (imlist)
	    call clpcls (clist)
	    call clpcls (olist)
	    call error (0, "Imcompatable image and output list lengths")
	}

	call clgstr ("icommands.p_filename", Memc[cname], SZ_FNAME)
	if (Memc[cname] != EOS)
	    interactive = NO
	#else if (lclist == 0)
	    #interactive = YES
	else
	    interactive = btoi (clgetb ("interactive"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	verbose = btoi (clgetb ("verbose"))

	# Get the parameters.
	call ap_gpfpars (ap)
	if (verify == YES && interactive == NO) {
	    call ap_pfconfirm (ap, NULL, 1)
	    if (update == YES)
		call ap_ppfpars (ap)
	}

	# Get the graphics and display devices.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)

	# Open the graphics and display devices.
	if (interactive == YES) {
	    if (Memc[graphics] == EOS)
		gd = NULL
	    else {
		iferr {
		    gd = gopen (Memc[graphics], APPEND+AW_DEFER, STDGRAPH)
		} then {
		    call eprintf (
			"Warning: Error opening graphics device.\n")
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
	} else {
	    id = NULL
	    gd = NULL
	}

	# Begin looping over the image list.
	sid = 1
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the image.
	    im = immap (Memc[image], READ_ONLY, 0)
	    call apsets (ap, IMNAME, Memc[image])
	    call ap_padu (im, ap)
	    call ap_rdnoise (im, ap)
	    call ap_itime (im, ap)
	    call ap_airmass (im, ap)
	    call ap_filter (im, ap)
	    call ap_otime (im, ap)
	    if ((id != NULL) && (id != gd))
		call ap_gswv (id, Memc[image], im, 4)

	    # Open coordinate file, where coords is assumed to be a simple text
	    # file in which the x and y positions are in columns 1 and 2
	    # respectively and all remaining fields are ignored.

	    if (lclist <= 0) {
		cl = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
		stat = clgfil (clist, Memc[coords], SZ_FNAME)
		root = fnldir (Memc[coords], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[coords+root], 7) == 0 || root ==
		    strlen (Memc[coords])) {
		    call ap_inname (Memc[image], Memc[outfname], "coo",
		        Memc[outfname], SZ_FNAME)
		    lclist = limlist
	            cl = open (Memc[outfname], READ_ONLY, TEXT_FILE)
		} else if (stat != EOF) {
		    call strcpy (Memc[coords], Memc[outfname], SZ_FNAME)
	            cl = open (Memc[outfname], READ_ONLY, TEXT_FILE)
		} else {
		    call apstats (ap, CLNAME, Memc[outfname], SZ_FNAME)
		    call seek (cl, BOF)
		}
	    }
	    call apsets (ap, CLNAME, Memc[outfname])

	    # Open output text file, if output is "default", dir$default or
	    # a directory specification then the extension "psf" is added on
	    # to the image name and a suitable version number is appended to
	    # the output name. If the output string is null then no output
	    # file is written.

	    if (lolist == 0) {
		out = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
	        stat = clgfil (olist, Memc[output], SZ_FNAME)
		root = fnldir (Memc[output], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[output+root], 7) == 0 || root ==
		    strlen (Memc[output])) {
		    call apoutname (Memc[image], Memc[outfname], "psf",
		        Memc[outfname], SZ_FNAME)
		    out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    lolist = limlist
		} else if (stat != EOF) {
		    call strcpy (Memc[output], Memc[outfname], SZ_FNAME)
		    out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		} else
		    call apstats (ap, OUTNAME, Memc[outfname], SZ_FNAME)
	    }
	    call apsets (ap, OUTNAME, Memc[outfname])

	    # Fit the PSF.
	    if (interactive == NO) {
	        if (Memc[cname] != EOS)
		    stat = apfitpsf (ap, im, cl, NULL, NULL, out, sid, NO)
	        else if (cl != NULL) {
		    lid = 1
		    call apbfitpsf (ap, im, cl, out, id, sid, lid, verbose)
		    stat = NO
		} else
		    stat = NO
	    } else
		stat = apfitpsf (ap, im, cl, gd, id, out, sid, YES)

	    call imunmap (im)
	    if (cl != NULL) {
		if (lclist > 1)
		    call close (cl)
	    }
	    if (out != NULL && lolist != 1) {
		call close (out)
		if (sid <= 1) {
		    call apstats (ap, OUTNAME, Memc[outfname], SZ_FNAME)
		    call delete (Memc[outfname])
		}
		sid = 1
	    }
	    if (stat == YES)
		break
	}

	# Close the plot files.
	if (id == gd && id != NULL)
	    call gclose (id)
	else {
	    if (id != NULL)
	        call gclose (id)
	    if (gd != NULL)
	        call gclose (gd)
	}

	# If only one coordinate file for a list of images close file.
	if (cl != NULL && lclist == 1)
	    call close (cl)

	# If only one output file for a list of images close file.
	if (out != NULL && lolist == 1) {
	    call close (out)
	    if (sid <= 1) {
		call apstats (ap, OUTNAME, Memc[outfname], SZ_FNAME)
	        call delete (Memc[outfname])
	    }
	}

	# Close up the PSF fitting structure.
	call apsffree (ap)

	# Close up the lists.
	call imtclose (imlist)
	call clpcls (clist)
	call clpcls (olist)

	call sfree (sp)
end
