include <gset.h>
include <fset.h>
include "../lib/apphot.h"

# T_FITSKY -- Procedure to fit sky values in a an annular region around
# a list of objects.

procedure t_fitsky ()

pointer image		# pointer to the name of the image
pointer	output		# pointer to output file name
pointer	coords		# coordinate file
pointer	plotfile	# pointer to file of graphics metacode
pointer graphics	# pointer to graphics display device
pointer	display		# pointer to display device
int	interactive	# mode of use
int	verify		# verify critical parameters
int	update		# update the critical parameter
int	verbose		# verbose mode

int	sid, lid, limlist, lclist, lolist, out, cl, pfd, root, stat
pointer	sp, outfname, cname, ap, im, mgd, gd, id, imlist, clist, olist

bool	clgetb(), streq()
int	imtlen(), imtgetim(), clplen(), clgfil(), btoi(), fnldir(), strncmp()
int	strlen(), apsky()
pointer	imtopenp(), clpopnu(), immap(), open(), gopen()
errchk	gopen

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (plotfile, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (cname, SZ_FNAME, TY_CHAR)

	# Set the standard output to flush on a newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get input and output file names.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)
	clist = clpopnu ("coords")
	lclist = clplen (clist)
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
	    call error (0, "Imcompatible image and output list lengths")
	}

	call clgstr ("commands.p_filename", Memc[cname], SZ_FNAME)
	if (Memc[cname] != EOS)
	    interactive = NO
	else if (lclist == 0)
	    interactive = YES
	else
	    interactive = btoi (clgetb ("interactive"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	verbose = btoi (clgetb ("verbose"))

	# Get the parameters.
	call ap_sgpars (ap)
	if (verify == YES && interactive == NO) {
	    call ap_sconfirm (ap, NULL, 1)
	    if (update == YES)
		call ap_pspars (ap)
	}

	# Open plot files.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)
	call clgstr ("plotfile", Memc[plotfile], SZ_FNAME)
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
	    gd = NULL
	    id = NULL
	}

	if (Memc[plotfile] == EOS)
	    pfd = NULL
	else
	    pfd = open (Memc[plotfile], APPEND, BINARY_FILE)
	if (pfd != NULL)
	    mgd = gopen (Memc[graphics], NEW_FILE, pfd)
	else
	    mgd = NULL

	# Begin looping over image list.
	sid = 1
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open image.
 	    im = immap (Memc[image], READ_ONLY, 0)
	    call apsets (ap, IMNAME, Memc[image])
	    call ap_padu (im, ap)
	    call ap_rdnoise (im, ap)
	    call ap_itime (im, ap)
	    call ap_airmass (im, ap)
	    call ap_filter (im, ap)

	    # Open the coordinate file, where coords is assumed to be a simple
	    # text file in which the x and y positions are in columns 1 and 2
	    # respectively and all remaining fields are ignored.

	    if (lclist <= 0) {
		cl = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else if (clgfil (clist, Memc[coords], SZ_FNAME) != EOF) {
		root = fnldir (Memc[coords], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[coords+root], 7) == 0 || root ==
		    strlen (Memc[coords]))
		    call ap_inname (Memc[image], "", "coo", Memc[outfname],
			SZ_FNAME)
		else
		    call strcpy (Memc[coords], Memc[outfname], SZ_FNAME)
	        cl = open (Memc[outfname], READ_ONLY, TEXT_FILE)
	    } else {
		root = fnldir (Memc[coords], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[coords+root], 7) == 0 || root ==
		    strlen (Memc[coords])) {
		    call ap_inname (Memc[image], "", "coo", Memc[outfname],
			SZ_FNAME)
	            cl = open (Memc[outfname], READ_ONLY, TEXT_FILE)
		} else {
		    call strcpy (Memc[coords], Memc[outfname], SZ_FNAME)
		    call seek (cl, BOF)
		}
	    }
	    call apsets (ap, CLNAME, Memc[outfname])

	    # Open the output text file, if output is "default", dir$default
	    # or a directory specification then the extension "sky" is added
	    # to the image name and a suitable version number is appended to
	    # the output name. If the output string is null to output file
	    # is created.

	    if (lolist == 0) {
		out = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
	        stat = clgfil (olist, Memc[output], SZ_FNAME)
		if (stat != EOF)
		    root = fnldir (Memc[output], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[output+root], 7) == 0 || root ==
		    strlen (Memc[output])) {
		    call apoutname (Memc[image], "", "sky", Memc[outfname],
		        SZ_FNAME)
		    out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    lolist = limlist
		} else if (stat != EOF) {
		    call strcpy (Memc[output], Memc[outfname], SZ_FNAME)
		    out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		}
	    }
	    call apsets (ap, OUTNAME, Memc[outfname])

	    # Fit the sky
	    if (interactive == NO) {
	        if (Memc[cname] != EOS)
		    stat =  apsky (ap, im, cl, NULL, NULL, mgd, NULL, out,
		        sid, NO)
	        else if (cl != NULL) {
		    lid = 1
	            call apbsky (ap, im, cl, NULL, out, sid, lid, gd, mgd, id,
		        verbose)
		    stat = NO
		} else
		    stat = NO
	    } else
		stat = apsky (ap, im, cl, NULL, gd, mgd, id, out, sid, YES)

	    call imunmap (im)
	    if (cl != NULL) {
		if (lclist > 1)
		    call close (cl)
	    }
	    if (out != NULL && lolist != 1) {
		call close (out)
		if (sid <= 1)
		    call delete (Memc[outfname])
		sid = 1
	    }
	    if (stat == YES)
		break
	}

	# Free sky fitting structure
	call apsfree (ap)

	# If only one coordinate file for a list of images close.
	if (cl != NULL && lclist == 1)
	    call close (cl)

	# If only one output file for a list of images close.
	if (out != NULL && lolist == 1) {
	    call close (out)
	    if (sid <= 1)
		call delete (Memc[outfname])
	}

	# Close up plot files.
	if (id == gd && id != NULL) {
	    call gclose (id)
	} else {
	    if (gd != NULL)
                call gclose (gd)
	    if (id != NULL)
	        call gclose (id)
	}
	if (mgd != NULL)
	    call gclose (mgd)
	if (pfd != NULL)
	    call close (pfd)

	# Close up lists.
	call imtclose (imlist)
	call clpcls (clist)
	call clpcls (olist)
	call sfree (sp)
end
