include <fset.h>
include <gset.h>
include "../lib/apphot.h"
include "../lib/noise.h"

# T_CENTER -- Procedure to compute accurate centers for a list of objects
# given a list of starting centers, a set of starting parameters and a list
# of images.

procedure t_center ()

pointer	image	               	# pointer to name of the image
pointer	output	 		# pointer to output file name
pointer	coords			# pointer to coordinate file
pointer plotfile		# name of plot metacode file
pointer	graphics		# pointer to graphics device
pointer display			# pointer to display device
int	interactive		# interactive mode
int	verify			# verify parameters
int	update			# update parameters
int	verbose			# verbose mode

int	limlist, lclist, lolist, sid, lid, cl, pfd, out, root, stat
pointer	sp, cname, outfname, ap, im, id, gd, mgd, imlist, clist, olist

bool	clgetb(), streq()
int	imtlen(), imtgetim(), clplen(), clgfil(), btoi(), fnldir(), strncmp()
int	strlen(), apcenter()
pointer	imtopenp(), clpopnu(), immap(), gopen(), open()
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

	# Set standard output to flush on newline.
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
	    call error (0, "Imcompatible image and coordinate list lengths")
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
	verbose = btoi (clgetb ("verbose"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	# Fetch the centering parameters.
	call ap_gcpars (ap)
	if (verify == YES && interactive == NO) {
	    call ap_cconfirm (ap, NULL, 1)
	    if (update == YES)
		call ap_pcpars (ap)
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
		    gd =  gopen (Memc[graphics], APPEND+AW_DEFER, STDGRAPH)
		} then {
		    call eprintf (
			"Warning: Error opening the graphics device.\n")
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

	# Begin looping over the image list.
	sid = 1
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open image.
	    im = immap (Memc[image], READ_ONLY, 0)
	    call apsets (ap, IMNAME, Memc[image])
	    call ap_padu (im, ap)
	    call ap_rdnoise (im, ap)
	    call ap_itime (im, ap)
	    call ap_filter (im, ap)
	    call ap_airmass (im, ap)

	    # Open the coordinate file; where coords is assumed to be a simple
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

	    # Open output text file; if output is "default", dir$default  or
	    # a directory specification then the extension "ctr" is added to
	    # the root image name and a suitable version number is appended to
	    # the output name. If the output string is null then no output
	    # file is created.

	    if (lolist == 0) {
		out = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
	        stat = clgfil (olist, Memc[output], SZ_FNAME)
		if (stat != EOF)
		    root = fnldir (Memc[output], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[output+root], 7) == 0 || root == 
		    strlen (Memc[output])) {
	            call apoutname (Memc[image], "", "ctr", Memc[outfname],
		        SZ_FNAME)
	            out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    lolist = limlist
		} else if (stat != EOF) {
		    call strcpy (Memc[output], Memc[outfname], SZ_FNAME)
	            out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		}
	    }
	    call apsets (ap, OUTNAME, Memc[outfname])

	    # Fit the centers.
	    if (interactive == NO) {
	        if (Memc[cname] != EOS)
		    stat = apcenter (ap, im, cl, NULL, mgd, NULL, out, sid,
		        NO)
	        else if (cl != NULL) {
		    lid = 1
	            call apbcenter (ap, im, cl, out, sid, lid, mgd, id,
		        verbose)
		    stat = NO
		} else
		    stat = NO
	    } else
	        stat = apcenter (ap, im, cl, gd, mgd, id, out, sid, YES)

	    # close up image and output file.
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

	# Free centering structure.
	call apcfree (ap)

	# If only one coordinate file for a list of images close file.
	if (cl != NULL && lclist == 1)
	    call close (cl)

	# If only one output file for a list of images close file.
	if (out != NULL && lolist == 1) {
	    call close (out)
	    if (sid <= 1)
		call delete (Memc[outfname])
	}

	# Close up plot files.
	if (id == gd && id != NULL)
	    call gclose (id)
	else {
	    if (gd != NULL)
	    call gclose (gd)
	    if (id != NULL)
		call gclose (id)
	}
	if (mgd != NULL)
	    call gclose (mgd)
	if (pfd != NULL)
	    call close (pfd)

	# Close up the file lists.
	call imtclose (imlist)
	call clpcls (clist)
	call clpcls (olist)
	call sfree (sp)
end
