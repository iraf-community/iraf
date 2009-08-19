include <fset.h>
include <gset.h>
include <imhdr.h>
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
int	cache			# cache image buffer in memory
int	verify			# verify parameters
int	update			# update parameters
int	verbose			# verbose mode

size_t	sz_val
pointer	sp, str, cname, outfname, ap, im, id, gd, mgd
int	limlist, lclist, lolist, cl, pfd, out, root, stat, memstat, wcs
long	sid, lid
pointer	imlist, clist, olist
size_t	req_size, old_size, buf_size
long	l_val

pointer	immap(), gopen(), clpopnu(), imtopenp()
int	imtlen(), imtgetim(), clplen(), clgfil(), btoi(), fnldir(), strncmp()
int	open(), strlen(), apcenter(), clgwrd()
int	ap_memstat(), sizeof()
bool	clgetb(), streq()
errchk	gopen

include	<nullptr.inc>

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)
	call salloc (output, sz_val, TY_CHAR)
	call salloc (coords, sz_val, TY_CHAR)
	call salloc (plotfile, sz_val, TY_CHAR)
	call salloc (graphics, sz_val, TY_CHAR)
	call salloc (display, sz_val, TY_CHAR)
	call salloc (outfname, sz_val, TY_CHAR)
	call salloc (cname, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)

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

	call clgstr ("icommands.p_filename", Memc[cname], SZ_FNAME)
	if (Memc[cname] != EOS)
	    interactive = NO
	#else if (lclist == 0)
	    #interactive = YES
	else
	    interactive = btoi (clgetb ("interactive"))
	cache = btoi (clgetb ("cache"))
	verbose = btoi (clgetb ("verbose"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))

	# Get the centering parameters.
	call ap_gcpars (ap)

	# confirm the centering algorithm parameters.
	if (verify == YES && interactive == NO) {
	    l_val = 1
	    call ap_cconfirm (ap, NULL, l_val)
	    if (update == YES)
		call ap_pcpars (ap)
	}

	# Get the wcs information.
        wcs = clgwrd ("wcsin", Memc[str], SZ_LINE, WCSINSTR)
        if (wcs <= 0) {
	    call eprintf (
	        "Warning: Setting the input coordinate system to logical\n")
            wcs = WCS_LOGICAL
	}
        call apseti (ap, WCSIN, wcs)
        wcs = clgwrd ("wcsout", Memc[str], SZ_LINE, WCSOUTSTR)
        if (wcs <= 0) {
	    call eprintf (
	        "Warning: Setting the output coordinate system to logical\n")
            wcs = WCS_LOGICAL
	}
        call apseti (ap, WCSOUT, wcs)
	
	# Get the graphics and display devices.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)

	# Open the graphics and image deisplay devices.
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

	# Open the plot metacode file.
	call clgstr ("plotfile", Memc[plotfile], SZ_FNAME)
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
	    im = immap (Memc[image], READ_ONLY, NULLPTR)
	    call apimkeys (ap, im, Memc[image])

	    # Set the image display viewport. 
	    if ((id != NULL) && (id != gd))
		call ap_gswv (id, Memc[image], im, 4)

	    # Cache the input image pixels.
	    req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
	        sizeof (IM_PIXTYPE(im))
	    memstat = ap_memstat (cache, req_size, old_size)
	    if (memstat == YES) {
		l_val = INDEFL
		call ap_pcache (im, l_val, buf_size)
	    }

	    # Open the coordinate file; where coords is assumed to be a simple
	    # text file in which the x and y positions are in columns 1 and 2
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
		    l_val = BOF
		    call seek (cl, l_val)
		}
	    }
	    call apsets (ap, CLNAME, Memc[outfname])
	    call apfroot (Memc[outfname], Memc[str], SZ_LINE)
	    call apsets (ap, CLROOT, Memc[str])

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
		root = fnldir (Memc[output], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[output+root], 7) == 0 || root == 
		    strlen (Memc[output])) {
	            call apoutname (Memc[image], Memc[outfname], "ctr",
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

	    # Fit the centers.
	    if (interactive == NO) {
	        if (Memc[cname] != EOS)
		    stat = apcenter (ap, im, cl, NULLPTR, mgd, NULLPTR, out, sid,
		        NO, cache)
	        else if (cl != NULL) {
		    lid = 1
	            call apbcenter (ap, im, cl, out, sid, lid, mgd, id,
		        verbose)
		    stat = NO
		} else
		    stat = NO
	    } else
	        stat = apcenter (ap, im, cl, gd, mgd, id, out, sid, YES, cache)

	    # Close up image, coordinates, and output file.
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

	    # Uncache memory.
	    call fixmem (old_size)

	    if (stat == YES)
		break

	}

	# If only one coordinate file for a list of images close it.
	if (cl != NULL && lclist == 1)
	    call close (cl)

	# If only one output file defined for a list of images close it.
	if (out != NULL && lolist == 1) {
	    call close (out)
	    if (sid <= 1) {
		call apstats (ap, OUTNAME, Memc[outfname], SZ_FNAME)
		call delete (Memc[outfname])
	    }
	}

	# Close up graphics and image display streams and the plot files.
	if (id == gd && id != NULL)
	    call gclose (gd)
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

	# Free centering structure.
	call apcfree (ap)

	# Close up the file lists.
	call imtclose (imlist)
	call clpcls (clist)
	call clpcls (olist)
	call sfree (sp)
end
