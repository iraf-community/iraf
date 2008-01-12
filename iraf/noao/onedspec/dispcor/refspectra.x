include	"refspectra.h"


# T_REFSPECTRA -- Assign reference spectra.
# Reference spectra are assigned to input spectra from a specified list of
# reference spectra with various criteria.  This procedure only gets some
# of the task parameters and switches to separate procedures for each
# implemented assignment method.  The reference spectra may be specified by
# and image list or a lookup table.  The difference is determined by attempting
# to map the first reference element in the list as an image.

procedure t_refspectra ()

pointer	input			# List of input images
pointer	refs			# List of reference images
#int	select			# Selection method for reference spectra
int	type			# Type of reference specification

int	clgwrd(), imtgetim()
pointer	sp, ref, im, imtopenp(), immap()
errchk	immap

include	"refspectra.com"

begin
	call smark (sp)
	call salloc (ref, SZ_LINE, TY_CHAR)

	# Get input and reference spectra lists.  Determine selection method.
	input = imtopenp ("input")
	call clgstr ("records", Memc[ref], SZ_LINE)
	call odr_openp (input, Memc[ref])
	refs = imtopenp ("references")
	select = clgwrd ("select", Memc[ref], SZ_FNAME, SELECT)

	# Determine if reference list is a table.
	if (imtgetim (refs, Memc[ref], SZ_FNAME) != EOF) {
	    call refnoextn (Memc[ref])
	    iferr {
		im = immap (Memc[ref], READ_ONLY, 0)
	        call imunmap (im)
		type = LIST
	    } then
		type = TABLE
	} else
	    call error (0, "No reference spectra specified")
	call imtrew (refs)

	# Initialize confirm flag, symbol table and logging streams.
	call refconfirm1 ()
	call refopen ()

	# Switch of reference list type and selection method.
	if (type == LIST) {
	    switch (select) {
	    case MATCH:
	        call refmatch(input, refs)
	    case NEAREST:
	        call refnearest (input, refs)
	    case PRECEDING:
	        call refprecede (input, refs)
	    case FOLLOWING:
	        call reffollow (input, refs)
	    case INTERP:
	        call refinterp (input, refs)
	    case AVERAGE:
	        call refaverage (input, refs)
	    }
	} else
	    call reftable (input, Memc[ref], select)

	call refclose ()
	call imtclose (input)
	call imtclose (refs)
	call sfree (sp)
end


# REFSPECTRA -- Confirm and set reference spectra in header.
# 	1.  Confirm assignments if desired.
#	2.  Log output to logfiles if desired.
#	3.  Update assignment if desired.
# Note that if wt1 > 0.995 then only the first reference spectrum is
# set with no weight specified.  No weight implies no interpolation.

procedure refspectra (image, ref1, wt1, ref2, wt2)

char	image[ARB]		# Spectrum image name
char	ref1[ARB]		# Reference spectrum image name
real	wt1			# Weight
char	ref2[ARB]		# Reference spectrum image name
real	wt2			# Weight
bool	confirm			# Confirm assignments?

int	fd, clgfil(), open(), clgwrd()
bool	clgetb(), streq()
pointer	im, sp, str, immap()
errchk	immap

include	"refspectra.com"

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Confirm assignments.
	if (confirm) {
	    if (wt1 < 0.995) {
	        call printf ("[%s] refspec1='%s %.8g'\n")
	            call pargstr (image)
	            call pargstr (ref1)
		    call pargr (wt1)
	        call printf ("[%s] refspec2='%s %.8g'   ")
	            call pargstr (image)
	            call pargstr (ref2)
		    call pargr (wt2)
	    } else {
	        call printf ("[%s] refspec1='%s'   ")
	            call pargstr (image)
	            call pargstr (ref1)
	    }
	    call flush (STDOUT)
	    fd = clgwrd ("answer", Memc[str], SZ_LINE, "|no|yes|YES|")
	    switch (fd) {
	    case 1:
		call sfree (sp)
		return
	    case 3:
		confirm = false
	    }
	}

	# Log output.
	while (clgfil (logfiles, Memc[str], SZ_LINE) != EOF) {
	    if (streq (Memc[str], "STDOUT") && confirm)
		next
	    fd = open (Memc[str], APPEND, TEXT_FILE)
	    if (wt1 < 0.995) {
	        call fprintf (fd, "[%s] refspec1='%s %.8g'\n")
	            call pargstr (image)
	            call pargstr (ref1)
		    call pargr (wt1)
	        call fprintf (fd, "[%s] refspec2='%s %.8g'\n")
	            call pargstr (image)
	            call pargstr (ref2)
		    call pargr (wt2)
	    } else {
	        call fprintf (fd, "[%s] refspec1='%s'\n")
	            call pargstr (image)
	            call pargstr (ref1)
	    }
	    call close (fd)
	}
	call clprew (logfiles)

	# If updating the assigments map the spectrum READ_WRITE and set
	# the keywords REFSPEC1 and REFSPEC2.  REFSPEC2 is not set if not
	# interpolating.

	if (clgetb ("assign")) {
	    im = immap (image, READ_WRITE, 0)
	    if (wt1 < 0.9999995D0) {
	        call sprintf (Memc[str], SZ_LINE, "%s %.8g")
	            call pargstr (ref1)
		    call pargr (wt1)
	        call imastr (im, "refspec1", Memc[str])
	        call sprintf (Memc[str], SZ_LINE, "%s %.8g")
	            call pargstr (ref2)
		    call pargr (wt2)
	        call imastr (im, "refspec2", Memc[str])
	    } else {
	        call imastr (im, "refspec1", ref1)
	        iferr (call imdelf (im, "refspec2"))
		    ;
	    }
	    call imunmap (im)
	}

	call sfree (sp)
	return

entry refconfirm1 ()

	confirm = clgetb ("confirm")

end
