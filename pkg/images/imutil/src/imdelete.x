# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>

# IMDELETE -- Delete a list of images.  If image cannot be deleted, warn but do
# not abort.  Verify before deleting each image if user wishes.

procedure t_imdelete()

bool	verify
int	list, nchars
pointer	sp, tty, imname, im

pointer	ttyodes(), immap()
int	imtopenp(), imtgetim(), imaccess(), strlen(), strncmp()
bool	clgetb()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	list = imtopenp ("images")
	verify = clgetb ("verify")
	if (verify)
	    tty = ttyodes ("terminal")

	while (imtgetim (list, Memc[imname], SZ_FNAME) != EOF) {

	    if (verify) {
		# If image does not exist, warn user (since verify mode is
		# in effect).

		if (imaccess (Memc[imname], 0) == NO) {
		    call eprintf ("Warning: %s `%s'\n")
			call pargstr ("Cannot delete nonexistent image")
			call pargstr (Memc[imname])
		    next
		}

		# Set default action of verify prompt (override learning of
		# most recent response).

		call clputb ("go_ahead", clgetb ("default_action"))

		# Output prompt, with image name.
		call printf ("delete image ")
		call ttyso (STDOUT, tty, YES)
		call printf ("`%s'")
		    call pargstr (Memc[imname])
		call ttyso (STDOUT, tty, NO)

		# Include portion of image title in prompt.
		ifnoerr (im = immap (Memc[imname], READ_ONLY, 0)) {
		    nchars = strlen (IM_TITLE(im))
		    if (nchars > 0) {
			call printf (" - %0.28s")
			    call pargstr (IM_TITLE(im))
			if (nchars > 28)
			    call printf ("...")
		    }
		    iferr (call imunmap (im))
			;
		}

		# Do the query.
		if (! clgetb ("go_ahead"))
		    next
	    }

	    iferr (call imdelete (Memc[imname]))
		call erract (EA_WARN)
	}

	# Reset the go_ahead parameter, overiding learn mode, in case delete
	# is subsequently called from the background.  Close tty descriptor.

	if (verify) {
	    call clputb ("go_ahead", true)
	    call ttycdes (tty)
	}

	call imtclose (list)
	call sfree (sp)
end
