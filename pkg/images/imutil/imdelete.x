# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>

# IMDELETE -- Delete a list of images.  If image cannot be deleted, warn but do
# not abort.  Verify before deleting each image if user wishes.

procedure t_imdelete()

int	list
bool	verify
pointer	sp, tty, imname

pointer	ttyodes()
int	imtopenp(), imtgetim(), imaccess()
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
		call printf ("delete image ")
		call ttyso (STDOUT, tty, YES)
		call printf ("`%s'")
		    call pargstr (Memc[imname])
		call ttyso (STDOUT, tty, NO)
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
