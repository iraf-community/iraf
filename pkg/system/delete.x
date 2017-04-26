# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# DELETE -- Delete a list of files.  If file cannot be deleted, warn but do
# not abort.  Verify before deleting each file if user wishes.

procedure t_delete()

bool	verify			# verify before deleting
bool	allversions		# delete all versions of each file
bool	subfiles		# delete any subfiles of a file
char	fname[SZ_FNAME]
int	list
pointer	tty

bool	clgetb()
int	clpopns(), clgfil(), access(), btoi(), strncmp()
pointer	ttyodes()

begin
	list = clpopns ("files")
	verify = clgetb ("verify")
	allversions = clgetb ("allversions")
	subfiles = clgetb ("subfiles")
	if (verify)
	    tty = ttyodes ("terminal")

	while (clgfil (list, fname, SZ_FNAME) != EOF) {
	    if (verify) {
		# If file does not exist, warn user (since verify mode is
		# in effect).
		if (access (fname, 0, 0) == NO) {
		    call eprintf ("Warning: Nonexistent file '%s'\n")
			call pargstr (fname)
		    next
		}

		# Set default action of verify prompt (override learning of
		# most recent response).

		call flush (STDOUT)
		call clputb ("go_ahead", clgetb ("default_action"))
		call eprintf ("Delete file ")
		call ttyso (STDERR, tty, YES)
		call eprintf ("'%s'")
		    call pargstr (fname)
		call ttyso (STDERR, tty, NO)
		call flush (STDERR)

		if (!clgetb ("go_ahead"))
		    next
	    }

	    if (strncmp ("http:", fname, 5) == 0) {
		call eprintf ("Cannot delete URL `%s'\n")
		    call pargstr (fname)
		next
	    }

	    iferr (call deletefg (fname, btoi(allversions), btoi(subfiles)))
		call erract (EA_WARN)
	}

	# Reset the go_ahead parameter, overiding learn mode, in case delete
	# is subsequently called from the background.  Close tty descriptor.

	if (verify) {
	    call clputb ("go_ahead", true)
	    call ttycdes (tty)
	}

	call clpcls (list)
end
