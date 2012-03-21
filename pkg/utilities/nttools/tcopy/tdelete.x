include	<error.h>

# tdelete -- Delete a list of tables.  If table cannot be deleted, warn the
# user but do not abort.  Verify before deleting each table if user wishes.
# This is based on the t_imdelete procedure.
#
# Phil Hodge, 24-Aug-1987  Task created.
# Phil Hodge,  7-Sep-1988  Change parameter name for table.
# Phil Hodge, 16-Mar-1992  Include check to prevent deleting text files.
# Phil Hodge, 26-Mar-1992  Remove call to tbtext.
# Phil Hodge, 19-Jul-1995  Replace fnt calls with tbn.
# B.Simon      5-May-1995  Call delete if deleting an entire table
procedure tdelete()

pointer list
bool	verify
pointer sp, tablename, tablist

pointer tbnopen()
int	tbnget()
bool	clgetb()

begin
	call smark (sp)
	call salloc (tablename, SZ_PATHNAME, TY_CHAR)
	call salloc (tablist, SZ_LINE, TY_CHAR)

	call clgstr ("table", Memc[tablist], SZ_LINE)
	list = tbnopen (Memc[tablist])
	verify = clgetb ("verify")

	while (tbnget (list, Memc[tablename], SZ_PATHNAME) != EOF)
	    call one_delete (Memc[tablename], verify)

	# Reset the go_ahead parameter, overriding learn mode, in case tdelete
	# is subsequently called from the background.

	if (verify)
	    call clputb ("go_ahead", true)

	call tbnclose (list)
	call sfree (sp)
end

# ONE_DELETE -- Delete a single table

procedure one_delete (file, verify)

char	file[ARB]	# i: current file name
bool	verify		# i: ask user for confirmation
#--
bool	doit
pointer	sp, fname

bool	clgetb(), is_wholetab()
int	access(), tbtacc(), strncmp()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Check to make sure the deletion is OK

	if (strncmp ("http:", file, 5) == 0) {
	    call eprintf ("Cannot delete URL `%s'\n")
	        call pargstr (file)
	    call sfree (sp)
	    return
	}

	if (verify) {
	    if (tbtacc (file) == NO) {
		# If table does not exist, warn user 
		# (since verify mode is in effect).
		doit = false
		call eprintf ("Cannot delete nonexistent table `%s'\n")
		call pargstr (file)
	    }

	    # Set default action of verify prompt (override learning of
	    # most recent response).

	    call clputb ("go_ahead", clgetb ("default_action"))
	    call printf ("delete table `%s'")
	    call pargstr (file)

	    doit = clgetb ("go_ahead")

	} else {
	    if (access (file, 0, TEXT_FILE) == YES) {
		# We don't want users to be able to delete text files 
		# with tdelete if verify = false.

		doit = false
		call eprintf ("Cannot delete text file with tdelete: `%s'\n")
		call pargstr (file)

	    } else {
		doit = true
	    }
	}

	# Do the deletion

	if (doit) {
	    if (is_wholetab (file)) {
		# Entire files are deleted with the fio delete

		call tbtext (file, Memc[fname], SZ_FNAME)
		iferr (call delete (Memc[fname])) {
		    call erract (EA_WARN)
		} 

	    } else {
		# Table extensions are deleted by the table 
		# library function tbtdel

		iferr (call tbtdel (file)) {
		    call erract (EA_WARN)
		}
	    }
	}

	call sfree (sp)
	return
end
