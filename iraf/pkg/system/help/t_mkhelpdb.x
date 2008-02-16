# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>

# MKHELPDB -- Make (compile) the help database.  The root help directory
# is compiled, along with all sub help directories in the package hierarchy.
# Each package in the system, including both CL packages and library packages,
# has an associated help directory file.  Despite the fact that the package
# structure is hierarchical, package names must be distinct because packages
# are indexed in the database in a linear structure.

procedure t_mkhelpdb()

bool	verbose
pointer	sp, helpdir, helpdb, tempname
int	envgets(), access()
bool	streq(), clgetb()
string	s_helpdir "helpdir"
string	s_helpdb "helpdb"

begin
	call smark (sp)
	call salloc (helpdir,  SZ_FNAME, TY_CHAR)
	call salloc (helpdb,   SZ_FNAME, TY_CHAR)
	call salloc (tempname, SZ_FNAME, TY_CHAR)

	# Fetch the names of the root help directory file and the new database
	# file.  If the names are "helpdir" or "helpdb", the filenames are
	# taken from the environment.

	call clgstr (s_helpdir, Memc[helpdir], SZ_FNAME)
	if (streq (Memc[helpdir], s_helpdir))
	    if (envgets (s_helpdir, Memc[helpdir], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, s_helpdir)

	call clgstr (s_helpdb, Memc[helpdb], SZ_FNAME)
	if (streq (Memc[helpdb], s_helpdb))
	    if (envgets (s_helpdb, Memc[helpdb], SZ_FNAME) <= 0)
		call syserrs (SYS_ENVNF, s_helpdb)

	verbose = clgetb ("verbose")
	if (verbose) {
	    call printf ("helpdir = %s\n")
		call pargstr (Memc[helpdir])
	    call printf ("helpdb = %s\n")
		call pargstr (Memc[helpdb])
	}

	# The database is compiled into a temporary file to protect the current
	# database in the event the compile is aborted.  When the compile has
	# successfully completed we will delete the original and replace it
	# with the new database.  This also has the advantage that the switch
	# takes very little time, making it less likely that anyone will notice
	# when the database is updated.

	call mktemp ("tmp$hdb", Memc[tempname], SZ_FNAME)

	# Perform the compilation.
	call hdb_compile (Memc[helpdir], Memc[tempname], verbose)

	# Now attempt to delete the old database and replace it with the new.
	# By default, the help database flle is maintained in dev$, a directory
	# with global write permission, so that anyone can delete the file.
	# If the old file cannot be deleted we delete the new database and
	# abort.

	if (access (Memc[helpdb],0,0) == YES)
	    iferr (call delete (Memc[helpdb])) {
		call delete (Memc[tempname])
		call erract (EA_ERROR)
	    }

	call rename (Memc[tempname], Memc[helpdb])
	call sfree (sp)
end
