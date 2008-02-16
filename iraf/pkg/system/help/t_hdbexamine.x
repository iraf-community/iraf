# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	"help.h"


# HDBEXAMINE -- Examine the help database.

procedure t_hdbexamine()

pointer	sp, helpdb
bool	streq(), clgetb()
int	envgets()
string	s_helpdb "helpdb"

begin
	call smark (sp)
	call salloc (helpdb, SZ_HELPDB, TY_CHAR)

	# Fetch the name of the help database file to be examined.  If the
	# name is "helpdb", the filename is taken from the environment.

	call clgstr (s_helpdb, Memc[helpdb], SZ_HELPDB)
	if (streq (Memc[helpdb], s_helpdb))
	    if (envgets (s_helpdb, Memc[helpdb], SZ_HELPDB) <= 0)
		call syserrs (SYS_ENVNF, s_helpdb)

	# Examine the structure and contents of the database, printing the
	# results on the standard output.

	call hdb_examine (STDOUT, Memc[helpdb], clgetb ("verbose"))

	call sfree (sp)
end
