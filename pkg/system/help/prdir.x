# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ttyset.h>
include	<error.h>
include	"help.h"

# PR_DIRECTORY -- Print a directory of the help blocks for a package.
# Extract the names of all the modules therein into a string buffer
# and print it as a table.

define	SZ_OBUF		2048		# buffer for directory
define	MAX_MODULES	500		# max modules that can be sorted
define	MAX_NAMELEN	20		# max chars in a module name
define	FIRST_COL	6
define	exit_		90


procedure pr_directory (db, hp, pakname, paknum, ctrl)

pointer	db			# help database descriptor
pointer	hp			# root help directory
char	pakname[ARB]		# name of package
int	paknum			# package number in root directory
pointer	ctrl			# help control params

bool	multiple_directories
int	m, fd
pointer	sp, indices, hdr, fname, hp_pak, lbuf, obuf

int	hd_getname(), ttystati(), stropen(), getline()
pointer	hdb_load()
errchk	hd_getname, ttystati

begin
	call smark (sp)
	call salloc (indices, MAX_MODULES, TY_POINTER)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (hdr, SZ_FNAME, TY_CHAR)
	call salloc (obuf, SZ_OBUF, TY_CHAR)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# Get the filename of the help directory for the package.
	if (hd_getname (hp,paknum,TY_PKG,Memc[fname],SZ_FNAME) == 0) {
	    call eprintf ("No help available for package `%s'\n")
		call pargstr (pakname)
	    goto exit_
	}

	# Open the help directory for the package.
	iferr (hp_pak = hdb_load (db, Memc[fname])) {
	    call sfree (sp)
	    call erract (EA_WARN)
	    goto exit_
	}

	# Extract the names of the modules in the package.  Save the pointers
	# in an array for the table print routine.

	for (m=0;  m < MAX_MODULES;  m=m+1) {
	    call salloc (Memi[indices+m], MAX_NAMELEN, TY_CHAR)
	    if (hd_getname (hp_pak, m+1, TY_MODNAME, Memc[Memi[indices+m]],
		MAX_NAMELEN) == 0)
		    break
	}

	# If the template supplied by the user specified more than a single
	# package, print the name of the package.

	multiple_directories = (H_LENTL(ctrl) > 1)

	# We may have been called with any legal abbreviation of the package
	# name; fetch the full name from the system package directory.

	if (multiple_directories) {
	    call houtput (ctrl, "\n")
	    if (hd_getname (hp, paknum, TY_MODNAME, Memc[hdr], SZ_FNAME) != 0) {
		call strcat (":\n", Memc[hdr], SZ_FNAME)
		call houtput (ctrl, Memc[hdr])
	    }
	}

	# Now print the table.  It is not necessary to sort the table,
	# because the "helpdir" code (which reads the help directory) has
	# already done so.  The directory is written into a string buffer
	# and then output line by line with houtput, since there is no
	# easy way to make STRTBL call houtput.
	
	# Format the table into the buffer.  A blank line marks the end of
	# the table.

	fd = stropen (Memc[obuf], SZ_OBUF, NEW_FILE)
	call strtbl (fd, Memc, Memi[indices], m, FIRST_COL,
	    ttystati (H_TTY(ctrl), TTY_NCOLS), MAX_NAMELEN, 0)
	call putline (fd, "\n")
	call close (fd)

	# Copy lines from the buffer to the HELP output.  Do not read off
	# end of buffer or FIO will file fault and abort.

	fd = stropen (Memc[obuf], SZ_OBUF, READ_ONLY)
	while (getline (fd, Memc[lbuf]) > 1)
	    call houtput (ctrl, Memc[lbuf])
	call close (fd)

	call hdb_free (db, hp_pak)
exit_
	call sfree (sp)
end
