include <ctype.h>	# for IS_WHITE
include <tbset.h>

# tchcol -- change column information
# This task can be used to change the name, print format, and/or units
# for one column of a list of tables.  If any of the new values is null
# or blank, the value will not be changed.  If the value is "default"
# for format or units, the value will be changed to the default.
# For units the default is null.
#
# J.-C. HSU,   11-Jul-1987  design and coding
# Phil Hodge,  15-Mar-1989  rewrite in spp
# Phil Hodge,  10-Apr-1990  change SZ_COLNAME to SZ_FNAME, etc for clgstr
# Phil Hodge,  10-May-1991  allow multiple input tables;
#	use "default" to set format or units to the default
# Phil Hodge,  18-Jun-1993  preserve case of newfmt to allow e.g. %12.1H
# Phil Hodge,  11-Aug-1993  print warning if text table and user has
#	requested a change of column name or units
# Phil Hodge,  3-Oct-1995  Modify to use tbn instead of fnt.
# Phil Hodge,  7-Jun-1999  Delete warning messages for text tables
#	(this undoes the change made on 11-Aug-1993).
# Phil Hodge, 30-Sep-1999  Remove trailing blanks from new name, units, format.

procedure tchcol()

pointer tp			# pointer to table descriptor
pointer cp			# pointer to column descriptor
pointer ilist			# for list of tables to change
char	table[SZ_FNAME]		# table name
char	oldname[SZ_COLNAME]	# column name before being changed
char	newname[SZ_COLNAME]	# new column name or ""
char	oldfmt[SZ_COLFMT]	# print format before being changed
char	newfmt[SZ_COLFMT]	# new column print format or "default"
char	newf[SZ_COLFMT]		# new spp style print format or ""
char	oldunits[SZ_COLUNITS]	# column units before being changed
char	newunits[SZ_COLUNITS]	# new column units or "default"
char	newu[SZ_COLUNITS]	# new column units or ""
char	newval[SZ_COLUNITS]	# actual new value of format or units in table
bool	verbose			# if true, tell user what's happening
int	i, strlen()		# for stripping off trailing blanks
pointer tbtopn()
pointer tbnopenp()
int	tbnget()
bool	clgetb(), streq()

begin
	ilist = tbnopenp ("table")
	call clgstr ("oldname", oldname, SZ_COLNAME)
	call clgstr ("newname", newname, SZ_COLNAME)
	call clgstr ("newfmt", newfmt, SZ_COLFMT)
	call clgstr ("newunits", newunits, SZ_COLUNITS)
	verbose = clgetb ("verbose")

	# Remove leading whitespace from new values.
	call xt_stripwhite (newname)
	call xt_stripwhite (newfmt)
	call xt_stripwhite (newunits)

	# Remove trailing whitespace from new values.
	do i = strlen (newname), 1, -1 {
	    if (IS_WHITE(newname[i]))
		newname[i] = EOS
	    else
		break
	}
	do i = strlen (newfmt), 1, -1 {
	    if (IS_WHITE(newfmt[i]))
		newfmt[i] = EOS
	    else
		break
	}
	do i = strlen (newunits), 1, -1 {
	    if (IS_WHITE(newunits[i]))
		newunits[i] = EOS
	    else
		break
	}

	if (newname[1] == EOS && newfmt[1] == EOS && newunits[1] == EOS) {
	    call eprintf ("no change specified\n")
	    call tbnclose (ilist)
	    return
	}

	# Check for "default" for format or units, and copy to newf & newu.

	call strcpy (newfmt, newf, SZ_COLFMT)
	call strlwr (newf)			# preserve case of newfmt
	if (streq (newf, "default"))
	    newf[1] = EOS
	else
	    call tbbftp (newfmt, newf)		# convert from Fortran style

	call strcpy (newunits, newu, SZ_COLUNITS)
	call strlwr (newu)
	if (streq (newu, "default"))
	    newu[1] = EOS
	else
	    call strcpy (newunits, newu, SZ_COLUNITS)	# preserve case

	# Process all the tables in the list.
	while (tbnget (ilist, table, SZ_FNAME) != EOF) {

	    if (verbose) {
		call printf ("table %s\n")
		    call pargstr (table)
	    }

	    tp = tbtopn (table, READ_WRITE, NULL)

	    call tbcfnd (tp, oldname, cp, 1)
	    if (cp == NULL) {
		call tbtclo (tp)
		if ( ! verbose ) {
		    call printf ("table %s\n")
			call pargstr (table)
		}
		call printf ("    warning:  column `%s' not found\n")
		    call pargstr (oldname)
		next
	    }

	    if (newname[1] != EOS) {
		call tbcnam (tp, cp, newname)
		if (verbose) {
		    call printf ("    column name changed from `%s' to `%s'\n")
			call pargstr (oldname)
			call pargstr (newname)
		}
	    }

	    # newf may be EOS even if newfmt is not.
	    if (newfmt[1] != EOS) {
		call tbcigt (cp, TBL_COL_FMT, oldfmt, SZ_COLFMT)
		call tbcfmt (tp, cp, newf)
		if (verbose) {
		    call tbcigt (cp, TBL_COL_FMT, newval, SZ_COLUNITS)
		    call printf ("    print format changed from `%s' to `%s'\n")
			call pargstr (oldfmt)
			call pargstr (newval)
		}
	    }

	    # newu may be EOS even if newunits is not.
	    if (newunits[1] != EOS) {
		call tbcigt (cp, TBL_COL_UNITS, oldunits, SZ_COLUNITS)
		call tbcnit (tp, cp, newu)
		if (verbose) {
		    call tbcigt (cp, TBL_COL_UNITS, newval, SZ_COLUNITS)
		    call printf ("    column units changed from `%s' to `%s'\n")
			call pargstr (oldunits)
			call pargstr (newval)
		}
	    }

	    call tbtclo (tp)

	    if (verbose)				# added 8/11/93
		call flush (STDOUT)
	}
	call tbnclose (ilist)
end
