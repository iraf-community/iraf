include <tbset.h>
include "tbtables.h"

# tbtdel -- delete a table
# This procedure deletes a table.  The default extension will be appended
# to the table name if no extension is present.
#
# For a FITS file, the HDU specified (or implied by default) in the table
# name will be deleted.  The FITS file itself will not be deleted, even if
# all that remains is the primary header.
#
# For a CDF file, the specified table will be deleted.  If there are no
# other parameters in the CDF file, the file itself will be deleted.
#
# Phil Hodge, 28-Dec-1989  Open before deleting to verify that it is a table.
# Phil Hodge, 14-May-1992  Don't call tbtext; check for text table.
# Phil Hodge, 14-Jul-1992  When making the previous change, I must have deleted
#			the call to error in the case that there was an error
#			from tbtopn, so this call has been put back in.
# Phil Hodge, 26-Jun-1995  Modify for FITS file or CDF file.
# Phil Hodge, 16-Apr-1999  Call tbttyp to get file type;
#		call tbnparse instead of tbparse; change SZ_LINE to SZ_FNAME;
#		delete most references to CDF.
# Phil Hodge,  7-Jun-1999  Replace TB_F_TYPE by TB_TYPE;
#		test on Memc[brackets] instead of Memc[extname] to check
#		that a particular FITS extension was specified.

procedure tbtdel (table)

char	table[ARB]	# i: name of table to be deleted
#--
pointer sp
pointer tname		# for table name (and possible error message)
pointer fname		# file name
pointer extname		# EXTNAME or number for FITS file
pointer brackets, rowsel, colsel	# returned by tbnparse and ignored
pointer tp		# pointer to table descriptor
int	hdu, extver, overwrite	# returned by tbnparse and ignored
int	ttype, exists	# returned by tbttyp; exists is ignored
#***	pointer fl		# file list pointer for fields in CDF file
#***	int	nparam		# number of parameters in CDF file
#***	pointer qp_ofnls()
#***	int	qp_lenfnl()
pointer tbtopn()
int	tbnparse(), tbttyp()
errchk	qp_deletef, tbtopn, tbfdel, tbnparse, tbttyp

begin
	call smark (sp)
	call salloc (tname, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (extname, SZ_FNAME, TY_CHAR)
	call salloc (brackets, SZ_FNAME, TY_CHAR)
	call salloc (rowsel, SZ_FNAME, TY_CHAR)
	call salloc (colsel, SZ_FNAME, TY_CHAR)

	# Check whether we have been asked to delete a FITS file without
	# any specification of which extension is the table.
	if (tbnparse (table, Memc[fname], Memc[extname], Memc[brackets],
		SZ_FNAME, extver, hdu, overwrite,
		Memc[rowsel], Memc[colsel], SZ_FNAME) < 1)
	    call error (1, "name of table to delete is blank")

	ttype = tbttyp (Memc[fname], exists)

	# No EXTNAME for FITS file?
	if (ttype == TBL_TYPE_FITS && Memc[brackets] == EOS) {
	    call sprintf (Memc[tname], SZ_FNAME,
	    "can't delete entire FITS file `%s'; specify extension")
		call pargstr (table)
	    call error (1, Memc[tname])
	}

	# Open the table that is to be deleted.
	tp = tbtopn (table, READ_WRITE, NULL)

	# Get the full name of the file, including filename extension.
	call strcpy (TB_NAME(tp), Memc[tname], SZ_FNAME)

	# Delete the table.
	if (TB_TYPE(tp) == TBL_TYPE_FITS) {

	    # Delete the current HDU, and close the table.  The FITS file
	    # itself is not deleted.
	    call tbfdel (tp)

	} else if (TB_TYPE(tp) == TBL_TYPE_CDF) {

	    ;		# skip this section until CDF format is defined

	    # Here the table is a field within a CDF file.
#***	    call qp_deletef (TB_CD(tp), TB_CDF_NAME(tp))

#***	    fl = qp_ofnls (TB_CD(tp), "")
#***	    nparam = qp_lenfnl (fl)		# total number of parameters
#***	    call qp_cfnl (fl)
#***	    call tbtclo (tp)

	    # If there's nothing left in the CDF file, delete the file itself.
#***	    if (nparam < 1)
#***		call delete (Memc[tname])

	} else {

	    # An ordinary FIO file.
	    call tbtclo (tp)
	    call delete (Memc[tname])
	}

	call sfree (sp)
end
