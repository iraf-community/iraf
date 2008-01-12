include <tbset.h>
include "tbtables.h"

# tbfwcd -- write column descriptor to FITS table
# This routine updates the column name, units, and print format.
# If the true data type of the column (i.e. in the table) differs from
# the apparent data type, then TSCALi and/or TZEROi will be updated.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 23-Jun-2000  Call tbfscal.

procedure tbfwcd (tp, cp)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
#--
pointer sp
pointer value		# for new value of name, units, format
errchk	tbfnam, tbfnit, tbffmt, tbfscal

begin
	call smark (sp)
	call salloc (value, SZ_FNAME, TY_CHAR)

	call tbcigt (cp, TBL_COL_NAME, Memc[value], SZ_FNAME)
	call tbfnam (tp, cp, Memc[value])

	call tbcigt (cp, TBL_COL_UNITS, Memc[value], SZ_FNAME)
	call tbfnit (tp, cp, Memc[value])

	call tbcigt (cp, TBL_COL_FMT, Memc[value], SZ_FNAME)
	call tbffmt (tp, cp, Memc[value])

	# Update TSCALi and/or TZEROi if the current column uses scaling.
	call tbfscal (tp, cp)

	call sfree (sp)
end
