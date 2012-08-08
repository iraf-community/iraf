include <tbset.h>
include "tbtables.h"

# tbctpe -- copy from template
# This procedure copies various table parameters (but not all) from a
# template table to a table which is being initialized but has not yet
# been created.  Then, for each column in the template table, this
# procedure creates a column descriptor for the new table and copies
# information from the column descriptor for the template to that of
# the new table.
#
# The table type is normally inherited from the template table.  The
# one exception to this is that if the new table has the extension
# ".fits", the user has specified the type, and we don't override it.
# For an input text table, the subtype is also copied to the output table.
#
# Important parameters which are not copied from the template are the
# number of rows TB_NROWS, the number of user parameters TB_NPAR, and
# TB_MAXCOLS.  The latter must have been set by the calling routine
# (tbtopn) which allocates the array of pointers to column descriptors.
# NOTE:  This procedure must be called before explicitly defining any new
# column, before setting any parameter, and before opening (creating) the
# table.
#
# Phil Hodge,  8-Oct-1987  Take most parameters from template; don't realloc.
# Phil Hodge,  7-Feb-1992  Add option for text table type.
# Phil Hodge, 11-May-1994  For text table, don't allocate comment buffer.
# Phil Hodge, 23-Dec-1994  Check for CDF or FITS file.
# Phil Hodge, 30-Sep-1997  For template table, use tbpsta & tbcnum instead of
#			TB_NCOLS, TB_COLINFO, TB_ROWLEN, and TB_COLUSED.
# Phil Hodge,  7-Jun-1999  Replace TB_F_TYPE by TB_TYPE.
# Phil Hodge, 15-Jun-1999  Copy subtype for text table.

procedure tbctpe (tp, template)

pointer tp			# i: pointer to descriptor of new table
pointer template		# i: pointer to descriptor of template table
#--
pointer icp, ocp		# pointers to column descriptors
int	k			# loop index
int	colnum			# column number, a loop index
int	offset			# for recomputing COL_OFFSET
long	tbtbod()
int	tbpsta(), tbcnum()
errchk	calloc

begin
	# Copy the table type from the template, unless either the
	# table or the template has a file type of FITS.
	if (TB_TYPE(tp) != TBL_TYPE_FITS &&
	    TB_TYPE(template) != TBL_TYPE_FITS)
	    TB_TYPE(tp) = TB_TYPE(template)	# copy from template

	# Copy the subtype, if the input is a text table.
	if (TB_TYPE(template) == TBL_TYPE_TEXT)
	    TB_SUBTYPE(tp) = TB_SUBTYPE(template)

	TB_NCOLS(tp) = tbpsta (template, TBL_NCOLS)
	TB_MAXPAR(tp) = TB_MAXPAR(template)
	TB_ALLROWS(tp) = TB_ALLROWS(template)
	TB_COLUSED(tp) = tbpsta (template, TBL_ROWLEN_CHAR_USED)
	TB_ROWLEN(tp) = tbpsta (template, TBL_ROWLEN_CHAR)

	TB_BOD(tp) = tbtbod (TB_MAXPAR(tp), TB_MAXCOLS(tp))

	# Create a descriptor for each column, and copy values.
	# icp is a column pointer in template, and
	# ocp is a column pointer in tp.
	offset = 0			# only needed if column selector used
	do colnum = 1, tbpsta (template, TBL_NCOLS) {
	    icp = tbcnum (template, colnum)
	    call calloc (ocp, LEN_COLSTRUCT, TY_STRUCT)
	    TB_COLINFO(tp,colnum) = ocp

	    # Copy the contents of the column descriptor.
	    do k = 1, LEN_COLSTRUCT
		Memi[ocp+k-1] = Memi[icp+k-1]

	    # Update column number and offset if a column selector was used.
	    if (TB_COLUMN_SELECT(template) == YES) {
		COL_NUMBER(ocp) = colnum
		COL_OFFSET(ocp) = offset
		offset = offset + COL_LEN(ocp)
	    }
	}

	# COL_OFFSET must be given reasonable values in case the template
	# table is of type "text" but the type of the new table will be
	# changed to something else.  If the type is still "text" when
	# tbtcre is called, COL_OFFSET will be overwritten at that time
	# by a pointer to memory for the column data.
	if (TB_TYPE(template) == TBL_TYPE_TEXT ||
	    TB_TYPE(template) == TBL_TYPE_FITS) {
	    offset = 0
	    do colnum = 1, TB_NCOLS(tp) {
		ocp = tbcnum (tp, colnum)
		COL_OFFSET(ocp) = offset
		offset = offset + COL_LEN(ocp)
	    }
	    TB_ROWLEN(tp) = offset
	    TB_COLUSED(tp) = offset
	}
end
