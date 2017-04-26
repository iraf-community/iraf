include <fset.h>
include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# tbpset -- set parameter
# Set parameters in table descriptor.
# If record length is to be set or increased, the unit is SZ_REAL even
# though internally the unit is SZ_CHAR.
#
# Phil Hodge, 30-Sep-1987  FIO options added.
# Phil Hodge, 15-Nov-1988  Remove option to set buffer size.
# Phil Hodge,  8-Apr-1993  Modify for short by including TBL_ROWLEN_CHAR.
# Phil Hodge,  5-Oct-1995  Include check on file type when setting table type.
# Phil Hodge,  7-Jun-1999  Add table subtype; delete TB_F_TYPE;
#		use SZ_FNAME instead of SZ_LINE for error message.
# Phil Hodge, 11-Jul-2003  Return without doing anything when called to
#		change the table type of STDOUT or STDERR.

procedure tbpset (tp, setwhat, value)

pointer tp			# i: pointer to table descriptor
int	setwhat			# i: specifies what parameter is to be set
int	value			# i: the value that is to be assigned
#--
pointer sp, errmess		# for possible error message
bool	streq()
errchk	tbcchg, tbrchg, tbtchs, tbtfst

begin
	switch (setwhat) {

	case (TBL_ROWLEN):		# Specify what row length to allocate
	    call tbcchg (tp, value*SZ_REAL)		# unit = SZ_REAL

	case (TBL_ROWLEN_CHAR):		# Specify what row length to allocate
	    call tbcchg (tp, value)			# unit = SZ_CHAR

	case (TBL_INCR_ROWLEN):		# Increase row length; unit = SZ_REAL
	    call tbcchg (tp, TB_ROWLEN(tp) + value * SZ_REAL)

	case (TBL_ALLROWS):		# Number of rows to allocate
	    call tbrchg (tp, value)

	case (TBL_INCR_ALLROWS):	# Increase allocated number of rows
	    call tbrchg (tp, TB_ALLROWS(tp) + value)

	case (TBL_WHTYPE):		# Specify table type

	    if (value != TBL_TYPE_S_ROW && value != TBL_TYPE_S_COL && 
		value != TBL_TYPE_TEXT && value != TBL_TYPE_FITS) {
		call smark (sp)
		call salloc (errmess, SZ_FNAME, TY_CHAR)
		call sprintf (Memc[errmess], SZ_FNAME,
			"tbpset:  %d is not a valid table type")
		    call pargi (value)
		call error (1, Memc[errmess])
	    }

	    if (TB_IS_OPEN(tp))
		call error (ER_TBTOOLATE,
			"can't specify table type after opening table")

	    # Can't set type of table for FITS file or CDF file.
	    if (TB_TYPE(tp) == TBL_TYPE_FITS || value == TBL_TYPE_FITS)
		return
	    if (TB_TYPE(tp) == TBL_TYPE_CDF || value == TBL_TYPE_CDF)
		return

	    # Can't change the type of STDOUT or STDERR.
	    if (streq (TB_NAME(tp), "STDOUT") || streq (TB_NAME(tp), "STDERR"))
		return
	    
	    TB_TYPE(tp) = value

	case (TBL_SUBTYPE):		# Specify table subtype

	    # Can only set subtype for text tables.
	    if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
		if (value == TBL_SUBTYPE_SIMPLE) {
		    TB_SUBTYPE(tp) = TBL_SUBTYPE_SIMPLE
		} else if (value == TBL_SUBTYPE_EXPLICIT) {
		    TB_SUBTYPE(tp) = TBL_SUBTYPE_EXPLICIT
		} else {
		    call smark (sp)
		    call salloc (errmess, SZ_FNAME, TY_CHAR)
		    call sprintf (Memc[errmess], SZ_FNAME,
			"tbpset:  %d is not a valid text table subtype")
			call pargi (value)
		    call error (1, Memc[errmess])
		}
	    }

	case (TBL_MAXPAR):		# "Maximum" number of header parameters
	    call tbtchs (tp, value, -1, -1, -1)

	case (TBL_MAXCOLS):		# "Maximum" number of columns
	    call tbtchs (tp, -1, value, -1, -1)

	case (TBL_ADVICE):		# suggest random or sequential access
	    if ( ! TB_IS_OPEN(tp) )
		call error (ER_TBNOTOPEN,
			"table must be open to set I/O advice")
	    call tbtfst (tp, F_ADVICE, value)

	default:
	    call error (ER_TBBADOPTION, "invalid option for tbpset")
	}
end
