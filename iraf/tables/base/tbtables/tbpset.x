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
long	lvalue

begin
	lvalue = value
	call tbpsetl(tp, setwhat, lvalue)
end


procedure tbpsetl (tp, setwhat, value)

pointer tp			# i: pointer to table descriptor
int	setwhat			# i: specifies what parameter is to be set
long	value			# i: the value that is to be assigned
#--
size_t	sz_val
long	l_val
int	ivalue
pointer sp, errmess		# for possible error message
bool	streq()
errchk	tbcchg, tbrchg, tbtchs, tbtfst

begin
	ivalue = value

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

	    if (ivalue != TBL_TYPE_S_ROW && ivalue != TBL_TYPE_S_COL && 
		ivalue != TBL_TYPE_TEXT && ivalue != TBL_TYPE_FITS) {
		call smark (sp)
		sz_val = SZ_FNAME
		call salloc (errmess, sz_val, TY_CHAR)
		call sprintf (Memc[errmess], SZ_FNAME,
			"tbpset:  %d is not a valid table type")
		    call pargi (ivalue)
		call error (1, Memc[errmess])
	    }

	    if (TB_IS_OPEN(tp))
		call error (ER_TBTOOLATE,
			"can't specify table type after opening table")

	    # Can't set type of table for FITS file or CDF file.
	    if (TB_TYPE(tp) == TBL_TYPE_FITS || ivalue == TBL_TYPE_FITS)
		return
	    if (TB_TYPE(tp) == TBL_TYPE_CDF || ivalue == TBL_TYPE_CDF)
		return

	    # Can't change the type of STDOUT or STDERR.
	    if (streq (TB_NAME(tp), "STDOUT") || streq (TB_NAME(tp), "STDERR"))
		return
	    
	    TB_TYPE(tp) = ivalue

	case (TBL_SUBTYPE):		# Specify table subtype

	    # Can only set subtype for text tables.
	    if (TB_TYPE(tp) == TBL_TYPE_TEXT) {
		if (ivalue == TBL_SUBTYPE_SIMPLE) {
		    TB_SUBTYPE(tp) = TBL_SUBTYPE_SIMPLE
		} else if (ivalue == TBL_SUBTYPE_EXPLICIT) {
		    TB_SUBTYPE(tp) = TBL_SUBTYPE_EXPLICIT
		} else {
		    call smark (sp)
		    sz_val = SZ_FNAME
		    call salloc (errmess, sz_val, TY_CHAR)
		    call sprintf (Memc[errmess], SZ_FNAME,
			"tbpset:  %d is not a valid text table subtype")
			call pargi (ivalue)
		    call error (1, Memc[errmess])
		}
	    }

	case (TBL_MAXPAR):		# "Maximum" number of header parameters
	    l_val = -1
	    call tbtchs (tp, ivalue, -1, l_val, l_val)

	case (TBL_MAXCOLS):		# "Maximum" number of columns
	    l_val = -1
	    call tbtchs (tp, -1, ivalue, l_val, l_val)

	case (TBL_ADVICE):		# suggest random or sequential access
	    if ( ! TB_IS_OPEN(tp) )
		call error (ER_TBNOTOPEN,
			"table must be open to set I/O advice")
	    call tbtfst (tp, F_ADVICE, ivalue)

	default:
	    call error (ER_TBBADOPTION, "invalid option for tbpset")
	}
end
