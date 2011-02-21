include "tbtables.h"
include "tblfits.h"		# defines FITS_KEYWORD_MISSING

# tbfnit -- change column units
# This procedure replaces the column units in a FITS table.
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbfnit (tp, cp, colunits)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to a column descriptor
char	colunits[ARB]	# i: column units
#--
pointer sp
pointer keyword		# scratch for keyword name
pointer dummy		# for current value, if keyword already exists
pointer comment		# for comment string
int	status		# zero is OK
errchk	tbferr

begin
	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (dummy, SZ_FNAME, TY_CHAR)
	call salloc (comment, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[keyword], SZ_FNAME, "TUNIT%d")
	    call pargi (COL_NUMBER(cp))

	status = 0

	# Get the comment, if the keyword already exists.
	call fsgkys (TB_FILE(tp), Memc[keyword],
		Memc[dummy], Memc[comment], status)
	if (status != 0) {
	    if (status == FITS_KEYWORD_MISSING) {
		status = 0
		call ftcmsg()
		call fsukys (TB_FILE(tp), Memc[keyword], colunits,
			"column units", status)
	    }
	} else {
	    # Modify existing keyword value, leaving comment unchanged.
	    call fsmkys (TB_FILE(tp), Memc[keyword], colunits,
			Memc[comment], status)
	}

	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end
