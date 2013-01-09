include "tbtables.h"
include "tblfits.h"		# defines FITS_KEYWORD_MISSING

# tbffmt -- change print format
# This procedure replaces the print format for a column in a FITS table.
#
# Phil Hodge,  6-Jul-1995  Subroutine created

procedure tbffmt (tp, cp, colfmt)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to a column descriptor
char	colfmt[ARB]	# i: print format for column
#--
pointer sp
pointer keyword		# scratch for keyword name
pointer dummy		# for current value, if keyword already exists
pointer comment		# for comment string
pointer pformat		# print format converted to Fortran style
int	status		# zero is OK
errchk	tbfptf, tbferr

begin
	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (dummy, SZ_FNAME, TY_CHAR)
	call salloc (comment, SZ_FNAME, TY_CHAR)
	call salloc (pformat, SZ_FNAME, TY_CHAR)

	# Convert print format to Fortran.
	call tbfptf (colfmt, Memc[pformat], SZ_FNAME)

	call sprintf (Memc[keyword], SZ_FNAME, "TDISP%d")
	    call pargi (COL_NUMBER(cp))

	status = 0

	# Get the comment, if the keyword already exists.
	call fsgkys (TB_FILE(tp), Memc[keyword],
		Memc[dummy], Memc[comment], status)
	if (status != 0) {
	    if (status == FITS_KEYWORD_MISSING) {
		status = 0
		call ftcmsg()
		call fsukys (TB_FILE(tp), Memc[keyword], colfmt,
			"print format for column", status)
	    }
	} else {
	    # Modify existing keyword value, leaving comment unchanged.
	    call fsmkys (TB_FILE(tp), Memc[keyword], colfmt,
			Memc[comment], status)
	}

	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end
