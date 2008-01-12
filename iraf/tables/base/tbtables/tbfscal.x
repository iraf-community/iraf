include "tbtables.h"

# This routine adds (or updates) the TSCALi and TZEROi keywords,
# if they differ from the default values of 1 and 0 respectively.
#
# Phil Hodge, 23-Jun-2000  Subroutine created.

procedure tbfscal (tp, cp)

pointer tp		# i: pointer to table struct
pointer cp		# i: pointer to column struct
#--
pointer sp
pointer keyword		# for keyword name
int	i		# column number
int	status

begin
	call smark (sp)
	call salloc (keyword, SZ_FNAME, TY_CHAR)

	i = COL_NUMBER(cp)
	status = 0			# initial value

	if (COL_TSCAL(cp) != 1.d0) {
	    call sprintf (Memc[keyword], SZ_FNAME, "TSCAL%d")
		call pargi (i)
	    call fsukyd (TB_FILE(tp), Memc[keyword],
		COL_TSCAL(cp), 14, "scale factor for column", status)
	    if (status != 0)
		call tbferr (status)
	}

	if (COL_TZERO(cp) != 0.d0) {
	    call sprintf (Memc[keyword], SZ_FNAME, "TZERO%d")
		call pargi (i)
	    call fsukyd (TB_FILE(tp), Memc[keyword],
		COL_TZERO(cp), 14, "zero offset for column", status)
	    if (status != 0)
		call tbferr (status)
	}

	# Make sure the fitsio interface knows about these keywords.
	call fsrdef (TB_FILE(tp), status)

	call sfree (sp)
end
