include <ctype.h>
include <tbset.h>
include "tbtables.h"
include "tblfits.h"		# defines FITS_KEYWORD_MISSING

# These specify the precision to be used for writing floating-point keywords.
# The fact that they're negative is a flag to CFITSIO to use g format.
define	NDECIMALS_SINGLE	(-7)
define	NDECIMALS_DOUBLE	(-15)

# Put a parameter into a FITS table header.  If the keyword already
# exists, it will be updated; otherwise, it will be added.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 20-Feb-1997  Change decimals of output in tbfhpd and tbfhpr:
#			in tbfhpd change 15 to 14; in tbfhpr change 7 to 6.
# Phil Hodge, 14-Jan-1998  Change decimals of output in tbfhpd and tbfhpr
#			for new keywords, 15 to 14 and 7 to 6 respectively.
# Phil Hodge, 20-Jul-1998  In tbfhpt, include explicit test for history,
#			comment, or blank, and use appropriate fitsio routine.
# Phil Hodge, 29-Aug-2000  Change 14 and 6 in tbfhpd and tbfhpr respectively
#			to -15 and -7, so cfitsio will use g format.
# Phil Hodge, 29-Jan-2002  In tbfhpt, for keyword = blank, sprintf only eight
#			spaces before the value, to agree with cfitsio.

# tbfhpd -- put double-precision header parameter

procedure tbfhpd (tp, keyword, value)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to put
double	value		# i: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for comment string
int	status		# zero is OK
bool	bval
errchk	tbferr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	status = 0

	# Get the current value to see if the keyword already exists,
	# and if so, to check the data type.
	call fsgkey (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)

	if (status == 0) {

	    # Modify existing keyword value, leaving comment unchanged.
	    if (Memc[sval] == '\'') {
		call sprintf (Memc[sval], SZ_LINE, "%-25.15g")
		    call pargd (value)
		call fsmkys (TB_FILE(tp), keyword, Memc[sval],
			Memc[comment], status)
	    } else if (Memc[sval] == 'T' || Memc[sval] == 'F') {
		bval = (value != 0.d0)
		call fsmkyl (TB_FILE(tp), keyword, bval, Memc[comment], status)
	    } else {
		# FITSIO should be able to handle other type conversions.
		call fsmkyd (TB_FILE(tp), keyword, value, NDECIMALS_DOUBLE,
			Memc[comment], status)
	    }

	} else if (status == FITS_KEYWORD_MISSING) {

	    status = 0
	    call ftcmsg()
	    call fspkyd (TB_FILE(tp), keyword, value, NDECIMALS_DOUBLE,
			"", status)
	    TB_NPAR(tp) = TB_NPAR(tp) + 1
	}

	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end

# tbfhpr -- put single-precision header parameter

procedure tbfhpr (tp, keyword, value)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to put
real	value		# i: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for comment string
int	status		# zero is OK
bool	bval
errchk	tbferr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	status = 0

	# Get the current value to see if the keyword already exists,
	# and if so, to check the data type.
	call fsgkey (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)

	if (status == 0) {

	    # Modify existing keyword value, leaving comment unchanged.
	    if (Memc[sval] == '\'') {
		call sprintf (Memc[sval], SZ_LINE, "%-15.7g")
		    call pargr (value)
		call fsmkys (TB_FILE(tp), keyword, Memc[sval],
			Memc[comment], status)
	    } else if (Memc[sval] == 'T' || Memc[sval] == 'F') {
		bval = (value != 0.)
		call fsmkyl (TB_FILE(tp), keyword, bval, Memc[comment], status)
	    } else {
		# FITSIO should be able to handle other type conversions.
		call fsmkye (TB_FILE(tp), keyword, value, NDECIMALS_SINGLE,
			Memc[comment], status)
	    }

	} else if (status == FITS_KEYWORD_MISSING) {

	    status = 0
	    call ftcmsg()
	    call fspkye (TB_FILE(tp), keyword, value, NDECIMALS_SINGLE,
			"", status)
	    TB_NPAR(tp) = TB_NPAR(tp) + 1
	}

	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end

# tbfhpi -- put integer header parameter

procedure tbfhpi (tp, keyword, value)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to put
int	value		# i: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for comment string
int	status		# zero is OK
bool	bval
errchk	tbferr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	status = 0

	# Get the current value to see if the keyword already exists,
	# and if so, to check the data type.
	call fsgkey (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)

	if (status == 0) {

	    # Modify existing keyword value, leaving comment unchanged.
	    if (Memc[sval] == '\'') {
		call sprintf (Memc[sval], SZ_LINE, "%-10d")
		    call pargi (value)
		call fsmkys (TB_FILE(tp), keyword, Memc[sval],
			Memc[comment], status)
	    } else if (Memc[sval] == 'T' || Memc[sval] == 'F') {
		bval = (value != 0)
		call fsmkyl (TB_FILE(tp), keyword, bval, Memc[comment], status)
	    } else {
		# FITSIO should be able to handle other type conversions.
		call fsmkyj (TB_FILE(tp), keyword, value, Memc[comment], status)
	    }

	} else if (status == FITS_KEYWORD_MISSING) {

	    status = 0
	    call ftcmsg()
	    call fspkyj (TB_FILE(tp), keyword, value, "", status)
	    TB_NPAR(tp) = TB_NPAR(tp) + 1
	}

	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end

# tbfhpb -- put Boolean header parameter

procedure tbfhpb (tp, keyword, value)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to put
bool	value		# i: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for comment string
int	status		# zero is OK
errchk	tbferr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	status = 0

	# Get the current value to see if the keyword already exists,
	# and if so, to check the data type.
	call fsgkey (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)

	if (status == 0) {

	    # Modify existing keyword value, leaving comment unchanged.
	    if (Memc[sval] == '\'') {
		if (value)
		    call strcpy ("TRUE", Memc[sval], SZ_LINE)
		else
		    call strcpy ("FALSE", Memc[sval], SZ_LINE)
		call fsmkys (TB_FILE(tp), keyword, Memc[sval],
			Memc[comment], status)
	    } else {
		# FITSIO should be able to handle other type conversions.
		call fsmkyl (TB_FILE(tp), keyword, value, Memc[comment], status)
	    }

	} else if (status == FITS_KEYWORD_MISSING) {

	    status = 0
	    call ftcmsg()
	    call fspkyl (TB_FILE(tp), keyword, value, "", status)
	    TB_NPAR(tp) = TB_NPAR(tp) + 1
	}

	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end

# tbfhpt -- put text-string header parameter

procedure tbfhpt (tp, keyword, text)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to put
char	text[ARB]	# i: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for comment string
char	uckey[SZ_KEYWORD]	# keyword converted to upper case
bool	iscomment	# true if the keyword is history, comment, or blank
int	k		# loop index
int	status		# zero is OK
int	strlen()
bool	streq()
errchk	tbferr

begin
	status = 0

	# Convert to upper case and trim trailing blanks.
	call strcpy (keyword, uckey, SZ_KEYWORD)
	call strupr (uckey)
	do k = strlen (uckey), 1, -1 {
	    if (IS_WHITE(uckey[k]))
		uckey[k] = EOS
	    else
		break
	}

	# If the keyword is history or comment, add a new keyword record.
	if (streq (uckey, "HISTORY")) {
	    iscomment = true
	    call fsphis (TB_FILE(tp), text, status)
	} else if (streq (uckey, "COMMENT")) {
	    iscomment = true
	    call fspcom (TB_FILE(tp), text, status)
	} else if (uckey[1] == EOS) {
	    iscomment = true
	    call smark (sp)
	    call salloc (comment, SZ_PARREC, TY_CHAR)
	    call sprintf (Memc[comment], SZ_PARREC, "        %s")
		call pargstr (text)
	    call fsprec (TB_FILE(tp), Memc[comment], status)
	    call sfree (sp)
	} else {
	    iscomment = false
	}
	if (iscomment) {
	    if (status != 0)
		call tbferr (status)
	    TB_NPAR(tp) = TB_NPAR(tp) + 1
	    return
	}

	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	# Get current value to see if the keyword already exists.
	call fsgkys (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)

	if (status == 0) {
	    # Modify existing keyword value, leaving comment unchanged.
	    call fsmkys (TB_FILE(tp), keyword, text, Memc[comment], status)
	} else if (status == FITS_KEYWORD_MISSING) {
	    status = 0
	    call ftcmsg()
	    call fspkys (TB_FILE(tp), keyword, text, "", status)
	    TB_NPAR(tp) = TB_NPAR(tp) + 1
	}

	if (status != 0)
	    call tbferr (status)

	call sfree (sp)
end
