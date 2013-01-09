include <tbset.h>
include "tbtables.h"

# Get a parameter from a FITS table header.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge, 14-Aug-1997  In tbfhgt, allocate local buffer.
# Phil Hodge,  5-Aug-1999  In tbfhgt, for history or comment, copy the
#		comment field to output, rather than the value field.

# tbfhgd -- get double-precision header parameter

procedure tbfhgd (tp, keyword, value)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to get
double	value		# o: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for getting the comment
int	status		# zero is OK
int	ip, junk, ctod()
errchk	tbferr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	status = 0

	# Get the value as a string.
	call fsgkey (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)
	if (status != 0)
	    call tbferr (status)

	ip = 1
	if (Memc[sval] == '\'')
	    ip = 2				# skip over the quote
	else if (Memc[sval] == 'T')
	    Memc[sval] = '1'
	else if (Memc[sval] == 'F')
	    Memc[sval] = '0'

	value = INDEFD
	junk = ctod (Memc[sval], ip, value)

	call sfree (sp)
end

# tbfhgr -- get single-precision header parameter

procedure tbfhgr (tp, keyword, value)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to get
real	value		# o: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for getting the comment
int	status		# zero is OK
int	ip, junk, ctor()
errchk	tbferr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	status = 0

	# Get the value as a string.
	call fsgkey (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)
	if (status != 0)
	    call tbferr (status)

	ip = 1
	if (Memc[sval] == '\'')
	    ip = 2				# skip over the quote
	else if (Memc[sval] == 'T')
	    Memc[sval] = '1'
	else if (Memc[sval] == 'F')
	    Memc[sval] = '0'

	value = INDEFR
	junk = ctor (Memc[sval], ip, value)

	call sfree (sp)
end

# tbfhgi -- get integer header parameter

procedure tbfhgi (tp, keyword, value)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to get
int	value		# o: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for getting the comment
double	dval
int	status		# zero is OK
int	ip, junk, ctod()
errchk	tbferr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	status = 0

	# Get the value as a string.
	call fsgkey (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)
	if (status != 0)
	    call tbferr (status)

	ip = 1
	if (Memc[sval] == '\'')
	    ip = 2				# skip over the quote
	else if (Memc[sval] == 'T')
	    Memc[sval] = '1'
	else if (Memc[sval] == 'F')
	    Memc[sval] = '0'

	dval = INDEFD
	junk = ctod (Memc[sval], ip, dval)
	if (IS_INDEFD(dval))
	    value = INDEFI
	else
	    value = nint (dval)

	call sfree (sp)
end

# tbfhgb -- get Boolean header parameter
# If the header keyword is not T or F, then zero is interpreted as false,
# and any other numerical value is true.

procedure tbfhgb (tp, keyword, value)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to get
bool	value		# o: value of parameter
#--
pointer sp
pointer sval		# for getting the value as a string
pointer comment		# for getting the comment
double	dval
int	status		# zero is OK
int	ip, junk, ctod()
errchk	tbferr

begin
	call smark (sp)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (comment, SZ_LINE, TY_CHAR)

	status = 0

	# Get the value as a string.
	call fsgkey (TB_FILE(tp), keyword, Memc[sval], Memc[comment], status)
	if (status != 0)
	    call tbferr (status)

	call strupr (Memc[sval])

	ip = 1
	if (Memc[sval] == '\'')
	    ip = 2				# skip over the quote

	if (Memc[sval+ip-1] == 'T') {
	    value = true
	    call sfree (sp)
	    return
	} else if (Memc[sval+ip-1] == 'F') {
	    value = false
	    call sfree (sp)
	    return
	}

	dval = INDEFD
	junk = ctod (Memc[sval], ip, dval)
	if (IS_INDEFD(dval))
	    value = false
	else if (nint (dval) == 0)
	    value = false
	else
	    value = true

	call sfree (sp)
end

# tbfhgt -- get text-string header parameter

procedure tbfhgt (tp, keyword, text, maxch)

pointer tp		# i: pointer to table descriptor
char	keyword[ARB]	# i: name of parameter to get
char	text[ARB]	# o: value of parameter
int	maxch		# i: maximum number of characters to get
#--
pointer sp
pointer temp		# for getting the value
pointer comment		# for getting the comment
int	i
int	status		# zero is OK
int	strlen()
bool	tbhisc()
errchk	tbferr

begin
	call smark (sp)
	call salloc (temp, max (maxch, SZ_FNAME), TY_CHAR)
	call salloc (comment, SZ_FNAME, TY_CHAR)

	status = 0

	call fsgkys (TB_FILE(tp), keyword, Memc[temp], Memc[comment], status)
	if (status != 0)
	    call tbferr (status)

	# For COMMENT and HISTORY keywords, FITSIO returns the value in
	# the comment argument rather than the value argument.
	if (tbhisc (keyword))
	    call strcpy (Memc[comment], Memc[temp], SZ_FNAME)

	# Trim trailing blanks.
	do i = strlen (Memc[temp]), 1, -1 {
	    if (Memc[temp+i-1] == ' ')
		Memc[temp+i-1] = EOS
	    else
		break
	}
	call strcpy (Memc[temp], text, maxch)

	call sfree (sp)
end
