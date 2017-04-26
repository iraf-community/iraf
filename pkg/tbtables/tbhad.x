include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# Add a keyword and value into the table header.  If the keyword already
# exists the value will be replaced; otherwise, a new keyword will be added.
#
# Phil Hodge,  7-Aug-1987  Subroutine created.
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  9-Mar-1989  Change dtype from char to int.
# Phil Hodge, 21-Jul-1992  Change format in tbhadd to %25.16g.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 14-Jun-1995  Modify for FITS tables.

# tbhadd -- add double header parameter

procedure tbhadd (tp, keyword, value)

pointer tp			# i: Pointer to table descriptor
double	value			# i: Value of parameter
char	keyword[ARB]		# i: Name of parameter
#--
pointer sp
pointer	par			# buffer for header record for parameter
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
bool	tbhisc()
errchk	tbhfkw, tbhpnp, tbhanp, tbfhpd

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (tbhisc (keyword))
	    call error (ER_TBDTYPECONFLICT,
		"tbhadd:  can't put numeric parameter as comment or history")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpd (tp, keyword, value)
	    TB_MODIFIED(tp) = true
	    return
	}

	dtype = TY_DOUBLE
	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call sprintf (Memc[par], SZ_PARREC, "%-25.16g")
	    call pargd (value)
	call tbhfkw (tp, keyword, parnum)			# find keyword
	if (parnum > 0)
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[par])	# put Nth param.
	else
	    call tbhanp (tp, keyword, dtype, Memc[par], parnum)	# add new param.

	TB_MODIFIED(tp) = true

	call sfree (sp)
end

# tbhadr -- add real header parameter

procedure tbhadr (tp, keyword, value)

pointer tp			# i: Pointer to table descriptor
real	value			# i: Value of parameter
char	keyword[ARB]		# i: Name of parameter
#--
pointer sp
pointer	par			# buffer for header record for parameter
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
bool	tbhisc()
errchk	tbhfkw, tbhpnp, tbhanp, tbfhpr

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (tbhisc (keyword))
	    call error (ER_TBDTYPECONFLICT,
		"tbhadr:  can't put numeric parameter as comment or history")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpr (tp, keyword, value)
	    TB_MODIFIED(tp) = true
	    return
	}

	dtype = TY_REAL
	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call sprintf (Memc[par], SZ_PARREC, "%-15.7g")
	    call pargr (value)
	call tbhfkw (tp, keyword, parnum)			# find keyword
	if (parnum > 0)
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[par])	# put Nth param.
	else
	    call tbhanp (tp, keyword, dtype, Memc[par], parnum)	# add new param.

	TB_MODIFIED(tp) = true

	call sfree (sp)
end

# tbhadi -- add integer header parameter

procedure tbhadi (tp, keyword, value)

pointer tp			# i: Pointer to table descriptor
int	value			# i: Value of parameter
char	keyword[ARB]		# i: Name of parameter
#--
pointer sp
pointer	par			# buffer for header record for parameter
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
bool	tbhisc()
errchk	tbhfkw, tbhpnp, tbhanp, tbfhpi

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (tbhisc (keyword))
	    call error (ER_TBDTYPECONFLICT,
		"tbhadi:  can't put numeric parameter as comment or history")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpi (tp, keyword, value)
	    TB_MODIFIED(tp) = true
	    return
	}

	dtype = TY_INT
	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	call sprintf (Memc[par], SZ_PARREC, "%-11d")
	    call pargi (value)
	call tbhfkw (tp, keyword, parnum)			# find keyword
	if (parnum > 0)
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[par])	# put Nth param.
	else
	    call tbhanp (tp, keyword, dtype, Memc[par], parnum)	# add new param.

	TB_MODIFIED(tp) = true

	call sfree (sp)
end

# tbhadb -- add Boolean header parameter

procedure tbhadb (tp, keyword, value)

pointer tp			# i: Pointer to table descriptor
bool	value			# i: Value of parameter
char	keyword[ARB]		# i: Name of parameter
#--
pointer sp
pointer	par			# buffer for header record for parameter
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
int	intval			# buffer for writing value into string
bool	tbhisc()
errchk	tbhfkw, tbhpnp, tbhanp, tbfhpb

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (tbhisc (keyword))
	    call error (ER_TBDTYPECONFLICT,
		"tbhadb:  can't put Boolean parameter as comment or history")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpb (tp, keyword, value)
	    TB_MODIFIED(tp) = true
	    return
	}

	dtype = TY_BOOL
	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)
	if (value)
	    intval = YES
	else
	    intval = NO
	call sprintf (Memc[par], SZ_PARREC, "%-11d")
	    call pargi (intval)
	call tbhfkw (tp, keyword, parnum)			# find keyword
	if (parnum > 0)
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[par])	# put Nth param.
	else
	    call tbhanp (tp, keyword, dtype, Memc[par], parnum)	# add new param.

	TB_MODIFIED(tp) = true

	call sfree (sp)
end

# tbhadt -- add character header parameter

procedure tbhadt (tp, keyword, text)

pointer tp			# i: Pointer to table descriptor
char	keyword[ARB]		# i: Name of parameter
char	text[ARB]		# i: Value of parameter
#--
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
bool	tbhisc()		# true if keyword is comment or history
errchk	tbhfkw, tbhpnp, tbhanp, tbfhpt

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpt (tp, keyword, text)
	    TB_MODIFIED(tp) = true
	    return
	}

	dtype = TY_CHAR

	if (tbhisc (keyword)) {				# comment or history?
	    call tbhanp (tp, keyword, dtype, text, parnum)	# then add new
	} else {
	    call tbhfkw (tp, keyword, parnum)			# find keyword
	    if (parnum > 0)
		call tbhpnp (tp, parnum, keyword, dtype, text)	# put Nth param.
	    else
		call tbhanp (tp, keyword, dtype, text, parnum)	# add new param.
	}

	TB_MODIFIED(tp) = true
end
