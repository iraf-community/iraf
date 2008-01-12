include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# Put a keyword and value into the table header.  It is an error (except
# for FITS tables) if the keyword does not already exist.
#
# Phil Hodge,  7-Aug-1987  Do not allow adding new parameter.
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  9-Mar-1989  Change dtype from char to int.
# Phil Hodge, 21-Jul-1992  Change format in tbhptd to %25.16g.
# Phil Hodge, 30-Mar-1995  Include keyword name in error message.
# Phil Hodge,  3-Apr-1995  Set TB_MODIFIED to true.
# Phil Hodge, 13-Jun-1995  Modify for FITS tables.

# tbhptd -- put double header parameter

procedure tbhptd (tp, keyword, value)

pointer tp			# i: Pointer to table descriptor
double	value			# i: Value of parameter
char	keyword[ARB]		# i: Name of parameter
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
bool	tbhisc()
errchk	tbhfkw, tbhpnp, tbfhpd

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (tbhisc (keyword))
	    call error (ER_TBDTYPECONFLICT,
		"tbhptd:  may not put numeric parameter as comment or history")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpd (tp, keyword, value)
	    TB_MODIFIED(tp) = true
	    return
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call tbhfkw (tp, keyword, parnum)			# find keyword
	if (parnum > 0) {
	    dtype = TY_DOUBLE
	    call sprintf (Memc[par], SZ_PARREC, "%-25.16g")
		call pargd (value)
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[par])	# put Nth param.
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
		"tbhptd:  `%s' not found; use tbhadd to add new parameter")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}

	TB_MODIFIED(tp) = true

	call sfree (sp)
end

# tbhptr -- put real header parameter

procedure tbhptr (tp, keyword, value)

pointer tp			# i: Pointer to table descriptor
real	value			# i: Value of parameter
char	keyword[ARB]		# i: Name of parameter
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
bool	tbhisc()
errchk	tbhfkw, tbhpnp, tbfhpr

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (tbhisc (keyword))
	    call error (ER_TBDTYPECONFLICT,
		"tbhptr:  may not put numeric parameter as comment or history")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpr (tp, keyword, value)
	    TB_MODIFIED(tp) = true
	    return
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call tbhfkw (tp, keyword, parnum)			# find keyword
	if (parnum > 0) {
	    dtype = TY_REAL
	    call sprintf (Memc[par], SZ_PARREC, "%-15.7g")
		call pargr (value)
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[par])	# put Nth param.
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
		"tbhptr:  `%s' not found; use tbhadr to add new parameter")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}

	TB_MODIFIED(tp) = true

	call sfree (sp)
end

# tbhpti -- put integer header parameter

procedure tbhpti (tp, keyword, value)

pointer tp			# i: Pointer to table descriptor
int	value			# i: Value of parameter
char	keyword[ARB]		# i: Name of parameter
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
bool	tbhisc()
errchk	tbhfkw, tbhpnp, tbfhpi

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (tbhisc (keyword))
	    call error (ER_TBDTYPECONFLICT,
		"tbhpti:  may not put numeric parameter as comment or history")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpi (tp, keyword, value)
	    TB_MODIFIED(tp) = true
	    return
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call tbhfkw (tp, keyword, parnum)			# find keyword
	if (parnum > 0) {
	    dtype = TY_INT
	    call sprintf (Memc[par], SZ_PARREC, "%-11d")
		call pargi (value)
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[par])	# put Nth param.
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
		"tbhpti:  `%s' not found; use tbhadi to add new parameter")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}

	TB_MODIFIED(tp) = true

	call sfree (sp)
end

procedure tbhptb (tp, keyword, value)

pointer tp			# i: Pointer to table descriptor
bool	value			# i: Value of parameter
char	keyword[ARB]		# i: Name of parameter
#--
pointer sp
pointer	par			# buffer for header record for parameter
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
int	intval			# buffer for writing value into string
bool	tbhisc()
errchk	tbhfkw, tbhpnp, tbfhpb

begin
	if (TB_READONLY(tp))
	    call error (ER_TBREADONLY, "can't write to table; it's readonly")
	if (tbhisc (keyword))
	    call error (ER_TBDTYPECONFLICT,
		"tbhptb:  may not put Boolean parameter as comment or history")

	if (TB_TYPE(tp) == TBL_TYPE_FITS) {
	    call tbfhpb (tp, keyword, value)
	    TB_MODIFIED(tp) = true
	    return
	}

	call smark (sp)
	call salloc (par, SZ_PARREC, TY_CHAR)

	call tbhfkw (tp, keyword, parnum)			# find keyword
	if (parnum > 0) {
	    dtype = TY_BOOL
	    if (value)
		intval = YES
	    else
		intval = NO
	    call sprintf (Memc[par], SZ_PARREC, "%-11d")
	    call pargi (intval)
	    call tbhpnp (tp, parnum, keyword, dtype, Memc[par])	# put Nth param.
	} else {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE,
		"tbhptb:  `%s' not found; use tbhadb to add new parameter")
		call pargstr (keyword)
	    call error (ER_TBPARNOTFND, Memc[errmess])
	}

	TB_MODIFIED(tp) = true

	call sfree (sp)
end

# tbhptt -- put character header parameter

procedure tbhptt (tp, keyword, text)

pointer tp			# i: Pointer to table descriptor
char	keyword[ARB]		# i: Name of parameter
char	text[ARB]		# i: Value of parameter
#--
pointer sp
pointer errmess			# scratch for possible error message
int	dtype			# data type
int	parnum			# parameter number (> 0 if found)
bool	tbhisc()		# true if keyword is comment or history
errchk	tbhfkw, tbhpnp, tbfhpt

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
	    call error (ER_TBMUSTADD,
		"use tbhadt, not tbhptt, to add new comment or history")
	} else {
	    call tbhfkw (tp, keyword, parnum)			# find keyword
	    if (parnum > 0) {
		call tbhpnp (tp, parnum, keyword, dtype, text)	# put Nth param.
	    } else {
		call smark (sp)
		call salloc (errmess, SZ_LINE, TY_CHAR)
		call sprintf (Memc[errmess], SZ_LINE,
		"tbhptt:  `%s' not found; use tbhadt to add new parameter")
		    call pargstr (keyword)
		call error (ER_TBPARNOTFND, Memc[errmess])
	    }
	}

	TB_MODIFIED(tp) = true
end
