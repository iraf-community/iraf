# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpex.h"
include	"qpio.h"

# QPIO_SETFILTER -- Modify the filter used to reject events during event
# extraction with qpio_getevents or qpio_readpix.  Possible items to be set
# here include the event attribute filter, region mask, and various QPIO
# parameters.  The input expression should be a comma delimited list of
# param=value terms, where PARAM is `filter', `mask', or the name of a QPIO
# or QPEX parameter, and where `value' is an expression, e.g., a comma
# delimited list of range terms enclosed in parenthesis.

procedure qpio_setfilter (io, expr)

pointer	io			#I QPIO descriptor
char	expr[ARB]		#I option setting expression

int	sz_filter
pointer	sp, filter, mask
errchk	qpio_parse, qpex_open, qpex_modfilter
int	qpex_modfilter(), qpio_parse()
pointer	qpex_open()

begin
	call smark (sp)
	call salloc (mask, SZ_FNAME, TY_CHAR)

	if (IO_DEBUG(io) > 0) {
	    call eprintf ("qpio_setfilter (%xX, `%s')\n")
		call pargi (io)
		call pargstr (expr)
	}

	# Parse full QPIO oriented filter expression.
	sz_filter = DEF_SZEXPRBUF
	call malloc (filter, sz_filter, TY_CHAR)
	if (qpio_parse (io,expr,filter,sz_filter,Memc[mask],SZ_FNAME) == ERR) {
	    call eprintf ("QPIO warning: error parsing `%s'\n")
		call pargstr (expr)
	}

	# Set event attribute filter.
	if (IO_EX(io) == NULL)
	    IO_EX(io) = qpex_open (IO_QP(io), Memc[filter])
	else if (qpex_modfilter (IO_EX(io), Memc[filter]) == ERR) {
	    call eprintf ("Warning: errors compiling `%s'\n")
		call pargstr (expr)
	}

	# Set region mask.
	if (Memc[mask] != EOS)
	    call qpio_loadmask (io, Memc[mask], NO)

	IO_ACTIVE(io) = NO

	call mfree (filter, TY_CHAR)
	call sfree (sp)
end
