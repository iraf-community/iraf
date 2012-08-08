# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"qpoe.h"

# QP_GMSYM -- Lookup the named macro in the symbol table and return a pointer
# to the symstruct describing the macro as the function value.  NULL is
# returned if the macro is not defined, or if the named symbol is not a macro.
# Local macros take precedence over global macros.  In the case of a local
# macro whose value is stored in the datafile, we have to allocate an internal
# buffer to hold the data after we exit; this data must be used promptly,
# before the routine is again called.

pointer procedure qp_gmsym (qp, macro, textp)

pointer	qp			#I QPOE descriptor
char	macro[ARB]		#I macro name
pointer	textp			#O char pointer to macro text

int	sz_textbuf, nchars, fd
pointer	st, sm, sym, textbuf
data	textbuf /NULL/, sz_textbuf /NULL/

int	fm_getfd(), read()
pointer	qm_symtab(), strefsbuf(), stfind()
errchk	realloc, fm_getfd, seek, read

begin
	st = QP_ST(qp)
	sm = qm_symtab (QP_QM(qp))

	# First look in the datafile local symbol table.  Macros are stored
	# in the datafile symbol table as string macros of type TY_MACRO.

	sym = stfind (st, macro)
	if (sym != NULL)
	    if (S_DTYPE(sym) == TY_MACRO)
		if (and (S_FLAGS(sym), SF_DELETED) == 0) {
		    if (S_LFILE(sym) > 0) {
			# Macro value stored as data.

			# Make sure the text buffer is large enough.
			if (sz_textbuf < S_NELEM(sym)) {
			    sz_textbuf = S_NELEM(sym)
			    call realloc (textbuf, sz_textbuf, TY_CHAR)
			}

			# Read the data.
			fd = fm_getfd (QP_FM(qp), S_LFILE(sym), READ_ONLY, 0)

			call seek (fd, S_OFFSET(sym))
			nchars = max (0, read (fd, Memc[textbuf], S_NELEM(sym)))
			Memc[textbuf+nchars] = EOS
			textp = textbuf

			call fm_retfd (QP_FM(qp), S_LFILE(sym))

		    } else {
			# Macro value stored in symbol table.
			textp = strefsbuf (st, S_OFFSET(sym))
		    }

		    # Exit if a local symbol was found.
		    return (sym)
		}

	# Next look in the global macro symbol table.
	sym = stfind (sm, macro)
	if (sym != NULL)
	    if (and (S_FLAGS(sym), SF_DELETED) == 0)
		textp = strefsbuf (sm, S_OFFSET(sym))
	    else
		sym = NULL

	return (sym)
end
