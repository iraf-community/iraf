# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_EXPANDTEXT -- Copy a statement to the output, breaking it up into tokens
# and expanding any macro references in the process.  This is used to resolve
# macro references which might otherwise be repeatedly expanded, or which it
# might not be possible to expand if this is left to some future time when
# the referenced macros are no longer defined.

int procedure qp_expandtext (qp, s1, s2, maxch)

pointer	qp			#I QPOE descriptor
char	s1[ARB]			#I input string containing macros
char	s2[maxch]		#O output string buffer
int	maxch			#I max chars out

int	op
pointer	sp, tokbuf, in
int	gstrcpy(), qp_gettok()
pointer	qp_opentext()

begin
	call smark (sp)
	call salloc (tokbuf, SZ_TOKBUF, TY_CHAR)

	# Open input text for macro expanded token input.
	in = qp_opentext (qp, s1)
	op = 1

	# Copy tokens to the output, inserting a space after every token.
	while (qp_gettok (in, Memc[tokbuf], SZ_TOKBUF) != EOF) {
	    op = op + gstrcpy (Memc[tokbuf], s2[op], maxch-op+1)
	    s2[op] = ' ';  op = op + 1
	    if (op > maxch)
		break
	}

	# Cancel the trailing blank and add the EOS.
	if (op > 1)
	    op = op - 1
	s2[op] = EOS

	call qp_closetext (in)
	call sfree (sp)

	return (op - 1)
end
