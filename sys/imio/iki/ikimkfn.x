# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"iki.h"

# IKI_MKFNAME -- Manufacture a filename from the root and extension fields
# given.

procedure iki_mkfname (root, extn, fname, maxch)

char	root[ARB]		# root filename
char	extn[ARB]		# filename extension
char	fname[maxch]		# output filename
int	maxch			# max chars out

int	op
int	gstrcpy()
bool	fnullfile()

begin
	op = gstrcpy (root, fname, maxch) + 1
	if (extn[1] != EOS && !fnullfile (root)) {
	    fname[op] = '.'
	    op = op + 1
	    call strcpy (extn, fname[op], maxch-op+1)
	}
end
