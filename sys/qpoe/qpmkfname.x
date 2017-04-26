# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

# QP_MKFNAME -- Construct the poefile filename, i.e., eliminate any whitespace
# and add the given extension if omitted.

procedure qp_mkfname (poefile, extn, fname, maxch)

char	poefile[ARB]		#I raw poefile name
char	extn[ARB]		#I extension to be added if absent
char	fname[maxch]		#O output filename
int	maxch			#I max chars out

int	n
bool	strne()
int	nowhite()

begin
	n = nowhite (poefile, fname, maxch)
	if (n <= 3 || strne (fname[n-2], extn))
	    call strcpy (extn, fname[n+1], maxch-n)
end
