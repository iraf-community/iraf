# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PLL_EQUAL -- Compare two line lists for equality.

bool procedure pll_equal (l1, l2)

short	l1[ARB]			#I line list 1
short	l2[ARB]			#I line list 2

int	i, off
int	l1_len, l1_first
int	l2_len, l2_first

begin
	# Support old format line lists.
	if (LL_OLDFORMAT(l1)) {
	    l1_len = OLL_LEN(l1)
	    l1_first = OLL_FIRST
	} else {
	    l1_len = LL_LEN(l1)
	    l1_first = LL_FIRST(l1)
	}

	# Support old format line lists.
	if (LL_OLDFORMAT(l2)) {
	    l2_len = OLL_LEN(l2)
	    l2_first = OLL_FIRST
	} else {
	    l2_len = LL_LEN(l2)
	    l2_first = LL_FIRST(l2)
	}

	if (l1_len != l2_len)
	    return (false)

	off = l2_first - l1_first
	do i = l1_first, l1_len
	    if (l1[i] != l2[i+off])
		return (false)

	return (true)
end
