# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PLL_EQUAL -- Compare two line lists for equality.

bool procedure pll_equal (l1, l2)

short	l1[ARB]			#I line list 1
short	l2[ARB]			#I line list 2

int	i
int	len1, len2

begin
	len1 = LL_LEN(l1)
	len2 = LL_LEN(l2)

	if (len1 != len2)
	    return (false)

	do i = LL_FIRST, len1
	    if (l1[i] != l2[i])
		return (false)

	return (true)
end
