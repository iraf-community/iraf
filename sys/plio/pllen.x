# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>


# PL_LLEN -- Return the length of an encoded line list.

int procedure pl_llen (ll)

short	ll[ARB]			#I encoded line list

begin
	return (LL_LEN(ll))
end
