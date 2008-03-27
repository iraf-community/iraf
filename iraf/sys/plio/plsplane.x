# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_SETPLANE -- Set the 2-Dim plane to be referenced in calls to the pl_box,
# pl_circle, etc. geometric region masking operators.

procedure pl_setplane (pl, v)

pointer	pl			#I mask descriptor
long	v[ARB]			#I vector defining plane

size_t	sz_val

begin
	sz_val = PL_MAXDIM
	call amovl (v, PL_PLANE(pl,1), sz_val)
end
