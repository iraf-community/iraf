# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_GETPLANE -- Get the 2-Dim plane to be referenced in calls to the pl_box,
# pl_circle, etc. geometric region masking operators.

procedure pl_getplane (pl, v)

pointer	pl			#I mask descriptor
long	v[ARB]			#O vector defining plane

begin
	call amovl (PL_PLANE(pl,1), v, PL_MAXDIM)
end
