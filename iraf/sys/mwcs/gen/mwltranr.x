# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../mwcs.h"

# MW_LTRAN -- Perform a general N-dimensional linear transformation, i.e.,
# matrix multiply and translation.

procedure mw_ltranr (p1, p2, ltm, ltv, ndim)

real	p1[ndim]			#I input point
real	p2[ndim]			#O transformed output point
real	ltm[ndim,ndim]			#I linear transformation matrix
real	ltv[ndim]			#I linear translation vector
int	ndim				#I dimension of system

int	i, j
real	p3[MAX_DIM]

begin
	call amovr (p1, p3, ndim)
	do j = 1, ndim {
	    p2[j] = ltv[j]
	    do i = 1, ndim
		p2[j] = p2[j] + ltm[i,j] * p3[i]
	}
end
