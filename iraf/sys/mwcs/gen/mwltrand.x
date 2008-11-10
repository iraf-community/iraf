# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../mwcs.h"

# MW_LTRAN -- Perform a general N-dimensional linear transformation, i.e.,
# matrix multiply and translation.

procedure mw_ltrand (p1, p2, ltm, ltv, ndim)

double	p1[ndim]			#I input point
double	p2[ndim]			#O transformed output point
double	ltm[ndim,ndim]			#I linear transformation matrix
double	ltv[ndim]			#I linear translation vector
int	ndim				#I dimension of system

int	i, j
double	p3[MAX_DIM]

begin
	call amovd (p1, p3, ndim)
	do j = 1, ndim {
	    p2[j] = ltv[j]
	    do i = 1, ndim
		p2[j] = p2[j] + ltm[i,j] * p3[i]
	}
end
