# Evaluate a cubic polynomial interpolation function at X.
#     xa(klo) <= x <= xa(klo+1)
#
# Copyright (c) 2017 Ole Streicher
#
# References: 1. A. C. Aitken, On interpolation by iteration of
#                proportional parts, without the use of differences.
#                Proc. Edinburgh Math. Soc, v. 3,  1932, pp.  56-76.
#             2. E. H. Neville, Iterative interpolation.
#                Indian Math. Soc, v. 20, 1934, pp. 87-120
#
procedure tuiepn (n, xa, ya, x, y)

int	n	# i: polynomial degree
double	xa[n]	# i: array of n independent-variable values
double	ya[n]	# i: array of n dependent-variable values
double	x	# i: the independent variable
double	y	# o: the result of interpolation

int	i, j
pointer	sp, p

begin
    call smark(sp)
    call salloc(p, n, TY_DOUBLE)

    call achtdd(ya, Memd[p], n)
    do i=1, n-1 {
        do j=1, n-i {
	    Memd[p+j-1] = ((x - xa[i+j]) * Memd[p+j-1] - (x - xa[j]) * Memd[p+j]) / (xa[j] - xa[i+j])
	}
    }
    y = Memd[p]
    call sfree(sp)
end
