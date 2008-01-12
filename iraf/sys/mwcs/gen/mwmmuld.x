# MW_MMUL -- Matrix multiply.

procedure mw_mmuld (a, b, c, ndim)

double	a[ndim,ndim]		#I left input matrix
double	b[ndim,ndim]		#I right input matrix
double	c[ndim,ndim]		#O output matrix
int	ndim			#I dimensionality of system

int	i, j, k
double	v

begin
	do j = 1, ndim
	    do i = 1, ndim {
		v = 0
		do k = 1, ndim
		    v = v + a[k,j] * b[i,k]
		c[i,j] = v
	    }
end
