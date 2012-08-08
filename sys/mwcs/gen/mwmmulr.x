# MW_MMUL -- Matrix multiply.

procedure mw_mmulr (a, b, c, ndim)

real	a[ndim,ndim]		#I left input matrix
real	b[ndim,ndim]		#I right input matrix
real	c[ndim,ndim]		#O output matrix
int	ndim			#I dimensionality of system

int	i, j, k
real	v

begin
	do j = 1, ndim
	    do i = 1, ndim {
		v = 0
		do k = 1, ndim
		    v = v + a[k,j] * b[i,k]
		c[i,j] = v
	    }
end
