# MW_VMUL -- Vector multiply.

procedure mw_vmulr (a, b, c, ndim)

real	a[ndim,ndim]		#I input matrix
real	b[ndim]			#I input vector
real	c[ndim]			#O output vector
int	ndim			#I system dimension

int	i, j
real	v

begin
	do j = 1, ndim {
	    v = 0
	    do i = 1, ndim
		v = v + a[i,j] * b[i]
	    c[j] = v
	}
end
