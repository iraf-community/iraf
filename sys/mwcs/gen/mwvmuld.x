# MW_VMUL -- Vector multiply.

procedure mw_vmuld (a, b, c, ndim)

double	a[ndim,ndim]		#I input matrix
double	b[ndim]			#I input vector
double	c[ndim]			#O output vector
int	ndim			#I system dimension

int	i, j
double	v

begin
	do j = 1, ndim {
	    v = 0
	    do i = 1, ndim
		v = v + a[i,j] * b[i]
	    c[j] = v
	}
end
