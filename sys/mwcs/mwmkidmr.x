# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MW_MKIDMR -- Make the identity matrix.

procedure mw_mkidmr (ltm, ndim)

real	ltm[ndim,ndim]		#O set to the identity matrix
int	ndim			#I dimension of (square) matrix

int	i, j

begin
	do j = 1, ndim {
	    do i = 1, ndim
		ltm[i,j] = 0.0
	    ltm[j,j] = 1.0
	}
end
