# AP_IJTOR2 -- Compute radius values given the center coordinates and the size
# of the subraster.

procedure ap_ijtor2 (r, nx, ny, xc, yc)

real	r[nx,ARB]	# array of output r values
int	nx		# x dimension of output array
int	ny		# Y dimension of output array
real	xc, yc		# subraster center

int	i, j
real	temp

begin
	do j = 1, ny {
	    temp = (j - yc) ** 2
	    do i = 1, nx
	        r[i,j] = sqrt ((i - xc) ** 2 + temp)
	}
end
