# AP_IJTOR -- Compute radius values given the center coordinates, the line
# number of the subraster and the length of the subraster x axis.

procedure ap_ijtor (r, nx, line, xc, yc)

real	r[nx]		# array of output r values
int	nx		# dimensions of output arrays
int	line		# line number
real	xc, yc		# subraster center

int	i
real	temp

begin
	temp = (line - yc ) ** 2
	do i = 1, nx
	    r[i] = sqrt ((i - xc) ** 2 + temp)
end
