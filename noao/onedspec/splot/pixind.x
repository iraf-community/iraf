# PIXIND -- Compute pixel index

procedure pixind (x1, x2, dx, valx, i1)

real	x1, x2, dx, valx
int	i1

begin
#	i1 = aint ((valx-x1)/dx +0.5) + 1
	i1 = (valx - x1) / dx + 1.5
end
