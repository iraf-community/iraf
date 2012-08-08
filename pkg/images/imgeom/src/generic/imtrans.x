# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.



# IMTR2 -- Generic transpose.  The arrays need not be identical.

procedure imtr2s (a, b, nx, ny)

short	a[nx, ny], b[ny, nx]
int	nx, ny, x, y

begin
	do x = 1, nx
	   do y = 1, ny
	       b[y, x] = a[x, y]
end



# IMTR2 -- Generic transpose.  The arrays need not be identical.

procedure imtr2i (a, b, nx, ny)

int	a[nx, ny], b[ny, nx]
int	nx, ny, x, y

begin
	do x = 1, nx
	   do y = 1, ny
	       b[y, x] = a[x, y]
end



# IMTR2 -- Generic transpose.  The arrays need not be identical.

procedure imtr2l (a, b, nx, ny)

long	a[nx, ny], b[ny, nx]
int	nx, ny, x, y

begin
	do x = 1, nx
	   do y = 1, ny
	       b[y, x] = a[x, y]
end



# IMTR2 -- Generic transpose.  The arrays need not be identical.

procedure imtr2r (a, b, nx, ny)

real	a[nx, ny], b[ny, nx]
int	nx, ny, x, y

begin
	do x = 1, nx
	   do y = 1, ny
	       b[y, x] = a[x, y]
end



# IMTR2 -- Generic transpose.  The arrays need not be identical.

procedure imtr2d (a, b, nx, ny)

double	a[nx, ny], b[ny, nx]
int	nx, ny, x, y

begin
	do x = 1, nx
	   do y = 1, ny
	       b[y, x] = a[x, y]
end



# IMTR2 -- Generic transpose.  The arrays need not be identical.

procedure imtr2x (a, b, nx, ny)

complex	a[nx, ny], b[ny, nx]
int	nx, ny, x, y

begin
	do x = 1, nx
	   do y = 1, ny
	       b[y, x] = a[x, y]
end


