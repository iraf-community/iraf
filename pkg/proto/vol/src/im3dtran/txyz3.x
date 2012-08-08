

# TXYZ3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txyz3s (a, b, nx, ny, nz)

short	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, z, y]
end



# TXYZ3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txyz3i (a, b, nx, ny, nz)

int	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, z, y]
end



# TXYZ3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txyz3l (a, b, nx, ny, nz)

long	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, z, y]
end



# TXYZ3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txyz3r (a, b, nx, ny, nz)

real	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, z, y]
end



# TXYZ3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txyz3d (a, b, nx, ny, nz)

double	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, z, y]
end



# TXYZ3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txyz3x (a, b, nx, ny, nz)

complex	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, z, y]
end


