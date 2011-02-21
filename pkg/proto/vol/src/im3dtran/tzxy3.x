

# TZXY3 -- Generic 3d transpose, x->z, y->x, z->y.  The arrays need not be
# identical.

procedure tzxy3s (a, b, nx, ny, nz)

short	a[nx, ny, nz], b[nz, nx, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, x, y] = a[x, y, z]
end



# TZXY3 -- Generic 3d transpose, x->z, y->x, z->y.  The arrays need not be
# identical.

procedure tzxy3i (a, b, nx, ny, nz)

int	a[nx, ny, nz], b[nz, nx, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, x, y] = a[x, y, z]
end



# TZXY3 -- Generic 3d transpose, x->z, y->x, z->y.  The arrays need not be
# identical.

procedure tzxy3l (a, b, nx, ny, nz)

long	a[nx, ny, nz], b[nz, nx, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, x, y] = a[x, y, z]
end



# TZXY3 -- Generic 3d transpose, x->z, y->x, z->y.  The arrays need not be
# identical.

procedure tzxy3r (a, b, nx, ny, nz)

real	a[nx, ny, nz], b[nz, nx, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, x, y] = a[x, y, z]
end



# TZXY3 -- Generic 3d transpose, x->z, y->x, z->y.  The arrays need not be
# identical.

procedure tzxy3d (a, b, nx, ny, nz)

double	a[nx, ny, nz], b[nz, nx, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, x, y] = a[x, y, z]
end



# TZXY3 -- Generic 3d transpose, x->z, y->x, z->y.  The arrays need not be
# identical.

procedure tzxy3x (a, b, nx, ny, nz)

complex	a[nx, ny, nz], b[nz, nx, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, x, y] = a[x, y, z]
end


