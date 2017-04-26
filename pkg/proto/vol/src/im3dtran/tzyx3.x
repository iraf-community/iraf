

# TZYX3 -- Generic 3d transpose, x->z, y->y, z->x.  The arrays need not be
# identical.

procedure tzyx3s (a, b, nx, ny, nz)

short	a[nx, ny, nz], b[nz, ny, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, y, x] = a[x, y, z]
end



# TZYX3 -- Generic 3d transpose, x->z, y->y, z->x.  The arrays need not be
# identical.

procedure tzyx3i (a, b, nx, ny, nz)

int	a[nx, ny, nz], b[nz, ny, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, y, x] = a[x, y, z]
end



# TZYX3 -- Generic 3d transpose, x->z, y->y, z->x.  The arrays need not be
# identical.

procedure tzyx3l (a, b, nx, ny, nz)

long	a[nx, ny, nz], b[nz, ny, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, y, x] = a[x, y, z]
end



# TZYX3 -- Generic 3d transpose, x->z, y->y, z->x.  The arrays need not be
# identical.

procedure tzyx3r (a, b, nx, ny, nz)

real	a[nx, ny, nz], b[nz, ny, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, y, x] = a[x, y, z]
end



# TZYX3 -- Generic 3d transpose, x->z, y->y, z->x.  The arrays need not be
# identical.

procedure tzyx3d (a, b, nx, ny, nz)

double	a[nx, ny, nz], b[nz, ny, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, y, x] = a[x, y, z]
end



# TZYX3 -- Generic 3d transpose, x->z, y->y, z->x.  The arrays need not be
# identical.

procedure tzyx3x (a, b, nx, ny, nz)

complex	a[nx, ny, nz], b[nz, ny, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[z, y, x] = a[x, y, z]
end


