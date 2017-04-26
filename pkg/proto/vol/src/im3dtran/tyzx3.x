

# TYZX3 -- Generic 3d transpose, x->y, y->z, z->x.  The arrays need not be
# identical.

procedure tyzx3s (a, b, nx, ny, nz)

short	a[nx, ny, nz], b[ny, nz, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, z, x] = a[x, y, z]
end



# TYZX3 -- Generic 3d transpose, x->y, y->z, z->x.  The arrays need not be
# identical.

procedure tyzx3i (a, b, nx, ny, nz)

int	a[nx, ny, nz], b[ny, nz, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, z, x] = a[x, y, z]
end



# TYZX3 -- Generic 3d transpose, x->y, y->z, z->x.  The arrays need not be
# identical.

procedure tyzx3l (a, b, nx, ny, nz)

long	a[nx, ny, nz], b[ny, nz, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, z, x] = a[x, y, z]
end



# TYZX3 -- Generic 3d transpose, x->y, y->z, z->x.  The arrays need not be
# identical.

procedure tyzx3r (a, b, nx, ny, nz)

real	a[nx, ny, nz], b[ny, nz, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, z, x] = a[x, y, z]
end



# TYZX3 -- Generic 3d transpose, x->y, y->z, z->x.  The arrays need not be
# identical.

procedure tyzx3d (a, b, nx, ny, nz)

double	a[nx, ny, nz], b[ny, nz, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, z, x] = a[x, y, z]
end



# TYZX3 -- Generic 3d transpose, x->y, y->z, z->x.  The arrays need not be
# identical.

procedure tyzx3x (a, b, nx, ny, nz)

complex	a[nx, ny, nz], b[ny, nz, nx]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, z, x] = a[x, y, z]
end


