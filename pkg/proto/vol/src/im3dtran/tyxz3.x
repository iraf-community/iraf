

# TYXZ3 -- Generic 3d transpose, x->y, y->x, z->z.  The arrays need not be
# identical.

procedure tyxz3s (a, b, nx, ny, nz)

short	a[nx, ny, nz], b[ny, nx, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, x, z] = a[x, y, z]
end



# TYXZ3 -- Generic 3d transpose, x->y, y->x, z->z.  The arrays need not be
# identical.

procedure tyxz3i (a, b, nx, ny, nz)

int	a[nx, ny, nz], b[ny, nx, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, x, z] = a[x, y, z]
end



# TYXZ3 -- Generic 3d transpose, x->y, y->x, z->z.  The arrays need not be
# identical.

procedure tyxz3l (a, b, nx, ny, nz)

long	a[nx, ny, nz], b[ny, nx, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, x, z] = a[x, y, z]
end



# TYXZ3 -- Generic 3d transpose, x->y, y->x, z->z.  The arrays need not be
# identical.

procedure tyxz3r (a, b, nx, ny, nz)

real	a[nx, ny, nz], b[ny, nx, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, x, z] = a[x, y, z]
end



# TYXZ3 -- Generic 3d transpose, x->y, y->x, z->z.  The arrays need not be
# identical.

procedure tyxz3d (a, b, nx, ny, nz)

double	a[nx, ny, nz], b[ny, nx, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, x, z] = a[x, y, z]
end



# TYXZ3 -- Generic 3d transpose, x->y, y->x, z->z.  The arrays need not be
# identical.

procedure tyxz3x (a, b, nx, ny, nz)

complex	a[nx, ny, nz], b[ny, nx, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[y, x, z] = a[x, y, z]
end


