

# TXYZ3 -- Generic 3d transpose, x->x, y->y, z->z.  The arrays need not be
# identical.

procedure txyz3s (a, b, nx, ny, nz)

short	a[nx, ny, nz], b[nx, ny, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, y, z] = a[x, y, z]
end


# TXZY3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txzy3s (a, b, nx, ny, nz)

short	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, y, z]
end


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



# TXYZ3 -- Generic 3d transpose, x->x, y->y, z->z.  The arrays need not be
# identical.

procedure txyz3i (a, b, nx, ny, nz)

int	a[nx, ny, nz], b[nx, ny, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, y, z] = a[x, y, z]
end


# TXZY3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txzy3i (a, b, nx, ny, nz)

int	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, y, z]
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



# TXYZ3 -- Generic 3d transpose, x->x, y->y, z->z.  The arrays need not be
# identical.

procedure txyz3l (a, b, nx, ny, nz)

long	a[nx, ny, nz], b[nx, ny, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, y, z] = a[x, y, z]
end


# TXZY3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txzy3l (a, b, nx, ny, nz)

long	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, y, z]
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



# TXYZ3 -- Generic 3d transpose, x->x, y->y, z->z.  The arrays need not be
# identical.

procedure txyz3r (a, b, nx, ny, nz)

real	a[nx, ny, nz], b[nx, ny, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, y, z] = a[x, y, z]
end


# TXZY3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txzy3r (a, b, nx, ny, nz)

real	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, y, z]
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



# TXYZ3 -- Generic 3d transpose, x->x, y->y, z->z.  The arrays need not be
# identical.

procedure txyz3d (a, b, nx, ny, nz)

double	a[nx, ny, nz], b[nx, ny, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, y, z] = a[x, y, z]
end


# TXZY3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txzy3d (a, b, nx, ny, nz)

double	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, y, z]
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



# TXYZ3 -- Generic 3d transpose, x->x, y->y, z->z.  The arrays need not be
# identical.

procedure txyz3x (a, b, nx, ny, nz)

complex	a[nx, ny, nz], b[nx, ny, nz]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, y, z] = a[x, y, z]
end


# TXZY3 -- Generic 3d transpose, x->x, y->z, z->y.  The arrays need not be
# identical.

procedure txzy3x (a, b, nx, ny, nz)

complex	a[nx, ny, nz], b[nx, nz, ny]
int	nx, ny, nz, x, y, z

begin
	do x = 1, nx
	   do y = 1, ny
	       do z = 1, nz
		   b[x, z, y] = a[x, y, z]
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


