include "igi.h"

procedure arrow (gp, x, y, size, direction)

# Draw an arrow pointing right, left, up, or down

pointer	gp
real	x, y
real	size
int	direction

real	xsize, ysize

define	ARROW_VEC	7
define	NUM_DIREC	4

real	arrow_x[ARROW_VEC,NUM_DIREC]
data	arrow_x /0.0,  0.5,  1.0,  0.5,  0.5,  0.0,  1.0,	# Down
		 1.0,  1.5,  1.0,  1.5,  0.5,  0.5,  0.5,	# Right
		 0.0,  0.5,  1.0,  0.5,  0.5,  1.0,  0.0,	# Up
		 0.0, -0.5,  0.0, -0.5,  0.5,  0.5,  0.5/	# Left
real	arrow_y[ARROW_VEC,NUM_DIREC]
data	arrow_y /0.0, -0.5,  0.0, -0.5,  0.5,  0.5,  0.5,	# Down
		 0.0,  0.5,  1.0,  0.5,  0.5,  0.0,  1.0,	# Right
		 1.0,  1.5,  1.0,  1.5,  0.5,  0.5,  0.5,	# Up
		 0.0,  0.5,  1.0,  0.5,  0.5,  1.0,  0.0/	# Left

begin
	switch (direction) {
	case UP, DOWN:
	    xsize = size
	    ysize = 2.0 * size
	case LEFT, RIGHT:
	    xsize = 2.0 * size
	    ysize = size
	}

	call gumark (gp, 
	    arrow_x[1,direction], arrow_y[1,direction], ARROW_VEC, 
	    x, y, xsize, ysize, NO)
end
