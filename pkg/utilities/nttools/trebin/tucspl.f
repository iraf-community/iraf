	subroutine tucspl (xa, ya, n, work, y2)
C
C  Compute the second derivative of YA at each point in the array.
C  This is the initialization needed in preparation for interpolating
C  using cubic splines by the subroutine TUISPL.  Input and output
C  are all double precision.
C
C  This routine was copied with slight modifications from the SPLINE
C  subroutine in Numerical Recipes by Press, Flannery, Teukolsky and
C  Vetterling.
C
C  N		i: number of elements in each array
C  XA		i: array of independent-variable values
C  YA		i: array of dependent-variable values
C  WORK		io: scratch array used for work space
C  Y2		o: second derivative of YA at each point
C
CH  Phil Hodge, 14-Apr-1988  Subroutine copied from Numerical Recipes SPLINE.
C
	integer n
	double precision xa(n), ya(n), work(n), y2(n)
C--
	integer i
	double precision p, sig

C  These values (and y2(n) = 0) are for a "natural" spline.
	y2(1) = 0.
	work(1) = 0.
C
C  This is the decomposition loop of the tridiagonal algorithm.
C  Y2 and WORK are used for temporary storage of the decomposed factors.
C
	do 10 i = 2, n-1
	    sig = (xa(i) - xa(i-1)) / (xa(i+1) - xa(i-1))
	    p = sig * y2(i-1) + 2.
	    y2(i) = (sig - 1.) / p
	    work(i) = (6. * ((ya(i+1) - ya(i)) / (xa(i+1) - xa(i))
     +			- (ya(i) - ya(i-1)) / (xa(i) - xa(i-1))) /
     +			(xa(i+1) - xa(i-1)) - sig * work(i-1)) / p
 10	continue

C					"natural" spline
	y2(n) = 0.
C
C  This is the backsubstitution loop of the tridiagonal algorithm.
C
	do 20 i = n-1, 1, -1
	    y2(i) = y2(i) * y2(i+1) + work(i)
 20	continue

	return
	end
