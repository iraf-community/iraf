	subroutine tuispl (xa, ya, y2, x, y)
C
C  Interpolate at point X using cubic splines.  The array Y2 must have
C  previously been computed by calling TUCSPL.  Note that XA, YA, Y2
C  are two-element subarrays of the arrays with the same names elsewhere.
C  Input and output are all double precision.
C  This routine was copied with slight modifications from the SPLINT
C  subroutine in Numerical Recipes by Press, Flannery, Teukolsky and
C  Vetterling.
C
C  XA		i: pair of independent-variable values
C  YA		i: pair of dependent-variable values
C  Y2		i: second derivatives of YA at each point
C  X		i: value at which spline is to be computed
C  Y		o: interpolated value at X
C
CH  Phil Hodge, 14-Apr-1988  Subroutine copied from Numerical Recipes SPLINT.
C
	double precision xa(2), ya(2), y2(2), x, y
C--
	double precision h, a, b

	h = xa(2) - xa(1)

	a = (xa(2) - x) / h
	b = (x - xa(1)) / h
	y = a * ya(1) + b * ya(2) +
     +		((a**3 - a) * y2(1) + (b**3 - b) * y2(2))
     +		* h * h / 6.

	return
	end
