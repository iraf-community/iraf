	subroutine tuiep3 (xa, ya, x, y, dy)
C
C  Evaluate a cubic polynomial interpolation function at X.
C       xa(klo) <= x <= xa(klo+1)
C  This routine was copied with slight modifications from the POLINT
C  subroutine in Numerical Recipes by Press, Flannery, Teukolsky and
C  Vetterling.
C
C  XA		i: array of four independent-variable values
C  YA		i: array of four dependent-variable values
C  X		i: the independent variable
C  Y		o: the result of interpolations
C  DY		o: an estimate of the error of interpolation
C
CH  Phil Hodge, 18-Apr-1988  Subroutine copied from Numerical Recipes POLINT.
C
	double precision xa(4), ya(4), x, y, dy
C--
	integer n
C					four-point interpolation
	parameter (n = 4)

	double precision c(n), d(n), dif, dift, den
	double precision ho, hp, w
	integer i, m, ns

	do 10 i = 1, n
	    if (x .eq. xa(i)) then
		y = ya(i)
		return
	    endif
 10	continue

	ns = 1
	dif = abs (x - xa(1))

	do 20 i = 1, n 
	    dift = abs (x - xa(i))
	    if (dift .lt. dif) then
		ns = i
		dif = dift
	    endif
	    c(i) = ya(i)
	    d(i) = ya(i)
 20	continue

	y = ya(ns)
	ns = ns - 1

	do 40 m = 1, n-1

	    do 30 i = 1, n-m
		ho = xa(i) - x
		hp = xa(i+m) - x
		w = c(i+1) - d(i)
		den = w / (ho - hp)
		d(i) = hp * den
		c(i) = ho * den
 30	    continue

	    if (2*ns .lt. n-m) then
		dy = c(ns+1)
	    else
		dy = d(ns)
		ns = ns - 1
	    endif
	    y = y + dy
 40	continue

	return
	end
