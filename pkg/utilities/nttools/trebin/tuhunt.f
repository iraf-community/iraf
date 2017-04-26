	subroutine tuhunt (xa, n, x, klo)
C
C  The array XA is searched for an element KLO such that
C       xa(klo) <= x <= xa(klo+1)
C  If X < XA(1) then KLO is set to zero; if X > XA(N) then KLO is set to N.
C  That is, KLO = 0 or N is a flag indicating X is out of bounds.
C
C  KLO must be set to an initial guess on input; it will then be replaced
C  by the correct value on output.
C
C  This routine was copied with some modifications from the HUNT
C  subroutine in Numerical Recipes by Press, Flannery, Teukolsky and
C  Vetterling.
C
C  N		i: number of elements in each array
C  XA		i: array of independent-variable values
C  X		i: the value to be bracketed by elements in XA
C  KLO		io: the lower index in XA that brackets X
C
CH  Phil Hodge, 14-Apr-1988  Subroutine copied from Numerical Recipes HUNT.
CH  Phil Hodge, 21-May-1996  Don't flag endpoints of XA as out of bounds.
C
	integer n, klo
	double precision xa(n), x
C--
	integer inc, km, khi
	logical ascnd

C	Set ASCND, and check for X out of bounds.

	if (xa(n).gt.xa(1)) then
	    ascnd = .true.
	    if (x .lt. xa(1)) then
		klo = 0
		return
	    else if (x .gt. xa(n)) then
		klo = n
		return
	    end if
	else
	    ascnd = .false.
	    if (x .gt. xa(1)) then
		klo = 0
		return
	    else if (x .lt. xa(n)) then
		klo = n
		return
	    end if
	end if

	if ((klo .le. 0) .or. (klo .gt. n)) then
	    klo = 1
	    khi = n
	    go to 3
	endif

	inc = 1
	if ((x.ge.xa(klo) .and. ascnd) .or.
     +      (x.lt.xa(klo) .and. .not.ascnd)) then
1	    khi = klo + inc
	    if (khi .gt. n) then
		khi = n + 1
	    else if ((x.ge.xa(khi) .and. ascnd) .or.
     +		     (x.lt.xa(khi) .and. .not.ascnd)) then
		klo = khi
		inc = inc + inc
		go to 1
	    endif
	else
	    khi = klo
2	    klo = khi - inc
	    if (klo .lt. 1) then
		klo = 0
	    else if ((x.lt.xa(klo) .and. ascnd) .or.
     +		     (x.ge.xa(klo) .and. .not.ascnd)) then
		khi = klo
		inc = inc + inc
		go to 2
	    endif
	endif

3	continue
C	Before we return, make sure we don't return a value of KLO that
C	implies X is out of bounds.  We know it isn't because we checked
C	at the beginning.
	if (khi-klo .eq. 1) then
	    klo = max (klo, 1)
	    klo = min (klo, n-1)
	    return
	end if

	km = (khi + klo) / 2

	if ((x .gt. xa(km) .and. ascnd) .or.
     +      (x .le. xa(km) .and. .not.ascnd)) then
	    klo = km
	else
	    khi = km
	endif

	go to 3

	end
