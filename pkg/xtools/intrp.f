	subroutine intrp (itab, xtab, ytab, ntab, x, y, ierr)
c
c Interpolator using CODIM1 algorithm which is admittedly
c obscure  but works well.
c
c	itab - a label between 1 and 20 to identify the table and its
c              most recent search index
c	xtab - array of length ntab containing the x-values
c	ytab -                                     y-values
c	ntab - number of x,y pairs in the table
c	   x - independent for which a y-value is desired
c	   y - returned interpolated (or extrapolated) value
c	ierr - =0 for ok, -1 for extrapolation
c
	real xtab(ntab), ytab(ntab), x, y
	integer itab, ierr
	real t(4), u(4)
c	integer savind
c	data savind/-1/
c
c----- Only 1 pt in table
	if (ntab .eq. 1) then
		y = ytab(1)
		ierr = 0
		return
	endif
c
c-----
c Locate search index
	call srch (itab, x, xtab, ntab, index, ierr)
c	if (index .eq. savind) go to 2000
c	savind = index
c
c-----
c Set interpolator index flags
	i1 = 2
	i2 = 3
	iload = max0 (index-2, 1)
c
	if (ntab .gt. 2) then
		if (index.eq.   2) i2 = 4
c
		if (index.eq.ntab) i1 = 1
	endif
c
	if (index.gt.2 .and. index.lt.ntab) then
		i1 = 1
		i2 = 4
	endif
c-----
c Load interpolation arrays
	do 1000 i = i1, i2
		j = iload + (i-i1)
		t(i) = xtab (j)
		u(i) = ytab (j)
1000	continue
c
c-----
c Get interpolated value
2000	call codim1 (x, t, u, i1, i2, y)
	return
	end
c
c--------------------------------------------------------------
c
	subroutine srch (itab, x, xtab, ntab, index, ierr)
c
c Search table of x-values to bracket the desired interpolant, x
c
c The returned search index will be:
c	2 - if extrapolation below the table is required
c    ntab -                  above
c   index - points to value just above x in the table if bounded.
c
c The index is saved as a starting point for subsequent entries
c in an array indexed through 'itab' which serves to label the
c set of saved search indices. Itab may be between 1 and 20.
c
c	itab - The table identifier (1-20)
c	   x - The value for which an index is desired
c	xtab - The table containing the x-values (array of length ntab)
c	ntab - number of elements in the table
c      index - returned index into the table (points just above x)
c	ierr - 0 for ok, -1 for extrapolation
c
	integer insave(20), ntab, index, ind
	real xtab(ntab), x
c
c intialize insaved indices
	data insave/20*0/
c
c-----
c Determine direction of table, ascending or descending
	idir = sign (1.0, xtab(ntab) - xtab(1))
c
c-----
c Reset error flag
	ierr = 0
c
c-----
c Check for previous insaved index
	last = insave(itab)
	if (last .eq. 0 .or. last .gt. ntab) then
c
c-----
c no previous entry
		isrch = 1
c check for extrapolation
		if ((x-xtab(   1)) * idir .lt. 0.0) go to 2000
		if ((x-xtab(ntab)) * idir .gt. 0.0) go to 2100
	else
c
c-----
c previous entry left a valid index
		isrch = last
c
c check for still wihin bounds - difference from above should be opposite
c                                sign of difference from below
c
		if ((xtab(last)-x) * (xtab(last-1)-x) .lt. 0.0) then
			index = last
			return
		endif
	endif
c
c -----
c Begin searching - first determine direction
c
	if ((x - xtab(isrch)) * idir .gt. 0.0) then
c forward
		do 1100 i = isrch+1, ntab
			if ((x-xtab(i)) * idir .gt. 0.0) go to 1100
			go to 1500
1100		continue
c fall thru implies extrapolation required at high end
		go to 2100
	else
c
c-----
c negative direction search
		do 1200 i = isrch-1,1,-1
			if ((x-xtab(i)) * idir .lt. 0.0) go to 1200
			go to 1400
1200		continue
c fall through implies extrapolation at low end
		go to 2000
	endif
c
c-----
c point has been bounded
1400	index = i + 1
	go to 3000
1500	index = i
	go to 3000
c
c-----
c extrapolations
2000	index = 2
	ierr = -1
	go to 3000
2100	index = ntab
	ierr = -1
	go to 3000
c
c-----
c insave index
3000	insave(itab) = index
	return
c
c------
c Entry to reset saved index
	entry intrp0 (itab)
c
	insave(itab) = 0
	return
c
c-----
c Entry to return current index
	entry intrpi (itab, ind)
c
	ind = insave(itab)
	return
	end 
c
c-------------------------------------------------------------------
c
	subroutine codim1 (x, t, u, i1, i2, y)
c
c this subroutine performs an interposlation in a fashion
c not really understandable, but it works well.
c
c	x - input independent variable
c	t - array of 4 table independents surrounding x if possible
c	u - array of 4 table dependents corresponding to the t array
c
c  i1, i2 - indicators as follows:
c
c	    i1 = 1, i2 = 4  :  4 pts available in t and u arrays
c           i1 = 1, i2 = 3  :  3 pts available (x near right edge of table)
c	    i1 = 2, i2 = 4  :                  (x near left  edge of table)
c	    i1 = 2, i2 = 3  :  2 pts available
c           i1 = 3, i3 = 3  :  1 pt  available
c
c	y - output interpolated (or extrapolated) dependent value
c
	real t(4), u(4), x, y
	integer i1, i2
c
c variable xk affects the extrapolation procedure. a value of -1.0
c appears to be a reliable value.
c
	data xk/-1.0/
c
	v = x
c the following code is extracted from an original source
c
      a2=v-t(2)
      al=a2/(t(3)-t(2))
      s=al*u(3)+(1.-al)*u(2)
      if(i1.gt.1.and.i2.lt.4)goto1530
      a3=v-t(3)
      if(i1.gt.1)goto1185
1180  a1=v-t(1)
      c1=a2/(t(1)-t(2))*a3/(t(1)-t(3))
      c2=a1/(t(2)-t(1))*a3/(t(2)-t(3))
      c3=a1/(t(3)-t(1))*a2/(t(3)-t(2))
      p1=c1*u(1)+c2*u(2)+c3*u(3)
      if(i2.lt.4)goto1400
1185  a4=v-t(4)
      c4=a3/(t(2)-t(3))*a4/(t(2)-t(4))
      c5=a2/(t(3)-t(2))*a4/(t(3)-t(4))
      c6=a2/(t(4)-t(2))*a3/(t(4)-t(3))
      p2=c4*u(2)+c5*u(3)+c6*u(4)
      if(i1.eq.1)goto1500
1200  if(xk.lt.0.)goto1230
      xe=xk
      goto1260
1230  slope1=abs((u(4)-u(3))/(t(4)-t(3)))
      slope2=abs((u(3)-u(2))/(t(3)-t(2)))
      xe=1.0
      if(slope1+slope2.ne.0.)xe=1.-abs(slope1-slope2)/(slope1+slope2)
1260  p1=s+xe*(p2-s)
      goto1500
1400  if(xk.lt.0.)goto1430
      xe=xk
      goto1460
1430  slope1=abs((u(2)-u(1))/(t(2)-t(1)))
      slope2=abs((u(3)-u(2))/(t(3)-t(2)))
      xe=1.0
      if(slope1+slope2.ne.0.)xe=1.-abs(slope1-slope2)/(slope1+slope2)
1460  p2=s+xe*(p1-s)
1500  e1=abs(p1-s)
      e2=abs(p2-s)
      if(e1+e2.gt.0.)goto1560
1530  z=s
      goto1700
1560  bt=(e1*al)/(e1*al+(1.-al)*e2)
      z=bt*p2+(1.-bt)*p1
c
1700	y = z
	return
	end
c
c----------------------------------------------------------------------
c
	subroutine lintrp (itab, xtab, ytab, ntab, x, y, ierr)
c
c Linear interpolator with last index save
c
c Arguments are identical to INTRP, and uses the same index search
c scheme so that values for ITAB should not clash with calls
c to INTRP and LINTRP.
c
	real xtab(ntab), ytab(ntab), x , y
	integer itab, ierr
c
c----- Only 1 pt in table
	if (ntab .eq. 1) then
		y = ytab (1)
		ierr = 0
		return
	endif
c
c-----locate search index
	call srch (itab, x, xtab, ntab, index, ierr)
c
c----- index points just above x
	y = ytab(index-1) + (x - xtab(index-1)) * 
     1   (ytab(index) - ytab(index-1)) / (xtab(index) - xtab(index-1))
c
	return
	end
