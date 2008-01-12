      subroutine interv ( xt, lxt, x, left, mflag )
c  from  * a practical guide to splines *  by c. de boor
computes  left = max( i , 1 .le. i .le. lxt  .and.  xt(i) .le. x )  .
c
c******  i n p u t  ******
c  xt.....a real sequence, of length  lxt , assumed to be nondecreasing
c  lxt.....number of terms in the sequence  xt .
c  x.....the point whose location with respect to the sequence	xt  is
c	 to be determined.
c
c******  o u t p u t  ******
c  left, mflag.....both integers, whose value is
c
c   1	  -1	  if		   x .lt.  xt(1)
c   i	   0	  if   xt(i)  .le. x .lt. xt(i+1)
c  lxt	   1	  if  xt(lxt) .le. x
c
c	 in particular,  mflag = 0 is the 'usual' case.  mflag .ne. 0
c	 indicates that  x  lies outside the halfopen interval
c	 xt(1) .le. y .lt. xt(lxt) . the asymmetric treatment of the
c	 interval is due to the decision to make all pp functions cont-
c	 inuous from the right.
c
c******  m e t h o d  ******
c  the program is designed to be efficient in the common situation that
c  it is called repeatedly, with  x  taken from an increasing or decrea-
c  sing sequence. this will happen, e.g., when a pp function is to be
c  graphed. the first guess for  left  is therefore taken to be the val-
c  ue returned at the previous call and stored in the  l o c a l  varia-
c  ble	ilo . a first check ascertains that  ilo .lt. lxt (this is nec-
c  essary since the present call may have nothing to do with the previ-
c  ous call). then, if	xt(ilo) .le. x .lt. xt(ilo+1), we set  left =
c  ilo	and are done after just three comparisons.
c     otherwise, we repeatedly double the difference  istep = ihi - ilo
c  while also moving  ilo  and	ihi  in the direction of  x , until
c		       xt(ilo) .le. x .lt. xt(ihi) ,
c  after which we use bisection to get, in addition, ilo+1 = ihi .
c  left = ilo  is then returned.
c
      integer left,lxt,mflag,	ihi,ilo,istep,middle
      real x,xt(lxt)
      data ilo /1/
c     save ilo	(a valid fortran statement in the new 1977 standard)
      ihi = ilo + 1
      if (ihi .lt. lxt) 		go to 20
	 if (x .ge. xt(lxt))		go to 110
	 if (lxt .le. 1)		go to 90
	 ilo = lxt - 1
	 ihi = lxt
c
   20 if (x .ge. xt(ihi))		go to 40
      if (x .ge. xt(ilo))		go to 100
c
c	       **** now x .lt. xt(ilo) . decrease  ilo	to capture  x .
      istep = 1
   31	 ihi = ilo
	 ilo = ihi - istep
	 if (ilo .le. 1)		go to 35
	 if (x .ge. xt(ilo))		go to 50
	 istep = istep*2
					go to 31
   35 ilo = 1
      if (x .lt. xt(1)) 		go to 90
					go to 50
c	       **** now x .ge. xt(ihi) . increase  ihi	to capture  x .
   40 istep = 1
   41	 ilo = ihi
	 ihi = ilo + istep
	 if (ihi .ge. lxt)		go to 45
	 if (x .lt. xt(ihi))		go to 50
	 istep = istep*2
					go to 41
   45 if (x .ge. xt(lxt))		go to 110
      ihi = lxt
c
c	    **** now xt(ilo) .le. x .lt. xt(ihi) . narrow the interval.
   50 middle = (ilo + ihi)/2
      if (middle .eq. ilo)		go to 100
c     note. it is assumed that middle = ilo in case ihi = ilo+1 .
      if (x .lt. xt(middle))		go to 53
	 ilo = middle
					go to 50
   53	 ihi = middle
					go to 50
c**** set output and return.
   90 mflag = -1
      left = 1
					return
  100 mflag = 0
      left = ilo
					return
  110 mflag = 1
      left = lxt
					return
      end
