      subroutine colloc(aleft,aright,lbegin,iorder,ntimes,addbrk,relerr,ier)
c  from  * a practical guide to splines *  by c. de boor
chapter xv, example. solution of an ode by collocation.
calls colpnt, difequ(ppvalu(interv)), knots, eqblok(putit(difequ*,
c     bsplvd(bsplvb)))), slvblk(various subprograms), bsplpp(bsplvb*),
c     newnot
c
c******  i n p u t  ******
c  aleft, aright  endpoints of interval of approximation
c  lbegin   initial number of polynomial pieces in the approximation.
c	    a uniform breakpoint sequence is chosen.
c  iorder   order of polynomial pieces in the approximation
c  ntimes   number of passes through  n e w n o t  to be made
c  addbrk   the number (possibly fractional) of breaks to be added per
c	    pass through newnot. e.g., if addbrk = .33334, then a break-
c	    point will be added at every third pass through newnot.
c  relerr   a tolerance. newton iteration is stopped if the difference
c	    between the b-coeffs of two successive iterates is no more
c	    than  relerr*(absol.largest b-coefficient).
c
c******  p r i n t e d	 o u t p u t  ******
c     consists of the pp-representation of the approximate solution,
c     and of the error at selected points.
c
c******  m e t h o d  ******
c  the m-th order ordinary differential equation with  m  side condit-
c  ions, to be specified in subroutine	d i f e q u , is solved approx-
c  imately by collocation.
c  the approximation  f  to the solution  g  is pp of order  k+m  with
c  l  pieces and  m-1 continuous derivatives.  f  is determined by the
c  requirement that it satisfy the d.e. at  k  points per interval (to
c  be specified in  c o l p n t ) and the  m  side conditions.
c     this usually nonlinear system of equations for  f  is solved by
c  newton's method. the resulting linear system for the b-coeffs of an
c  iterate is constructed appropriately in  e q b l o k  and then solved
c  in  s l v b l k , a program designed to solve  a l m o s t  b l o c k
c  d i a g o n a l  linear systems efficiently.
c     there is an opportunity to attempt improvement of the breakpoint
c  sequence (both in number and location) through use of  n e w n o t .
c
c     parameter npiece=100, ndim=200, ncoef=2000, lenblk=2000
c     integer iorder,lbegin,ntimes,   i,iflag,ii,integs(3,npiece),iside
c    *			,iter,itermx,k,kpm,l,lnew,m,n,nbloks,nncoef,nt
      integer iorder,lbegin,ntimes,   i,iflag,ii,integs(3,100),iside
     *		  ,iter,itermx,k,kpm,l,lnew,m,n,nbloks,ncoef,nncoef,nt
c     real addbrk,aleft,aright,relerr,	 a(ndim),amax,asave(ndim)
c    *	   ,b(ndim),bloks(lenblk),break,coef,dx,err,rho,t(ndim)
c    *	   ,templ(lenblk),temps(ndim),xside
      real addbrk,aleft,aright,relerr,	 a(200),amax,asave(200)
     *	   ,b(200),bloks(2000),break,coef,dx,err,rho,t(200)
     *	   ,templ(2000),temps(200),xside
      equivalence (bloks,templ)
c     common /approx/ break(npiece), coef(ncoef), l,kpm
      common /approx/ break(100), coef(2000), l,kpm
      common /side/ m, iside, xside(10)
      common /other/ itermx,k,rho(19)
      data ncoef,lenblk / 2000,2000 /
c
      ier = 0
      kpm = iorder
      if (lbegin*kpm .gt. ncoef)	go to 999
c  *** set the various parameters concerning the particular dif.equ.
c     including a first approx. in case the de is to be solved by
c     iteration ( itermx .gt. 0) .
      call difequ (1, temps(1), temps )
c  *** obtain the  k  collocation points for the standard interval.
      k = kpm - m
      call colpnt ( k, rho )
c  *** the following five statements could be replaced by a read in or-
c     der to obtain a specific (nonuniform) spacing of the breakpnts.
      dx = (aright - aleft)/float(lbegin)
      temps(1) = aleft
      do 4 i=2,lbegin
    4	 temps(i) = temps(i-1) + dx
      temps(lbegin+1) = aright
c  *** generate, in knots, the required knots t(1),...,t(n+kpm).
      call knots ( temps, lbegin, kpm, t, n )
      nt = 1
c  *** generate the almost block diagonal coefficient matrix  bloks  and
c     right side  b  from collocation equations and side conditions.
c     then solve via  slvblk , obtaining the b-representation of the ap-
c     proximation in  t , a ,  n  , kpm  .
   10	 call eqblok(t,n,kpm,temps,a,bloks,lenblk,integs,nbloks,b)
	 call slvblk(bloks,integs,nbloks,b,temps,a,iflag)
	 iter = 1
	 if (itermx .le. 1)		go to 30
c  *** save b-spline coeff. of current approx. in  asave , then get new
c     approx. and compare with old. if coeff. are more than  relerr
c     apart (relatively) or if no. of iterations is less than  itermx ,
c     continue iterating.
   20	    call bsplpp(t,a,n,kpm,templ,break,coef,l)
	    do 25 i=1,n
   25	       asave(i) = a(i)
	    call eqblok(t,n,kpm,temps,a,bloks,lenblk,integs,nbloks,b)
	    call slvblk(bloks,integs,nbloks,b,temps,a,iflag)
	    err = 0.
	    amax = 0.
	    do 26 i=1,n
	       amax = amax1(amax,abs(a(i)))
   26	       err = amax1(err,abs(a(i)-asave(i)))
	    if (err .le. relerr*amax)	go to 30
	    iter = iter+1
	    if (iter .lt. itermx)	go to 20
c  *** iteration (if any) completed. print out approx. based on current
c     breakpoint sequence, then try to improve the sequence.
   30	 print 630,kpm,l,n,(break(i),i=2,l)
  630	 format(47h approximation from a space of splines of order,i3
     *	       ,4h on ,i3,11h intervals,/13h of dimension,i4
     *	       ,16h.  breakpoints -/(5e20.10))
 	 if (itermx .gt. 0)  print 635,iter,itermx
  635	 format(6h after,i3,3h of,i3,20h allowed iterations,)
	 call bsplpp(t,a,n,kpm,templ,break,coef,l)
 	 print 637
  637	 format(46h the pp representation of the approximation is)
 	 do 38 i=1,l
 	    ii = (i-1)*kpm
   38	    print 638, break(i),(coef(ii+j),j=1,kpm)
  638	 format(f9.3,e13.6,10e11.3)
c  *** the following call is provided here for possible further analysis
c     of the approximation specific to the problem being solved.
c     it is, of course, easily omitted.
 	 call difequ ( 4, temps(1), temps )
c
	 if (nt .gt. ntimes)		return
c  *** from the pp-rep. of the current approx., obtain in  newnot  a new
c     (and possibly better) sequence of breakpoints, adding (on the ave-
c     rage)  a d d b r k  breakpoints per pass through newnot.
	 lnew = lbegin + int(float(nt)*addbrk)
	 if (lnew*kpm .gt. ncoef)	go to 999
	 call newnot(break,coef,l,kpm,temps,lnew,templ)
	 call knots(temps,lnew,kpm,t,n)
	 nt = nt + 1
					go to 10
  999 nncoef = ncoef
      print 699,nncoef
  699 format(11h **********/23h the assigned dimension,i5
     *	    ,25h for  coef  is too small.)
 					return
      end
