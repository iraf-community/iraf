      subroutine l2err ( prfun , ftau , error )
c  from  * a practical guide to splines *  by c. de boor
c  this routine is to be called in the main program  l 2 m a i n .
calls subprogram  ppvalu(interv)
c  this subroutine computes various errors of the current l2-approxi-
c  mation , whose pp-repr. is contained in common block  approx  ,
c  to the given data contained in common block	data . it prints out
c  the average error  e r r l 1 , the l2-error	e r r l 2,  and the
c  maximum error  e r r m a x .
c
c******  i n p u t  ******
c  prfun  a hollerith string.  if prfun = 2hon, the routine prints out
c	   the value of the approximation as well as its error at
c	   every data point.
c
c******  o u t p u t  ******
c  ftau(1), ..., ftau(ntau),  with  ftau(i)  the approximation	f at
c	   tau(i), all i.
c  error(1), ..., error(ntau),	with  error(i) = scale*(g - f)
c	   at tau(i), all i. here,  s c a l e  equals  1. in case
c	   prfun .ne. 2hon , or the abs.error is greater than 100 some-
c	   where. otherwise, s c a l e	is such that the maximum of
c	   abs(error))	over all  i  lies between  10  and  100. this
c	   makes the printed output more illustrative.
c
      integer prfun,   ie,k,l,ll,ntau,on
      real ftau(1),error(1),  break,coef,err,errmax,errl1,errl2
     *			     ,gtau,scale,tau,totalw,weight
c     dimension ftau(ntau),error(ntau)
      real ppvalu
c     parameter lpkmax=100,ntmax=200,ltkmax=2000
c     common / data / ntau, tau(ntmax),gtau(ntmax),weight(ntmax),totalw
c     common /approx/ break(lpkmax),coef(ltkmax),l,k
      common / data / ntau, tau(200),gtau(200),weight(200),totalw
      common /approx/ break(100),coef(2000),l,k
      data on /2hon /
      errl1 = 0.
      errl2 = 0.
      errmax = 0.
      do 10 ll=1,ntau
	 ftau(ll) = ppvalu (break, coef, l, k, tau(ll), 0 )
	 error(ll) = gtau(ll) - ftau(ll)
	 err = abs(error(ll))
	 if (errmax .lt. err)	errmax = err
	 errl1 = errl1 + err*weight(ll)
   10	 errl2 = errl2 + err**2*weight(ll)
      errl1 = errl1/totalw
      errl2 = sqrt(errl2/totalw)
      print 615,errl2,errl1,errmax
  615 format(///21h least square error =,e20.6/
     1		21h average error      =,e20.6/
     2		21h maximum error      =,e20.6//)
      if (prfun .ne. on)		return
c     **  scale error curve and print  **
      ie = 0
      scale = 1.
      if (errmax .ge. 10.)		go to 18
      do 17 ie=1,9
	 scale = scale*10.
	 if (errmax*scale .ge. 10.)	go to 18
   17	 continue
   18 do 19 ll=1,ntau
   19	 error(ll) = error(ll)*scale
      print 620,ie,(ll,tau(ll),ftau(ll),error(ll),ll=1,ntau)
  620 format(///14x,36happroximation and scaled error curve/7x,
     110hdata point,7x,13happroximation,3x,16hdeviation x 10**,i1/
     2(i4,f16.8,f16.8,f17.6))
					return
      end
